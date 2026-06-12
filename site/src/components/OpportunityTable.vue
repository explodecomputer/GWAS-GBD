<template>
  <section class="opportunity-table-section" aria-label="Low attention country-condition outcomes">
    <div class="table-controls">
      <div class="filter-row">
        <label class="filter-label">
          Country
          <select
            class="filter-select"
            :value="countryFilter"
            @change="$emit('update:countryFilter', $event.target.value)"
            data-testid="country-filter"
          >
            <option value="">All countries</option>
            <option v-for="c in countries" :key="c.location_id" :value="c.location_name">
              {{ c.location_name }}
            </option>
          </select>
        </label>

        <label class="filter-label">
          Condition
          <select
            class="filter-select"
            :value="conditionFilter"
            @change="$emit('update:conditionFilter', $event.target.value)"
            data-testid="condition-filter"
          >
            <option value="">All conditions</option>
            <option v-for="c in conditions" :key="c.cause_id" :value="c.cause_name">
              {{ c.cause_name }}
            </option>
          </select>
        </label>

        <span class="row-count">{{ filtered.length.toLocaleString() }} low attention conditions</span>
      </div>
    </div>
    <dl class="column-definitions" aria-label="Column definitions">
      <div><dt>Country:</dt><dd>GBD admin0 location</dd></div>
      <div><dt>Condition:</dt><dd>GBD study term</dd></div>
      <div><dt>Mismatch share:</dt><dd>burden share minus GWAS attention share</dd></div>
      <div><dt>Burden share:</dt><dd>share of the country's DALYs</dd></div>
      <div><dt>GWAS attention share:</dt><dd>share of mapped global GWAS attention</dd></div>
      <div><dt>DALYs:</dt><dd>healthy life years lost</dd></div>
      <div><dt>Zero attention:</dt><dd>no mapped GWAS attention</dd></div>
    </dl>

    <div class="table-scroll">
      <table class="opp-table" data-testid="opportunity-table">
        <thead>
          <tr>
            <th @click="setSort('location_name')" :class="thClass('location_name')">Country</th>
            <th @click="setSort('cause_name')"    :class="thClass('cause_name')">Condition</th>
            <th @click="setSort('mismatch_share')" :class="thClass('mismatch_share')">Mismatch share</th>
            <th @click="setSort('burden_share')"  :class="thClass('burden_share')">Burden share</th>
            <th @click="setSort('attention_share')" :class="thClass('attention_share')">GWAS attention share</th>
            <th @click="setSort('dalys')"          :class="thClass('dalys')">DALYs</th>
            <th class="col-flag">Zero attention</th>
          </tr>
        </thead>
        <tbody>
          <tr
            v-for="row in paginated"
            :key="row.location_id + '-' + row.cause_id"
            class="opp-row"
            :class="{ 'zero-attn': row.zero_attention }"
            @click="$emit('open-country', { locationId: row.location_id, causeId: row.cause_id })"
            data-testid="opportunity-row"
          >
            <td class="country-link-cell">{{ row.location_name }}</td>
            <td class="condition-cell">
              <button
                type="button"
                class="condition-link"
                data-testid="condition-filter-link"
                @click.stop="filterToCondition(row)"
              >
                {{ row.cause_name }}
              </button>
            </td>
            <td class="num-cell">
              <span class="mismatch-bar-wrap">
                <span
                  class="mismatch-bar"
                  :style="{ width: barWidth(row.mismatch_share) }"
                  :class="mismatchClass(row.mismatch_share)"
                ></span>
                <span class="mismatch-label">{{ fmtPct(row.mismatch_share) }}</span>
              </span>
            </td>
            <td class="num-cell">{{ fmtPct(row.burden_share) }}</td>
            <td class="num-cell">{{ fmtPct(row.attention_share) }}</td>
            <td class="num-cell">{{ fmtDalys(row.dalys) }}</td>
            <td class="flag-cell">
              <span v-if="row.zero_attention" class="zero-badge" title="No mapped GWAS attention">●</span>
            </td>
          </tr>
        </tbody>
      </table>
    </div>

    <div class="pagination" v-if="pages > 1">
      <button :disabled="page === 0" @click="page--">‹ Prev</button>
      <span>Page {{ page + 1 }} / {{ pages }}</span>
      <button :disabled="page >= pages - 1" @click="page++">Next ›</button>
    </div>
  </section>
</template>

<script setup>
import { ref, computed } from 'vue'
import { fmtPct, fmtDalys, mismatchClass } from '../lib/fmt.js'

const PAGE_SIZE = 50

const props = defineProps({
  opportunities: { type: Array, default: () => [] },
  countries:     { type: Array, default: () => [] },
  conditions:    { type: Array, default: () => [] },
  countryFilter:   { type: String, default: '' },
  conditionFilter: { type: String, default: '' },
  sortBy:  { type: String, default: 'mismatch_share' },
  sortDir: { type: String, default: 'desc' },
})

const emit = defineEmits(['open-country', 'update:countryFilter', 'update:conditionFilter', 'sort'])

const page = ref(0)

// Only show 2023 opportunities in the main table (default year)
const base2023 = computed(() => props.opportunities.filter(o => o.year === 2023))

const filtered = computed(() => {
  let rows = base2023.value
  if (props.countryFilter)   rows = rows.filter(r => r.location_name === props.countryFilter)
  if (props.conditionFilter) rows = rows.filter(r => r.cause_name === props.conditionFilter)
  return rows
})

const sorted = computed(() => {
  const key = props.sortBy
  const dir = props.sortDir === 'asc' ? 1 : -1
  return [...filtered.value].sort((a, b) => {
    const av = a[key], bv = b[key]
    if (typeof av === 'string') return dir * av.localeCompare(bv)
    return dir * (av - bv)
  })
})

const pages   = computed(() => Math.ceil(sorted.value.length / PAGE_SIZE))
const paginated = computed(() => sorted.value.slice(page.value * PAGE_SIZE, (page.value + 1) * PAGE_SIZE))

const maxMismatch = computed(() => Math.max(...base2023.value.map(r => r.mismatch_share), 0.01))
const barWidth = (ms) => Math.round((ms / maxMismatch.value) * 100) + '%'

function setSort(col) {
  const dir = props.sortBy === col && props.sortDir === 'desc' ? 'asc' : 'desc'
  page.value = 0
  emit('sort', { by: col, dir })
}

function filterToCondition(row) {
  page.value = 0
  emit('update:countryFilter', '')
  emit('update:conditionFilter', row.cause_name)
  emit('sort', { by: 'mismatch_share', dir: 'desc' })
}

function thClass(col) {
  return ['th-sortable', props.sortBy === col ? 'th-active' : '', props.sortBy === col ? 'th-' + props.sortDir : '']
}
</script>
