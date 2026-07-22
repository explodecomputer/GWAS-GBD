<template>
  <section class="opportunity-table-section" aria-label="Low attention country-condition outcomes">
    <div class="table-controls">
      <div class="filter-row">
        <div class="filter-field">
          <input
            id="country-search"
            class="filter-input"
            type="search"
            list="country-options"
            placeholder="Country search"
            aria-label="Country search"
            :value="countryFilter"
            @input="setCountryFilter($event.target.value)"
            data-testid="country-search"
          />
          <datalist id="country-options">
            <option v-for="c in countries" :key="c.location_id" :value="c.location_name" />
          </datalist>
        </div>

        <div class="filter-field filter-field-wide">
          <input
            id="condition-search"
            class="filter-input filter-input-wide"
            type="search"
            list="condition-options"
            placeholder="Condition search"
            aria-label="Condition search"
            :value="conditionFilter"
            @input="setConditionFilter($event.target.value)"
            data-testid="condition-search"
          />
          <datalist id="condition-options">
            <option v-for="c in conditions" :key="c.cause_id" :value="c.cause_name" />
          </datalist>
        </div>

        <span class="row-count" data-testid="opportunity-row-count">
          {{ filtered.length.toLocaleString() }} low attention conditions
        </span>
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
      <table
        ref="tableRef"
        class="opp-table display"
        data-testid="opportunity-table"
        @click="onTableClick"
      >
        <thead>
          <tr>
            <th>Country</th>
            <th>Condition</th>
            <th>Mismatch share</th>
            <th>Burden share</th>
            <th>GWAS attention share</th>
            <th>DALYs</th>
            <th class="col-flag">Zero attention</th>
          </tr>
        </thead>
        <tbody></tbody>
        <tfoot>
          <tr>
            <th><input class="column-search" type="search" aria-label="Search country column" /></th>
            <th><input class="column-search" type="search" aria-label="Search condition column" /></th>
            <th><input class="column-search" type="search" aria-label="Search mismatch share column" /></th>
            <th><input class="column-search" type="search" aria-label="Search burden share column" /></th>
            <th><input class="column-search" type="search" aria-label="Search GWAS attention share column" /></th>
            <th><input class="column-search" type="search" aria-label="Search DALYs column" /></th>
            <th><input class="column-search" type="search" aria-label="Search zero attention column" /></th>
          </tr>
        </tfoot>
      </table>
    </div>
  </section>
</template>

<script setup>
import { computed, ref } from 'vue'
import { fmtPct, fmtDalys, mismatchClass } from '../lib/fmt.js'
import { useDataTable } from '../lib/useDataTable.js'

const props = defineProps({
  opportunities: { type: Array, default: () => [] },
  countries: { type: Array, default: () => [] },
  conditions: { type: Array, default: () => [] },
  countryFilter: { type: String, default: '' },
  conditionFilter: { type: String, default: '' },
})

const emit = defineEmits(['open-country', 'update:countryFilter', 'update:conditionFilter'])
const tableRef = ref(null)

const base2023 = computed(() => props.opportunities.filter(o => o.year === 2023))

const normalize = value => String(value ?? '').trim().toLocaleLowerCase()
const contains = (value, query) => normalize(value).includes(normalize(query))

const filtered = computed(() => {
  let rows = base2023.value
  if (props.countryFilter) rows = rows.filter(r => contains(r.location_name, props.countryFilter))
  if (props.conditionFilter) rows = rows.filter(r => contains(r.cause_name, props.conditionFilter))
  return rows
})

const maxMismatch = computed(() => Math.max(...base2023.value.map(r => r.mismatch_share), 0.01))
const barWidth = ms => Math.round((ms / maxMismatch.value) * 100) + '%'
const escapeHtml = value =>
  String(value ?? '')
    .replaceAll('&', '&amp;')
    .replaceAll('<', '&lt;')
    .replaceAll('>', '&gt;')
    .replaceAll('"', '&quot;')
    .replaceAll("'", '&#39;')

function setCountryFilter(value) {
  emit('update:countryFilter', value)
}

function setConditionFilter(value) {
  emit('update:conditionFilter', value)
}

function filterToCondition(row) {
  setCountryFilter('')
  setConditionFilter(row.cause_name)
}

function onTableClick(event) {
  const conditionButton = event.target.closest('.condition-link')
  if (conditionButton) {
    const causeId = Number(conditionButton.dataset.causeId)
    const row = filtered.value.find(item => item.cause_id === causeId)
    if (row) filterToCondition(row)
    return
  }

  const rowEl = event.target.closest('tr[data-location-id][data-cause-id]')
  if (!rowEl) return

  emit('open-country', {
    locationId: Number(rowEl.dataset.locationId),
    causeId: Number(rowEl.dataset.causeId),
  })
}

useDataTable(tableRef, filtered, () => ({
  data: filtered.value,
  columns: [
    {
      data: 'location_name',
      render(data, type) {
        if (type !== 'display') return data
        return `<span class="country-link-cell">${escapeHtml(data)}</span>`
      },
    },
    {
      data: 'cause_name',
      render(data, type, row) {
        if (type !== 'display') return data
        return `<button type="button" class="condition-link" data-testid="condition-filter-link" data-cause-id="${row.cause_id}">${escapeHtml(data)}</button>`
      },
    },
    {
      data: 'mismatch_share',
      render(data, type) {
        if (type !== 'display') return data
        return `<span class="mismatch-bar-wrap"><span class="mismatch-bar ${mismatchClass(data)}" style="width:${barWidth(data)}"></span><span class="mismatch-label">${fmtPct(data)}</span></span>`
      },
    },
    {
      data: 'burden_share',
      render(data, type) {
        return type === 'display' ? fmtPct(data) : data
      },
    },
    {
      data: 'attention_share',
      render(data, type) {
        return type === 'display' ? fmtPct(data) : data
      },
    },
    {
      data: 'dalys',
      render(data, type) {
        return type === 'display' ? fmtDalys(data) : data
      },
    },
    {
      data: 'zero_attention',
      render(data, type) {
        if (type === 'filter') return data ? 'yes zero attention' : 'no'
        return type === 'display' && data
          ? '<span class="zero-badge" title="No mapped GWAS attention">●</span>'
          : ''
      },
    },
  ],
  order: [[2, 'desc']],
  columnDefs: [
    { targets: [2, 3, 4, 5], className: 'dt-body-right dt-head-right' },
    { targets: [6], className: 'dt-body-center dt-head-center' },
  ],
  createdRow(row, data) {
    row.classList.add('opp-row')
    if (data.zero_attention) row.classList.add('zero-attn')
    row.setAttribute('data-testid', 'opportunity-row')
    row.dataset.locationId = data.location_id
    row.dataset.causeId = data.cause_id
  },
}))
</script>
