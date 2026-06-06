<!--
  Table of under-attended conditions for the country story.
  Highlights the selected condition if one is active.
-->
<template>
  <section class="condition-table-section" aria-label="Under-attended conditions">
    <h3 class="section-title">Under-attended conditions</h3>
    <p class="section-desc">
      Conditions where disease burden share exceeds global GWAS attention share.
    </p>

    <div v-if="rows.length === 0" class="empty-state">No under-attended conditions found.</div>

    <div v-else class="table-scroll">
      <table class="cond-table" data-testid="condition-table">
        <thead>
          <tr>
            <th>Condition</th>
            <th class="num-th">Mismatch share</th>
            <th class="num-th">Burden share</th>
            <th class="num-th">Attention share</th>
            <th class="num-th">DALYs</th>
            <th class="num-th">Zero attention</th>
          </tr>
        </thead>
        <tbody>
          <tr
            v-for="row in rows"
            :key="row.cause_id"
            :class="['cond-row', row.cause_id === highlightId ? 'cond-highlighted' : '', row.zero_attention ? 'zero-attn' : '']"
            @click="$emit('click-condition', row.cause_id)"
            style="cursor:pointer"
            :data-highlighted="row.cause_id === highlightId ? 'true' : undefined"
          >
            <td>{{ row.cause_name }}</td>
            <td class="num-cell">{{ fmtPct(row.mismatch_share) }}</td>
            <td class="num-cell">{{ fmtPct(row.burden_share) }}</td>
            <td class="num-cell">{{ fmtPct(row.attention_share) }}</td>
            <td class="num-cell">{{ fmtDalys(row.dalys) }}</td>
            <td class="num-cell">
              <span v-if="row.zero_attention" class="zero-badge" title="No mapped GWAS attention">●</span>
            </td>
          </tr>
        </tbody>
      </table>
    </div>
  </section>
</template>

<script setup>
import { computed } from 'vue'
import { fmtPct, fmtDalys } from '../lib/fmt.js'

const props = defineProps({
  conditions:  { type: Array, default: () => [] },  // all conditions for country+year
  highlightId: { type: Number, default: null },
})
defineEmits(['click-condition'])

const rows = computed(() =>
  [...props.conditions]
    .filter(c => c.burden_share > c.attention_share)
    .sort((a, b) => b.mismatch_share - a.mismatch_share)
)
</script>
