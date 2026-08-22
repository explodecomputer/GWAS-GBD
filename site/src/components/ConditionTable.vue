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
    <dl class="column-definitions" aria-label="Column definitions">
      <div><dt>Condition:</dt><dd>GBD study term</dd></div>
      <div><dt>Mismatch share:</dt><dd>burden share minus GWAS attention share</dd></div>
      <div><dt>Burden share:</dt><dd>share of the country's disease burden</dd></div>
      <div><dt>GWAS attention share:</dt><dd>share of mapped global GWAS attention</dd></div>
      <div><dt>Zero attention:</dt><dd>no mapped GWAS attention</dd></div>
    </dl>

    <div v-if="rows.length === 0" class="empty-state">No under-attended conditions found.</div>

    <div v-else class="table-scroll">
      <table
        ref="tableRef"
        class="cond-table display"
        data-testid="condition-table"
        @click="onTableClick"
      >
        <thead>
          <tr>
            <th>Condition</th>
            <th class="num-th">Mismatch share</th>
            <th class="num-th">Burden share</th>
            <th class="num-th">GWAS attention share</th>
            <th class="num-th">Zero attention</th>
          </tr>
        </thead>
        <tbody></tbody>
        <tfoot>
          <tr>
            <th><input class="column-search" type="search" aria-label="Search condition column" /></th>
            <th><input class="column-search" type="search" aria-label="Search mismatch share column" /></th>
            <th><input class="column-search" type="search" aria-label="Search burden share column" /></th>
            <th><input class="column-search" type="search" aria-label="Search GWAS attention share column" /></th>
            <th><input class="column-search" type="search" aria-label="Search zero attention column" /></th>
          </tr>
        </tfoot>
      </table>
    </div>
  </section>
</template>

<script setup>
import { computed, ref } from 'vue'
import { fmtPct, mismatchClass } from '../lib/fmt.js'
import { useDataTable } from '../lib/useDataTable.js'

const props = defineProps({
  conditions:  { type: Array, default: () => [] },  // all conditions for country+year
  highlightId: { type: Number, default: null },
})
const emit = defineEmits(['click-condition'])
const tableRef = ref(null)

const rows = computed(() =>
  [...props.conditions]
    .filter(c => c.burden_share > c.attention_share)
    .sort((a, b) => b.mismatch_share - a.mismatch_share)
)

const maxMismatch = computed(() => Math.max(...rows.value.map(r => r.mismatch_share), 0.01))
const barWidth = (ms) => Math.round((ms / maxMismatch.value) * 100) + '%'
const escapeHtml = value =>
  String(value ?? '')
    .replaceAll('&', '&amp;')
    .replaceAll('<', '&lt;')
    .replaceAll('>', '&gt;')
    .replaceAll('"', '&quot;')
    .replaceAll("'", '&#39;')

function onTableClick(event) {
  const rowEl = event.target.closest('tr[data-cause-id]')
  if (!rowEl) return
  emit('click-condition', Number(rowEl.dataset.causeId))
}

useDataTable(tableRef, () => [rows.value, props.highlightId], () => ({
  data: rows.value,
  columns: [
    {
      data: 'cause_name',
      render(data) {
        return escapeHtml(data)
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
      data: 'zero_attention',
      render(data, type) {
        if (type === 'filter') return data ? 'yes zero attention' : 'no'
        return type === 'display' && data
          ? '<span class="zero-badge" title="No mapped GWAS attention">●</span>'
          : ''
      },
    },
  ],
  order: [[1, 'desc']],
  columnDefs: [
    { targets: [1, 2, 3, 4], className: 'dt-body-right dt-head-right' },
  ],
  createdRow(row, data) {
    row.classList.add('cond-row')
    if (data.cause_id === props.highlightId) {
      row.classList.add('cond-highlighted')
      row.dataset.highlighted = 'true'
    }
    if (data.zero_attention) row.classList.add('zero-attn')
    row.dataset.causeId = data.cause_id
    row.style.cursor = 'pointer'
  },
}))
</script>
