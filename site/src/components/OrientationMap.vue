<!--
  Cross-country orientation panel.
  Shows ALL countries in a scrollable sidebar with a three-way sort toggle.
  Clicking a bar navigates directly to the country story.
-->
<template>
  <section class="orientation-panel" aria-label="Cross-country orientation">
    <h2 class="panel-title">Country orientation</h2>
    <div class="sort-toggle">
      <button :class="{ active: sortMode === 'burden' }"    @click="emit('update:sortMode', 'burden')">Burden share</button>
      <button :class="{ active: sortMode === 'alignment' }" @click="emit('update:sortMode', 'alignment')">Alignment</button>
      <button :class="{ active: sortMode === 'alpha' }"     @click="emit('update:sortMode', 'alpha')">A–Z</button>
    </div>
    <p class="panel-desc">Click a country to open its story.</p>
    <div class="rank-chart-scroll" ref="scrollEl">
      <div class="rank-chart" ref="chartEl" aria-label="Country ranking"></div>
    </div>
  </section>
</template>

<script setup>
import { ref, watch, onMounted, onUnmounted } from 'vue'
import * as d3 from 'd3'
import { opportunityColor, metricDomain, metricValue } from '../lib/colorScale.js'

const props = defineProps({
  summaries: { type: Array, default: () => [] },
  sortMode:  { type: String, default: 'alignment' },
})
const emit = defineEmits(['select-country', 'update:sortMode'])

const chartEl  = ref(null)
const scrollEl = ref(null)

const ROW_H = 22
const VAL_W = 32
const PAD_L = 4

function truncate(name, maxChars) {
  return name.length > maxChars ? name.slice(0, maxChars - 1) + '…' : name
}

function activeMode() {
  return props.sortMode === 'alpha' ? 'alignment' : props.sortMode
}

function getSorted() {
  const base = [...props.summaries].filter(s => s.under_attended_burden_share != null)
  if (props.sortMode === 'alpha') return base.sort((a, b) => a.location_name.localeCompare(b.location_name))
  if (props.sortMode === 'alignment') {
    return base.filter(s => s.share_alignment != null).sort((a, b) => a.share_alignment - b.share_alignment)
  }
  return base.sort((a, b) => b.under_attended_burden_share - a.under_attended_burden_share)
}

function valFn(d) {
  return props.sortMode === 'alignment'
    ? (d.share_alignment ?? 0)
    : (d.under_attended_burden_share ?? 0)
}

let lastWidth = 0

function draw() {
  if (!chartEl.value) return
  const el = chartEl.value

  const containerW = scrollEl.value?.getBoundingClientRect().width || 220
  if (containerW === lastWidth && el.children.length > 0) return
  lastWidth = containerW

  el.innerHTML = ''

  const sorted = getSorted()
  if (sorted.length === 0) return

  const mode = activeMode()
  const [dMin, dMax] = metricDomain(props.summaries, mode)

  const width    = containerW - 2
  const labelW   = Math.min(120, Math.floor(width * 0.50))
  const barAreaW = Math.max(20, width - PAD_L - labelW - VAL_W - 4)
  const height   = sorted.length * ROW_H + 4

  const svg = d3.select(el).append('svg').attr('width', width).attr('height', height)

  const x = d3.scaleLinear()
    .domain([0, d3.max(sorted, valFn)])
    .range([0, barAreaW])

  const maxChars = Math.floor(labelW / 6.2)

  const rows = svg.selectAll('g.row')
    .data(sorted, d => d.location_id)
    .join('g')
    .attr('class', 'row')
    .attr('transform', (_, i) => `translate(${PAD_L}, ${i * ROW_H + 2})`)
    .style('cursor', 'pointer')
    .on('click', (event, d) => { event.stopPropagation(); emit('select-country', d.location_id) })

  rows
    .on('mouseenter', function() { d3.select(this).select('.bar-fill').attr('opacity', 1) })
    .on('mouseleave', function() { d3.select(this).select('.bar-fill').attr('opacity', 0.75) })

  rows.append('text')
    .attr('x', labelW - 4).attr('y', ROW_H / 2)
    .attr('text-anchor', 'end').attr('dominant-baseline', 'middle')
    .attr('font-size', 11).attr('fill', '#374151')
    .text(d => truncate(d.location_name, maxChars))

  rows.append('rect')
    .attr('x', labelW).attr('y', 4)
    .attr('height', ROW_H - 8).attr('width', barAreaW)
    .attr('rx', 2).attr('fill', '#e5e7eb')

  rows.append('rect')
    .attr('class', 'bar-fill')
    .attr('x', labelW).attr('y', 4)
    .attr('height', ROW_H - 8).attr('rx', 2)
    .attr('width', d => x(valFn(d)))
    .attr('fill', d => opportunityColor(metricValue(d, mode), dMin, dMax, mode))
    .attr('opacity', 0.85)

  rows.append('text')
    .attr('x', labelW + barAreaW + 3).attr('y', ROW_H / 2)
    .attr('dominant-baseline', 'middle')
    .attr('font-size', 10).attr('fill', '#6b7280')
    .text(d => (valFn(d) * 100).toFixed(0) + '%')
}

onMounted(draw)
watch(() => [props.summaries, props.sortMode], () => { lastWidth = 0; draw() })

let ro
onMounted(() => {
  ro = new ResizeObserver(() => { lastWidth = 0; draw() })
  if (scrollEl.value) ro.observe(scrollEl.value)
})
onUnmounted(() => ro && ro.disconnect())
</script>
