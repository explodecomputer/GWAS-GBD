<!--
  Scatterplot of conditions by burden share (x) vs GWAS attention share (y).
  Zero-attention conditions sit on the x-axis (y = 0).
  A diagonal line marks equal shares.
  Points below the diagonal are under-attended.
  The highlighted condition is rendered larger with a label.
-->
<template>
  <div class="scatter-wrap" ref="wrapEl">
    <svg ref="svgEl" class="scatter-svg" role="img" aria-label="Condition scatterplot: burden share vs GWAS attention share"></svg>
    <div v-if="tooltip.visible" class="scatter-tooltip" :style="{ left: tooltip.x + 'px', top: tooltip.y + 'px' }">
      <div class="tt-name">{{ tooltip.data.cause_name }}</div>
      <div class="tt-row"><span>Burden share</span><span>{{ fmtPct(tooltip.data.burden_share) }}</span></div>
      <div class="tt-row"><span>Attention share</span><span>{{ fmtPct(tooltip.data.attention_share) }}</span></div>
      <div class="tt-row"><span>Mismatch share</span><span>{{ fmtPct(tooltip.data.mismatch_share) }}</span></div>
      <div class="tt-row"><span>DALYs</span><span>{{ fmtDalys(tooltip.data.dalys) }}</span></div>
      <div class="tt-row"><span>Attention score</span><span>{{ fmtScore(tooltip.data.attention_score) }}</span></div>
      <div v-if="tooltip.data.zero_attention" class="tt-zero">Zero attention</div>
    </div>
  </div>
</template>

<script setup>
import { ref, reactive, watch, onMounted, onUnmounted } from 'vue'
import * as d3 from 'd3'
import { fmtPct, fmtDalys, fmtScore } from '../lib/fmt.js'

const props = defineProps({
  conditions: { type: Array, default: () => [] },   // per-country conditions for selected year
  highlightId: { type: Number, default: null },       // cause_id to highlight
})
const emit = defineEmits(['click-condition'])

const wrapEl = ref(null)
const svgEl  = ref(null)
const tooltip = reactive({ visible: false, x: 0, y: 0, data: {} })

const MARGIN = { top: 20, right: 20, bottom: 50, left: 60 }

function draw() {
  if (!svgEl.value || props.conditions.length === 0) return

  const containerW = wrapEl.value?.getBoundingClientRect().width || 600
  const W = containerW
  const H = Math.max(320, Math.min(W * 0.65, 480))

  const innerW = W - MARGIN.left - MARGIN.right
  const innerH = H - MARGIN.top - MARGIN.bottom

  const svg = d3.select(svgEl.value)
  svg.selectAll('*').remove()
  svg.attr('width', W).attr('height', H).attr('viewBox', `0 0 ${W} ${H}`)

  const g = svg.append('g').attr('transform', `translate(${MARGIN.left},${MARGIN.top})`)

  const data = props.conditions
  const maxBS = d3.max(data, d => d.burden_share) || 0.01
  const maxAS = d3.max(data, d => d.attention_share) || 0.01
  const axMax = Math.max(maxBS, maxAS) * 1.3

  // Log axes: zero values are shifted to EPS so they land at the bottom/left edge
  const EPS = 1e-4
  const axMin = EPS

  const x = d3.scaleLog().domain([axMin, axMax]).range([0, innerW]).clamp(true)
  const y = d3.scaleLog().domain([axMin, axMax]).range([innerH, 0]).clamp(true)

  // Helper: map a share to its log-axis position (zero → EPS)
  const px = d => x(Math.max(d.burden_share,   EPS))
  const py = d => y(Math.max(d.attention_share, EPS))

  // Tick formatter: show clean % labels
  const pctFmt = v => {
    const p = v * 100
    if (p < 0.1)  return p.toFixed(2) + '%'
    if (p < 1)    return p.toFixed(1) + '%'
    return p.toFixed(0) + '%'
  }

  // Equal-share diagonal (x = y line across the log domain)
  g.append('line')
    .attr('x1', x(axMin)).attr('y1', y(axMin))
    .attr('x2', x(axMax)).attr('y2', y(axMax))
    .attr('stroke', '#d1d5db').attr('stroke-dasharray', '4 3').attr('stroke-width', 1)

  g.append('text')
    .attr('x', x(axMax * 0.15)).attr('y', y(axMax * 0.15) - 6)
    .attr('text-anchor', 'middle').attr('font-size', 10).attr('fill', '#9ca3af')
    .text('equal shares')

  // X axis
  g.append('g').attr('transform', `translate(0,${innerH})`).call(
    d3.axisBottom(x).ticks(5, '.0%').tickFormat(pctFmt)
  ).call(ax => ax.select('.domain').attr('stroke', '#d1d5db'))

  g.append('text')
    .attr('x', innerW / 2).attr('y', innerH + 40)
    .attr('text-anchor', 'middle').attr('font-size', 11).attr('fill', '#374151')
    .text('Disease burden share (log scale)')

  // Y axis
  g.append('g').call(
    d3.axisLeft(y).ticks(5, '.0%').tickFormat(pctFmt)
  ).call(ax => ax.select('.domain').attr('stroke', '#d1d5db'))

  g.append('text')
    .attr('transform', 'rotate(-90)')
    .attr('x', -innerH / 2).attr('y', -50)
    .attr('text-anchor', 'middle').attr('font-size', 11).attr('fill', '#374151')
    .text('Global GWAS attention share (log scale)')

  // Zero-axis reference lines (where zero-value conditions cluster)
  g.append('line')
    .attr('x1', 0).attr('y1', y(EPS))
    .attr('x2', innerW).attr('y2', y(EPS))
    .attr('stroke', '#fca5a5').attr('stroke-dasharray', '2 3').attr('stroke-width', 1)
  g.append('text')
    .attr('x', 2).attr('y', y(EPS) - 3)
    .attr('font-size', 9).attr('fill', '#f87171')
    .text('zero attention →')

  // Points
  const pts = g.selectAll('circle.pt')
    .data(data, d => d.cause_id)
    .join('circle')
    .attr('class', 'pt')
    .attr('cx', px)
    .attr('cy', py)
    .attr('r', d => d.cause_id === props.highlightId ? 8 : d.eligible ? 5 : 4)
    .attr('fill', d => {
      if (d.cause_id === props.highlightId) return '#f59e0b'
      if (d.zero_attention) return '#ef4444'
      if (d.eligible) return '#3b82f6'
      return '#d1d5db'
    })
    .attr('opacity', d => d.cause_id === props.highlightId ? 1 : 0.75)
    .attr('stroke', d => d.cause_id === props.highlightId ? '#b45309' : 'none')
    .attr('stroke-width', 2)
    .style('cursor', 'pointer')

  pts
    .on('mousemove', (event, d) => {
      const rect = wrapEl.value.getBoundingClientRect()
      tooltip.x = event.clientX - rect.left + 12
      tooltip.y = event.clientY - rect.top - 10
      tooltip.data = d
      tooltip.visible = true
    })
    .on('mouseleave', () => { tooltip.visible = false })
    .on('click', (_, d) => emit('click-condition', d.cause_id))

  // Highlight label
  if (props.highlightId) {
    const hl = data.find(d => d.cause_id === props.highlightId)
    if (hl) {
      g.append('text')
        .attr('x', px(hl) + 10)
        .attr('y', py(hl) - 4)
        .attr('font-size', 11)
        .attr('fill', '#b45309')
        .attr('font-weight', 'bold')
        .text(hl.cause_name.length > 30 ? hl.cause_name.slice(0, 30) + '…' : hl.cause_name)
    }
  }
}

onMounted(draw)
watch([() => props.conditions, () => props.highlightId], draw)

let ro
onMounted(() => {
  ro = new ResizeObserver(draw)
  if (wrapEl.value) ro.observe(wrapEl.value)
})
onUnmounted(() => ro && ro.disconnect())
</script>
