<!--
  Cross-country orientation panel (issue 005).
  Shows ALL countries ranked by under-attended burden share in a scrollable sidebar.
  Clicking a bar highlights it and emits select-country.
  When highlightedCountry changes, the panel auto-scrolls to keep it visible.
-->
<template>
  <section class="orientation-panel" aria-label="Cross-country orientation">
    <h2 class="panel-title">Country orientation</h2>
    <p class="panel-subtitle-text">under-attended burden share, 2023</p>
    <p class="panel-desc">
      Click a bar to filter opportunities.<br>
      Click again to clear.
    </p>
    <div class="rank-chart-scroll" ref="scrollEl">
      <div class="rank-chart" ref="chartEl" aria-label="Country ranking by under-attended burden share"></div>
    </div>
  </section>
</template>

<script setup>
import { ref, watch, onMounted, onUnmounted } from 'vue'
import * as d3 from 'd3'

const props = defineProps({
  summaries:          { type: Array,  default: () => [] },
  highlightedCountry: { type: Number, default: null },
})
const emit = defineEmits(['select-country'])

const chartEl = ref(null)
const scrollEl = ref(null)

const ROW_H  = 22
const VAL_W  = 32   // px reserved for the % label after the bar
const PAD_L  = 4    // left margin inside SVG

// Max chars before truncating country name (proportional to labelW)
function truncate(name, maxChars) {
  return name.length > maxChars ? name.slice(0, maxChars - 1) + '…' : name
}

let lastWidth = 0

function draw() {
  if (!chartEl.value) return
  const el = chartEl.value

  // Use the scroll container's width so bars fill the available space
  const containerW = scrollEl.value?.getBoundingClientRect().width || 220
  if (containerW === lastWidth && el.children.length > 0) return  // skip same-width redraws
  lastWidth = containerW

  el.innerHTML = ''

  const sorted = [...props.summaries]
    .filter(s => s.under_attended_burden_share != null)
    .sort((a, b) => b.under_attended_burden_share - a.under_attended_burden_share)

  if (sorted.length === 0) return

  const width    = containerW - 2   // tiny inset for border
  const labelW   = Math.min(120, Math.floor(width * 0.50))
  const barAreaW = Math.max(20, width - PAD_L - labelW - VAL_W - 4)
  const height   = sorted.length * ROW_H + 4

  const svg = d3.select(el)
    .append('svg')
    .attr('width', width)
    .attr('height', height)

  const x = d3.scaleLinear()
    .domain([0, d3.max(sorted, d => d.under_attended_burden_share)])
    .range([0, barAreaW])

  const maxChars = Math.floor(labelW / 6.2)  // approx chars at font-size 11px

  const rows = svg.selectAll('g.row')
    .data(sorted, d => d.location_id)
    .join('g')
    .attr('class', 'row')
    .attr('transform', (_, i) => `translate(${PAD_L}, ${i * ROW_H + 2})`)
    .style('cursor', 'pointer')
    .on('click', (event, d) => { event.stopPropagation(); emit('select-country', d.location_id) })

  // Hover highlight
  rows
    .on('mouseenter', function() { d3.select(this).select('rect').attr('opacity', 1) })
    .on('mouseleave', function(_, d) {
      d3.select(this).select('rect').attr('opacity', d.location_id === props.highlightedCountry ? 1 : 0.75)
    })

  // Label
  rows.append('text')
    .attr('x', labelW - 4)
    .attr('y', ROW_H / 2)
    .attr('text-anchor', 'end')
    .attr('dominant-baseline', 'middle')
    .attr('font-size', 11)
    .attr('fill', d => d.location_id === props.highlightedCountry ? '#1a56db' : '#374151')
    .attr('font-weight', d => d.location_id === props.highlightedCountry ? '700' : '400')
    .text(d => truncate(d.location_name, maxChars))

  // Bar background track
  rows.append('rect')
    .attr('x', labelW)
    .attr('y', 4)
    .attr('height', ROW_H - 8)
    .attr('width', barAreaW)
    .attr('rx', 2)
    .attr('fill', '#e5e7eb')

  // Bar fill
  rows.append('rect')
    .attr('class', 'bar-fill')
    .attr('x', labelW)
    .attr('y', 4)
    .attr('height', ROW_H - 8)
    .attr('rx', 2)
    .attr('width', d => x(d.under_attended_burden_share))
    .attr('fill', d => d.location_id === props.highlightedCountry ? '#1a56db' : '#60a5fa')
    .attr('opacity', d => d.location_id === props.highlightedCountry ? 1 : 0.75)

  // Value label
  rows.append('text')
    .attr('x', labelW + barAreaW + 3)
    .attr('y', ROW_H / 2)
    .attr('dominant-baseline', 'middle')
    .attr('font-size', 10)
    .attr('fill', '#6b7280')
    .text(d => (d.under_attended_burden_share * 100).toFixed(0) + '%')
}

function scrollToHighlighted() {
  if (!scrollEl.value || !props.highlightedCountry) return
  const sorted = [...props.summaries]
    .filter(s => s.under_attended_burden_share != null)
    .sort((a, b) => b.under_attended_burden_share - a.under_attended_burden_share)
  const idx = sorted.findIndex(d => d.location_id === props.highlightedCountry)
  if (idx < 0) return
  const targetTop = idx * ROW_H
  const el = scrollEl.value
  // Scroll so the row is in the middle of the visible area
  el.scrollTop = targetTop - el.clientHeight / 2 + ROW_H / 2
}

onMounted(draw)

watch(() => props.summaries, draw)
watch(() => props.highlightedCountry, () => {
  draw()
  scrollToHighlighted()
})

let ro
onMounted(() => {
  ro = new ResizeObserver(() => { lastWidth = 0; draw() })
  if (scrollEl.value) ro.observe(scrollEl.value)
})
onUnmounted(() => ro && ro.disconnect())
</script>
