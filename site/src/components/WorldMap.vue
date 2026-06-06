<!--
  Choropleth world map for the global overview.
  Colours countries by the active orientation metric (alignment or burden share).
  Clicking a country navigates directly to its story.
-->
<template>
  <div class="world-map-section">
    <div class="world-map-wrap" ref="mapEl">
      <div v-if="tooltip.visible" class="map-tooltip" :style="{ left: tooltip.x + 'px', top: tooltip.y + 'px' }">
        <div class="tt-name">{{ tooltip.name }}</div>
        <div class="tt-row"><span>{{ tooltipLabel }}</span><span>{{ tooltip.value }}</span></div>
      </div>
    </div>
  </div>
</template>

<script setup>
import { ref, computed, watch, onMounted, onUnmounted } from 'vue'
import * as d3 from 'd3'
import * as topojson from 'topojson-client'
import { GBD_TO_ISO } from '../lib/isoMap.js'
import { opportunityColor, metricDomain, metricValue } from '../lib/colorScale.js'
import { fmtPct } from '../lib/fmt.js'

const props = defineProps({
  summaries: { type: Array, default: () => [] },
  sortMode:  { type: String, default: 'alignment' },
})
const emit = defineEmits(['select-country'])

const mapEl  = ref(null)
const tooltip = ref({ visible: false, x: 0, y: 0, name: '', value: '' })

const tooltipLabel = computed(() =>
  props.sortMode === 'alignment' ? 'Alignment' : 'Under-attended burden'
)

const BASE = import.meta.env.BASE_URL
let world = null
let ro = null

async function loadWorld() {
  const r = await fetch(BASE + 'data/world-110m.json')
  world = await r.json()
  draw()
}

function draw() {
  if (!mapEl.value || !world || props.summaries.length === 0) return

  const container = mapEl.value
  const width  = container.getBoundingClientRect().width
  if (width < 10) return
  const height = Math.round(width * 0.46)

  // Remove old SVG but keep tooltip div
  container.querySelectorAll('svg').forEach(el => el.remove())

  const mode = props.sortMode === 'alpha' ? 'alignment' : props.sortMode
  const [dMin, dMax] = metricDomain(props.summaries, mode)

  // ISO → summary lookup
  const byIso = new Map()
  props.summaries.forEach(s => {
    const iso = GBD_TO_ISO[s.location_name]
    if (iso != null) byIso.set(iso, s)
  })

  const projection = d3.geoNaturalEarth1().fitSize([width, height], { type: 'Sphere' })
  const path = d3.geoPath(projection)

  const svg = d3.select(container)
    .append('svg')
    .attr('width', width)
    .attr('height', height)
    .style('display', 'block')

  // Ocean
  svg.append('path')
    .datum({ type: 'Sphere' })
    .attr('d', path)
    .attr('fill', '#dbeafe')

  // Graticule
  svg.append('path')
    .datum(d3.geoGraticule()())
    .attr('d', path)
    .attr('fill', 'none')
    .attr('stroke', '#bfdbfe')
    .attr('stroke-width', 0.3)

  const countries = topojson.feature(world, world.objects.countries)

  svg.selectAll('path.country')
    .data(countries.features)
    .join('path')
    .attr('class', 'country')
    .attr('d', path)
    .attr('fill', d => {
      const s = byIso.get(+d.id)
      if (!s) return '#e5e7eb'
      const v = metricValue(s, mode)
      return opportunityColor(v, dMin, dMax, mode)
    })
    .attr('stroke', '#fff')
    .attr('stroke-width', 0.3)
    .style('cursor', d => byIso.has(+d.id) ? 'pointer' : 'default')
    .on('mouseenter', function(event, d) {
      const s = byIso.get(+d.id)
      if (!s) return
      const v = metricValue(s, mode)
      const rect = container.getBoundingClientRect()
      const [mx, my] = d3.pointer(event, container)
      tooltip.value = {
        visible: true,
        x: mx + 12,
        y: my - 8,
        name: s.location_name,
        value: v != null ? fmtPct(v) : '—',
      }
      d3.select(this).attr('stroke', '#1e3a5f').attr('stroke-width', 1)
    })
    .on('mousemove', function(event) {
      const [mx, my] = d3.pointer(event, container)
      tooltip.value.x = mx + 12
      tooltip.value.y = my - 8
    })
    .on('mouseleave', function() {
      tooltip.value.visible = false
      d3.select(this).attr('stroke', '#fff').attr('stroke-width', 0.3)
    })
    .on('click', (event, d) => {
      const s = byIso.get(+d.id)
      if (s) emit('select-country', s.location_id)
    })
}

onMounted(() => {
  loadWorld()
  ro = new ResizeObserver(() => draw())
  if (mapEl.value) ro.observe(mapEl.value)
})
onUnmounted(() => ro && ro.disconnect())

watch(() => [props.summaries, props.sortMode], draw)
</script>
