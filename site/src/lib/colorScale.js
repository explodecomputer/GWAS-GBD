/**
 * Shared color scale for the world map choropleth and orientation bars.
 * Convention: high opportunity → warm (yellow→orange→red).
 * "High opportunity" means low alignment OR high under-attended burden share.
 */
import * as d3 from 'd3'

/**
 * Returns a CSS colour for a given value.
 * mode: 'alignment'  → low value = high opportunity (scale is inverted)
 *       'burden'     → high value = high opportunity
 *       'alpha'      → falls back to alignment metric
 */
export function opportunityColor(value, min, max, mode) {
  if (value == null || isNaN(value)) return '#e5e7eb'
  const range = max - min
  if (range === 0) return d3.interpolateYlOrRd(0.5)
  let t = (value - min) / range
  if (mode === 'alignment') t = 1 - t  // invert: low alignment = warm
  t = Math.max(0, Math.min(1, t))
  // Use 0.1–0.9 of the ramp to avoid too-pale and too-dark extremes
  return d3.interpolateYlOrRd(0.1 + t * 0.8)
}

/** Compute [min, max] for the active metric across a summaries array. */
export function metricDomain(summaries, mode) {
  const fn = mode === 'alignment'
    ? s => s.share_alignment
    : s => s.under_attended_burden_share
  const vals = summaries.map(fn).filter(v => v != null && !isNaN(v))
  if (vals.length === 0) return [0, 1]
  return [Math.min(...vals), Math.max(...vals)]
}

/** Return the metric value for a summary row given the current mode. */
export function metricValue(summary, mode) {
  return mode === 'alignment' ? summary.share_alignment : summary.under_attended_burden_share
}
