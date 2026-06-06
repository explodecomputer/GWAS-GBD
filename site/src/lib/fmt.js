/**
 * Number and label formatting helpers for the explorer UI.
 * All functions are pure — no side effects.
 */

/** Format a share (0–1) as a percentage string, e.g. "12.3%" */
export function fmtPct(val, decimals = 1) {
  if (val == null || isNaN(val)) return '—'
  return (val * 100).toFixed(decimals) + '%'
}

/** Format an absolute DALY count with SI suffix, e.g. 1 234 567 → "1.2M" */
export function fmtDalys(val) {
  if (val == null || isNaN(val)) return '—'
  if (val >= 1e9) return (val / 1e9).toFixed(1) + 'B'
  if (val >= 1e6) return (val / 1e6).toFixed(1) + 'M'
  if (val >= 1e3) return (val / 1e3).toFixed(1) + 'K'
  return Math.round(val).toLocaleString()
}

/** Format an attention score with SI suffix */
export function fmtScore(val) {
  if (val == null || isNaN(val)) return '—'
  if (val === 0) return '0'
  if (val >= 1e6) return (val / 1e6).toFixed(1) + 'M'
  if (val >= 1e3) return (val / 1e3).toFixed(1) + 'K'
  return Math.round(val).toLocaleString()
}

/** Return a mismatch share badge class: high (>15%), medium (5-15%), low (<5%) */
export function mismatchClass(ms) {
  if (ms == null || isNaN(ms)) return ''
  if (ms >= 0.15) return 'mismatch-high'
  if (ms >= 0.05) return 'mismatch-medium'
  return 'mismatch-low'
}
