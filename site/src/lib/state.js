/**
 * URL state management for the country explorer.
 * Reads and writes ?country=<location_id>&condition=<cause_id>&year=<year>
 *
 * Tested in site/tests/state.test.js
 */

const VALID_YEARS = [1990, 2023]
const DEFAULT_YEAR = 2023

/**
 * Parse URL search params into app state.
 * Unknown or invalid values are replaced with defaults.
 */
export function parseUrlState(search = window.location.search) {
  const p = new URLSearchParams(search)

  const country = p.has('country') ? parseInt(p.get('country'), 10) : null
  const condition = p.has('condition') ? parseInt(p.get('condition'), 10) : null
  const yearRaw = p.has('year') ? parseInt(p.get('year'), 10) : DEFAULT_YEAR
  const year = VALID_YEARS.includes(yearRaw) ? yearRaw : DEFAULT_YEAR
  const view = (country != null && !isNaN(country)) ? 'country' : 'opportunity'

  return {
    view,
    selectedCountry: (!isNaN(country) && country != null) ? country : null,
    selectedCondition: (!isNaN(condition) && condition != null) ? condition : null,
    selectedYear: year,
  }
}

/**
 * Serialize app state to a URL search string (no leading ?).
 */
export function serializeUrlState({ selectedCountry, selectedCondition, selectedYear }) {
  const p = new URLSearchParams()
  if (selectedCountry != null) p.set('country', String(selectedCountry))
  if (selectedCondition != null) p.set('condition', String(selectedCondition))
  if (selectedYear != null && selectedYear !== DEFAULT_YEAR) p.set('year', String(selectedYear))
  const s = p.toString()
  return s ? '?' + s : ''
}

/**
 * Push a new state to the browser history.
 */
export function pushUrlState(state) {
  const url = serializeUrlState(state)
  window.history.pushState(state, '', url || window.location.pathname)
}
