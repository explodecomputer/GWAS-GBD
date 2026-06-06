<template>
  <div class="app">
    <header class="site-header">
      <div class="header-inner">
        <button class="wordmark" @click="goOpportunity">GWAS alignment to global disease burden</button>
        <nav class="header-nav">
          <button class="nav-link" :class="{ active: view === 'opportunity' }" @click="goOpportunity">
            Global overview
          </button>
          <button class="nav-link" :class="{ active: view === 'country' }" :disabled="!selectedCountry" @click="view = 'country'">
            Country
          </button>
          <button class="nav-link about-btn" @click="showDoc = !showDoc">About</button>
        </nav>
      </div>
    </header>

    <main class="main-content">
      <div v-if="loading" class="loading-state">Loading data…</div>
      <div v-else-if="error" class="error-state">{{ error }}</div>
      <template v-else>
        <div v-if="view === 'opportunity'" class="overview-layout">
          <OrientationMap
            :summaries="summaries2023"
            :highlighted-country="highlightedCountry"
            @select-country="onMapCountry"
          />
          <OpportunityTable
            :opportunities="opportunities"
            :countries="countries"
            :conditions="conditions"
            :country-filter="countryFilter"
            :condition-filter="conditionFilter"
            :sort-by="sortBy"
            :sort-dir="sortDir"
            @open-country="onOpenCountry"
            @update:country-filter="v => { countryFilter = v; if (!v) highlightedCountry = null }"
            @update:condition-filter="conditionFilter = $event"
            @sort="onSort"
          />
        </div>

        <CountryStory
          v-if="view === 'country'"
          :location-id="selectedCountry"
          :selected-condition="selectedCondition"
          :selected-year="selectedYear"
          :countries="countries"
          :summaries="summaries"
          @change-country="onChangeCountry"
          @change-year="onChangeYear"
          @change-condition="selectedCondition = $event; syncUrl()"
        />
      </template>
    </main>

    <DocPanel v-if="showDoc" @close="showDoc = false" :metadata="metadata" />
  </div>
</template>

<script setup>
import { ref, computed, onMounted } from 'vue'
import OpportunityTable from './components/OpportunityTable.vue'
import OrientationMap from './components/OrientationMap.vue'
import CountryStory from './components/CountryStory.vue'
import DocPanel from './components/DocPanel.vue'
import { parseUrlState, pushUrlState } from './lib/state.js'

// ── reactive state ─────────────────────────────────────────────────────────
const view             = ref('opportunity')
const selectedCountry  = ref(null)   // location_id
const selectedCondition = ref(null)  // cause_id
const selectedYear     = ref(2023)
const countryFilter      = ref('')
const conditionFilter    = ref('')
const highlightedCountry = ref(null)  // location_id highlighted in orientation panel
const sortBy             = ref('mismatch_share')
const sortDir            = ref('desc')
const showDoc            = ref(false)

// ── loaded data ────────────────────────────────────────────────────────────
const opportunities = ref([])
const countries     = ref([])
const conditions    = ref([])
const summaries     = ref([])
const metadata      = ref(null)
const loading       = ref(true)
const error         = ref(null)

const summaries2023 = computed(() => summaries.value.filter(s => s.year === 2023))

// ── data loading ───────────────────────────────────────────────────────────
const BASE = import.meta.env.BASE_URL

async function loadJson(path) {
  const r = await fetch(BASE + 'data/' + path)
  if (!r.ok) throw new Error(`Failed to load ${path}: ${r.status}`)
  return r.json()
}

onMounted(async () => {
  try {
    const [opps, ctrs, conds, sums, meta] = await Promise.all([
      loadJson('opportunities.json'),
      loadJson('countries.json'),
      loadJson('conditions.json'),
      loadJson('country_summaries.json'),
      loadJson('metadata.json'),
    ])
    opportunities.value = opps
    countries.value     = ctrs
    conditions.value    = conds
    summaries.value     = sums
    metadata.value      = meta

    // Restore URL state
    const s = parseUrlState()
    if (s.selectedCountry) {
      const exists = ctrs.some(c => c.location_id === s.selectedCountry)
      if (exists) {
        selectedCountry.value  = s.selectedCountry
        selectedCondition.value = s.selectedCondition
        selectedYear.value     = s.selectedYear
        view.value             = 'country'
      }
    }
  } catch (e) {
    error.value = e.message
  } finally {
    loading.value = false
  }
})

// Handle browser back/forward
window.addEventListener('popstate', () => {
  const s = parseUrlState()
  selectedCountry.value  = s.selectedCountry
  selectedCondition.value = s.selectedCondition
  selectedYear.value     = s.selectedYear
  view.value             = s.view
})

// ── navigation helpers ─────────────────────────────────────────────────────
function syncUrl() {
  pushUrlState({
    selectedCountry:  selectedCountry.value,
    selectedCondition: selectedCondition.value,
    selectedYear:     selectedYear.value,
  })
}

function goOpportunity() {
  view.value = 'opportunity'
  selectedCountry.value = null
  selectedCondition.value = null
  pushUrlState({})
}

function onOpenCountry({ locationId, causeId }) {
  selectedCountry.value   = locationId
  selectedCondition.value = causeId
  view.value              = 'country'
  syncUrl()
}

function onMapCountry(locationId) {
  // Toggle: clicking the already-highlighted country clears the filter
  if (highlightedCountry.value === locationId) {
    highlightedCountry.value = null
    countryFilter.value = ''
  } else {
    highlightedCountry.value = locationId
    countryFilter.value = countries.value.find(c => c.location_id === locationId)?.location_name ?? ''
  }
}

function onChangeCountry(locationId) {
  selectedCountry.value   = locationId
  selectedCondition.value = null
  syncUrl()
}

function onChangeYear(year) {
  selectedYear.value = year
  syncUrl()
}

function onSort({ by, dir }) {
  sortBy.value  = by
  sortDir.value = dir
}
</script>
