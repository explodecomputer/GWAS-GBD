<template>
  <section class="country-story" aria-label="Country story">
    <!-- Country + year selector bar -->
    <div class="story-controls">
      <label class="filter-label">
        Country
        <select
          class="filter-select"
          :value="locationId"
          @change="$emit('change-country', parseInt($event.target.value))"
          data-testid="country-selector"
        >
          <option value="">— select a country —</option>
          <option v-for="c in countries" :key="c.location_id" :value="c.location_id">
            {{ c.location_name }}
          </option>
        </select>
      </label>

      <label class="filter-label">
        Analysis year
        <select
          class="filter-select"
          :value="selectedYear"
          @change="$emit('change-year', parseInt($event.target.value))"
          data-testid="year-selector"
        >
          <option :value="2023">2023</option>
          <option :value="1990" :disabled="!has1990">1990{{ !has1990 ? ' (no data)' : '' }}</option>
        </select>
      </label>
    </div>

    <div v-if="!locationId" class="empty-state">Select a country to see the country story.</div>

    <template v-else-if="loading">
      <div class="loading-state">Loading country data…</div>
    </template>

    <template v-else-if="error">
      <div class="error-state">{{ error }}</div>
    </template>

    <template v-else>
      <!-- Summary panel -->
      <div class="summary-panel" data-testid="summary-panel">
        <h2 class="story-title">{{ countryName }}, {{ selectedYear }}</h2>

        <div class="summary-cards">
          <div class="summary-card">
            <div class="card-label">Attention-burden alignment</div>
            <div class="card-value" data-testid="alignment-value">
              {{ fmtPct(summary?.share_alignment) }}
            </div>
            <div class="card-hint">1 = perfect alignment; 0 = no alignment</div>
          </div>
          <div class="summary-card">
            <div class="card-label">Under-attended burden share</div>
            <div class="card-value" data-testid="under-attended-share">
              {{ fmtPct(summary?.under_attended_burden_share) }}
            </div>
            <div class="card-hint">Share of burden in under-attended conditions</div>
          </div>
          <div class="summary-card">
            <div class="card-label">Under-attended DALYs</div>
            <div class="card-value" data-testid="under-attended-burden">
              {{ fmtDalys(summary?.under_attended_burden) }}
            </div>
            <div class="card-hint">Absolute burden, under-attended conditions</div>
          </div>
          <div class="summary-card">
            <div class="card-label">Total DALYs</div>
            <div class="card-value">{{ fmtDalys(summary?.total_dalys) }}</div>
            <div class="card-hint">All conditions, all ages, both sexes</div>
          </div>
        </div>

        <p class="global-note">
          GWAS attention is <strong>global</strong> evidence — it does not measure GWAS studies
          performed within {{ countryName }}. A cohort from {{ countryName }} can contribute
          to global evidence by studying conditions that matter locally and are under-attended worldwide.
        </p>
      </div>

      <!-- Scatterplot -->
      <div class="scatter-section">
        <h3 class="section-title">Conditions: burden share vs global GWAS attention share</h3>
        <p class="section-desc">
          Points below the diagonal have more local burden than global GWAS attention.
          <span class="legend">
            <span class="dot eligible"></span> High country burden, low global attention &nbsp;
            <span class="dot zero"></span> Zero attention &nbsp;
            <span class="dot other"></span> Other condition &nbsp;
            <span class="dot highlight"></span> Selected condition
          </span>
        </p>
        <ScatterPlot
          :conditions="yearConditions"
          :highlight-id="selectedCondition"
          @click-condition="$emit('change-condition', $event)"
        />
      </div>

      <!-- Under-attended table -->
      <ConditionTable
        :conditions="yearConditions"
        :highlight-id="selectedCondition"
        @click-condition="$emit('change-condition', $event)"
      />
    </template>
  </section>
</template>

<script setup>
import { ref, computed, watch } from 'vue'
import ScatterPlot from './ScatterPlot.vue'
import ConditionTable from './ConditionTable.vue'
import { fmtPct, fmtDalys } from '../lib/fmt.js'

const props = defineProps({
  locationId:        { type: Number, default: null },
  selectedCondition: { type: Number, default: null },
  selectedYear:      { type: Number, default: 2023 },
  countries:         { type: Array, default: () => [] },
  summaries:         { type: Array, default: () => [] },
})
defineEmits(['change-country', 'change-year', 'change-condition'])

const countryData = ref(null)
const loading     = ref(false)
const error       = ref(null)

const BASE = import.meta.env.BASE_URL

async function fetchCountry(lid) {
  if (!lid) return
  loading.value = true
  error.value   = null
  try {
    const r = await fetch(`${BASE}data/country/${lid}.json`)
    if (!r.ok) throw new Error(`Country data not found (${r.status})`)
    countryData.value = await r.json()
  } catch (e) {
    error.value = e.message
    countryData.value = null
  } finally {
    loading.value = false
  }
}

watch(() => props.locationId, lid => { countryData.value = null; fetchCountry(lid) }, { immediate: true })

const countryName = computed(() =>
  props.countries.find(c => c.location_id === props.locationId)?.location_name ?? ''
)

const has1990 = computed(() => countryData.value?.y1990?.length > 0)

const yearKey = computed(() => 'y' + props.selectedYear)

const yearConditions = computed(() => {
  if (!countryData.value) return []
  return countryData.value[yearKey.value] ?? []
})

const summary = computed(() =>
  props.summaries.find(
    s => s.location_id === props.locationId && s.year === props.selectedYear
  )
)
</script>
