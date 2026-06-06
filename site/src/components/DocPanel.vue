<!--
  Documentation, terminology, and methodology panel (issue 009).
  Appears as a modal overlay triggered by the About button.
-->
<template>
  <div class="doc-overlay" role="dialog" aria-modal="true" aria-label="About this explorer">
    <div class="doc-panel">
      <button class="doc-close" @click="$emit('close')" aria-label="Close">✕</button>

      <h2>About the GWAS Opportunity Explorer</h2>

      <section>
        <h3>What this explorer does</h3>
        <p>
          The explorer helps cohort study designers discover
          <strong>country-condition opportunities</strong> — pairings where a condition
          carries meaningful local disease burden but receives comparatively little
          global GWAS attention. It is not a recommendation to study any specific condition;
          it surfaces opportunities for further evaluation.
        </p>
      </section>

      <section>
        <h3>Key terms</h3>
        <dl>
          <dt>Condition</dt>
          <dd>A health condition included in the prepared GBD-GWAS analysis set after exclusions,
              mapping, and aggregation. The explorer does not expose the full GBD hierarchy.</dd>

          <dt>Disease burden</dt>
          <dd>DALY count for all ages and both sexes for a selected country and year.
              Rates, age-standardised values, and sex-specific values are out of scope.</dd>

          <dt>GWAS attention</dt>
          <dd>An all-time score representing how much GWAS research has been mapped to a condition
              through the curated GBD-GWAS alignment. This is <em>global</em> evidence —
              it does not measure GWAS activity performed within the selected country.</dd>

          <dt>Mismatch share</dt>
          <dd>Disease burden share minus GWAS attention share. Eligible opportunities are ranked
              by mismatch share by default.</dd>

          <dt>Eligible opportunity</dt>
          <dd>A country-condition pair where the condition carries at least
              {{ props.metadata?.eligibility_threshold != null ? (props.metadata.eligibility_threshold * 100).toFixed(0) : 1 }}%
              of the country's disease burden and its burden share is greater than its attention share.</dd>

          <dt>Under-attended condition</dt>
          <dd>A condition whose disease burden share is greater than its GWAS attention share.
              Zero-attention conditions can be under-attended when they carry meaningful burden.</dd>

          <dt>Attention-burden alignment</dt>
          <dd>How well GWAS attention matches the condition burden profile for a country.
              Computed as 1 − ½ × Σ|burden share − attention share| across conditions.</dd>
        </dl>
      </section>

      <section>
        <h3>Data sources</h3>
        <dl>
          <dt>Disease burden</dt>
          <dd>{{ props.metadata?.burden_source ?? 'IHME GBD 2023' }}</dd>

          <dt>GWAS attention</dt>
          <dd>{{ props.metadata?.attention_source ?? 'GWAS Catalog, curated GBD-GWAS alignment' }}</dd>

          <dt>Analysis years</dt>
          <dd>{{ props.metadata?.years ? props.metadata.years.join(', ') : '1990, 2023' }}</dd>

          <dt>Countries</dt>
          <dd>{{ props.metadata?.n_countries ?? '—' }} GBD admin0 locations
              (SDI groups, regions, and Global are excluded from the country selector)</dd>

          <dt>Conditions</dt>
          <dd>{{ props.metadata?.n_conditions ?? '—' }} conditions after GBD-GWAS alignment</dd>

          <dt>Artifacts built</dt>
          <dd>{{ props.metadata?.build_time ? new Date(props.metadata.build_time).toLocaleDateString() : '—' }}</dd>
        </dl>
      </section>

      <section>
        <h3>Caveats</h3>
        <ul>
          <li>GWAS attention is global evidence, not a measure of GWAS activity within the selected country.</li>
          <li>A high mismatch share signals a potential opportunity, not a recommendation.</li>
          <li>Zero-attention conditions are labelled and included; absence of GWAS evidence
              does not mean absence of biological plausibility.</li>
          <li>Cohort recruitment feasibility, sample-size estimation, ethics review, and
              local data availability are outside the scope of this tool.</li>
        </ul>
      </section>
    </div>
  </div>
</template>

<script setup>
const props = defineProps({ metadata: { type: Object, default: null } })
defineEmits(['close'])
</script>
