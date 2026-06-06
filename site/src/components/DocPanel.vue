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
          GWAS attention does not always align with disease burden, and alignment shifts globally.
          This explorer aims to help identify traits that are particularly under-represented by
          current global GWAS coverage on a per-country basis. Such information may help with
          designing current and future cohort studies.
        </p>
      </section>

      <section>
        <h3>Key terms</h3>
        <dl>
          <dt>GWAS</dt>
          <dd>Genome-Wide Association Study. A study design that scans the genome for genetic
              variants associated with a trait or disease across large numbers of participants.</dd>

          <dt>GBD</dt>
          <dd>Global Burden of Disease. A systematic effort led by IHME to quantify health loss
              from hundreds of diseases, injuries, and risk factors across countries and over time.</dd>

          <dt>DALY</dt>
          <dd>Disability-Adjusted Life Year. One DALY represents one year of healthy life lost,
              combining years of life lost due to premature death and years lived with disability.
              Disease burden in this explorer is measured in DALYs for all ages and both sexes.</dd>

          <dt>Condition</dt>
          <dd>A health condition included in the prepared GBD-GWAS analysis set after exclusions,
              mapping, and aggregation. The explorer does not expose the full GBD hierarchy.</dd>

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
          <li>This is one perspective on the suitability of conditions or countries for study.
              Findings should be integrated into broader contextual considerations before
              informing study design decisions.</li>
        </ul>
      </section>

      <section>
        <h3>Citation</h3>
        <p>
          Alolayet R, Chong AHW, Aldridge RW, Davey Smith G, Hemani G, Walker JG.
          The (mis-)alignment of genetic association studies to global health needs.
          <em>medRxiv</em> 2026.02.09.26345919.
          doi: <a href="https://doi.org/10.64898/2026.02.09.26345919" target="_blank" rel="noopener">10.64898/2026.02.09.26345919</a>
        </p>
      </section>
    </div>
  </div>
</template>

<script setup>
const props = defineProps({ metadata: { type: Object, default: null } })
defineEmits(['close'])
</script>
