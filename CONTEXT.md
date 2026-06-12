# GWAS-GBD Country Explorer

This context defines the domain language for explaining how GWAS research attention relates to disease burden across countries.

## Language

**Attention-Burden Alignment**:
The degree to which GWAS attention is concentrated on the conditions responsible for the largest disease burden in a selected population. Conditions are ordered by disease burden, then GWAS attention is assessed across that burden ordering.
_Avoid_: GWAS attention vs burden, inequality between GWAS and GBD, attention-burden disparity

**Alignment Quality Check**:
An expected pattern used to sanity-check a canonical mapping and attention-score pipeline. The default checks are that attention-burden alignment improves over time and is higher in High SDI populations than in Low SDI populations.
_Avoid_: Plot looks right, validation metric, benchmark fit

**Mapping Quality Gate**:
A hard invariant that a canonical mapping must satisfy before default GWAS attention scores are produced. Examples include observed-term-only mappings, no duplicate accepted mapping decisions, residual exclusivity, reproducibility for identical inputs, and sentinel known non-zero conditions remaining non-zero.
_Avoid_: Diagnostic plot, sanity check, quality-control figure

**Disease Burden**:
The DALY count for a condition in all ages and both sexes for a selected country and year. Rates, age-standardised values, age-specific values, and sex-specific values are outside the default country explorer meaning unless explicitly named.
_Avoid_: DALY rate, age-standardised burden, sex-specific burden

**Condition**:
A health condition included in the prepared GBD-GWAS analysis set after exclusions, mapping, and aggregation from GBD causes. The explorer treats this prepared set as the analyzable condition universe rather than exposing the full GBD hierarchy.
_Avoid_: Trait, disease, GBD term

**GWAS Attention**:
An all-time score representing how much GWAS research attention has been mapped to a condition through the curated GBD-GWAS alignment. Alternative attention scoring methods are methodological variants, not default public measures.
_Avoid_: Study count, GWAS hits, impact-factor attention, case-count attention

**Reference Attention Dataset**:
A historical GWAS attention dataset used for comparison and quality-control checks, not as the source of truth for condition mappings. A reference dataset can contain useful expected behavior, such as a COVID-era signal, while still having known mapping errors.
_Avoid_: Ground truth dataset, target dataset, gold standard

**Canonical GBD-EFO Mapping**:
The reviewed many-to-many relationship between GBD conditions and ontology terms used to compute GWAS attention. It is the mapping source of truth even when a reference attention dataset differs from it.
_Avoid_: Rayan mapping, generated mapping, temporary mapping

**Catalog-Release Mapping**:
A canonical GBD-EFO mapping tied to one GWAS Catalog release. Decisions can be carried forward across releases, but each release has its own observed term universe and drift report.
_Avoid_: Timeless mapping, universal mapping, latest mapping

**Observed GWAS Catalog Ontology Term**:
An ontology identifier that appears in the GWAS Catalog mapped trait field for a specific GWAS Catalog release. Canonical mappings target observed terms so every mapped condition can be audited against real catalog evidence for that release.
_Avoid_: GWAS ontology, arbitrary EFO term, OLS match

**Candidate Mapping Term**:
An observed GWAS Catalog ontology term proposed for review as a possible mapping to a GBD condition. Candidate terms can be discovered through lexical similarity, ontology neighborhoods, GWAS trait labels, or model assistance, but they are not counted as GWAS attention until accepted into the canonical mapping.
_Avoid_: Automatic match, inferred mapping, hierarchy match

**Candidate Channel**:
An independent method that proposes candidate mapping terms, such as lexical search, embedding retrieval, GWAS trait-label evidence, ontology neighborhood evidence, legacy mapping evidence, or obsolete-term replacement. A candidate channel records provenance for review but is not by itself an acceptance reason.
_Avoid_: Matching method, mapping source, acceptance rule

**Legacy Mapping Evidence**:
Evidence from prior mapping artifacts, including First_part, Second_part, manual trait spreadsheets, current master mappings, and reference attention dataset differences. Legacy mapping evidence can propose or explain candidate mapping terms but does not make them accepted mappings.
_Avoid_: Prior truth, inherited mapping, Rayan-approved mapping

**Condition Alias**:
A search phrase or synonym used to retrieve candidate mapping terms for a GBD condition. Aliases are retrieval aids only and do not create GWAS attention unless they lead to human-accepted observed ontology terms.
_Avoid_: String mapping, accepted synonym, trait match

**Accepted Mapping Term**:
An observed GWAS Catalog ontology term that denotes the GBD condition itself, a named subtype of it, or a deliberately accepted broader or narrower clinical grouping. A term is not accepted solely because of ontology proximity, string containment, comorbidity, or statistical association.
_Avoid_: Similar term, related trait, associated phenotype

**Mapping Recommendation**:
A model- or rule-generated judgement about whether a candidate mapping term should be accepted for a GBD condition. Recommendations support review but do not contribute to GWAS attention until a human curator accepts them.
_Avoid_: Automated mapping, model-approved mapping, final mapping

**LLM Reviewer**:
A language model used to evaluate candidate mapping terms and produce mapping recommendations, rationales, and uncertainty labels. An LLM reviewer can assist or cross-check review but is not the canonical decision-maker unless a mapping is explicitly marked as provisional.
_Avoid_: Automated curator, final reviewer, mapping authority

**Row-Level Mapping Review**:
The review task of judging one candidate mapping term for one GBD condition as accept, reject, or unsure. The task is not to choose a single best ontology term, because a condition can have multiple acceptable observed ontology terms.
_Avoid_: Best-term selection, winner-take-all mapping, condition-level classification

**Mapping Decision**:
A row-level decision for one GBD condition and one observed GWAS Catalog ontology term. Grouped review can help curators compare related candidates, but the canonical mapping is accepted or rejected one ontology term at a time.
_Avoid_: Group decision, condition-level mapping, packet-level mapping

**Mapping Evidence Package**:
The review context shown for one candidate mapping term, including GBD hierarchy context, ontology label/definition/synonyms, GWAS Catalog evidence, candidate provenance, model recommendation, and human review fields. It should be sufficient for a curator to judge semantic fit and GWAS relevance without opening external sites.
_Avoid_: Candidate row, review metadata, prompt context

**Mapping Review Set**:
A catalog-release-specific collection of observed ontology terms, GBD condition context, candidate mapping terms, evidence packages, and quality-control diagnostics prepared for curator or LLM review. It precedes the canonical mapping and is not itself used for scoring.
_Avoid_: Final mapping, scoring input, generated mapping

**Human-Accepted Mapping**:
A mapping decision accepted by a human curator and eligible for default GWAS attention scoring. Provisional model-reviewed mappings may be analysed separately but should not be silently mixed into the canonical mapping.
_Avoid_: Model-reviewed mapping, provisional mapping, high-confidence recommendation

**Binary Mapping Decision**:
The default rule that an accepted mapping term counts fully and a rejected or unsure candidate does not count in GWAS attention scoring. Confidence, relationship type, and model rationale are review metadata rather than score weights.
_Avoid_: Weighted mapping, partial credit, confidence-weighted mapping

**Residual GBD Condition**:
A GBD condition whose name represents an “Other …” residual category within a GBD hierarchical group. Its accepted mapping terms must be exclusive of ontology terms already accepted for more specific sibling GBD conditions in the same hierarchical group.
_Avoid_: Broad condition, parent condition, catch-all condition

**Specificity Conflict**:
A situation where the same observed GWAS Catalog ontology term could be accepted for both a more specific GBD condition and a broader or residual GBD condition. The more specific GBD condition wins by default; GBD hierarchy rollup, not duplicate direct mapping, should propagate attention to parents.
_Avoid_: Shared mapping, duplicate parent mapping, residual overlap

**Global GWAS Evidence**:
The worldwide body of GWAS research attention mapped to conditions. The explorer compares country-specific disease burden against global GWAS evidence; it does not measure GWAS activity performed within that country.
_Avoid_: National GWAS output, country GWAS attention, local GWAS activity

**Zero Attention Condition**:
A condition with no mapped GWAS attention in the curated GBD-GWAS alignment. Zero attention conditions remain part of explorer views and should be labelled explicitly rather than silently excluded.
_Avoid_: Unranked condition, excluded condition, unmatched condition

**Under-Attended Condition**:
A condition whose share of disease burden is greater than its share of GWAS attention in a selected country and analysis year. Zero attention conditions can be under-attended when they carry meaningful disease burden.
_Avoid_: Positive rank difference, research gap, priority trait

**Under-Attended Burden**:
The disease burden carried by conditions whose burden share exceeds their GWAS attention share in a country and analysis year. This is the primary cross-country opportunity concept for identifying where new GWAS cohort studies may be most valuable.
_Avoid_: Opportunity score, unmet burden, research gap burden

**Under-Attended Burden Share**:
The percentage of a country's disease burden carried by under-attended conditions. This is the default cross-country ranking measure because it compares countries without letting population size dominate the view.
_Avoid_: Percent opportunity, relative gap, normalized opportunity

**Country-Condition Opportunity**:
A pairing of a country and an under-attended condition where new GWAS cohort work could add value to global GWAS evidence. The explorer's discovery workflow starts from these pairings, then lets users open the broader country story.
_Avoid_: Country trait opportunity, research opportunity, priority pair

**Eligible Opportunity**:
A country-condition opportunity where the condition carries at least 1% of the country's disease burden and its burden share is greater than its GWAS attention share. Eligibility is a discovery threshold, not a claim that smaller opportunities do not matter.
_Avoid_: Significant opportunity, valid opportunity, included opportunity

**Mismatch Share**:
The difference between a condition's disease burden share and its GWAS attention share. Eligible opportunities are ranked by mismatch share by default.
_Avoid_: Rank difference, opportunity score, gap score

**Opportunity View**:
The explorer's landing view, centered on a ranked table of eligible country-condition opportunities with cross-country orientation. It is designed for discovering where new GWAS cohort studies could add value.
_Avoid_: Home page, dashboard, country ranking

**Country Story**:
A drill-down view for one country and analysis year, optionally focused on a selected condition. It explains the country's attention-burden alignment and highlights its under-attended conditions.
_Avoid_: Country page, country report, detail page

**Country**:
A GBD admin0 location used as the selectable population unit in the country explorer. SDI groups, regions, and Global are comparison groups, not countries.
_Avoid_: Location, region, geography

**Analysis Year**:
The GBD burden year used for a country view. The country explorer defaults to 2023; earlier years are historical comparisons.
_Avoid_: Data year, selected year, burden year

## Example Dialogue

Dev: "For Bangladesh, should the headline metric show whether high-burden conditions receive more GWAS attention?"

Domain Expert: "Yes. That is attention-burden alignment: order conditions by disease burden, then assess how GWAS attention is distributed across that ordering."

Dev: "When the explorer says disease burden, does it mean DALY counts or DALY rates?"

Domain Expert: "It means DALY counts for all ages and both sexes, unless the interface explicitly says otherwise."

Dev: "Should the country page let users move through GBD hierarchy levels?"

Domain Expert: "No. It should show conditions from the prepared GBD-GWAS analysis set."

Dev: "Should users choose between study counts, GWAS hits, and impact-factor scores?"

Domain Expert: "No. The default public measure is GWAS attention: the all-time score mapped to each condition."

Dev: "Does Bangladesh's GWAS attention score mean GWAS studies performed in Bangladesh?"

Domain Expert: "No. GWAS attention is global evidence. A Bangladesh cohort can contribute globally by studying conditions that matter locally and are under-attended worldwide."

Dev: "Should high-burden conditions with no GWAS attention disappear from the rank table?"

Domain Expert: "No. They are zero attention conditions and should be visible, clearly labelled, and included in the country story."

Dev: "What do we call a condition with high DALYs but little or no GWAS attention?"

Domain Expert: "An under-attended condition. Its burden share is larger than its GWAS attention share."

Dev: "What should the cross-country landing page rank?"

Domain Expert: "It should rank under-attended burden share by default, while still showing absolute under-attended burden for study planning."

Dev: "Should the landing page start with a country list or a condition list?"

Domain Expert: "It should start with country-condition opportunities so cohort designers can discover actionable pairings."

Dev: "Should every tiny share mismatch appear as an opportunity?"

Domain Expert: "No. An eligible opportunity should carry at least 1% of national disease burden and have burden share greater than GWAS attention share."

Dev: "Which eligible opportunity should appear first?"

Domain Expert: "The one with the largest mismatch share: the biggest difference between local burden share and global GWAS attention share."

Dev: "What happens when someone clicks an opportunity?"

Domain Expert: "They open the country story, with that condition highlighted in the country's wider attention-burden pattern."

Dev: "Should High SDI appear next to Bangladesh in the country selector?"

Domain Expert: "No. Bangladesh is a country; High SDI is a comparison group and belongs in a different view."

Dev: "Which year should someone see first when they open a country?"

Domain Expert: "The analysis year should default to 2023. Earlier years are for comparison."
