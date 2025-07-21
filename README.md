# Project Title: GBD-GWAS

# Project Plan:

This study integrates metadata from the GWAS Catalog and the Global Burden of Disease (GBD) studies to evaluate the role of genome-wide association studies (GWAS) in addressing global health outcomes. Combining data from these two sources—specifically, "traits" in GWAS and "health conditions" in the GBD study—is a complex process due to differences in disease mapping between them. Hence, we employed multiple methods to align the two datasets, we first used Experimental Factor Ontology (EFO) terms to match manually mapped EFO terms from the GBD with the corresponding traits in the GWAS Catalog. For cases where no direct match was found, we applied a string-based matching function to identify similarities between GWAS traits and GBD health conditions. This helped maximize coverage of GBD conditions and address misalignments between diseases and their mapped EFO terms. Also, we conducted manual curation of GWAS traits to identify any remaining semantic relationships with GBD health conditions, ensuring a more comprehensive alignment between the two datasets.


## 1. GBD terms: 

## A. GBD terms 
In the latest GBD study, there are 377 diseases and injuries, grouped into several categories and levels. Initially, we excluded all causes in the injury category (37), as well as causes at levels 0, 1, and 2 from other categories (32), to provide specific insights into genetic conditions that are more likely to be investigated through GWAS. The remaining GBD terms (308) were then split into two groups: "GBD First_part" and "GBD Second_part, as some GBD terms include other terms within them (i.e., descendants). Therefore, these terms were separated in order to extract their descendants from the ontology tree using the ontologyIndex library. These extracted terms will then be matched with GWAS data.

## B. GBD terms mapping
All GBD terms were mapped to EFO manually. For entries with no specific EFO terms, broader EFO terms have been used instead. If no broader terms were found, placeholders were used, as those terms will also be matched with GWAS using the str_detect function via traits.


## C. Sources 
The GBD health conditions are obtained from the Global Burden of Disease data available at https://www.healthdata.org/research-analysis/gbd. Two files have been uploaded to the data folder: "First_part_GBD.xlsx" (without descendants) and "Second_part_GBD.xlsx" (with descendants). The descendant information is obtained from the efo-obo.txt file, which was sourced from https://www.ebi.ac.uk/efo/. This file could not be uploaded here due to its large size.




## 2. GWAS Catalog: 
To measure the attention given to GBD health conditions by GWAS, we developed attention scores using several approaches, as outlined below:

## A. Developing an 'Attention Score' for each health condtiosn in the GWAS Catalog
1-Attention Score: Number of studies for that EFO term.

2-Weighted Attention Score: Sum(1 / n EFO per study). Accounting for studies publishing a large number of GWAS without focusing on a specific phenotype.

3-GWAS Hits: Number of GWAS hits for that EFO term. This could be a proxy for the attention received by the study.

4-Weighted Attention Score Impact Factor: Sum(1 / n EFO per study * impact factor). It indicates its quality and the degree to which it is valued.

5-Total number of cases from intinal and Replication samcples, ncase = Initial_Sample_Cases + Replication_Sample_Cases

These approaches allow us to obtain accurate findings that are not biased toward any single method. All the approaches are based on data from the GWAS Catalog, except for the journal impact factor associated with GWAS attention, which was manually obtained from Clarivate's Journal Citation Reports (JCR).

## B. Incorporating the Impact Factor to the GWAS Catalog
Impact factors were searched using Clarivate. There are some journals that do not have impact factors, for these journals, CiteScore was used. If neither impact factor nor CiteScore was available, a value of zero was given i.e., journals are discontinued, or where studies were published in news or conferences.


## C. Manual Curation of traits in the GWAS Catalog
Limiting the matching process to traits explicitly listed in the GWAS Catalog could underestimate GWAS attention. Therefore, we manually curated the traits in GWAS to align with GBD conditions, allowing us to capture diseases that might have been missed due to semantic differences. The manually curated mapping of GWAS traits to GBD conditions has been uploaded to the data folder.


## D. Sources 
The GWAS Catalog was obtained from https://www.ebi.ac.uk/gwas/, and the journal impact factors were sourced from Clarivate's Journal Citation Reports (JCR) at https://mjl.clarivate.com/home.




## 3. Merging the GWAS Attention Scores with the GBD Disease Burden Results

## A. Matching process
The matching process was done via EFO terms of the GBD or its descendants with GWAS EFO. For GBD terms that did not have a direct EFO match, the str_detect function was used to find matches between traits in GWAS and health conditions in the GBD study. Any matching from both methods: if multiple EFO terms match one GBD trait, sum the attention score across those EFO terms (as long as they are from independent publications). If there are no matches for a GBD trait in both methods, the attention score is set to 0.

###  Matching results

|   | Matching process         |Terms no|
|---|------------------------- |------- |
| 1 | EFO term-based matching	 |   153  |
| 2 | String-based matching	   |   95   |
| 3 | Unmatched GBD conditions |   78   |


## B. Terms aggregation
For the matched GBD conditions, attention scores were aggregated after the matching process. Scores assigned to level 4 causes were rolled up to their corresponding level 3 parent categories, ensuring that parent scores accurately reflect the total attention received by their associated causes.


## 4. Merging the aligned health conditions 

## A. Linking the dataset to their associated burden 
Link the matched diseases to their respective burden (e.g., DALYs) using Global Burden of Disease data by filtering based on the number of DALYs and comparing values between the years 1990 and 2021.

To unify the GBD conditions in 1990 and 2021, the GBD terms from both datasets were standardized to those used in 2021. This means that the following GBD terms from 1990 were replaced with the corresponding terms from 2021. This step was important to minimize discrepancies during the matching process and manual curation:


|   | Only in 1990                                                     |Only in 2021|
|---|---------------------------------------------------------------   |-------------------------------------------------------- |
| 1 | Cirrhosis and other chronic liver diseases due to NAFLD          |   Nonalcoholic fatty liver disease including cirrhosis  |
| 2 | Cirrhosis and other chronic liver diseases due to hepatitis B	   |   Chronic hepatitis B including cirrhosis               |
| 3 | Cirrhosis and other chronic liver diseases due to hepatitis C    |   Chronic hepatitis C including cirrhosis               |
| 4 | Cirrhosis and other chronic liver diseases due to alcohol use    |   Cirrhosis due to alcohol                              |
| 5 | Cirrhosis and other chronic liver diseases due to other causes   |   Cirrhosis due to other causes                         |



## B. Split the dataset into three categories: 
After linking the matched diseases to their respective burden, the health conditions were categorized into three groups—overall health conditions, NCDs (non-communicable diseases), and CMNNs (communicable, maternal, neonatal, and nutritional diseases)—by merging with the GBD hierarchy to stratify the alignment within each category.


## C. Sources 
The Global Burden of Disease data for the years 1990 and 2021, along with the GBD hierarchy, were obtained from the Global Burden of Disease data available at https://www.healthdata.org/research-analysis/gbd. 


## 5. Analyzing the aligned health conditions and their associated burden (DALYs)

## A. Distribution of different attention scores across DALY
The distribution of different attention scores across DALY values was analyzed using a logarithmic scale to enhance visualization. To avoid infinite values, attention scores of zero were replaced with 0.9 × (minimum non-zero value) / 2 for each approach. Similarly, DALY values of zero were replaced using the same method based on the minimum non-zero DALY value in the dataset.


## B. Measuring Inequality Between GWAS and GBD
To assess disparities between GWAS attention and the global burden of disease (GBD), concentration curves and concentration indices were developed using the Conindex package in Stata. The analysis was stratified by geographic location and year (1990 and 2021), based on regional classifications and disease burden data from the GBD dataset.




## C.Rank Difference
Health conditions were ranked based on GWAS attention and DALY, and the difference between these rankings (rank difference) was calculated.
Positive rank difference: Suggests the disease receives less attention than its burden warrants (under-attended).
Negative rank difference: Indicates the disease receives more attention than its burden justifies (over-attended).
Rank difference close to 0: Implies that the attention given to the disease is proportional to its burden, indicating a balanced attention-to-burden ratio.

Note: Health conditions with zero GWAS attention were excluded from the analysis to avoid artificial ranks and distortion of the rank difference




