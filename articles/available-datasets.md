# Available datasets in opencodecounts

Clinical codes are unique numeric or alphanumeric identifiers used in
healthcare settings for administrative, billing and clinical care
purposes. They are grouped into different classification systems to
allow standardised recording of diagnoses, procedures or medications, as
well as other clinical, demographic and administrative data. In England,
three clinical coding classification systems are used: SNOMED Clinical
Terms ([SNOMED
CT](https://www.england.nhs.uk/digitaltechnology/digital-primary-care/snomed-ct/))
in primary care (GP practices); and International Statistical
Classification of Diseases and Related Health Problems, 10th Revision
([ICD-10](https://digital.nhs.uk/developer/guides-and-documentation/building-healthcare-software/clinical-coding-classifications-and-terminology#icd-10))
and OPCS Classification of Interventions and Procedures, version 4
([OPCS-4](https://digital.nhs.uk/developer/guides-and-documentation/building-healthcare-software/clinical-coding-classifications-and-terminology#opcs-4))
in secondary care.

NHS England makes annual summaries of clinical coding activity in
general practice (SNOMED CT UK) and NHS hospitals (ICD-10 and OPCS-4,
with the most complete data available for inpatient admissions) openly
available. For each of the three classification systems, the summaries
provide annual usage of each code used at least once each year. The
*opencodecounts* package aggregates these summaries to allow data
exploration. The key consideration is that instances of clinical code
recording should not be misinterpreted as estimates of incidence,
prevalence or healthcare utilisation.

### R setup

To work with the datasets in R, ensure that the *opencodecounts* package
is loaded.

``` r

# Load opencodecounts package
library(opencodecounts)
#> To cite opencodecounts use: https://doi.org/10.1101/2025.10.14.25338005
```

## SNOMED CT code usage in GP practices in England

SNOMED CT is the world’s most comprehensive clinical terminology system,
encoding clinical findings, observations, anatomical terms, disease
causes, products and procedures. In England, the SNOMED CT UK edition
has been used in general practices since 2019, and contains 357,000
globally common codes, alongside UK-specific extension for local
screening procedures and products. In most cases, the information coded
prior to 2019 has been translated to SNOMED CT.

The annual code usage is
[published](https://digital.nhs.uk/data-and-information/publications/statistical/mi-snomed-code-usage-in-primary-care)
every October by NHS England, covering the preceding August to July. The
number of individuals included has been increasing, reaching over 62
million patient records across 6,600 providers in 2023/24. The annual
usage count reflects how many times each listed SNOMED code was added to
a GP patient record in England in a given year. The codes are included
if they have been used at least ones across the selected years. The
counts are rounded to the nearest 10, while counts between 1 and 4 are
withheld. The codes with no usage are excluded.

This is only a selection of the full dataset published by NHS Digital,
for data pre-processing see `/data-raw/gp_snomed.R`.

``` r

# Return SNOMED code usage data
get_gp_snomed()
#> ℹ Downloading "gp_snomed" (v0.2.0)
#> ✔ Downloading "gp_snomed" (v0.2.0) [428ms]
#> 
#> # A tibble: 1,857,443 × 7
#>    start_date end_date   snomed_code      description      usage active_at_start
#>    <date>     <date>     <chr>            <chr>            <int> <lgl>          
#>  1 2025-08-01 2026-07-31 279991000000102  Short message … 4.41e8 TRUE           
#>  2 2025-08-01 2026-07-31 184103008        Patient teleph… 2.91e8 TRUE           
#>  3 2025-08-01 2026-07-31 423876004        Clinical docum… 9.45e7 TRUE           
#>  4 2025-08-01 2026-07-31 1068881000000101 eConsultation … 9.40e7 TRUE           
#>  5 2025-08-01 2026-07-31 72313002         Systolic arter… 7.14e7 TRUE           
#>  6 2025-08-01 2026-07-31 1091811000000102 Diastolic arte… 7.13e7 TRUE           
#>  7 2025-08-01 2026-07-31 60621009         Body mass inde… 5.33e7 TRUE           
#>  8 2025-08-01 2026-07-31 27113001         Body weight (o… 5.27e7 TRUE           
#>  9 2025-08-01 2026-07-31 1000731000000107 Serum creatini… 5.26e7 TRUE           
#> 10 2025-08-01 2026-07-31 1000661000000107 Serum sodium l… 5.06e7 TRUE           
#> # ℹ 1,857,433 more rows
#> # ℹ 1 more variable: active_at_end <lgl>
```

## ICD-10 code usage in inpatient admissions in England

ICD-10 is a global classification system, containing 18,000 diagnoses
coded as four or five-character alphanumeric codes. It is used to
determine financial reimbursement for admitted patient care in all
NHS-commissioned acute hospitals in England. The coding data submitted
by the hospitals is aggregated and published openly by NHS England as
the Admitted Patient Care Activity of the Hospital Episode Statistics
([HES-APC](https://digital.nhs.uk/data-and-information/publications/statistical/hospital-admitted-patient-care-activity)).
It is released in annual intervals, covering April to March. Hospital
Episode Statistics also include information on the emergency, outpatient
and critical care, which is seperate from the Admitted Patient Care
activity presented in *opencodecounts*.

Activity in HES-APC is captured in episodes of care under one
consultant, known as Finished Consultant Episodes (FCEs). Each FCE can
be assigned up to 20 clinical diagnoses (ICD-10 codes). The usage count
for ICD-10 codes equals the number of FCEs with a specific
four-character ICD-10 code recorded in any diagnostic position. This
means that each diagnoses can be counted only once per FCE. To be
included in the dataset, each diagnosis code must have been used at
least once across the selected years. The counts in HES-APC are not
rounded and codes with no usage are excluded.

This is only a selection of the full dataset published by NHS Digital,
for data pre-processing see `/data-raw/hesapc_icd10.R`.

``` r

# Return ICD-10 code usage data
get_hesapc_icd10()
#> ℹ Downloading "hesapc_icd10" (v0.2.0)
#> ✔ Downloading "hesapc_icd10" (v0.2.0) [59ms]
#> 
#> # A tibble: 158,849 × 5
#>    start_date end_date   icd10_code description                            usage
#>    <date>     <date>     <chr>      <chr>                                  <int>
#>  1 2025-04-01 2026-03-31 A000       Cholera due to Vibrio cholerae 01, bi…     4
#>  2 2025-04-01 2026-03-31 A001       Cholera due to Vibrio cholerae 01, bi…     2
#>  3 2025-04-01 2026-03-31 A009       Cholera, unspecified                      31
#>  4 2025-04-01 2026-03-31 A010       Typhoid fever                            836
#>  5 2025-04-01 2026-03-31 A011       Paratyphoid fever A                      112
#>  6 2025-04-01 2026-03-31 A012       Paratyphoid fever B                       13
#>  7 2025-04-01 2026-03-31 A013       Paratyphoid fever C                        1
#>  8 2025-04-01 2026-03-31 A014       Paratyphoid fever, unspecified            48
#>  9 2025-04-01 2026-03-31 A020       Salmonella enteritis                    2789
#> 10 2025-04-01 2026-03-31 A021       Salmonella sepsis                        436
#> # ℹ 158,839 more rows
```

## OPCS-4 code usage in inpatient admissions in England

OPCS-4 consists of 11,500 four-letter alphanumeric codes and was
developed by NHS Digital for recording procedures. Similarly to ICD-10,
it determines financial reimbursement for admitted patient care in all
NHS-commissioned acute hospitals in England and is published in annual
April - March intervals by the NHS England as the Admitted Patient Care
Activity of the Hospital Episode Statistics
([HES-APC](https://digital.nhs.uk/data-and-information/publications/statistical/hospital-admitted-patient-care-activity)).

Activity in HES-APC is captured in episodes of care under one
consultant, known as Finished Consultant Episodes (FCEs). Each FCE can
be assigned up to 24 procedures (OPCS-4 codes). The usage count for the
OPCS-4 codes equals the number of times each 4-character OPCS-4 code is
listed across all FCEs in a given year. This means that unlike
diagnoses, a procedure can be counted multiple times within one FCE. To
be included in the dataset, each procedure code must have been used at
least once across the selected years. The counts in HES-APC are not
rounded and codes with no usage are excluded.

This is only a selection of the full dataset published by NHS Digital,
for the data pre-processing see `/data-raw/hesapc_opcs4.R`.

``` r

# Return OPCS-4 code usage data
get_hesapc_opcs4()
#> ℹ Downloading "hesapc_opcs4" (v0.2.0)
#> ✔ Downloading "hesapc_opcs4" (v0.2.0) [57ms]
#> 
#> # A tibble: 125,925 × 5
#>    start_date end_date   opcs4_code description                            usage
#>    <date>     <date>     <chr>      <chr>                                  <int>
#>  1 2025-04-01 2026-03-31 A011       Hemispherectomy                           15
#>  2 2025-04-01 2026-03-31 A012       Total lobectomy of brain                  33
#>  3 2025-04-01 2026-03-31 A013       Partial lobectomy of brain                98
#>  4 2025-04-01 2026-03-31 A018       Other specified major excision of tis…    32
#>  5 2025-04-01 2026-03-31 A019       Unspecified major excision of tissue …     1
#>  6 2025-04-01 2026-03-31 A021       Excision of lesion of tissue of front…  1446
#>  7 2025-04-01 2026-03-31 A022       Excision of lesion of tissue of tempo…  1063
#>  8 2025-04-01 2026-03-31 A023       Excision of lesion of tissue of parie…   723
#>  9 2025-04-01 2026-03-31 A024       Excision of lesion of tissue of occip…   252
#> 10 2025-04-01 2026-03-31 A025       Excision of lesion of tissue of cereb…   638
#> # ℹ 125,915 more rows
```
