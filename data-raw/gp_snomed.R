# Loads SNOMED CT code usage data from GP electronic health records,
# published at files.digital.nhs.uk

library(tidyverse)
library(janitor)
library(here)

# Source urls per year, defined in inst/config/raw_gp_snomed.yml
gp_snomed_urls <- opencodecounts:::get_raw_source_periods("gp_snomed")

# Data dictionary from SNOMED_code_usage_metadata.xlsx
# https://files.digital.nhs.uk/31/097702/SNOMED_code_usage_metadata.xlsx

# * snomed_code (Text string of digits up to 18 characters long)
#   SNOMED concepts which have been added to a patient record in a general practice system during the reporting period.
# * Description (Text string)
#   The fully specified name associated with the snomed_code on the final day of the reporting period (31 July).
# * Usage (Numeric (integer) or *)
#   The number of times that the snomed_code was added into any patient record within the reporting period, rounded to the nearerst 10.
#   Usage of 1 to 4 is displayed as *.
# * Active_at_Start
#   Active status of the snomed_code on the first day of the reporting period.
#   This is taken from the most recent UK clinical extension, or associated International extention, which was published up to the start of the reporting year (1 August).
#   1 = SNOMED concept was published and was active (active = 1).
#   0 = SNOMED concept was either not yet available or was inactive (active = 0).
# * Active_at_End	"Active status of the snomed_code on the first day of the reporting period.
#   This is taken from the most recent UK clinical extension, or associated International extention, which was published up to the end of the reporting year (31 July).
#   1 = SNOMED concept was published and was active (active = 1).
#   0 = SNOMED concept was either not yet available or was inactive (active = 0).

# The following files show the number of times each listed SNOMED code was added to a GP patient record within the period 1 Aug to 31 July for the years available, aggregated at England level.

gp_snomed <- gp_snomed_urls %>%
  map(
    read_tsv,
    col_types = list(
      SNOMED_Concept_ID = "c",
      Description = "c",
      Usage = "i",
      Active_at_Start = "l",
      Active_at_End = "l"
    )
  ) %>%
  bind_rows(.id = "nhs_fy") |>
  clean_names() |>
  opencodecounts:::add_period_dates("nhs_fy", "08-01", "07-31") |>
  rename(snomed_code = snomed_concept_id)

# Count number of usage with NAs
sum(is.na(gp_snomed$usage))
# [1] 507357

# Replace NAs with 5
gp_snomed <- gp_snomed |>
  mutate(usage = replace_na(usage, 5))

# Check number of usage with NAs is 0
sum(is.na(gp_snomed$usage)) == 0

# Check codes with missing description
gp_snomed |>
  filter(is.na(description)) |>
  select(snomed_code, description, usage) |>
  distinct()
# A tibble: 0 × 3

# Check encoding problems before fix
codes_with_encoding_problems <- opencodecounts:::get_codes_with_encoding_problems(
  gp_snomed,
  snomed_code
)
# [1] "1011271000000107"   "1011311000000107"   "13445001"           "83901003"           "40956001"           "201281002"
# [7] "190818004"          "111303009"          "43234007"           "150091000000106"    "266994001"          "313005"
# [13] "275542004"          "408521009"          "236504007"          "239912009"          "27982003"           "75895005"
# [19] "4950009"            "313421002"          "239915006"          "64936001"           "193776001"          "80734006"
# [25] "118611004"          "238609000"          "193253000"          "196137000"          "923701000000106"    "194349005"
# [31] "194350005"          "232283001"          "403824007"          "716722005"          "297860001"          "398719004"
# [37] "446087008"          "783541009"          "83886009"           "920601000000106"    "970821000000100"    "111396008"
# [43] "111499002"          "194348002"          "194351009"          "232282006"          "240218006"          "255101006"
# [49] "298691007"          "298693005"          "39795003"           "63204009"           "920581000000102"    "21001001"
# [55] "239913004"          "239914005"          "239917003"          "27540008"           "298692000"          "40158001"
# [61] "402910001"          "402912009"          "45853006"           "46442004"           "740215071000132100" "78946008"
# [67] "85559002"           "95263006"           "971021000000103"    "188637007"          "239916007"          "297858003"
# [73] "60925002"           "717705004"          "973131000000103"    "191306005"          "239946005"          "403816002"
# [79] "440348009"          "53605000"           "797751000000100"    "972931000000108"

# Fix encoding problems
gp_snomed <- gp_snomed |>
  mutate(description = opencodecounts:::fix_encoding(description))

# Check encoding problems after fix
opencodecounts:::get_codes_with_encoding_problems(gp_snomed, snomed_code)
# character(0)

# Check (but dont fix) codes with multiple descriptions
codes_with_multiple_desc <- opencodecounts:::get_codes_with_multiple_desc(
  gp_snomed,
  snomed_code
)
length(codes_with_multiple_desc)
# [1] 11802

arrow::write_parquet(gp_snomed, here("data-raw", "gp_snomed.parquet"))
