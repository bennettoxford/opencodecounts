# Loads ICD-10 diagnosis code usage data from Hospital Episode Statistics
# Admitted Patient Care, published at files.digital.nhs.uk
library(tidyverse)
library(janitor)
library(here)
library(httr)

# Source urls per year, defined in inst/config/raw_hesapc_icd10.yml
# (shared with hesapc_icd10_breakdowns.R, which reads the same files)
hesapc_icd10_urls <- opencodecounts:::get_raw_source_periods("hesapc_icd10")

# Function to download and read the xlsx files
read_hesapc_icd10_xlsx_from_url <- function(url_list, ...) {
  temp_file <- tempfile(fileext = ".xlsx")
  GET(
    url_list$url,
    write_disk(temp_file, overwrite = TRUE)
  )
  readxl::read_xlsx(
    temp_file,
    col_names = FALSE,
    .name_repair = janitor::make_clean_names,
    sheet = url_list$sheet,
    range = url_list$range,
    ...
  )
}

# Function to select the correct columns
select_all_diag_counts <- function(data, url_list) {
  dplyr::select(
    data,
    icd10_code = 1,
    description = 2,
    usage = url_list$usage_col
  ) |>
    dplyr::mutate(
      usage = as.integer(usage)
    )
}

# Combine both functions
get_icd10_data <- function(url_list, ...) {
  df_temp <- read_hesapc_icd10_xlsx_from_url(url_list, ...)
  select_all_diag_counts(df_temp, url_list)
}

hesapc_icd10 <- hesapc_icd10_urls |>
  map(get_icd10_data) |>
  bind_rows(.id = "nhs_fy") |>
  opencodecounts:::add_period_dates("nhs_fy", "04-01", "03-31") |>
  mutate(
    icd10_code = gsub("\\s?[^[:alnum:]]+\\s?", "", icd10_code)
  )

# Count number of usage with NAs
sum(is.na(hesapc_icd10$usage))
# [1] 215

# Replace NAs with 5
hesapc_icd10 <- hesapc_icd10 |>
  mutate(usage = replace_na(usage, 5))

# Check number of usage with NAs is 0
sum(is.na(hesapc_icd10$usage)) == 0

# Check codes with missing description
hesapc_icd10 |>
  filter(is.na(description)) |>
  select(icd10_code, description, usage) |>
  distinct() |>
  print()
# A tibble: 7 × 3

# Remove codes with missing description. These are invalid codes in the
# 2021-22 and 2022-23 files (e.g. "NULL", "W19.X", "X62.."), which NHS
# Digital lists without a description.
hesapc_icd10 <- hesapc_icd10 |>
  filter(!is.na(description))

# Check encoding problems before fix
codes_with_encoding_problems <- opencodecounts:::get_codes_with_encoding_problems(
  hesapc_icd10,
  icd10_code
)
# [1] "C841" "C880" "D510" "D511" "D513" "D518" "D519" "E672" "E750" "G375" "G610" "H810" "L705"
# [14] "L813" "M350" "M352" "M911" "M931" "T470" "Y441" "Y530"

# Fix encoding problems
hesapc_icd10 <- hesapc_icd10 |>
  mutate(description = opencodecounts:::fix_encoding(description))

# Check encoding problems after fix
opencodecounts:::get_codes_with_encoding_problems(hesapc_icd10, icd10_code)
# character(0)

# Check (but dont fix) codes with multiple descriptions
codes_with_multiple_desc <- opencodecounts:::get_codes_with_multiple_desc(
  hesapc_icd10,
  icd10_code
)
length(codes_with_multiple_desc)
# [1] 213

arrow::write_parquet(hesapc_icd10, here("data-raw", "hesapc_icd10.parquet"))
