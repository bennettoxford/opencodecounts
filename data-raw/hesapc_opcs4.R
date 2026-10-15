library(tidyverse)
library(janitor)
library(here)
library(httr)

# Source urls per year, defined in inst/config/raw_hesapc_opcs4.yml
# (shared with hesapc_opcs4_breakdowns.R, which reads the same files)
hesapc_opcs4_urls <- opencodecounts:::get_raw_source_periods("hesapc_opcs4")

# Function to download and read the xlsx files
read_hesapc_opcs4_xlsx_from_url <- function(url_list, ...) {
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
    opcs4_code = 1,
    description = 2,
    usage = url_list$usage_col
  ) |>
    dplyr::mutate(
      usage = as.integer(usage)
    )
}

# Combine both functions
get_opcs4_data <- function(url_list, ...) {
  df_temp <- read_hesapc_opcs4_xlsx_from_url(url_list, ...)
  select_all_diag_counts(df_temp, url_list)
}

hesapc_opcs4 <- hesapc_opcs4_urls |>
  map(get_opcs4_data) |>
  bind_rows(.id = "nhs_fy") |>
  opencodecounts:::add_period_dates("nhs_fy", "04-01", "03-31") |>
  mutate(
    opcs4_code = gsub("\\s?[^[:alnum:]]+\\s?", "", opcs4_code)
  )

# Count number of usage with NAs
sum(is.na(hesapc_opcs4$usage))
# [1] 3

# Replace NAs with 5
hesapc_opcs4 <- hesapc_opcs4 |>
  mutate(usage = replace_na(usage, 5))

# Check number of usage with NAs is 0
sum(is.na(hesapc_opcs4$usage)) == 0

# Check codes with missing description
hesapc_opcs4 |>
  filter(is.na(description)) |>
  select(opcs4_code, description, usage) |>
  distinct() |>
  print()
# A tibble: 0 × 3

# Remove "codes" with missing description
hesapc_opcs4 <- hesapc_opcs4 |>
  filter(!is.na(description))

# Check encoding problems before fix
codes_with_encoding_problems <- opencodecounts:::get_codes_with_encoding_problems(
  hesapc_opcs4,
  opcs4_code
)
# character(0)

# Check (but dont fix) codes with multiple descriptions
codes_with_multiple_desc <- opencodecounts:::get_codes_with_multiple_desc(
  hesapc_opcs4,
  opcs4_code
)
length(codes_with_multiple_desc)
# [1] 99

arrow::write_parquet(hesapc_opcs4, here("data-raw", "hesapc_opcs4.parquet"))
