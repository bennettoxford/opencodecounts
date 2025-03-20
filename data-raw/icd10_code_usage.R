# This script loads all available code usage data from files.digital.nhs.uk
library(tidyverse)
library(janitor)
library(here)
library(httr)

url_start <- "https://files.digital.nhs.uk/"

icd10_code_usage_urls <- list(
  "fy23to24" = list(
    url = paste0(url_start, "A5/5B8474/hosp-epis-stat-admi-diag-2023-24-tab.xlsx"),
    sheet = 6,
    skip_rows = 12,
    usage_col = 8
  ),
  "fy22to23" = list(
    url = paste0(url_start, "7A/DB1B00/hosp-epis-stat-admi-diag-2022-23-tab_V2.xlsx"),
    sheet = 6,
    skip_rows = 12,
    usage_col = 8
  ),
  "fy21to22" = list(
    url = paste0(url_start, "0E/E70963/hosp-epis-stat-admi-diag-2021-22-tab.xlsx"),
    sheet = 6,
    skip_rows = 12,
    usage_col = 8
  ),
  "fy20to21" = list(
    url = paste0(url_start, "5B/AD892C/hosp-epis-stat-admi-diag-2020-21-tab.xlsx"),
    sheet = 6,
    skip_rows = 12,
    usage_col = 8
  ),
  "fy19to20" = list(
    url = paste0(url_start, "37/8D9781/hosp-epis-stat-admi-diag-2019-20-tab%20supp.xlsx"),
    sheet = 6,
    skip_rows = 11,
    usage_col = 8
  ),
  "fy18to19" = list(
    url = paste0(url_start, "1C/B2AD9B/hosp-epis-stat-admi-diag-2018-19-tab.xlsx"),
    sheet = 6,
    skip_rows = 11,
    usage_col = 8
  ),
  "fy17to18" = list(
    url = paste0(url_start, "B2/5CEC8D/hosp-epis-stat-admi-diag-2017-18-tab.xlsx"),
    sheet = 6,
    skip_rows = 11,
    usage_col = 8
  ),
  "fy16to17" = list(
    url = paste0(url_start, "publication/7/d/hosp-epis-stat-admi-diag-2016-17-tab.xlsx"),
    sheet = 6,
    skip_rows = 11,
    usage_col = 8
  ),
  "fy15to16" = list(
    url = paste0(url_start, "publicationimport/pub22xxx/pub22378/hosp-epis-stat-admi-diag-2015-16-tab.xlsx"),
    sheet = 6,
    skip_rows = 12,
    usage_col = 8
  ),
  "fy14to15" = list(
    url = paste0(url_start, "publicationimport/pub19xxx/pub19124/hosp-epis-stat-admi-diag-2014-15-tab.xlsx"),
    sheet = 6,
    skip_rows = 12,
    usage_col = 8
  ),
  "fy13to14" = list(
    url = paste0(url_start, "publicationimport/pub16xxx/pub16719/hosp-epis-stat-admi-diag-2013-14-tab.xlsx"),
    sheet = 6,
    skip_rows = 18,
    usage_col = 3
  ),
  "fy12to13" = list(
    url = paste0(url_start, "publicationimport/pub12xxx/pub12566/hosp-epis-stat-admi-diag-2012-13-tab.xlsx"),
    sheet = 6,
    skip_rows = 19,
    usage_col = 3
  )
)

# Function to download and read the xlsx files
read_icd10_usage_xlsx_from_url <- function(url_list, ...) {
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
    skip = url_list$skip,
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
  df_temp <- read_icd10_usage_xlsx_from_url(url_list, ...)
  select_all_diag_counts(df_temp, url_list)
}

icd10_usage <- icd10_code_usage_urls |>
  map(get_icd10_data) |>
  bind_rows(.id = "nhs_fy") |>
  separate(nhs_fy, c("start_date", "end_date"), "to") |>
  mutate(
    start_date = as.Date(
      paste0("20", str_extract_all(start_date, "\\d+"), "-04-01")
    ),
    end_date = as.Date(
      paste0("20", str_extract_all(end_date, "\\d+"), "-03-31")
    ),
    icd10_code = gsub("\\s?[^[:alnum:]]+\\s?", "", icd10_code)
  )

# Count number of usage with NAs
sum(is.na(icd10_usage$usage))
# [1] 323

# Replace NAs with 10
icd10_usage <- icd10_usage |>
  mutate(usage = replace_na(usage, 10))

# Check number of usage with NAs is 0
sum(is.na(icd10_usage$usage)) == 0

# Check codes with missing description
icd10_usage |>
  filter(is.na(description)) |>
  select(icd10_code, description, usage) |>
  distinct() |>
  print(n = 39)
# A tibble: 38 × 3

# Remove "codes" with missing description
icd10_usage <- icd10_usage |>
  filter(!is.na(description))

# Check encoding problems before fix
codes_with_encoding_problems <- opencodecounts:::get_codes_with_encoding_problems(icd10_usage, icd10_code)
# [1] "C841" "C880" "D510" "D511" "D513" "D518" "D519" "E672" "E750" "G375" "G610" "H810" "L705"
# [14] "L813" "M350" "M352" "M911" "M931" "T470" "Y441" "Y530"

# Fix encoding problems
icd10_usage <- icd10_usage |>
  mutate(description = opencodecounts:::fix_encoding(description))

# Check encoding problems after fix
opencodecounts:::get_codes_with_encoding_problems(icd10_usage, icd10_code)
# character(0)

# Check (but dont fix) codes with multiple descriptions
codes_with_multiple_desc <- opencodecounts:::get_codes_with_multiple_desc(icd10_usage, icd10_code)
length(codes_with_multiple_desc)
# [1] 214

usethis::use_data(
  icd10_usage,
  compress = "bzip2",
  overwrite = TRUE
)
