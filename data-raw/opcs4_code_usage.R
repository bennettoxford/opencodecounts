library(tidyverse)
library(janitor)
library(here)
library(httr)

url_start <- "https://files.digital.nhs.uk/"

opcs4_code_usage_urls <- list(
  "fy23to24" = list(
    url = paste0(url_start, "92/DB66C9/hosp-epis-stat-admi-proc-2023-24-tab-v2.xlsx"),
    sheet = 6,
    skip_rows = 11,
    usage_col = 8
  ),
  "fy22to23" = list(
    url = paste0(url_start, "CB/515826/hosp-epis-stat-admi-proc-2022-23-tab-V2.xlsx"),
    sheet = 6,
    skip_rows = 11,
    usage_col = 8
  ),
  "fy21to22" = list(
    url = paste0(url_start, "FA/DA0567/hosp-epis-stat-admi-proc-2021-22-tab.xlsx"),
    sheet = 6,
    skip_rows = 11,
    usage_col = 8
  ),
  "fy20to21" = list(
    url = paste0(url_start, "A6/43CDC1/hosp-epis-stat-admi-proc-2020-21-tab.xlsx"),
    sheet = 6,
    skip_rows = 11,
    usage_col = 8
  ),
  "fy19to20" = list(
    url = paste0(url_start, "20/0864E6/hosp-epis-stat-admi-proc-2019-20-tab.xlsx"),
    sheet = 6,
    skip_rows = 11,
    usage_col = 8
  ),
  "fy18to19" = list(
    url = paste0(url_start, "77/0C8B3F/hosp-epis-stat-admi-proc-2018-19-tab.xlsx"),
    sheet = 6,
    skip_rows = 11,
    usage_col = 8
  ),
  "fy17to18" = list(
    url = paste0(url_start, "B6/E239FA/hosp-epis-stat-admi-proc-2017-18-tab.xlsx"),
    sheet = 6,
    skip_rows = 11,
    usage_col = 8
  ),
  "fy16to17" = list(
    url = paste0(url_start, "publication/7/g/hosp-epis-stat-admi-proc-2016-17-tab.xlsx"),
    sheet = 6,
    skip_rows = 11,
    usage_col = 8
  ),
  "fy15to16" = list(
    url = paste0(url_start, "publicationimport/pub22xxx/pub22378/hosp-epis-stat-admi-proc-2015-16-tab.xlsx"),
    sheet = 6,
    skip_rows = 11,
    usage_col = 8
  ),
  "fy14to15" = list(
    url = paste0(url_start, "publicationimport/pub19xxx/pub19124/hosp-epis-stat-admi-proc-2014-15-tab.xlsx"),
    sheet = 6,
    skip_rows = 11,
    usage_col = 8
  ),
  "fy13to14" = list(
    url = paste0(url_start, "publicationimport/pub16xxx/pub16719/hosp-epis-stat-admi-proc-2013-14-tab.xlsx"),
    sheet = 6,
    skip_rows = 18,
    usage_col = 3
  ),
  "fy12to13" = list(
    url = paste0(url_start, "publicationimport/pub12xxx/pub12566/hosp-epis-stat-admi-proc-2012-13-tab.xlsx"),
    sheet = 6,
    skip_rows = 19,
    usage_col = 3
  )
)

# Function to download and read the xlsx files
read_opcs4_usage_xlsx_from_url <- function(url_list, ...) {
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
  df_temp <- read_opcs4_usage_xlsx_from_url(url_list, ...)
  select_all_diag_counts(df_temp, url_list)
}

opcs4_usage <- opcs4_code_usage_urls |>
  map(get_opcs4_data) |>
  bind_rows(.id = "nhs_fy") |>
  separate(nhs_fy, c("start_date", "end_date"), "to") |>
  mutate(
    start_date = as.Date(
      paste0("20", str_extract_all(start_date, "\\d+"), "-04-01")
    ),
    end_date = as.Date(
      paste0("20", str_extract_all(end_date, "\\d+"), "-03-31")
    ),
    opcs4_code = gsub("\\s?[^[:alnum:]]+\\s?", "", opcs4_code)
  )

# Count number of usage with NAs
sum(is.na(opcs4_usage$usage))
# [1] 143

# Replace NAs with 5
opcs4_usage <- opcs4_usage |>
  mutate(usage = replace_na(usage, 5))

# Check number of usage with NAs is 0
sum(is.na(opcs4_usage$usage)) == 0

# Check codes with missing description
opcs4_usage |>
  filter(is.na(description)) |>
  select(opcs4_code, description, usage) |>
  distinct() |>
  print(n = 32)
# A tibble: 32 × 3

# Remove "codes" with missing description
opcs4_usage <- opcs4_usage |>
  filter(!is.na(description))

# Check encoding problems before fix
codes_with_encoding_problems <- opencodecounts:::get_codes_with_encoding_problems(opcs4_usage, opcs4_code)
# character(0)

# Check (but dont fix) codes with multiple descriptions
codes_with_multiple_desc <- opencodecounts:::get_codes_with_multiple_desc(opcs4_usage, opcs4_code)
length(codes_with_multiple_desc)
# [1] 99

usethis::use_data(
  opcs4_usage,
  compress = "bzip2",
  overwrite = TRUE
)
