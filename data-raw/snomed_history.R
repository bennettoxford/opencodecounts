# Loads the SNOMED CT History Substitution Table from NHS TRUD (Technology
# Reference data Update Distribution) item 276, the SNOMED CT UK Clinical
# Edition Query Table and History Substitution Table.
# Needs a TRUD API key in TRUD_API_KEY. Copy it from the "Your account" page on
# https://isd.digital.nhs.uk/trud and subscribe to item 276.

library(tidyverse)
library(here)

# TRUD item and release, defined in inst/config/raw_snomed_history.yml
trud <- yaml::read_yaml(
  system.file("config", "raw_snomed_history.yml", package = "opencodecounts")
)

api_key <- Sys.getenv("TRUD_API_KEY")
stopifnot(nzchar(api_key))

zip_file <- tempfile(fileext = ".zip")
httr2::request("https://isd.digital.nhs.uk/download/api/v1/keys") |>
  httr2::req_url_path_append(
    api_key,
    "content/items",
    trud$item,
    paste0(trud$release, ".zip")
  ) |>
  httr2::req_retry(max_tries = 3) |>
  httr2::req_perform(path = zip_file)

history_file <- unzip(zip_file, list = TRUE)$Name |>
  str_subset("HistorySubstitutionTable_Concepts.*\\.txt$")
stopifnot(length(history_file) == 1)

# The old code is an inactive concept and the new code is its replacement.
# Inactive concepts without an active replacement map to themselves or to
# another inactive concept, so only rows with an active new code are kept.
snomed_history <- read_tsv(
  unz(zip_file, history_file),
  col_types = cols(.default = col_character())
) |>
  filter(NEWCONCEPTSTATUS == "0") |>
  transmute(
    old_code = OLDCONCEPTID,
    old_term = OLDCONCEPTFSN,
    new_code = NEWCONCEPTID,
    new_term = NEWCONCEPTFSN,
    is_ambiguous = ISAMBIGUOUS != "0"
  )

nrow(snomed_history)
# [1] 303594

# Keep only pairs whose old code has GP usage. check_codelists() only reports
# a pair when the old code has usage, so the other pairs are never reported.
gp_snomed_codes <- opencodecounts::get_gp_snomed() |>
  filter(usage > 0) |>
  distinct(snomed_code)

snomed_history <- snomed_history |>
  semi_join(gp_snomed_codes, by = c(old_code = "snomed_code"))

nrow(snomed_history)
# [1] 35976

arrow::write_parquet(snomed_history, here("data-raw", "snomed_history.parquet"))
