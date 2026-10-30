#' Path to a copy of a dataset shipped inside the app, if there is one
#'
#' `just deploy` copies the current released parquet for every dataset into
#' `inst/app-data/` before deploying the Shiny app, so the hosted app reads
#' data straight off disk instead of downloading it on every cold start (see
#' `copy_app_data_for_deploy()`). Local installs and package users never have
#' this directory, so this returns `NULL` for them.
#'
#' Only `get_app_dataset()` calls this. `get_dataset()` (and therefore every
#' exported `get_*()` accessor) never does, so a stray `inst/app-data/`
#' left over from a deploy cannot change what a package user's `get_*()`
#' call does: it always downloads and caches.
#'
#' @param dataset String, dataset name
#' @param version String, dataset version
#'
#' @return Character path, or `NULL` if no local copy exists
#'
#' @keywords internal
get_local_data_path <- function(dataset, version) {
  local_dir <- system.file("app-data", package = "opencodecounts")
  if (local_dir == "") {
    return(NULL)
  }

  path <- file.path(local_dir, paste0(dataset, "_", version, ".parquet"))
  if (file.exists(path)) path else NULL
}

#' Make sure a dataset's parquet is in the cache, downloading it if not
#'
#' @param dataset String, dataset name as listed in `tidy_data_sources.yml`
#' @param version String, dataset version, or `NULL` for the latest
#'
#' @return The resolved version string
#'
#' @keywords internal
ensure_tidy_source_cached <- function(dataset, version = NULL) {
  cfg <- get_tidy_source_config(dataset, version)

  if (!tidy_source_cache_is_current(dataset, cfg$version)) {
    download_tidy_source(dataset, cfg$url, cfg$version)
  }

  cfg$version
}

#' Get a dataset, downloading and caching it if necessary
#'
#' This is what every exported `get_*()` accessor calls, so it is the public
#' contract package users rely on: always download-and-cache, regardless of
#' what happens to be on disk. It deliberately never looks at
#' `inst/app-data/` (see `get_app_dataset()` for the app's fast path).
#'
#' @param dataset String, dataset name as listed in `tidy_data_sources.yml`
#' @param version String, dataset version, or `NULL` for the latest
#'
#' @return Tibble
#'
#' @keywords internal
get_dataset <- function(dataset, version = NULL) {
  resolved_version <- ensure_tidy_source_cached(dataset, version)
  load_tidy_source(dataset, resolved_version)
}

#' Get the SNOMED CT History Substitution Table used by check_codelists()
#'
#' Built from NHS TRUD by `data-raw/snomed_history.R`, keeping only pairs
#' whose old code has GP usage.
#'
#' @param version String, dataset version, or `NULL` for the latest
#'
#' @return Tibble with columns `old_code`, `old_term`, `new_code`, `new_term`,
#'   `is_ambiguous` and `trud_release`, the TRUD release it was built from
#'
#' @noRd
get_snomed_history <- function(version = NULL) {
  tibble::as_tibble(get_dataset("snomed_history", version))
}

#' Get a dataset for the Shiny app, preferring the bundled local copy
#'
#' Used only by the app's own dataset dispatch (`mod_sidebar.R`), never
#' exposed to package users. Reads straight from `inst/app-data/` when the
#' deployed app has one (see `get_local_data_path()`), otherwise falls back
#' to `get_dataset()`'s normal download-and-cache path, so this also works
#' for local development where `inst/app-data/` doesn't exist.
#'
#' @param dataset String, dataset name as listed in `tidy_data_sources.yml`
#' @param version String, dataset version, or `NULL` for the latest
#'
#' @return Tibble
#'
#' @importFrom arrow read_parquet
#'
#' @keywords internal
get_app_dataset <- function(dataset, version = NULL) {
  cfg <- get_tidy_source_config(dataset, version)

  local_path <- get_local_data_path(dataset, cfg$version)
  if (!is.null(local_path)) {
    return(read_parquet(local_path))
  }

  get_dataset(dataset, version)
}

#' Make sure every dataset the Shiny app can show is available before it starts
#'
#' Called once from `run_app()`. On the deployed app, every dataset is
#' already bundled in `inst/app-data/`, so this is a no-op: no downloads, no
#' parquet reads. In local development it downloads and caches whatever
#' isn't already there, so picking a dataset from the "Select data" dropdown
#' never stalls the UI on a first-time download once the app has started.
#'
#' @keywords internal
ensure_app_datasets_cached <- function() {
  registry <- load_dataset_registry()

  for (dataset_cfg in registry) {
    dataset <- dataset_cfg$dataset
    cfg <- get_tidy_source_config(dataset)

    if (is.null(get_local_data_path(dataset, cfg$version))) {
      ensure_tidy_source_cached(dataset)
    }
  }

  invisible(NULL)
}

#' Get yearly SNOMED CT code usage in GP electronic health records in England
#'
#' Downloads (if not already cached) and returns the yearly summary of
#' SNOMED CT code usage from 1st August 2011 onwards. The variables in this
#' dataset include:
#' \describe{
#'   \item{start_date}{Start date of code usage count}
#'   \item{end_date}{End date of code usage count}
#'   \item{snomed_code}{SNOMED Concept ID}
#'   \item{usage}{Yearly summary of code usage.
#'   Note that counts are rounded to the nearest 10.
#'   Counts of 5 or below are displayed as 5.}
#'   \item{active_at_start}{Specifying whether code was active at the start date.}
#'   \item{active_at_end}{Specifying whether code was active at the end date.}
#'   \item{description}{Description of SNOMED Concept ID}
#' }
#'
#' @param version String, dataset version, or `NULL` for the latest (default)
#'
#' @return A tibble
#'
#' @source <https://digital.nhs.uk/data-and-information/publications/statistical/mi-snomed-code-usage-in-primary-care>
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Filter for code usage records from 2022-08-01 onwards
#' get_gp_snomed() |>
#'   dplyr::filter(start_date >= "2022-08-1")
#' }
get_gp_snomed <- function(version = NULL) {
  get_dataset("gp_snomed", version)
}

#' Get yearly ICD-10 diagnosis code usage from Hospital Episode Statistics Admitted Patient Care in England
#'
#' Downloads (if not already cached) and returns the yearly summary of
#' 4-character ICD-10 code usage from 1st April 2012 onwards. The code usage
#' represents the annual count of all episodes which record the given ICD-10
#' code in any primary or secondary position. Restricted codes for which
#' annual usage is not published have been removed.
#' \describe{
#'   \item{start_date}{Start date of code usage count}
#'   \item{end_date}{End date of code usage count}
#'   \item{icd10_code}{The 4-character ICD-10 Code.
#'   Note that the punctuation from the code has been removed for compatibility with OpenCodelists.}
#'   \item{usage}{Annual count of code usage.}
#'   \item{description}{Description of the ICD-10 Code}
#' }
#'
#' @param version String, dataset version, or `NULL` for the latest (default)
#'
#' @return A tibble
#'
#' @source <https://digital.nhs.uk/data-and-information/publications/statistical/hospital-admitted-patient-care-activity>
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Filter to codes in the ICD-10 Chapter XIX: "Injury, poisoning..."
#' # (codes begin with letters "S" or "T"), with usage > 10,000.
#' get_hesapc_icd10() |>
#'   dplyr::filter(grepl("^[ST]", icd10_code) & usage > 10000)
#' }
get_hesapc_icd10 <- function(version = NULL) {
  get_dataset("hesapc_icd10", version)
}

#' Get yearly ICD-10 diagnosis code usage breakdowns from Hospital Episode Statistics Admitted Patient Care in England
#'
#' Downloads (if not already cached) and returns the yearly summary of
#' 4-character ICD-10 code usage with demographic breakdowns from 1st April
#' 2012 onwards. Includes breakdowns by diagnosis type (all/main), sex, and
#' age group. Restricted codes for which annual usage is not published have
#' been removed.
#' \describe{
#'   \item{start_date}{Start date of code usage count}
#'   \item{end_date}{End date of code usage count}
#'   \item{icd10_code}{The 4-character ICD-10 Code.
#'   Note that the punctuation from the code has been removed for compatibility with OpenCodelists.}
#'   \item{description}{Description of the ICD-10 Code}
#'   \item{breakdown}{Type of breakdown: all_diagnoses, main_diagnosis, male, female,
#'   gender_unknown, or age groups (age_0, age_1_4, age_5_9, ..., age_85_89, age_90plus)}
#'   \item{usage}{Annual count of code usage. NA where suppressed due to small numbers.}
#' }
#'
#' @param version String, dataset version, or `NULL` for the latest (default)
#'
#' @return A tibble
#'
#' @source <https://digital.nhs.uk/data-and-information/publications/statistical/hospital-admitted-patient-care-activity>
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Compare male vs female usage for codes containing "pregnancy"
#' get_hesapc_icd10_breakdowns() |>
#'   dplyr::filter(grepl("pregnancy", description, ignore.case = TRUE)) |>
#'   dplyr::filter(breakdown %in% c("male", "female"))
#' }
get_hesapc_icd10_breakdowns <- function(version = NULL) {
  get_dataset("hesapc_icd10_breakdowns", version)
}

#' Get yearly OPCS-4 procedure code usage from Hospital Episode Statistics Admitted Patient Care in England
#'
#' Downloads (if not already cached) and returns the yearly summary of
#' 4-character OPCS-4 code usage from 1st April 2012 onwards. The code usage
#' represents the total annual count of each procedure, recorded across the
#' primary and the secondary procedure positions. Restricted codes for which
#' annual usage is not published have been removed.
#' \describe{
#'   \item{start_date}{Start date of code usage count}
#'   \item{end_date}{End date of code usage count}
#'   \item{opcs4_code}{The 4-character OPCS-4 code.
#'   Note that the punctuation from the code has been removed for compatibility with OpenCodelists.}
#'   \item{usage}{Annual count of code usage.}
#'   \item{description}{Description of the OPCS-4 Code}
#' }
#'
#' @param version String, dataset version, or `NULL` for the latest (default)
#'
#' @return A tibble
#'
#' @source <https://digital.nhs.uk/data-and-information/publications/statistical/hospital-admitted-patient-care-activity>
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Filter to procedures involving "biopsy" after March 2020 (note each year runs April - March).
#' get_hesapc_opcs4() |>
#'   dplyr::filter(grepl("biopsy", description, ignore.case = TRUE) & lubridate::year(end_date) > 2020)
#' }
get_hesapc_opcs4 <- function(version = NULL) {
  get_dataset("hesapc_opcs4", version)
}

#' Get yearly OPCS-4 procedure code usage breakdowns from Hospital Episode Statistics Admitted Patient Care in England
#'
#' Downloads (if not already cached) and returns the yearly summary of
#' 4-character OPCS-4 code usage with demographic breakdowns from 1st April
#' 2012 onwards. Includes breakdowns by procedure type (all/main), sex, and
#' age group. Restricted codes for which annual usage is not published have
#' been removed.
#' \describe{
#'   \item{start_date}{Start date of code usage count}
#'   \item{end_date}{End date of code usage count}
#'   \item{opcs4_code}{The 4-character OPCS-4 code.
#'   Note that the punctuation from the code has been removed for compatibility with OpenCodelists.}
#'   \item{description}{Description of the OPCS-4 Code}
#'   \item{breakdown}{Type of breakdown: all_procedures, main_procedure, male, female,
#'   gender_unknown, or age groups (age_0, age_1_4, age_5_9, ..., age_85_89, age_90plus)}
#'   \item{usage}{Annual count of code usage. NA where suppressed due to small numbers.}
#' }
#'
#' @param version String, dataset version, or `NULL` for the latest (default)
#'
#' @return A tibble
#'
#' @source <https://digital.nhs.uk/data-and-information/publications/statistical/hospital-admitted-patient-care-activity>
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get sex breakdown for hip replacement procedures
#' get_hesapc_opcs4_breakdowns() |>
#'   dplyr::filter(grepl("hip replacement", description, ignore.case = TRUE)) |>
#'   dplyr::filter(breakdown %in% c("male", "female"))
#' }
get_hesapc_opcs4_breakdowns <- function(version = NULL) {
  get_dataset("hesapc_opcs4_breakdowns", version)
}

#' Get yearly SNOMED CT code usage in primary care in England
#'
#' `r lifecycle::badge("deprecated")`
#'
#' Renamed to [get_gp_snomed()].
#'
#' @inheritParams get_gp_snomed
#'
#' @return A tibble
#'
#' @keywords internal
#'
#' @export
get_snomed_usage <- function(version = NULL) {
  lifecycle::deprecate_warn(
    when = "0.8.0",
    what = "get_snomed_usage()",
    with = "get_gp_snomed()"
  )
  get_gp_snomed(version)
}

#' Get yearly ICD-10 code usage from Hospital Admitted Patient Care Activity in England
#'
#' `r lifecycle::badge("deprecated")`
#'
#' Renamed to [get_hesapc_icd10()].
#'
#' @inheritParams get_hesapc_icd10
#'
#' @return A tibble
#'
#' @keywords internal
#'
#' @export
get_icd10_usage <- function(version = NULL) {
  lifecycle::deprecate_warn(
    when = "0.8.0",
    what = "get_icd10_usage()",
    with = "get_hesapc_icd10()"
  )
  get_hesapc_icd10(version)
}

#' Get yearly ICD-10 code usage breakdowns from Hospital Admitted Patient Care Activity in England
#'
#' `r lifecycle::badge("deprecated")`
#'
#' Renamed to [get_hesapc_icd10_breakdowns()].
#'
#' @inheritParams get_hesapc_icd10_breakdowns
#'
#' @return A tibble
#'
#' @keywords internal
#'
#' @export
get_icd10_usage_breakdowns <- function(version = NULL) {
  lifecycle::deprecate_warn(
    when = "0.8.0",
    what = "get_icd10_usage_breakdowns()",
    with = "get_hesapc_icd10_breakdowns()"
  )
  get_hesapc_icd10_breakdowns(version)
}

#' Get yearly OPCS-4 code usage from Hospital Admitted Patient Care Activity in England
#'
#' `r lifecycle::badge("deprecated")`
#'
#' Renamed to [get_hesapc_opcs4()].
#'
#' @inheritParams get_hesapc_opcs4
#'
#' @return A tibble
#'
#' @keywords internal
#'
#' @export
get_opcs4_usage <- function(version = NULL) {
  lifecycle::deprecate_warn(
    when = "0.8.0",
    what = "get_opcs4_usage()",
    with = "get_hesapc_opcs4()"
  )
  get_hesapc_opcs4(version)
}

#' Get yearly OPCS-4 code usage breakdowns from Hospital Admitted Patient Care Activity in England
#'
#' `r lifecycle::badge("deprecated")`
#'
#' Renamed to [get_hesapc_opcs4_breakdowns()].
#'
#' @inheritParams get_hesapc_opcs4_breakdowns
#'
#' @return A tibble
#'
#' @keywords internal
#'
#' @export
get_opcs4_usage_breakdowns <- function(version = NULL) {
  lifecycle::deprecate_warn(
    when = "0.8.0",
    what = "get_opcs4_usage_breakdowns()",
    with = "get_hesapc_opcs4_breakdowns()"
  )
  get_hesapc_opcs4_breakdowns(version)
}
