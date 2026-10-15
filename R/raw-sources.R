#' Get raw NHS Digital source locations for a dataset
#'
#' Reads `inst/config/raw_<publication>.yml`, named by this dataset's
#' `publication` field in `tidy_data_sources.yml`, and returns its `periods`
#' list. To add a new year, add one entry to that file's `periods:` map.
#'
#' A publication can back more than one dataset (e.g. `hesapc_icd10` and
#' `hesapc_icd10_breakdowns` read the same xlsx file, different sheet
#' region); `variant` picks which sub-fields to merge in.
#'
#' @param dataset String, dataset name, e.g. "gp_snomed", "hesapc_icd10"
#'
#' @return A named list, one entry per period. For `gp_snomed` each entry is a
#'   url string; for the others, a list with `url` plus fields like `sheet`,
#'   `range` or `usage_col`.
#'
#' @importFrom yaml read_yaml
#'
#' @keywords internal
get_raw_source_periods <- function(dataset) {
  cfg <- get_tidy_source_config(dataset)
  publication <- cfg$publication
  variant <- cfg$variant

  config_path <- system.file(
    "config",
    paste0("raw_", publication, ".yml"),
    package = "opencodecounts"
  )

  if (config_path == "") {
    cli::cli_abort(
      "{.file inst/config/raw_{publication}.yml} not found in package installation",
      class = "opencodecounts_error_raw_source_not_found"
    )
  }

  periods <- read_yaml(config_path)$periods

  # Single-dataset publications are already plain url strings. Nothing to merge.
  if (!is.list(periods[[1]])) {
    return(periods)
  }

  lapply(periods, function(period) {
    shared_fields <- period[setdiff(names(period), c("usage", "breakdowns"))]
    c(shared_fields, period[[variant]])
  })
}
