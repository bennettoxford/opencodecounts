.onLoad <- function(...) {
  S7::methods_register()
  options(rmarkdown.html_vignette.check_title = FALSE)
}

#' @importFrom utils citation
.onAttach <- function(libname, pkgname) {
  cit <- utils::citation(pkgname)
  doi_url <- paste0("https://doi.org/", cit$doi)
  packageStartupMessage("To cite ", pkgname, " use: ", doi_url)
}

utils::globalVariables(c(
  "annual_proportion",
  "start_date",
  "end_date",
  "snomed_code",
  "description",
  "usage",
  "usage_data_available",
  "icd10_code",
  "opcs4_code",
  "total_usage",
  "full_slug",
  "codelist",
  "old_code",
  "new_code",
  "old_usage",
  "new_usage",
  "is_old_in_codelist",
  "is_new_in_codelist",
  "is_missing_with_usage",
  "is_ambiguous",
  "is_old_missing",
  "old_term",
  "new_term",
  "share",
  "code",
  "n_missing_codes",
  "missing_usage",
  "side",
  "linked_code",
  "term",
  "series",
  "trud_release",
  "active_at_start",
  "active_at_end",
  "new_description",
  "panel",
  "code_type",
  "code_label",
  "status",
  "usage_label",
  "share_label"
))
