# Get SNOMED CT concept history from NHS TRUD (Technology Reference data
# Update Distribution). TRUD item 276 is the SNOMED CT UK Clinical Edition
# Query Table and History Substitution Table.

trud_snomed_history_item <- 276

#' Get the TRUD API key from the TRUD_API_KEY environment variable
#' @noRd
get_trud_api_key <- function() {
  api_key <- Sys.getenv("TRUD_API_KEY")

  if (!nzchar(api_key)) {
    cli::cli_abort(
      c(
        "No TRUD API key found.",
        "i" = "Copy your key from the {.strong Your account} page on {.url https://isd.digital.nhs.uk/trud}.",
        "i" = "Add {.code TRUD_API_KEY=your-key} to your {.file .Renviron} with {.run usethis::edit_r_environ()} and restart R."
      ),
      class = "opencodecounts_error_trud_api_key"
    )
  }

  api_key
}

#' Perform a TRUD request
#'
#' Errors are rethrown without the URL, because it contains the API key.
#'
#' @param url String, TRUD API or download URL
#' @param item Number, TRUD item, used in error messages
#' @param path String, file to save the response body to, or `NULL`
#'
#' @noRd
perform_trud_request <- function(url, item, path = NULL) {
  tryCatch(
    httr2::request(url) |>
      httr2::req_user_agent(
        "opencodecounts (https://github.com/bennettoxford/opencodecounts)"
      ) |>
      httr2::req_retry(max_tries = 3) |>
      httr2::req_perform(path = path),
    error = function(e) {
      status <- if (inherits(e, "httr2_http")) httr2::resp_status(e$resp)
      if (isTRUE(status %in% c(401, 403, 404))) {
        cli::cli_abort(
          c(
            "TRUD refused access to item {item} (HTTP {status}).",
            "i" = "Check that {.envvar TRUD_API_KEY} is correct, that your TRUD account is subscribed to item {item}, and that the release exists."
          ),
          class = "opencodecounts_error_trud_access",
          call = NULL
        )
      }
      cli::cli_abort(
        c(
          "Failed to download from TRUD.",
          "i" = if (!is.null(status)) "HTTP status {status}."
        ),
        class = "opencodecounts_error_download_failed",
        call = NULL
      )
    }
  )
}

#' Get the latest release of a TRUD item
#'
#' Returns the release file name without `.zip`.
#'
#' @param item Number, TRUD item. Default is the History Substitution Table.
#'
#' @noRd
get_latest_trud_release <- function(item = trud_snomed_history_item) {
  url <- paste0(
    "https://isd.digital.nhs.uk/trud/api/v1/keys/",
    get_trud_api_key(),
    "/items/",
    item,
    "/releases?latest"
  )

  releases <- perform_trud_request(url, item) |>
    httr2::resp_body_json() |>
    purrr::pluck("releases")
  if (length(releases) == 0) {
    cli::cli_abort(
      c(
        "TRUD has no releases for item {item}.",
        "i" = "Check that your TRUD account is subscribed to item {item}."
      ),
      class = "opencodecounts_error_trud_release"
    )
  }

  sub("\\.zip$", "", releases[[1]]$archiveFileName)
}

#' Download a TRUD release and cache its History Substitution Table
#'
#' Keeps only the History Substitution Table, as parquet.
#'
#' @noRd
download_snomed_history <- function(version, cache_path) {
  url <- paste0(
    "https://isd.digital.nhs.uk/download/api/v1/keys/",
    get_trud_api_key(),
    "/content/items/",
    trud_snomed_history_item,
    "/",
    version,
    ".zip"
  )

  zip_file <- tempfile(fileext = ".zip")
  on.exit(unlink(zip_file), add = TRUE)

  cli::cli_progress_step("Downloading SNOMED CT history {.val {version}}")
  perform_trud_request(url, trud_snomed_history_item, path = zip_file)

  history_file <- grep(
    "HistorySubstitutionTable_Concepts.*\\.txt$",
    utils::unzip(zip_file, list = TRUE)$Name,
    value = TRUE
  )
  if (length(history_file) != 1) {
    cli::cli_abort(
      "Expected one History Substitution Table in the TRUD release, found {length(history_file)}.",
      class = "opencodecounts_error_trud_release"
    )
  }

  # The old code is an inactive concept and the new code is its replacement.
  # Inactive concepts without an active replacement map to themselves or to
  # another inactive concept, so only rows with an active new code are kept.
  history <- readr::read_tsv(
    unz(zip_file, history_file),
    col_types = readr::cols(.default = readr::col_character()),
    progress = FALSE
  ) |>
    dplyr::filter(NEWCONCEPTSTATUS == "0") |>
    dplyr::transmute(
      old_code = OLDCONCEPTID,
      old_term = OLDCONCEPTFSN,
      new_code = NEWCONCEPTID,
      new_term = NEWCONCEPTFSN,
      is_ambiguous = ISAMBIGUOUS != "0"
    )

  # Write to a temporary file first so a failed run leaves no partial cache
  temp_path <- paste0(cache_path, ".tmp")
  arrow::write_parquet(history, temp_path)
  if (!file.rename(temp_path, cache_path)) {
    unlink(temp_path)
    cli::cli_abort(
      "Could not save the SNOMED CT history table to {.file {cache_path}}.",
      class = "opencodecounts_error_cache"
    )
  }

  invisible(cache_path)
}

#' Get the SNOMED CT History Substitution Table, downloading it if not cached
#'
#' @return Tibble with columns `old_code`, `old_term`, `new_code`, `new_term`
#'   and `is_ambiguous`
#'
#' @noRd
get_snomed_history <- function(version) {
  cache_path <- file.path(get_history_cache_dir(), paste0(version, ".parquet"))

  if (!file.exists(cache_path)) {
    download_snomed_history(version, cache_path)
  }

  tibble::as_tibble(arrow::read_parquet(cache_path))
}
