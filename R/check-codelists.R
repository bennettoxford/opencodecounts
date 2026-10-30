#' Result of check_codelists()
#' @noRd
CodelistCheck <- S7::new_class(
  "CodelistCheck",
  properties = list(
    results = S7::class_data.frame,
    summary = S7::class_data.frame,
    skipped_codelists = S7::class_character,
    history_version = S7::class_character,
    trud_release = S7::class_character,
    data_version = S7::class_character,
    start_date = S7::class_Date,
    end_date = S7::class_Date,
    not_selected_usage_found = S7::new_property(
      S7::class_logical,
      getter = function(self) any(self@results$is_not_selected_with_usage)
    )
  )
)

#' Check codelists for inactive SNOMED CT codes with usage
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Finds old and new SNOMED CT codes with recorded GP usage that are linked to
#' the codes in codelists but not selected, for review.
#'
#' * `check_codelists_dir()` checks the codelists in `codelists.txt` in an
#'   OpenSAFELY study, using the CSV files from `opensafely codelists update`.
#' * `check_codelists_url()` checks codelists from their URLs on
#'   [OpenCodelists](https://www.opencodelists.org).
#' * `check_codelists()` calls `check_codelists_url()` when `url` is set and
#'   `check_codelists_dir()` otherwise.
#'
#' @param dir String, path to the codelists directory. Default `"codelists"`.
#'   Ignored when `url` is set.
#' @param url Character vector of codelist URLs on OpenCodelists. Default
#'   `NULL` uses `dir`.
#' @param ... Arguments passed to `check_codelists_dir()` or
#'   `check_codelists_url()`.
#' @param start_date,end_date Dates or strings, first and last day of the
#'   reporting periods to use. Default `NULL` uses all periods.
#' @param data_version String, version of the [get_gp_snomed()] data. Default
#'   `NULL` uses the latest.
#' @param history_version String, version of the SNOMED CT History
#'   Substitution Table. Default `NULL` uses the latest.
#'
#' @return A `CodelistCheck` object with `@summary`, one row per codelist, and
#'   `@results`, one row per old and new code pair.
#'
#' @export
check_codelists <- function(dir = "codelists", url = NULL, ...) {
  if (is.null(url)) {
    check_codelists_dir(dir, ...)
  } else {
    check_codelists_url(url, ...)
  }
}

#' @rdname check_codelists
#' @export
check_codelists_dir <- function(
  dir = "codelists",
  start_date = NULL,
  end_date = NULL,
  data_version = NULL,
  history_version = NULL
) {
  # Read local files first so missing files are reported before any download
  codelists_txt <- read_codelists_txt(dir)
  codelists <- read_codelists(dir)

  run_codelist_check(
    codelists,
    codelist_ids = codelists_txt$codelist_id,
    coding_systems = get_coding_systems(codelists_txt$codelist_id),
    start_date = start_date,
    end_date = end_date,
    data_version = data_version,
    history_version = history_version
  )
}

#' @rdname check_codelists
#' @export
check_codelists_url <- function(
  url,
  start_date = NULL,
  end_date = NULL,
  data_version = NULL,
  history_version = NULL
) {
  codelists <- purrr::map(url, get_codelist)
  codelist_ids <- purrr::map_chr(codelists, \(codelist) codelist@full_slug)

  run_codelist_check(
    purrr::set_names(
      purrr::map(codelists, \(codelist) {
        tibble::as_tibble(S7::S7_data(codelist))
      }),
      codelist_name(codelist_ids)
    ),
    codelist_ids = codelist_ids,
    coding_systems = purrr::map_chr(
      codelists,
      \(codelist) codelist@coding_system
    ),
    start_date = start_date,
    end_date = end_date,
    data_version = data_version,
    history_version = history_version
  )
}

#' Run the codelist check on codelists that are already read
#'
#' @param codelists Named list of tibbles, one per codelist, each with a
#'   `code` column
#' @param codelist_ids,coding_systems Character vectors, one per codelist
#'
#' @noRd
run_codelist_check <- function(
  codelists,
  codelist_ids,
  coding_systems,
  start_date,
  end_date,
  data_version,
  history_version
) {
  is_snomed <- coding_systems %in% "snomedct"

  if (!any(is_snomed)) {
    n_not_found <- sum(is.na(coding_systems))
    cli::cli_abort(
      c(
        "No SNOMED CT codelists found.",
        "i" = if (n_not_found > 0) {
          "{n_not_found} codelist{?s} not found on OpenCodelists."
        }
      ),
      class = "opencodecounts_error_no_snomed_codelists"
    )
  }

  codelists <- codelists[is_snomed] |>
    purrr::list_rbind(names_to = "codelist")
  if (nrow(codelists) == 0) {
    cli::cli_abort(
      "The SNOMED CT codelists have no codes.",
      class = "opencodecounts_error_no_snomed_codelists"
    )
  }

  data_version <- get_tidy_source_config("gp_snomed", data_version)$version
  history_version <- get_tidy_source_config(
    "snomed_history",
    history_version
  )$version

  snomed_usage <- get_gp_snomed(version = data_version) |>
    filter_usage_periods(start_date, end_date)
  first_date <- min(snomed_usage$start_date)
  last_date <- max(snomed_usage$end_date)
  # Totals across all years can exceed the integer range
  usage_total <- snomed_usage |>
    dplyr::summarise(usage = sum(as.numeric(usage)), .by = snomed_code)

  snomed_history <- get_snomed_history(history_version)
  trud_release <- unique(snomed_history$trud_release)

  results <- find_linked_codes(codelists, usage_total, snomed_history) |>
    dplyr::mutate(
      usage_start_date = first_date,
      usage_end_date = last_date,
      opencodecounts_data_version = data_version,
      opencodecounts_pkg_version = as.character(
        utils::packageVersion("opencodecounts")
      ),
      history_version = history_version
    ) |>
    dplyr::relocate("trud_release", .after = "history_version")

  CodelistCheck(
    results = results,
    summary = summarise_codelists(codelists, usage_total, results),
    skipped_codelists = purrr::set_names(
      coding_systems[!is_snomed],
      codelist_ids[!is_snomed]
    ),
    history_version = history_version,
    trud_release = trud_release,
    data_version = data_version,
    start_date = first_date,
    end_date = last_date
  )
}

#' Keep the usage reporting periods between two dates
#'
#' Dates must match a period boundary so that no period is partly included.
#'
#' @noRd
filter_usage_periods <- function(snomed_usage, start_date, end_date) {
  if (length(start_date) > 1 || length(end_date) > 1) {
    cli::cli_abort(
      "{.arg start_date} and {.arg end_date} must each be a single date.",
      class = "opencodecounts_error_invalid_date"
    )
  }
  start_dates <- sort(unique(snomed_usage$start_date))
  end_dates <- sort(unique(snomed_usage$end_date))
  # A fixed format turns invalid dates into NA instead of an error, so they
  # fail the period checks below
  period_start <- if (is.null(start_date)) {
    min(start_dates)
  } else {
    as.Date(start_date, format = "%Y-%m-%d")
  }
  period_end <- if (is.null(end_date)) {
    max(end_dates)
  } else {
    as.Date(end_date, format = "%Y-%m-%d")
  }

  if (!period_start %in% start_dates) {
    cli::cli_abort(
      c(
        "{.arg start_date} {.val {format(start_date)}} is not the start of a reporting period.",
        "i" = "Available start dates: {.val {format(start_dates)}}"
      ),
      class = "opencodecounts_error_invalid_date"
    )
  }

  if (!period_end %in% end_dates) {
    cli::cli_abort(
      c(
        "{.arg end_date} {.val {format(end_date)}} is not the end of a reporting period.",
        "i" = "Available end dates: {.val {format(end_dates)}}"
      ),
      class = "opencodecounts_error_invalid_date"
    )
  }

  if (period_start > period_end) {
    cli::cli_abort(
      "{.arg start_date} must be before {.arg end_date}.",
      class = "opencodecounts_error_invalid_date"
    )
  }

  snomed_usage |>
    dplyr::filter(start_date >= period_start, end_date <= period_end)
}

#' Find the old and new code pairs of codelists, with their usage
#'
#' Used by [check_codelists()] and the Shiny app, which pass data that is
#' already loaded.
#'
#' @param codelists Tibble with columns `codelist` and `code`
#' @param usage_total Tibble with columns `snomed_code` and `usage`, the total
#'   usage of each code in the reporting periods
#' @param snomed_history Tibble from `get_snomed_history()`
#'
#' @return Tibble with one row per pair in each codelist
#'
#' @noRd
find_linked_codes <- function(codelists, usage_total, snomed_history) {
  snomed_history |>
    find_codelist_history(codelists) |>
    dplyr::left_join(
      dplyr::rename(usage_total, old_code = snomed_code, old_usage = usage),
      by = "old_code"
    ) |>
    dplyr::left_join(
      dplyr::rename(usage_total, new_code = snomed_code, new_usage = usage),
      by = "new_code"
    ) |>
    dplyr::mutate(
      # Codes missing from the usage data have no recorded usage
      old_usage = dplyr::coalesce(old_usage, 0),
      new_usage = dplyr::coalesce(new_usage, 0),
      # Any linked code with usage that the codelist does not select, old or
      # new. Ambiguous pairs often point to a broad parent code.
      is_not_selected_with_usage = !is_ambiguous &
        ((!is_old_in_codelist & old_usage > 0) |
          (!is_new_in_codelist & new_usage > 0))
    ) |>
    dplyr::relocate("is_old_in_codelist", "old_usage", .after = "old_term") |>
    dplyr::relocate(
      "is_new_in_codelist",
      "new_usage",
      "is_ambiguous",
      .after = "new_term"
    ) |>
    dplyr::arrange(codelist, old_code)
}

#' Get the linked codes that codelists do not select
#'
#' @param results Tibble from `find_linked_codes()`
#'
#' @return Tibble with one row per reported pair: a `pair` key of the old and
#'   new code, the `code` not selected,
#'   its `term`, whether it is the old or new code (`side`), its `usage`, its
#'   `share` of the usage of the pair and the `linked_code` in the codelist
#'   with its `linked_term` and `linked_usage`
#'
#' @noRd
linked_codes_not_selected <- function(results) {
  results |>
    dplyr::filter(is_not_selected_with_usage) |>
    dplyr::mutate(
      is_old_not_selected = !is_old_in_codelist,
      code = dplyr::if_else(is_old_not_selected, old_code, new_code),
      term = dplyr::if_else(is_old_not_selected, old_term, new_term),
      side = dplyr::if_else(is_old_not_selected, "old", "new"),
      usage = dplyr::if_else(is_old_not_selected, old_usage, new_usage),
      share = usage / (old_usage + new_usage),
      linked_code = dplyr::if_else(is_old_not_selected, new_code, old_code),
      linked_term = dplyr::if_else(is_old_not_selected, new_term, old_term),
      linked_usage = dplyr::if_else(is_old_not_selected, new_usage, old_usage),
      pair = paste(old_code, new_code)
    ) |>
    dplyr::select(
      codelist,
      pair,
      code,
      term,
      side,
      usage,
      share,
      linked_code,
      linked_term,
      linked_usage
    )
}

#' Find the old and new code pairs that involve each codelist
#'
#' @return Tibble with one row per pair in each codelist, with flags for
#'   which of the two codes are in the codelist
#'
#' @noRd
find_codelist_history <- function(snomed_history, codelists) {
  codelists |>
    split(~codelist) |>
    purrr::map(\(codelist) {
      snomed_history |>
        dplyr::mutate(
          is_old_in_codelist = old_code %in% codelist$code,
          is_new_in_codelist = new_code %in% codelist$code
        ) |>
        dplyr::filter(is_old_in_codelist | is_new_in_codelist)
    }) |>
    purrr::list_rbind(names_to = "codelist")
}

#' Summarise each codelist and the reported linked codes not selected
#'
#' Each code not selected is counted once per codelist, even when it is part of
#' several pairs.
#'
#' @noRd
summarise_codelists <- function(codelists, usage_total, results) {
  codelist_usage <- codelists |>
    dplyr::distinct(codelist, code) |>
    dplyr::left_join(usage_total, by = c(code = "snomed_code")) |>
    dplyr::summarise(
      n_codes = dplyr::n(),
      usage = sum(usage, na.rm = TRUE),
      .by = codelist
    )

  reported <- dplyr::filter(results, is_not_selected_with_usage)
  not_selected_usage <- dplyr::bind_rows(
    reported |>
      dplyr::filter(!is_old_in_codelist) |>
      dplyr::select(codelist, code = old_code, usage = old_usage),
    reported |>
      dplyr::filter(!is_new_in_codelist) |>
      dplyr::select(codelist, code = new_code, usage = new_usage)
  ) |>
    dplyr::distinct() |>
    dplyr::summarise(
      n_not_selected_codes = dplyr::n(),
      not_selected_usage = sum(usage),
      .by = codelist
    )

  codelist_usage |>
    dplyr::left_join(not_selected_usage, by = "codelist") |>
    dplyr::mutate(
      n_not_selected_codes = dplyr::coalesce(n_not_selected_codes, 0L),
      not_selected_usage = dplyr::coalesce(not_selected_usage, 0),
      not_selected_share = dplyr::if_else(
        not_selected_usage > 0,
        not_selected_usage / (usage + not_selected_usage),
        0
      )
    ) |>
    # Organisations first, then user codelists
    dplyr::arrange(startsWith(codelist, "user-"), codelist)
}

S7::method(print, CodelistCheck) <- function(x, ...) {
  cli::cli_h1("Codelist check")
  cli::cli_text(
    "SNOMED CT history version {x@history_version} (TRUD release {x@trud_release})"
  )
  cli::cli_text(
    "OpenCodeCounts SNOMED CT data version {x@data_version} from {format(x@start_date, '%d %b %Y')} to {format(x@end_date, '%d %b %Y')}"
  )
  n_other <- sum(!is.na(x@skipped_codelists))
  if (n_other > 0) {
    cli::cli_alert_warning("Skipped {n_other} codelist{?s} not in SNOMED CT.")
  }
  not_found <- names(x@skipped_codelists)[is.na(x@skipped_codelists)]
  if (length(not_found) > 0) {
    cli::cli_alert_warning(
      "Skipped {length(not_found)} codelist{?s} not found on OpenCodelists:"
    )
    cli::cli_bullets(purrr::set_names(not_found, rep("!", length(not_found))))
  }

  flagged <- x@summary[x@summary$n_not_selected_codes > 0, ]
  n_complete <- nrow(x@summary) - nrow(flagged)
  if (n_complete > 0) {
    cli::cli_alert_success(
      "{n_complete} codelist{?s} {?selects/select} all linked codes with usage."
    )
  }
  if (nrow(flagged) == 0) {
    return(invisible(x))
  }
  cli::cli_alert_info(
    "{nrow(flagged)} codelist{?s} {?does/do} not select some linked codes with usage:"
  )

  codelist <- c("Codelist", flagged$codelist)
  # Right-align the numbers so they line up across rows
  included <- c(
    "Selected codes",
    paste0(
      pad(flagged$n_codes),
      " ",
      pad(code_word(flagged$n_codes), "left"),
      " ",
      pad(scales::comma(flagged$usage)),
      " uses"
    )
  )
  not_selected <- c(
    "Linked codes not selected",
    paste0(
      pad(flagged$n_not_selected_codes),
      " ",
      pad(code_word(flagged$n_not_selected_codes), "left"),
      " ",
      pad(scales::comma(flagged$not_selected_usage)),
      " uses, ",
      pad(format_share(flagged$not_selected_share))
    )
  )
  included[-1] <- cli::col_green(included[-1])
  not_selected[-1] <- cli::col_yellow(not_selected[-1])
  table <- paste0(
    "  ",
    pad(codelist, "left"),
    "  ",
    pad(included, "left"),
    "  ",
    not_selected
  )
  table[1] <- cli::style_bold(table[1])
  cli::cli_text("")
  cli::cli_verbatim(table)
  cli::cli_text("")

  print_not_selected_codes(x@results, flagged$codelist)

  invisible(x)
}

#' Print each linked code not selected with its share of the usage of its pair
#'
#' The share is the code's usage divided by the usage of the old and new code
#' together, so it shows how much of the pair the codelist does not select.
#'
#' @noRd
print_not_selected_codes <- function(results, codelists) {
  codes <- linked_codes_not_selected(results) |>
    dplyr::arrange(match(codelist, codelists), dplyr::desc(usage))

  # Pad across all codelists so the columns line up
  rows <- paste0(
    "    ",
    pad(codes$code, "left"),
    "  ",
    pad(codes$side, "left"),
    "  ",
    pad(scales::comma(codes$usage)),
    " uses  ",
    pad(format_share(codes$share)),
    " of pair  ",
    codes$term
  )

  cli::cli_text(cli::style_bold(
    "Linked codes not selected and their share of each old and new pair:"
  ))
  for (codelist in codelists) {
    cli::cli_text("")
    cli::cli_verbatim(c(
      paste0("  ", codelist),
      cli::col_yellow(rows[codes$codelist == codelist])
    ))
  }
  cli::cli_text("")
}

#' Format a proportion as a percentage, showing small shares as <0.1%
#' @noRd
format_share <- function(share) {
  ifelse(share > 0 & share < 0.001, "<0.1%", sprintf("%.1f%%", 100 * share))
}

#' Word for a number of codes in the report table
#' @noRd
code_word <- function(n) {
  ifelse(n == 1, "code,", "codes,")
}

#' Pad strings to the same width, right-aligned by default
#' @noRd
pad <- function(x, align = "right") {
  x <- as.character(x)
  cli::ansi_align(x, max(cli::ansi_nchar(x)), align)
}
