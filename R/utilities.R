#' Strip semantic tag from SNOMED CT description
#'
#' Removes semantic tag from the description
#'
#' @param string String, description of SNOMED CT codes
#' @importFrom stringr str_remove_all
#' @export
#' @examples
#' strip_semantic_tag("Blood Pressure (observable entity)")
strip_semantic_tag <- function(string) {
  description_short <- str_remove_all(string, " \\(([^()]+)\\)$")

  description_short
}

#' Extract semantic tag from SNOMED CT description
#'
#' Add description
#'
#' @param string String, description of SNOMED CT codes
#' @importFrom stringr str_extract str_replace_all
#' @export
#' @examples
#' extract_semantic_tag("Blood Pressure (observable entity)")
extract_semantic_tag <- function(string) {
  sem_tag <- str_extract(string, "\\(([^()]+)\\)$")
  sem_tag <- str_replace_all(sem_tag, "[()]", "")
  sem_tag
}

#' Helper function fill usage for missing years
#'
#' This only fills gaps between existing start and end dates for each code
#' but does not extent the date range for a code.
#' @importFrom tidyr complete
#' @keywords internal
complete_usage_gaps_with_zeros <- function(data) {
  # This function currently expects a specific data format.
  # This isn't a problem because we know what the data in the app looks like.
  # But we should improve this with tidyselect at some point in the future.
  data_without_gaps <- data |>
    group_by(code) |>
    complete(
      end_date = seq.Date(
        from = min(end_date),
        to = max(end_date),
        by = "year"
      ),
      fill = list(usage = 0)
    ) |>
    arrange(code, end_date) |>
    mutate(
      start_date = seq.Date(
        from = min(start_date, na.rm = TRUE),
        to = max(start_date, na.rm = TRUE),
        by = "year"
      )
    ) |>
    tidyr::fill(description, .direction = "down") |>
    ungroup()

  data_without_gaps
}

#' Find codes with multiple description
#' @importFrom dplyr count distinct filter group_by pull select
#' @keywords internal
get_codes_with_multiple_desc <- function(code_usage_data, code_column) {
  select(code_usage_data, code = {{ code_column }}, description) |>
    distinct() |>
    group_by(code) |>
    count() |>
    filter(n > 1) |>
    pull(code) |>
    unique()
}

#' Find codes with encoding problems
#' @importFrom dplyr filter distinct select
#' @importFrom stringr str_detect
#' @keywords internal
get_codes_with_encoding_problems <- function(code_usage_data, code_column) {
  select(code_usage_data, code = {{ code_column }}, description) |>
    filter(str_detect(description, "\\S*\u00C3|\u00E2\\S*")) |>
    distinct() |>
    pull(code)
}

#' Fix encoding problems
#' @importFrom stringr str_replace_all
#' @keywords internal
fix_encoding <- function(string, print_replacement_dict = FALSE) {
  # This isn't the best way of doing this but it's quick and addresses the problem
  # We should explore other ways to read the data and fix the problems as part of
  # the data cleaning problem.

  # This includes sequences starting with:
  # - Latin Capital letter A with tilde (U+00C3)
  # - Latin Small Letter A with circumflex (U+00E2)
  replacement_dict <- c(
    # Although some numbers here are technically subscript
    # we are using the normal format to match the most used formatting.
    # We might also want to replace subscript numbers to regular text
    # e.g., 1 instead of subscript 1, but for now we wont address this and
    # accept the differences in the raw data.

    # The following mapping is used below:
    # \u00E2 = Latin Small Letter A with circumflex
    # \u002C = Comma
    # \u201A = Single low-9 quotation mark
    # \u0081 = High Octet Preset
    # \u20AC = Euro sign
    # \u2122 = Trade Mark Sign
    # \u2020 = Dagger
    # \u00B6 = Pilcrow Sign
    # \u00A9 = Copyright Symbol
    # \u00F6 = Latin Small Letter O with Diaeresis
    # \u00A8 = Diaeresis
    # \u00E8 = Latin Small Letter E with grave
    # \u0020 = Space
    # \u00A0 = Non-breaking space
    # \u00E0 = Latin Small Letter A with grave
    # \u00BC = Vulgar fraction one quarter
    # \u00FC = Latin Small Letter U with diaeresis
    # \u00C3 Latin Capital letter A with tilde
    # \u2026 Horizontal ellipsis
    # \u00AB Left-pointing double angle quotation mark
    # \u00A7 Section sign
    # \u00A5 Yen sign
    # \u00B3 Superscript three
    # \u00C5 Latin Capital letter A with ring above
    # \u00EB Latin Small Letter E with diaeresis
    # \u00E7 Latin Small Letter C with cedilla
    # \u00E5 Latin Small Letter A with ring above
    # \u00F3 Latin Small Letter O with acute
    # \u0031 Digit One
    # \u0032 Digit Two
    # \u0036 Digit Six
    # \u0027 Apostrophe
    # \u002D Hyphen-minus

    # Dictionary mappings
    "\u00E2\u002C\u0081" = "\u0031",
    "\u00E2\u002C\u2020" = "\u0036",
    "\u00E2\u002C\u002C" = "\u0032",
    "\u00E2\u201A\u0081" = "\u0031",
    "\u00E2\u201A\u2020" = "\u0036",
    "\u00E2\u201A\u201A" = "\u0032",
    "\u00E2\u20AC\u2122" = "\u0027",
    "\u00E2\u20AC\u201C" = "\u002D",
    "\u00C3\u00B6" = "\u00F6",
    "\u00C3\u00A9" = "\u00E9",
    "\u00C3\u00A8" = "\u00E8",
    "\u00C3\u00BC" = "\u00FC",
    "\u00C3\u2026" = "\u00C5",
    "\u00C3\u00AB" = "\u00EB",
    "\u00C3\u00A7" = "\u00E7",
    "\u00C3\u00A5" = "\u00E5",
    "\u00C3\u00B3" = "\u00F3",
    "\u00C3\u00A7" = "\u00E7",
    "\u00C3\u00A0" = "\u00E0"
  )
  if (print_replacement_dict) {
    print(replacement_dict)
  } else {
    str_replace_all(string, replacement_dict)
  }
}

#' Turn a "<start>to<end>" period column into start_date/end_date
#'
#' The data-raw build scripts key each NHS Digital source file by a period
#' label like "2024to2025" (see e.g. inst/config/raw_snomed.yml). This turns
#' that label into real dates once the files for all periods have been
#' combined with `bind_rows(.id = period_col)`. Datasets differ in which
#' month/day their reporting period starts and ends on (e.g. snomed_usage
#' runs 1 Aug - 31 Jul, icd10/opcs4 run the NHS financial year 1 Apr - 31
#' Mar), so that's passed in rather than assumed.
#'
#' @param data A data frame with a character column of period labels
#' @param period_col String, name of the period column to parse and replace
#' @param start_month_day String, "MM-DD" of the first day of the period
#' @param end_month_day String, "MM-DD" of the last day of the period
#' @importFrom tidyr separate
#' @keywords internal
add_period_dates <- function(data, period_col, start_month_day, end_month_day) {
  data |>
    separate(period_col, c("start_date", "end_date"), sep = "to") |>
    mutate(
      start_date = as.Date(paste0(start_date, "-", start_month_day)),
      end_date = as.Date(paste0(end_date, "-", end_month_day))
    )
}

#' Add the usage of linked codes not included as a second plot series
#'
#' Old codes have no rows after they stop being used, so both series get zero
#' usage in every period of either series.
#'
#' @param summary_data Tibble with columns `start_date`, `end_date` and
#'   `total_usage` for the selected codes
#' @param linked_data Usage data of the linked codes not included
#'
#' @return Tibble with a `series` column added
#'
#' @noRd
add_linked_summary <- function(summary_data, linked_data) {
  linked_summary <- linked_data |>
    dplyr::summarise(
      total_usage = sum(usage, na.rm = TRUE),
      .by = c(start_date, end_date)
    )

  dplyr::bind_rows(
    "Selected codes" = summary_data,
    "Linked codes not included" = linked_summary,
    .id = "series"
  ) |>
    tidyr::complete(
      series,
      tidyr::nesting(start_date, end_date),
      fill = list(total_usage = 0)
    ) |>
    dplyr::arrange(series, end_date)
}

#' Add the usage of linked codes not included to the individual codes
#'
#' Like `add_linked_summary()`, every code gets zero usage in every period of
#' either series, so the individual and total plots show the same periods.
#'
#' @param data Usage data of the selected codes
#' @param linked_data Usage data of the linked codes not included
#'
#' @return Usage data with a `series` column added
#'
#' @noRd
add_linked_individual <- function(data, linked_data) {
  dplyr::bind_rows(
    "Selected codes" = data,
    "Linked codes not included" = linked_data,
    .id = "series"
  ) |>
    tidyr::complete(
      tidyr::nesting(series, code),
      tidyr::nesting(start_date, end_date),
      fill = list(usage = 0)
    ) |>
    # Codes can have different descriptions in different periods
    dplyr::group_by(code) |>
    tidyr::fill(description, .direction = "downup") |>
    dplyr::ungroup() |>
    dplyr::arrange(series, code, end_date)
}

#' Usage of linked code pairs for the timeline
#'
#' Each pair has the missing code and its linked selected code. Pairs with
#' the same new code description share a panel.
#'
#' @param pairs Rows of `missing_linked_codes()`
#' @param usage_data Usage data with `active_at_start` and `active_at_end`
#'
#' @return Tibble with one row per code and period, with the panel title, a
#'   code label and the code's share of the panel's usage in each period
#'
#' @noRd
linked_timeline_data <- function(pairs, usage_data) {
  usage_data <- usage_data |>
    dplyr::filter(code %in% c(pairs$code, pairs$linked_code))

  descriptions <- usage_data |>
    dplyr::slice_max(end_date, n = 1, by = code, with_ties = FALSE) |>
    dplyr::select(new = code, new_description = description)

  codes <- pairs |>
    dplyr::mutate(
      missing = code,
      old = dplyr::if_else(side == "old code", code, linked_code),
      new = dplyr::if_else(side == "old code", linked_code, code)
    ) |>
    dplyr::left_join(descriptions, by = "new") |>
    # The new code may have no usage, so fall back to the missing code's term
    dplyr::mutate(panel = dplyr::coalesce(new_description, term)) |>
    tidyr::pivot_longer(
      c(old, new),
      names_to = "code_type",
      values_to = "snomed_code"
    ) |>
    dplyr::mutate(
      code_label = paste0(
        snomed_code,
        "\n(",
        code_type,
        dplyr::if_else(snomed_code == missing, ", missing)", ", selected)")
      )
    ) |>
    dplyr::distinct(panel, snomed_code, code_type, code_label) |>
    dplyr::mutate(panel = factor(panel, levels = unique(panel))) |>
    # The y axis starts at the bottom, so new codes come first to put old
    # codes on top
    dplyr::arrange(code_type == "old") |>
    dplyr::mutate(code_label = factor(code_label, levels = unique(code_label)))

  usage_data |>
    dplyr::select(
      snomed_code = code,
      start_date,
      end_date,
      usage,
      active_at_start,
      active_at_end
    ) |>
    dplyr::inner_join(
      codes,
      by = "snomed_code",
      relationship = "many-to-many"
    ) |>
    dplyr::mutate(share = usage / sum(usage), .by = c(panel, end_date))
}

#' Height of the linked codes timeline in pixels
#'
#' Every code row gets the same height, so plots with more codes grow
#' instead of squashing the rows.
#'
#' @param data Output of `linked_timeline_data()`, or `NULL`
#'
#' @noRd
linked_timeline_height <- function(data) {
  if (is.null(data) || nrow(data) == 0) {
    return(300)
  }
  n_rows <- nrow(dplyr::distinct(data, panel, code_label))
  90 + 45 * dplyr::n_distinct(data$panel) + 60 * n_rows
}
