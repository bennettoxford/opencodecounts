#' URL parameters of the Shiny app
#'
#' Each entry parses one query string parameter into a clean value, or `NULL`
#' to drop a value that is not valid, and formats a value back into the URL.
#' To add a parameter, add an entry here and use its value where the input is
#' created in `mod_sidebar_ui()` or `mod_sidebar_server()`.
#'
#' @return A named list of parameter specs, in the order they appear in URLs
#' @keywords internal
url_param_specs <- function() {
  list(
    dataset = list(
      parse = \(x) if (x %in% names(load_dataset_registry())) x,
      format = identity
    ),
    codelist = list(
      parse = parse_codelist_param,
      format = identity
    ),
    codes = list(
      parse = \(x) {
        codes <- unique(trimws(strsplit(x, ",")[[1]]))
        codes[codes != ""]
      },
      format = \(x) paste(x, collapse = ",")
    ),
    description = list(parse = identity, format = identity),
    pattern = list(parse = identity, format = identity),
    from = list(parse = parse_reporting_year, format = format_reporting_year),
    to = list(parse = parse_reporting_year, format = format_reporting_year)
  )
}

#' Parse the app's URL query string
#'
#' @param query_string String, e.g. `"?dataset=snomedct&codes=123,456"`
#'
#' @return A named list of the valid parameters. Unknown parameters, empty
#'   values and values that are not valid are dropped.
#' @keywords internal
parse_url_params <- function(query_string) {
  if (is.null(query_string) || query_string == "") {
    return(list())
  }

  specs <- url_param_specs()
  query <- shiny::parseQueryString(query_string)
  query <- query[intersect(names(specs), names(query))]
  query <- query[query != ""]

  params <- purrr::imap(query, \(value, name) specs[[name]]$parse(value))
  params <- purrr::compact(params)
  params <- params[lengths(params) > 0]

  # A codelist replaces the code search, as it does in the app
  if (!is.null(params$codelist)) {
    params[c("codes", "description", "pattern")] <- NULL
  }

  # Without a dataset, the app shows the first one in the registry
  dataset <- params$dataset
  if (is.null(dataset)) {
    dataset <- names(load_dataset_registry())[1]
  }
  if (!isTRUE(get_dataset_by_id(dataset)$has_code_pattern_search)) {
    params$pattern <- NULL
  }

  params
}

#' Format app state as a URL query string
#'
#' @param params Named list of parameter values, as returned by
#'   [parse_url_params()]
#'
#' @return String starting with `"?"`, or `""` when no parameter has a value
#' @keywords internal
format_url_params <- function(params) {
  specs <- url_param_specs()
  params <- params[intersect(names(specs), names(params))]
  params <- purrr::discard(params, \(x) length(x) == 0 || all(x == ""))

  if (length(params) == 0) {
    return("")
  }

  values <- purrr::imap_chr(
    params,
    \(value, name) encode_url_value(specs[[name]]$format(value))
  )
  paste0("?", paste0(names(values), "=", values, collapse = "&"))
}

#' Send the URL query to the page that embeds the app
#'
#' When the app is in an iframe, e.g. on the pkgdown site, the browser shows
#' the URL of the page around it. The script in `app_ui()` passes the query on
#' to that page, which can then show it.
#'
#' @param session Shiny session
#' @param query String, as returned by [format_url_params()]
#' @keywords internal
send_query_to_parent <- function(session, query) {
  session$sendCustomMessage("parent-url-query", query)
}

#' Percent-encode a URL query value
#'
#' Slashes and commas are valid in a query string and keep links readable.
#' @keywords internal
encode_url_value <- function(x) {
  x <- utils::URLencode(x, reserved = TRUE)
  x <- gsub("%2F", "/", x, fixed = TRUE)
  gsub("%2C", ",", x, fixed = TRUE)
}

#' Parse a codelist URL parameter into a codelist slug
#'
#' @param x String, an OpenCodelists URL or a codelist slug
#'
#' @return The codelist slug, or `NULL` when `x` is not a codelist
#' @keywords internal
parse_codelist_param <- function(x) {
  if (!grepl("^https://", x)) {
    x <- paste0("https://www.opencodelists.org/codelist/", x)
  }
  tryCatch(extract_codelist_slug(x), error = function(e) NULL)
}

#' Parse a reporting year URL parameter
#'
#' Reporting years are labelled as in NHS Digital publications, e.g.
#' `"2014-15"`. The months differ by dataset. GP data runs from August to
#' July and hospital data from April to March.
#'
#' @param x String, e.g. `"2014-15"`
#'
#' @return Integer year in which the reporting year ends, e.g. `2015L`, or
#'   `NULL` when `x` is not a reporting year
#' @keywords internal
parse_reporting_year <- function(x) {
  if (!grepl("^[0-9]{4}-[0-9]{2}$", x)) {
    return(NULL)
  }
  end_year <- as.integer(substr(x, 1, 4)) + 1L
  if (end_year %% 100L == as.integer(substr(x, 6, 7))) end_year
}

#' Format the year a reporting year ends in as its label
#'
#' @param end_year Integer, e.g. `2015L`
#' @return String, e.g. `"2014-15"`
#' @keywords internal
format_reporting_year <- function(end_year) {
  sprintf("%d-%02d", end_year - 1L, end_year %% 100L)
}

#' Select the end dates of a date range from reporting years
#'
#' @param end_dates Date vector of the available end dates
#' @param from,to Integer years in which the first and last reporting year
#'   end, as returned by [parse_reporting_year()], or `NULL` for the first or
#'   last year in the data
#'
#' @return Date vector of length 2, the first and last end date with a year
#'   from `from` to `to`, or the full range when none match
#' @keywords internal
select_end_dates <- function(end_dates, from = NULL, to = NULL) {
  years <- as.integer(format(end_dates, "%Y"))
  if (is.null(from)) {
    from <- min(years)
  }
  if (is.null(to)) {
    to <- max(years)
  }

  keep <- years >= from & years <= to
  if (!any(keep)) {
    return(range(end_dates))
  }
  range(end_dates[keep])
}

#' OpenCodelists URL of a codelist slug
#'
#' @param slug String, codelist slug, or `NULL`
#' @return String, or `NULL` when `slug` is `NULL`
#' @keywords internal
codelist_url <- function(slug) {
  if (!is.null(slug)) {
    paste0("https://www.opencodelists.org/codelist/", slug, "/")
  }
}
