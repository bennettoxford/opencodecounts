#' Codelist
#' Note that the "@usage" argument is explicitly coded here to suppress an error.
#' This is most likely due to S7 class, we should revisit this in future.
#' @param .data Initial data
#' @param row.names Row names
#' @section Properties:
#' \describe{
#'   \item{coding_system}{String, specifying the coding system of the codelist.}
#'   \item{full_slug}{slug of the codelist from OpenCodelists.}
#' }
#' @return New instance of class Codelist
#' @keywords internal
#' @importFrom S7 new_class class_data.frame class_character
#' @usage Codelist(.data = list(),
#' row.names = NULL,
#' coding_system = character(0),
#' full_slug = character(0))
Codelist <- S7::new_class("Codelist",
  parent = S7::class_data.frame,
  properties = list(
    coding_system = S7::class_character,
    full_slug = S7::class_character
  )
)

#' Helper function to extract the codelist slug from the OpenCodelists URL
#' @keywords internal
extract_codelist_slug <- function(url) {
  # Check if URL is from opencodelists.org
  if (!grepl("^https://www\\.opencodelists\\.org/", url)) {
    stop("URL must be from https://www.opencodelists.org")
  }
  
  # Remove fragment identifier if present
  url_clean <- sub("#.*$", "", url)
  
  # Extract path after /codelist/ - handles both org and user patterns
  pattern <- "^https://www\\.opencodelists\\.org/codelist/((?:user/[^/]+|[^/]+)/[^/]+/[^/]+)"
  
  if (grepl(pattern, url_clean)) {
    codelist_slug <- (sub(pattern, "\\1", url_clean))
    codelist_slug <- sub("/$", "", codelist_slug)
    codelist_slug
  } else {
    stop("URL does not match expected OpenCodelists codelist pattern")
  }
}

#' Helper function to get the organisation for a codelist from OpenCodelists
#' This is important to use the API from OpenCodelists
#' @keywords internal
get_codelist_organisation <- function(codelist_slug) {
  first_part <- stringr::str_extract(codelist_slug, "^[^/]+")
  if (!first_part == "user") {
    first_part
  } else {
    all_parts <- stringr::str_split(codelist_slug, "/")
    paste(all_parts[[1]][1], all_parts[[1]][2], sep = "/")
  }
}

#' Get codelist from [OpenCodelists](https://www.opencodelists.org)
#'
#' @param url String, specifying URL to codelist on [OpenCodelists](https://www.opencodelists.org)
#' @export
#' @examples
#' # Get the 'cpeptide_cod' codelist from OpenCodelists.org
#' cpeptide_cod <- get_codelist("https://www.opencodelists.org/codelist/nhsd-primary-care-domain-refsets/cpeptide_cod/20200812/")
#'
#' # Return all codes
#' cpeptide_cod$code
#'
#' # Return 'coding_system' of codelist
#' cpeptide_cod@coding_system
#'
#' # Return 'full_slug' of codelist
#' cpeptide_cod@full_slug
get_codelist <- function(url) {

  codelist_slug <- extract_codelist_slug(url)
  url_api_base <- "https://www.opencodelists.org/api/v1/codelist/"
  url_download_base <- "https://www.opencodelists.org/codelist/"
  url_download <- paste0(url_download_base, codelist_slug, "/download.csv")

  codelist_org <- get_codelist_organisation(codelist_slug)
  url_request <- paste0(url_api_base, codelist_org)

  request <- httr2::request(url_request) |>
    httr2::req_url_query(`include-users` = "true")

  response <- httr2::req_perform(request)
  response_json <- response |> httr2::resp_body_json()

  codelists_dfr <- response_json$codelists |>
    purrr::map_dfr(~ {
      coding_system_id <- .x$coding_system_id
      purrr::map_dfr(.x$versions, ~ tibble::tibble(
        coding_system_id = coding_system_id,
        full_slug = .x$full_slug
      ))
    })

  codelist_info <- codelists_dfr |>
    dplyr::filter(full_slug == codelist_slug) |>
    as.vector()

  codelist_dfr <- readr::read_csv(url_download,
    col_types = readr::cols(.default = readr::col_character())
  ) |>
    dplyr::rename(code = 1)

  Codelist(
    codelist_dfr,
    coding_system = codelist_info$coding_system_id,
    full_slug = codelist_info$full_slug
  )
}
