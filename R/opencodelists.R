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
Codelist <- S7::new_class(
  "Codelist",
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
    cli::cli_abort(
      "URL must be from {.url https://www.opencodelists.org}",
      class = "opencodecounts_error_codelist_url"
    )
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
    cli::cli_abort(
      "URL does not match expected OpenCodelists codelist pattern",
      class = "opencodecounts_error_codelist_url"
    )
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

#' Helper function to get the coding system and slug of every codelist
#' version of an organisation or user on OpenCodelists
#' @param codelist_org String, organisation from
#'   [get_codelist_organisation()]
#' @return Tibble with columns `coding_system_id`, `full_slug` and `hash`.
#'   Tagged versions are listed by tag and by hash.
#' @keywords internal
get_codelist_versions <- function(codelist_org) {
  url_api_base <- "https://www.opencodelists.org/api/v1/codelist/"
  request <- httr2::request(paste0(url_api_base, codelist_org)) |>
    httr2::req_url_query(`include-users` = "true") |>
    # An organisation or user that does not exist has no codelists
    httr2::req_error(
      is_error = \(resp) {
        httr2::resp_is_error(resp) && httr2::resp_status(resp) != 404
      }
    )

  response <- httr2::req_perform(request)
  no_versions <- tibble::tibble(
    coding_system_id = character(),
    full_slug = character(),
    hash = character()
  )
  if (httr2::resp_status(response) == 404) {
    return(no_versions)
  }
  response_json <- response |> httr2::resp_body_json()

  versions <- response_json$codelists |>
    purrr::map_dfr(
      ~ {
        coding_system_id <- .x$coding_system_id
        purrr::map_dfr(
          .x$versions,
          ~ tibble::tibble(
            coding_system_id = coding_system_id,
            # Tagged versions are listed by tag but can also be used by hash
            full_slug = unique(c(
              .x$full_slug,
              if (!is.null(.x$hash)) sub("[^/]+$", .x$hash, .x$full_slug)
            )),
            hash = if (is.null(.x$hash)) NA_character_ else .x$hash
          )
        )
      }
    )

  # Keep the columns when the listing is empty
  dplyr::bind_rows(no_versions, versions)
}

#' Helper function to get the coding system of codelist versions from
#' OpenCodelists
#'
#' @param codelist_ids Character vector of codelist slugs including the
#'   version
#' @return Character vector of coding system IDs, `NA` for codelists not
#'   found on OpenCodelists
#' @keywords internal
get_coding_systems <- function(codelist_ids) {
  codelist_versions <- purrr::map_chr(
    codelist_ids,
    get_codelist_organisation
  ) |>
    unique() |>
    purrr::map(get_codelist_versions) |>
    purrr::list_rbind()

  coding_systems <- codelist_versions$coding_system_id[
    match(codelist_ids, codelist_versions$full_slug)
  ]

  is_not_found <- is.na(coding_systems)
  if (any(is_not_found)) {
    coding_systems[is_not_found] <- get_moved_coding_systems(
      codelist_ids[is_not_found]
    )
  }

  coding_systems
}

#' Helper function to get the coding system of codelist versions that moved
#' or were renamed on OpenCodelists
#'
#' Version hashes are unique across OpenCodelists and do not change when a
#' codelist moves or is renamed, so versions are matched on their hash. Versions
#' referred to by tag are not matched.
#'
#' @param codelist_ids Character vector of codelist slugs including the
#'   version, not found under their owner
#' @return Character vector of coding system IDs, `NA` for codelists not
#'   found by hash
#' @keywords internal
get_moved_coding_systems <- function(codelist_ids) {
  # Listing every codelist is slow, so it is only done when needed
  all_versions <- get_codelist_versions("") |>
    dplyr::filter(basename(full_slug) == hash)

  match_index <- match(basename(codelist_ids), all_versions$hash)
  is_moved <- !is.na(match_index)
  if (any(is_moved)) {
    cli::cli_inform(c(
      "i" = "Found {sum(is_moved)} codelist{?s} under a new ID on OpenCodelists. Update {.file codelists.txt} to use the new ID{?s}:",
      purrr::set_names(
        paste(
          codelist_ids[is_moved],
          "is now",
          all_versions$full_slug[match_index[is_moved]]
        ),
        " "
      )
    ))
  }

  all_versions$coding_system_id[match_index]
}

#' Helper function to name the code column of a codelist `code`
#'
#' Drops unnamed columns and renames the first column to `code` if there is no
#' `code` column.
#'
#' @param codelist Data frame read from a codelist CSV file
#' @return `codelist` with a `code` column
#' @keywords internal
rename_code_column <- function(codelist) {
  # readr names empty column names ...1, ...2 and so on
  codelist <- dplyr::select(codelist, !dplyr::matches("^\\.\\.\\.[0-9]+$"))
  if ("code" %in% names(codelist)) {
    return(codelist)
  }
  dplyr::rename(codelist, code = 1)
}

#' Get codelist from [OpenCodelists](https://www.opencodelists.org)
#'
#' @param url String, specifying URL to codelist on [OpenCodelists](https://www.opencodelists.org)
#' @export
#' @examples
#' # Get the 'cpeptide_cod' codelist from OpenCodelists.org
#' cpeptide_slug <- "nhsd-primary-care-domain-refsets/cpeptide_cod/20200812"
#' cpeptide_cod <- get_codelist(paste0("https://www.opencodelists.org/codelist/", cpeptide_slug))
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
  if (grepl("^https://www\\.opencodelists\\.org/", url)) {
    codelist_slug <- extract_codelist_slug(url)
  } else {
    if (!grepl("^[^/]+/[^/]+/[^/]+/?$|^user/[^/]+/[^/]+/[^/]+/?$", url)) {
      cli::cli_abort(
        "Invalid format. Please use full OpenCodelists URL or ensure slug follows {.val org/name/version} or {.val user/username/name/version} pattern.",
        class = "opencodecounts_error_codelist_url"
      )
    }

    cli::cli_inform(c(
      "i" = "For clarity, please use the full OpenCodelists URL instead of just the slug.",
      "i" = "Full URL would be: {.url https://www.opencodelists.org/codelist/{url}}"
    ))
    codelist_slug <- sub("/$", "", url)
  }

  url_download_base <- "https://www.opencodelists.org/codelist/"
  url_download <- paste0(url_download_base, codelist_slug, "/download.csv")

  coding_system <- get_coding_systems(codelist_slug)
  if (is.na(coding_system)) {
    cli::cli_abort(
      "Codelist {.val {codelist_slug}} not found on OpenCodelists.",
      class = "opencodecounts_error_codelist_not_found"
    )
  }

  codelist_dfr <- readr::read_csv(
    url_download,
    col_types = readr::cols(.default = readr::col_character()),
    name_repair = "unique_quiet"
  ) |>
    rename_code_column()

  Codelist(
    codelist_dfr,
    coding_system = coding_system,
    full_slug = codelist_slug
  )
}

#' Name codelists the way `opensafely codelists update` names their CSV files
#'
#' @param codelist_ids Character vector of codelist slugs including the
#'   version
#'
#' @return Character vector, the slug without the version and with `/`
#'   replaced by `-`
#'
#' @noRd
codelist_name <- function(codelist_ids) {
  gsub("/", "-", sub("/[^/]+$", "", codelist_ids))
}

#' Parse a codelists.txt file
#'
#' Parses `codelists.txt` the same way as `opensafely codelists update`.
#'
#' @param dir String, path to the codelists directory
#'
#' @return Tibble with columns `codelist`, `codelist_id` and `file`
#'
#' @noRd
read_codelists_txt <- function(dir) {
  path <- file.path(dir, "codelists.txt")
  if (!file.exists(path)) {
    cli::cli_abort(
      "No codelists file found at {.file {path}}.",
      class = "opencodecounts_error_codelists_txt"
    )
  }

  ids <- sub("/+$", "", trimws(readLines(path, warn = FALSE)))
  ids <- ids[nzchar(ids) & !startsWith(ids, "#")]
  codelist <- codelist_name(ids)

  tibble::tibble(
    codelist = codelist,
    codelist_id = ids,
    file = file.path(dir, paste0(codelist, ".csv"))
  )
}

#' Read codelists listed in codelists.txt
#'
#' Reads the codelist CSV files from `opensafely codelists update`.
#'
#' @param dir String, path to the codelists directory. Default `"codelists"`.
#'
#' @return Named list of tibbles, one per codelist, each with a `code` column.
#'
#' @export
read_codelists <- function(dir = "codelists") {
  codelists_txt <- read_codelists_txt(dir)

  missing_files <- codelists_txt$file[!file.exists(codelists_txt$file)]
  if (length(missing_files) > 0) {
    cli::cli_abort(
      c(
        "Codelist CSV{?s} not found: {.file {basename(missing_files)}}.",
        "i" = "Run {.code opensafely codelists update} to download them."
      ),
      class = "opencodecounts_error_codelist_files"
    )
  }

  codelists_txt$file |>
    purrr::set_names(codelists_txt$codelist) |>
    purrr::map(
      \(path) {
        readr::read_csv(
          path,
          col_types = readr::cols(.default = readr::col_character()),
          name_repair = "unique_quiet",
          progress = FALSE
        ) |>
          rename_code_column()
      }
    )
}
