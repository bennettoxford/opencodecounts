#' Get cache directory path
#'
#' @return Character path to cache directory
#'
#' @importFrom tools R_user_dir
#'
#' @keywords internal
get_cache_dir <- function() {
  test_cache <- Sys.getenv("OPENCODECOUNTS_TEST_CACHE_DIR", unset = "")
  cache_dir <- if (nzchar(test_cache)) {
    test_cache
  } else {
    R_user_dir("opencodecounts", "cache")
  }

  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }

  cache_dir
}

#' Get data cache directory
#'
#' @return Character path to the directory holding cached dataset parquet files
#'
#' @keywords internal
get_data_cache_dir <- function() {
  data_dir <- file.path(get_cache_dir(), "data")

  if (!dir.exists(data_dir)) {
    dir.create(data_dir, recursive = TRUE)
  }

  data_dir
}

#' Display cache information
#'
#' Shows the cache directory location, the number and total size of cached
#' datasets, and warns if the cache exceeds a recommended size limit.
#'
#' @param max_size_mb Numeric, specifying maximum recommended cache size in MB. Default 1000
#'
#' @return Invisibly returns a list with cache information
#'
#' @export
#'
#' @examples
#' \dontrun{
#' cache_info()
#' cache_info(max_size_mb = 500)
#' }
cache_info <- function(max_size_mb = 1000) {
  cache_dir <- get_cache_dir()
  data_dir <- get_data_cache_dir()

  data_files <- list.files(data_dir, full.names = TRUE, pattern = "\\.parquet$")
  data_size <- sum(file.info(data_files)$size, na.rm = TRUE)
  data_count <- length(data_files)

  cli::cli_inform("Cache directory: {.path {cache_dir}}")
  cli::cli_inform(
    "Cached datasets: {format(structure(data_size, class = 'object_size'), units = 'auto')} ({data_count} dataset{?s})"
  )

  size_mb <- data_size / (1024^2)
  if (size_mb > max_size_mb) {
    cli::cli_warn(
      c(
        "Cache size ({.val {round(size_mb)}} MB) exceeds the recommended limit ({.val {max_size_mb}} MB).",
        "i" = "Run {.run opencodecounts::cache_clear()} to free space."
      ),
      class = "opencodecounts_warning_cache_size"
    )
  }

  invisible(list(
    cache_dir = cache_dir,
    data_size = data_size,
    data_count = data_count
  ))
}

#' Clear the cache
#'
#' Removes all cached dataset parquet files, e.g. to force a fresh download
#' on the next call to a `get_*()` function.
#'
#' @return Invisible `TRUE`
#'
#' @export
#'
#' @examples
#' \dontrun{
#' cache_clear()
#' }
cache_clear <- function() {
  data_dir <- get_data_cache_dir()

  if (dir.exists(data_dir)) {
    unlink(data_dir, recursive = TRUE)
    cli::cli_inform("Cleared cached datasets")
  } else {
    cli::cli_inform("No cached datasets to clear")
  }

  invisible(TRUE)
}
