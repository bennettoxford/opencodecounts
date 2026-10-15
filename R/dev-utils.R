#' Copy the released parquet for every dataset into a local directory
#'
#' Used by `just deploy` to populate `inst/app-data/` before deploying the
#' Shiny app, so the hosted app ships with its data instead of downloading it
#' on every cold start (see `get_local_data_path()`). Always uses the
#' released version from `tidy_data_sources.yml`, downloading it first if it
#' isn't already in the local cache - never bundles unreleased data.
#'
#' @param dest_dir String, directory to copy the parquet files into
#'
#' @return Invisible character vector of the copied file paths
#'
#' @keywords internal
copy_app_data_for_deploy <- function(dest_dir) {
  if (!dir.exists(dest_dir)) {
    dir.create(dest_dir, recursive = TRUE)
  }

  datasets <- names(load_tidy_sources_config())

  paths <- vapply(
    datasets,
    function(dataset) {
      cfg <- get_tidy_source_config(dataset)
      version <- cfg$version

      if (!tidy_source_cache_is_current(dataset, version)) {
        download_tidy_source(dataset, cfg$url, version)
      }

      dest_path <- file.path(
        dest_dir,
        paste0(dataset, "_", version, ".parquet")
      )
      file.copy(
        get_tidy_source_cache_path(dataset, version),
        dest_path,
        overwrite = TRUE
      )
      cli::cli_inform("Copied {.val {dataset}} (v{version})")

      dest_path
    },
    character(1)
  )

  invisible(paths)
}
