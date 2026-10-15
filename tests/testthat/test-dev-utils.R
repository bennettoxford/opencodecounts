test_that("copy_app_data_for_deploy() copies the cached parquet for every dataset into dest_dir", {
  dest_dir <- withr::local_tempdir()
  source_dir <- withr::local_tempdir()

  # A fake cached parquet for each dataset, standing in for the real cache
  cache_paths <- list()
  for (dataset in names(load_tidy_sources_config())) {
    version <- get_tidy_source_config(dataset)$version
    path <- file.path(source_dir, paste0(dataset, "_", version, ".parquet"))
    writeLines(paste("fake parquet for", dataset), path)
    cache_paths[[dataset]] <- path
  }

  local_mocked_bindings(
    tidy_source_cache_is_current = function(dataset, version) TRUE,
    get_tidy_source_cache_path = function(dataset, version) {
      cache_paths[[dataset]]
    }
  )

  result <- copy_app_data_for_deploy(dest_dir)

  for (dataset in names(load_tidy_sources_config())) {
    version <- get_tidy_source_config(dataset)$version
    expected_path <- file.path(
      dest_dir,
      paste0(dataset, "_", version, ".parquet")
    )
    expect_true(file.exists(expected_path), info = dataset)
    expect_true(expected_path %in% result, info = dataset)
  }
})

test_that("copy_app_data_for_deploy() downloads a dataset first if it isn't cached yet", {
  dest_dir <- withr::local_tempdir()
  source_dir <- withr::local_tempdir()
  download_calls <- character(0)

  local_mocked_bindings(
    tidy_source_cache_is_current = function(dataset, version) FALSE,
    download_tidy_source = function(dataset, url, version) {
      download_calls <<- c(download_calls, dataset)
      path <- file.path(source_dir, paste0(dataset, "_", version, ".parquet"))
      writeLines("fake parquet", path)
      invisible(path)
    },
    get_tidy_source_cache_path = function(dataset, version) {
      file.path(source_dir, paste0(dataset, "_", version, ".parquet"))
    }
  )

  copy_app_data_for_deploy(dest_dir)

  expect_setequal(download_calls, names(load_tidy_sources_config()))
})

test_that("copy_app_data_for_deploy() creates dest_dir if it doesn't exist", {
  dest_dir <- file.path(withr::local_tempdir(), "app-data")
  source_dir <- withr::local_tempdir()

  local_mocked_bindings(
    tidy_source_cache_is_current = function(dataset, version) TRUE,
    get_tidy_source_cache_path = function(dataset, version) {
      path <- file.path(source_dir, paste0(dataset, "_", version, ".parquet"))
      if (!file.exists(path)) {
        writeLines("fake parquet", path)
      }
      path
    }
  )

  expect_false(dir.exists(dest_dir))
  copy_app_data_for_deploy(dest_dir)
  expect_true(dir.exists(dest_dir))
})
