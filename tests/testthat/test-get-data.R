test_that("get_dataset() downloads when not cached, then loads from cache", {
  download_calls <- 0
  load_calls <- 0

  local_mocked_bindings(
    tidy_source_cache_is_current = function(dataset, version) FALSE,
    download_tidy_source = function(dataset, url, version) {
      download_calls <<- download_calls + 1
      invisible(NULL)
    },
    load_tidy_source = function(dataset, version) {
      load_calls <<- load_calls + 1
      tibble::tibble(x = 1)
    }
  )

  result <- get_dataset("gp_snomed")

  expect_equal(download_calls, 1)
  expect_equal(load_calls, 1)
  expect_equal(result, tibble::tibble(x = 1))
})

test_that("get_local_data_path() returns NULL when inst/app-data doesn't exist", {
  local_dir <- system.file("app-data", package = "opencodecounts")
  skip_if(
    nzchar(local_dir) && dir.exists(local_dir),
    "inst/app-data already exists locally"
  )

  expect_null(get_local_data_path("gp_snomed", "1.0.0"))
})

test_that("get_local_data_path() finds a local file and ignores a missing one", {
  app_data_dir <- file.path(system.file(package = "opencodecounts"), "app-data")
  dir_already_existed <- dir.exists(app_data_dir)
  if (!dir_already_existed) {
    dir.create(app_data_dir)
  }
  withr::defer({
    if (dir_already_existed) {
      unlink(file.path(app_data_dir, "gp_snomed_1.0.0.parquet"))
    } else {
      unlink(app_data_dir, recursive = TRUE)
    }
  })

  writeLines("fake parquet", file.path(app_data_dir, "gp_snomed_1.0.0.parquet"))

  expect_equal(
    get_local_data_path("gp_snomed", "1.0.0"),
    file.path(app_data_dir, "gp_snomed_1.0.0.parquet")
  )
  expect_null(get_local_data_path("gp_snomed", "9.9.9"))
})

test_that("get_dataset() always downloads-and-caches, even when a local app copy exists", {
  cache_calls <- 0
  download_calls <- 0
  local_path_calls <- 0

  local_mocked_bindings(
    get_local_data_path = function(dataset, version) {
      local_path_calls <<- local_path_calls + 1
      "fake/path/gp_snomed_1.0.0.parquet"
    },
    tidy_source_cache_is_current = function(dataset, version) {
      cache_calls <<- cache_calls + 1
      TRUE
    },
    download_tidy_source = function(dataset, url, version) {
      download_calls <<- download_calls + 1
      invisible(NULL)
    },
    load_tidy_source = function(dataset, version) tibble::tibble(x = 1)
  )

  result <- get_dataset("gp_snomed")

  expect_equal(result, tibble::tibble(x = 1))
  expect_equal(local_path_calls, 0)
  expect_equal(cache_calls, 1)
  expect_equal(download_calls, 0)
})

test_that("get_app_dataset() reads the local copy when there is one, skipping cache and download", {
  cache_calls <- 0
  download_calls <- 0

  local_mocked_bindings(
    get_local_data_path = function(dataset, version) {
      "fake/path/gp_snomed_1.0.0.parquet"
    },
    read_parquet = function(path) {
      expect_equal(path, "fake/path/gp_snomed_1.0.0.parquet")
      tibble::tibble(x = 1)
    },
    tidy_source_cache_is_current = function(dataset, version) {
      cache_calls <<- cache_calls + 1
      TRUE
    },
    download_tidy_source = function(dataset, url, version) {
      download_calls <<- download_calls + 1
      invisible(NULL)
    }
  )

  result <- get_app_dataset("gp_snomed")

  expect_equal(result, tibble::tibble(x = 1))
  expect_equal(cache_calls, 0)
  expect_equal(download_calls, 0)
})

test_that("get_app_dataset() falls back to get_dataset() when there is no local copy", {
  dataset_calls <- 0

  local_mocked_bindings(
    get_local_data_path = function(dataset, version) NULL,
    get_dataset = function(dataset, version = NULL) {
      dataset_calls <<- dataset_calls + 1
      tibble::tibble(x = 1)
    }
  )

  result <- get_app_dataset("gp_snomed")

  expect_equal(result, tibble::tibble(x = 1))
  expect_equal(dataset_calls, 1)
})

test_that("get_dataset() skips downloading when already cached", {
  download_calls <- 0

  local_mocked_bindings(
    tidy_source_cache_is_current = function(dataset, version) TRUE,
    download_tidy_source = function(dataset, url, version) {
      download_calls <<- download_calls + 1
      invisible(NULL)
    },
    load_tidy_source = function(dataset, version) tibble::tibble(x = 1)
  )

  get_dataset("gp_snomed")

  expect_equal(download_calls, 0)
})

test_that("each get_*() accessor requests the correct dataset name", {
  requested <- character(0)

  local_mocked_bindings(
    get_dataset = function(dataset, version = NULL) {
      requested <<- c(requested, dataset)
      tibble::tibble(x = 1)
    }
  )

  get_gp_snomed()
  get_hesapc_icd10()
  get_hesapc_icd10_breakdowns()
  get_hesapc_opcs4()
  get_hesapc_opcs4_breakdowns()

  expect_equal(
    requested,
    c(
      "gp_snomed",
      "hesapc_icd10",
      "hesapc_icd10_breakdowns",
      "hesapc_opcs4",
      "hesapc_opcs4_breakdowns"
    )
  )
})

test_that("get_*() accessors pass version through to get_dataset()", {
  captured_version <- NULL

  local_mocked_bindings(
    get_dataset = function(dataset, version = NULL) {
      captured_version <<- version
      tibble::tibble(x = 1)
    }
  )

  get_gp_snomed(version = "1.0.0")

  expect_equal(captured_version, "1.0.0")
})

test_that("deprecated get_*() names still work, warn once, and delegate to the renamed dataset", {
  local_mocked_bindings(get_dataset = function(dataset, version = NULL) {
    tibble::tibble(dataset = dataset)
  })

  lifecycle::expect_deprecated(result <- get_snomed_usage())
  expect_equal(result$dataset, "gp_snomed")

  lifecycle::expect_deprecated(result <- get_icd10_usage())
  expect_equal(result$dataset, "hesapc_icd10")

  lifecycle::expect_deprecated(result <- get_icd10_usage_breakdowns())
  expect_equal(result$dataset, "hesapc_icd10_breakdowns")

  lifecycle::expect_deprecated(result <- get_opcs4_usage())
  expect_equal(result$dataset, "hesapc_opcs4")

  lifecycle::expect_deprecated(result <- get_opcs4_usage_breakdowns())
  expect_equal(result$dataset, "hesapc_opcs4_breakdowns")
})

test_that("ensure_app_datasets_cached() skips datasets that already have a local app copy", {
  ensure_calls <- character(0)

  local_mocked_bindings(
    get_local_data_path = function(dataset, version) "fake/path.parquet",
    ensure_tidy_source_cached = function(dataset, version = NULL) {
      ensure_calls <<- c(ensure_calls, dataset)
      version
    }
  )

  ensure_app_datasets_cached()

  expect_equal(ensure_calls, character(0))
})

test_that("ensure_app_datasets_cached() caches every dataset with no local app copy", {
  ensure_calls <- character(0)

  local_mocked_bindings(
    get_local_data_path = function(dataset, version) NULL,
    ensure_tidy_source_cached = function(dataset, version = NULL) {
      ensure_calls <<- c(ensure_calls, dataset)
      version
    }
  )

  ensure_app_datasets_cached()

  expect_equal(
    sort(ensure_calls),
    sort(c("gp_snomed", "hesapc_icd10", "hesapc_opcs4"))
  )
})
