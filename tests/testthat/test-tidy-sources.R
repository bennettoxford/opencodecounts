test_that("load_tidy_sources_config() parses the package's tidy_data_sources.yml", {
  sources <- load_tidy_sources_config()

  expect_true(all(
    c(
      "gp_snomed",
      "hesapc_icd10",
      "hesapc_icd10_breakdowns",
      "hesapc_opcs4",
      "hesapc_opcs4_breakdowns"
    ) %in%
      names(sources)
  ))
  expect_equal(sources$gp_snomed$version, "0.2.0")
  expect_true(grepl("^https://", sources$gp_snomed$url))
})

test_that("get_tidy_source_config() returns the latest version by default", {
  cfg <- get_tidy_source_config("gp_snomed")

  expect_equal(cfg$version, "0.2.0")
  expect_true(grepl("gp_snomed\\.parquet$", cfg$url))
})

test_that("get_tidy_source_config() errors for an unknown dataset", {
  expect_error(
    get_tidy_source_config("not_a_real_dataset"),
    class = "opencodecounts_error_dataset_not_found"
  )
})

test_that("get_tidy_source_config() errors for an unknown version", {
  expect_error(
    get_tidy_source_config("gp_snomed", version = "99.0.0"),
    class = "opencodecounts_error_version_not_found"
  )
})

test_that("available_versions() lists known versions newest first", {
  expect_equal(available_versions("gp_snomed"), c("0.2.0", "0.1.0"))
})

test_that("available_versions() errors for an unknown dataset", {
  expect_error(
    available_versions("not_a_real_dataset"),
    class = "opencodecounts_error_dataset_not_found"
  )
})

test_that("get_tidy_source_cache_path() and sidecar path live under the cache data dir", {
  test_dir <- withr::local_tempdir()
  withr::local_envvar(OPENCODECOUNTS_TEST_CACHE_DIR = test_dir)

  expect_equal(
    get_tidy_source_cache_path("gp_snomed", "1.0.0"),
    file.path(test_dir, "data", "gp_snomed_1.0.0.parquet")
  )
  expect_equal(
    get_tidy_source_sidecar_path("gp_snomed", "1.0.0"),
    file.path(test_dir, "data", "gp_snomed_1.0.0.json")
  )
})

test_that("tidy_source_cache_is_current() reflects presence of the cached file", {
  test_dir <- withr::local_tempdir()
  withr::local_envvar(OPENCODECOUNTS_TEST_CACHE_DIR = test_dir)

  expect_false(tidy_source_cache_is_current("gp_snomed", "1.0.0"))

  writeLines(
    "fake parquet",
    get_tidy_source_cache_path("gp_snomed", "1.0.0")
  )

  expect_true(tidy_source_cache_is_current("gp_snomed", "1.0.0"))
})

test_that("download_tidy_source() writes the parquet into the cache and a sidecar JSON", {
  test_dir <- withr::local_tempdir()
  withr::local_envvar(OPENCODECOUNTS_TEST_CACHE_DIR = test_dir)

  local_mocked_bindings(
    req_perform = function(req, path = NULL, ...) {
      writeLines("fake parquet bytes", path)
      invisible(NULL)
    }
  )

  cache_path <- download_tidy_source(
    "gp_snomed",
    "https://example.com/gp_snomed.parquet",
    "1.0.0"
  )

  expect_true(file.exists(cache_path))
  expect_true(file.exists(get_tidy_source_sidecar_path(
    "gp_snomed",
    "1.0.0"
  )))

  sidecar <- jsonlite::read_json(get_tidy_source_sidecar_path(
    "gp_snomed",
    "1.0.0"
  ))
  expect_equal(sidecar$dataset, "gp_snomed")
  expect_equal(sidecar$version, "1.0.0")
})

test_that("download_tidy_source() cleans up the temp file and errors on failure", {
  test_dir <- withr::local_tempdir()
  withr::local_envvar(OPENCODECOUNTS_TEST_CACHE_DIR = test_dir)

  local_mocked_bindings(
    req_perform = function(req, path = NULL, ...) stop("network error")
  )

  expect_error(
    download_tidy_source(
      "gp_snomed",
      "https://example.com/gp_snomed.parquet",
      "1.0.0"
    ),
    class = "opencodecounts_error_download_failed"
  )
  expect_false(file.exists(get_tidy_source_cache_path("gp_snomed", "1.0.0")))
  expect_false(file.exists(paste0(
    get_tidy_source_cache_path("gp_snomed", "1.0.0"),
    ".tmp"
  )))
})

test_that("load_tidy_source() errors when the cache is empty", {
  test_dir <- withr::local_tempdir()
  withr::local_envvar(OPENCODECOUNTS_TEST_CACHE_DIR = test_dir)

  expect_error(
    load_tidy_source("gp_snomed", "1.0.0"),
    class = "opencodecounts_error_cache_missing"
  )
})
