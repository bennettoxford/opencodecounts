test_that("get_trud_api_key() errors when TRUD_API_KEY is not set", {
  withr::local_envvar(TRUD_API_KEY = "")

  expect_error(get_trud_api_key(), class = "opencodecounts_error_trud_api_key")
})

test_that("perform_trud_request() does not show the URL in errors", {
  error <- expect_error(
    perform_trud_request("http://localhost:1/keys/secret-key/items/276", 276),
    class = "opencodecounts_error_download_failed"
  )

  expect_no_match(conditionMessage(error), "secret-key")
  expect_null(error$parent)
})

test_that("perform_trud_request() explains access errors", {
  httr2::local_mocked_responses(function(req) httr2::response(403))

  error <- expect_error(
    perform_trud_request("https://example.org/keys/secret-key/items/101", 101),
    class = "opencodecounts_error_trud_access"
  )
  expect_no_match(conditionMessage(error), "secret-key")
  expect_match(conditionMessage(error), "item 101")
})

test_that("get_latest_trud_release() reads the release file name", {
  withr::local_envvar(TRUD_API_KEY = "test-key")
  local_mocked_bindings(
    perform_trud_request = function(url, item, path = NULL) {
      httr2::response_json(
        body = list(
          releases = list(
            list(archiveFileName = "uk_sctqths_43.0.0_20260923000001.zip")
          )
        )
      )
    }
  )

  expect_equal(get_latest_trud_release(), "uk_sctqths_43.0.0_20260923000001")
})

test_that("download_snomed_history() caches active substitutions from a release", {
  release_zip <- test_path("fixtures", "trud", "uk_sctqths_test.zip")
  withr::local_envvar(TRUD_API_KEY = "test-key")
  requested_url <- NULL
  local_mocked_bindings(
    perform_trud_request = function(url, item, path = NULL) {
      requested_url <<- url
      file.copy(release_zip, path)
    }
  )
  cache_path <- withr::local_tempfile(fileext = ".parquet")

  suppressMessages(download_snomed_history("uk_test_release", cache_path))

  expect_equal(
    requested_url,
    "https://isd.digital.nhs.uk/download/api/v1/keys/test-key/content/items/276/uk_test_release.zip"
  )
  expect_equal(
    tibble::as_tibble(arrow::read_parquet(cache_path)),
    tibble::tibble(
      old_code = c("836541000000100", "109006"),
      old_term = c("PHQ-2 score (old)", "Anxiety (old)"),
      new_code = c("1359723003", "192614003"),
      new_term = c("PHQ-2 score", "Anxiety"),
      is_ambiguous = c(FALSE, TRUE)
    )
  )
})

test_that("get_snomed_history() downloads once and then uses the cache", {
  withr::local_envvar(OPENCODECOUNTS_TEST_CACHE_DIR = withr::local_tempdir())
  download_calls <- 0
  local_mocked_bindings(
    download_snomed_history = function(version, cache_path) {
      download_calls <<- download_calls + 1
      arrow::write_parquet(test_snomed_history(), cache_path)
    }
  )

  first <- get_snomed_history("uk_test_release")
  second <- get_snomed_history("uk_test_release")

  expect_equal(download_calls, 1)
  expect_equal(first, test_snomed_history())
  expect_equal(second, first)
})

test_that("cache_info() and cache_clear() include cached history tables", {
  withr::local_envvar(OPENCODECOUNTS_TEST_CACHE_DIR = withr::local_tempdir())
  trud_dir <- file.path(get_cache_dir(), "trud")
  dir.create(trud_dir)
  writeLines("fake", file.path(trud_dir, "uk_test_release.parquet"))

  expect_equal(suppressMessages(cache_info())$history_count, 1)

  suppressMessages(cache_clear())

  expect_false(dir.exists(trud_dir))
})

test_that("get_latest_trud_release() errors when TRUD has no releases", {
  withr::local_envvar(TRUD_API_KEY = "test-key")
  local_mocked_bindings(
    perform_trud_request = function(url, item, path = NULL) {
      httr2::response_json(body = list(releases = list()))
    }
  )

  expect_error(
    get_latest_trud_release(),
    class = "opencodecounts_error_trud_release"
  )
})

test_that("download_snomed_history() errors when the release has no history table", {
  withr::local_envvar(TRUD_API_KEY = "test-key")
  release_zip <- test_path("fixtures", "trud", "uk_sctqths_no_history.zip")
  local_mocked_bindings(
    perform_trud_request = function(url, item, path = NULL) {
      file.copy(release_zip, path)
    }
  )

  expect_error(
    suppressMessages(
      download_snomed_history("uk_test_release", withr::local_tempfile())
    ),
    class = "opencodecounts_error_trud_release"
  )
})
