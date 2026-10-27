test_that("check_codelists() checks only SNOMED CT codelists", {
  check <- local_test_check()

  expect_true(S7::S7_inherits(check, CodelistCheck))
  expect_equal(check@summary$codelist, "user-test-phq-snomed")
  expect_equal(
    check@skipped_codelists,
    c("opensafely/test-dmd/def67890" = "dmd")
  )
  expect_equal(unique(check@results$codelist), "user-test-phq-snomed")
})

test_that("check_codelists() adds usage for both codes of each pair", {
  results <- local_test_check()@results

  expect_equal(nrow(results), 3)
  expect_equal(
    names(results)[1:9],
    c(
      "codelist",
      "old_code",
      "old_term",
      "is_old_in_codelist",
      "old_usage",
      "new_code",
      "new_term",
      "is_new_in_codelist",
      "new_usage"
    )
  )

  missing_old <- results[results$old_code == "836541000000100", ]
  expect_false(missing_old$is_old_in_codelist)
  expect_equal(missing_old$old_usage, 150L)
  expect_equal(missing_old$new_usage, 50L)

  expect_true(missing_old$is_missing_with_usage)

  no_usage <- results[results$old_code == "111111111", ]
  expect_equal(no_usage$old_usage, 0)
  expect_false(no_usage$is_missing_with_usage)
})

test_that("check_codelists() sums usage beyond the integer range", {
  local_check_mocks()
  local_mocked_bindings(
    get_gp_snomed = function(version = NULL) {
      test_snomed_usage() |>
        dplyr::mutate(
          usage = dplyr::if_else(snomed_code == "836541000000100", 2e9L, usage)
        )
    }
  )
  check <- check_codelists(
    test_codelists_dir(),
    data_version = "0.2.0",
    history_version = "0.1.0"
  )
  missing_old <- check@results[check@results$old_code == "836541000000100", ]

  expect_equal(missing_old$old_usage, 4e9)
})

test_that("missing_usage_found flags missing codes with usage", {
  expect_true(local_test_check()@missing_usage_found)

  local_check_mocks()
  local_mocked_bindings(
    get_snomed_history = function(version) test_snomed_history()[2:3, ]
  )
  check <- check_codelists(
    test_codelists_dir(),
    data_version = "0.2.0",
    history_version = "0.1.0"
  )
  expect_false(check@missing_usage_found)
  expect_snapshot(print(check))
})

test_that("is_missing_with_usage ignores ambiguous pairs and unused old codes", {
  local_check_mocks()
  local_mocked_bindings(
    # 1359726006 is in the codelist but has no usage
    get_gp_snomed = function(version = NULL) {
      dplyr::filter(test_snomed_usage(), snomed_code != "1359726006")
    },
    # Each old code is in the codelist and the new code is missing with usage
    get_snomed_history = function(version) {
      tibble::tibble(
        old_code = c("1104961000000104", "1359723003", "1359726006"),
        old_term = c("Used", "Used but ambiguous", "Not used"),
        new_code = "836541000000100",
        new_term = "Missing with usage",
        is_ambiguous = c(FALSE, TRUE, FALSE)
      )
    }
  )
  results <- check_codelists(
    test_codelists_dir(),
    data_version = "0.2.0",
    history_version = "0.1.0"
  )@results

  expect_equal(
    results$is_missing_with_usage[match(
      c("1104961000000104", "1359723003", "1359726006"),
      results$old_code
    )],
    c(TRUE, FALSE, FALSE)
  )
})

test_that("check_codelists() summarises each codelist", {
  summary <- local_test_check()@summary

  expect_equal(summary$n_codes, 3L)
  expect_equal(summary$usage, 85)
  # 836541000000100 has usage and is missing. 111111111 has no usage.
  expect_equal(summary$n_missing_codes, 1L)
  expect_equal(summary$missing_usage, 150)
  expect_equal(summary$missing_share, 150 / 235)
})

test_that("check_codelists() only uses the selected reporting periods", {
  check <- local_test_check(start_date = "2012-08-01")
  missing_old <- check@results[check@results$old_code == "836541000000100", ]

  expect_equal(check@start_date, as.Date("2012-08-01"))
  expect_equal(missing_old$old_usage, 50L)
})

test_that("check_codelists() skips codelists not found on OpenCodelists", {
  local_check_mocks()
  local_mocked_bindings(
    get_coding_systems = function(codelist_ids) {
      ifelse(grepl("dmd", codelist_ids), NA_character_, "snomedct")
    }
  )

  check <- check_codelists(
    test_codelists_dir(),
    data_version = "0.2.0",
    history_version = "0.1.0"
  )
  expect_equal(check@summary$codelist, "user-test-phq-snomed")
  expect_equal(
    check@skipped_codelists,
    c("opensafely/test-dmd/def67890" = NA_character_)
  )
  expect_snapshot(print(check))
})

test_that("check_codelists() errors without SNOMED CT codelists", {
  local_check_mocks()
  local_mocked_bindings(
    get_coding_systems = function(codelist_ids) rep("dmd", length(codelist_ids))
  )

  expect_error(
    check_codelists(
      test_codelists_dir(),
      data_version = "0.2.0",
      history_version = "0.1.0"
    ),
    class = "opencodecounts_error_no_snomed_codelists"
  )
})

test_that("filter_usage_periods() only accepts period boundaries", {
  usage <- test_snomed_usage()

  expect_equal(nrow(filter_usage_periods(usage, NULL, NULL)), 6)
  expect_equal(nrow(filter_usage_periods(usage, NULL, "2012-07-31")), 3)
  expect_error(
    filter_usage_periods(usage, NULL, "2012-08-01"),
    class = "opencodecounts_error_invalid_date"
  )
  expect_error(
    filter_usage_periods(usage, "2012-08-01", "2012-07-31"),
    class = "opencodecounts_error_invalid_date"
  )
  expect_error(
    filter_usage_periods(usage, "2020-13-01", NULL),
    class = "opencodecounts_error_invalid_date"
  )
  expect_snapshot(filter_usage_periods(usage, "2012-01-01", NULL), error = TRUE)
})

test_that("print() shows the report", {
  expect_snapshot(print(local_test_check()))
})

test_that("summarise_codelists() lists organisations before users", {
  codelists <- tibble::tibble(
    codelist = c("user-b-x", "org-y", "user-a-y", "nhsd-z"),
    code = c("1", "2", "3", "4")
  )
  usage_total <- tibble::tibble(snomed_code = character(), usage = numeric())
  results <- tibble::tibble(
    codelist = character(),
    old_code = character(),
    old_usage = numeric(),
    is_old_in_codelist = logical(),
    new_code = character(),
    new_usage = numeric(),
    is_new_in_codelist = logical(),
    is_missing_with_usage = logical()
  )

  expect_equal(
    summarise_codelists(codelists, usage_total, results)$codelist,
    c("nhsd-z", "org-y", "user-a-y", "user-b-x")
  )
})

test_that("check_codelists() errors when the SNOMED CT codelists have no codes", {
  dir <- withr::local_tempdir()
  writeLines("user/test/empty/1", file.path(dir, "codelists.txt"))
  writeLines("code,term", file.path(dir, "user-test-empty.csv"))
  local_check_mocks()

  expect_error(
    check_codelists(dir, data_version = "0.2.0", history_version = "x"),
    class = "opencodecounts_error_no_snomed_codelists"
  )
})

test_that("check_codelists() says how many codelists were not found", {
  local_check_mocks()
  local_mocked_bindings(
    get_coding_systems = function(codelist_ids) {
      rep(NA_character_, length(codelist_ids))
    }
  )

  error <- expect_error(
    check_codelists(
      test_codelists_dir(),
      data_version = "0.2.0",
      history_version = "0.1.0"
    ),
    class = "opencodecounts_error_no_snomed_codelists"
  )
  expect_match(conditionMessage(error), "2 codelists not found")
})

test_that("check_codelists() handles codelists without history pairs", {
  local_check_mocks()
  local_mocked_bindings(
    get_snomed_history = function(version) test_snomed_history()[0, ]
  )
  check <- check_codelists(
    test_codelists_dir(),
    data_version = "0.2.0",
    history_version = "0.1.0"
  )

  expect_equal(nrow(check@results), 0)
  expect_false(check@missing_usage_found)
  expect_no_error(capture.output(print(check), type = "message"))
})

test_that("filter_usage_periods() rejects more than one date", {
  expect_error(
    filter_usage_periods(
      test_snomed_usage(),
      c("2011-08-01", "2012-08-01"),
      NULL
    ),
    class = "opencodecounts_error_invalid_date"
  )
})

test_that("check_codelists() checks codelists from their URLs", {
  local_check_mocks()
  local_mocked_bindings(
    get_codelist = function(url) {
      Codelist(
        readr::read_csv(
          file.path(test_codelists_dir(), "user-test-phq-snomed.csv"),
          col_types = readr::cols(.default = readr::col_character())
        ),
        coding_system = "snomedct",
        full_slug = "user/test/phq-snomed/abc12345"
      )
    }
  )

  check <- check_codelists(
    url = "https://www.opencodelists.org/codelist/user/test/phq-snomed/abc12345/",
    data_version = "0.2.0",
    history_version = "0.1.0"
  )

  expect_equal(check@summary, local_test_check()@summary)
  expect_equal(check@results, local_test_check()@results)
})
