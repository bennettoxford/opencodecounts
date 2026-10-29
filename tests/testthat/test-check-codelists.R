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

  not_selected_old <- results[results$old_code == "836541000000100", ]
  expect_false(not_selected_old$is_old_in_codelist)
  expect_equal(not_selected_old$old_usage, 150L)
  expect_equal(not_selected_old$new_usage, 50L)

  expect_true(not_selected_old$is_not_selected_with_usage)

  no_usage <- results[results$old_code == "111111111", ]
  expect_equal(no_usage$old_usage, 0)
  expect_false(no_usage$is_not_selected_with_usage)
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
  not_selected_old <- check@results[
    check@results$old_code == "836541000000100",
  ]

  expect_equal(not_selected_old$old_usage, 4e9)
})

test_that("not_selected_usage_found flags linked codes not selected with usage", {
  expect_true(local_test_check()@not_selected_usage_found)

  local_check_mocks()
  local_mocked_bindings(
    get_snomed_history = function(version) test_snomed_history()[2:3, ]
  )
  check <- check_codelists(
    test_codelists_dir(),
    data_version = "0.2.0",
    history_version = "0.1.0"
  )
  expect_false(check@not_selected_usage_found)
  expect_snapshot(print(check))
})

test_that("is_not_selected_with_usage reports linked codes with usage, except ambiguous pairs", {
  local_check_mocks()
  local_mocked_bindings(
    # 1359726006 is in the codelist but has no usage
    get_gp_snomed = function(version = NULL) {
      dplyr::filter(test_snomed_usage(), snomed_code != "1359726006")
    },
    # Each old code is in the codelist and the new code is not selected with usage
    get_snomed_history = function(version) {
      tibble::tibble(
        old_code = c("1104961000000104", "1359723003", "1359726006"),
        old_term = c("Used", "Used but ambiguous", "Not used"),
        new_code = "836541000000100",
        new_term = "Not selected with usage",
        is_ambiguous = c(FALSE, TRUE, FALSE),
        trud_release = "uk_sctqths_test_release"
      )
    }
  )
  results <- check_codelists(
    test_codelists_dir(),
    data_version = "0.2.0",
    history_version = "0.1.0"
  )@results

  expect_equal(
    results$is_not_selected_with_usage[match(
      c("1104961000000104", "1359723003", "1359726006"),
      results$old_code
    )],
    # The selected old code needs no usage of its own
    c(TRUE, FALSE, TRUE)
  )
})

test_that("check_codelists() summarises each codelist", {
  summary <- local_test_check()@summary

  expect_equal(summary$n_codes, 3L)
  expect_equal(summary$usage, 85)
  # 836541000000100 has usage and is not selected. 111111111 has no usage.
  expect_equal(summary$n_not_selected_codes, 1L)
  expect_equal(summary$not_selected_usage, 150)
  expect_equal(summary$not_selected_share, 150 / 235)
})

test_that("check_codelists() only uses the selected reporting periods", {
  check <- local_test_check(start_date = "2012-08-01")
  not_selected_old <- check@results[
    check@results$old_code == "836541000000100",
  ]

  expect_equal(check@start_date, as.Date("2012-08-01"))
  expect_equal(not_selected_old$old_usage, 50L)
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
    is_not_selected_with_usage = logical()
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
  expect_false(check@not_selected_usage_found)
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

test_that("find_linked_codes() flags pairs from data that is already loaded", {
  results <- find_linked_codes(
    dplyr::mutate(test_codelist(), codelist = "phq"),
    test_usage_total(),
    test_snomed_history()
  )

  expect_equal(nrow(results), 3)
  expect_equal(
    results$old_code[results$is_not_selected_with_usage],
    "836541000000100"
  )
})

test_that("linked_codes_not_selected() returns one row per reported pair", {
  not_selected <- find_linked_codes(
    dplyr::mutate(test_codelist(), codelist = "phq"),
    test_usage_total(),
    test_snomed_history()
  ) |>
    linked_codes_not_selected()

  expect_equal(
    not_selected,
    tibble::tibble(
      codelist = "phq",
      pair = "836541000000100 1359723003",
      code = "836541000000100",
      term = "PHQ-2 score (old)",
      side = "old",
      usage = 150,
      share = 0.75,
      linked_code = "1359723003",
      linked_term = "PHQ-2 score",
      linked_usage = 50
    )
  )
})

test_that("linked_codes_not_selected() reports a new code not selected", {
  codelist <- tibble::tibble(codelist = "phq", code = "836541000000100")

  not_selected <- find_linked_codes(
    codelist,
    test_usage_total(),
    test_snomed_history()
  ) |>
    linked_codes_not_selected()

  expect_equal(not_selected$code, "1359723003")
  expect_equal(not_selected$side, "new")
  expect_equal(not_selected$usage, 50)
  expect_equal(not_selected$linked_code, "836541000000100")
  expect_equal(not_selected$linked_term, "PHQ-2 score (old)")
  expect_equal(not_selected$linked_usage, 150)
})

test_that("check_codelists() records the TRUD release of the history table", {
  check <- local_test_check()

  expect_equal(check@trud_release, "uk_sctqths_test_release")
  expect_equal(unique(check@results$trud_release), "uk_sctqths_test_release")
  expect_equal(
    utils::tail(names(check@results), 2),
    c("history_version", "trud_release")
  )
})
