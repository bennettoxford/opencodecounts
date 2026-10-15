# Integration tests: hit real external services (NHS Digital, OpenCodelists.org).
# Slow and network-dependent - run manually via `just test-integration`, not
# part of the fast unit suite (`just test-unit` / devtools::test()).

devtools::load_all()
library(testthat)

# --- Data rebuild checks --------------------------------------------------
# Rebuilds each dataset from live NHS Digital sources and compares it
# against the currently committed data/*.rda snapshot. This refactor only
# changed where each data-raw/*.R script gets its raw source urls from (now
# inst/config/raw_*.yml instead of a hardcoded list) and how period labels
# turn into start_date/end_date (now the shared add_period_dates() helper
# instead of a per-script regex) - everything else in each script is
# untouched, so a mismatch here would point at one of those two changes.

compare_rebuilt_dataset <- function(build_script, dataset_name) {
  old <- get(dataset_name, envir = asNamespace("opencodecounts"))

  build_env <- new.env(parent = globalenv())
  sys.source(build_script, envir = build_env)
  new <- get(dataset_name, envir = build_env)

  test_that(
    paste(dataset_name, "rebuilds identically to the committed data/*.rda"),
    {
      expect_equal(new, old)
    }
  )
}

compare_rebuilt_dataset(
  "data-raw/gp_snomed.R",
  "snomed_usage"
)
compare_rebuilt_dataset(
  "data-raw/hesapc_icd10.R",
  "icd10_usage"
)
compare_rebuilt_dataset(
  "data-raw/hesapc_icd10_breakdowns.R",
  "icd10_usage_breakdowns"
)
compare_rebuilt_dataset(
  "data-raw/hesapc_opcs4.R",
  "opcs4_usage"
)
compare_rebuilt_dataset(
  "data-raw/hesapc_opcs4_breakdowns.R",
  "opcs4_usage_breakdowns"
)

# --- OpenCodelists.org API calls ------------------------------------------
# Moved from tests/testthat/test-opencodelists.R so the fast unit suite
# doesn't depend on a live third-party API.

test_that("Get codelist from OpenCodelists URL", {
  codelist_slug_user <- "https://www.opencodelists.org/codelist/user/milanwiedemann/diastolic-blood-pressure-qof/697e3433/"
  codelist_slug_org <- "https://www.opencodelists.org/codelist/nhsd-primary-care-domain-refsets/cpeptide_cod/20200812/"

  test_codelist_user <- get_codelist(codelist_slug_user)
  test_codelist_org <- get_codelist(codelist_slug_org)

  expect_equal(nrow(test_codelist_user), 17L)
  expect_equal(nrow(test_codelist_org), 6L)

  expect_equal(test_codelist_user@coding_system, "snomedct")
  expect_equal(test_codelist_org@coding_system, "snomedct")

  expect_equal(
    test_codelist_user@full_slug,
    "user/milanwiedemann/diastolic-blood-pressure-qof/697e3433"
  )
  expect_equal(
    test_codelist_org@full_slug,
    "nhsd-primary-care-domain-refsets/cpeptide_cod/20200812"
  )

  expect_equal(
    test_codelist_user$code,
    c(
      "1091811000000102",
      "163031004",
      "174255007",
      "198091000000104",
      "271650006",
      "314451001",
      "314454009",
      "314456006",
      "314458007",
      "314459004",
      "314461008",
      "314462001",
      "314465004",
      "400975005",
      "407555005",
      "407557002",
      "716632005"
    )
  )
  expect_equal(
    test_codelist_org$code,
    c(
      "1106701000000107",
      "1106721000000103",
      "271227006",
      "401124003",
      "88705004",
      "999351000000102"
    )
  )
})

test_that("Get codelist from OpenCodelists slug", {
  codelist_slug_user <- "user/milanwiedemann/diastolic-blood-pressure-qof/697e3433/"
  codelist_slug_org <- "nhsd-primary-care-domain-refsets/cpeptide_cod/20200812"

  test_codelist_user <- get_codelist(codelist_slug_user)
  test_codelist_org <- get_codelist(codelist_slug_org)

  expect_equal(nrow(test_codelist_user), 17L)
  expect_equal(nrow(test_codelist_org), 6L)

  expect_equal(test_codelist_user@coding_system, "snomedct")
  expect_equal(test_codelist_org@coding_system, "snomedct")

  expect_equal(
    test_codelist_user@full_slug,
    "user/milanwiedemann/diastolic-blood-pressure-qof/697e3433"
  )
  expect_equal(
    test_codelist_org@full_slug,
    "nhsd-primary-care-domain-refsets/cpeptide_cod/20200812"
  )

  expect_equal(
    test_codelist_user$code,
    c(
      "1091811000000102",
      "163031004",
      "174255007",
      "198091000000104",
      "271650006",
      "314451001",
      "314454009",
      "314456006",
      "314458007",
      "314459004",
      "314461008",
      "314462001",
      "314465004",
      "400975005",
      "407555005",
      "407557002",
      "716632005"
    )
  )
  expect_equal(
    test_codelist_org$code,
    c(
      "1106701000000107",
      "1106721000000103",
      "271227006",
      "401124003",
      "88705004",
      "999351000000102"
    )
  )
})

test_that("Get codelist shows message for valid slug input", {
  codelist_slug_org <- "nhsd-primary-care-domain-refsets/cpeptide_cod/20200812/"

  expect_message(
    get_codelist(codelist_slug_org),
    "Note: For clarity, please use the full OpenCodelists URL instead of just the slug."
  )

  expect_message(
    get_codelist(codelist_slug_org),
    "Full URL would be: https://www.opencodelists.org/codelist/nhsd-primary-care-domain-refsets/cpeptide_cod/20200812/"
  )
})
