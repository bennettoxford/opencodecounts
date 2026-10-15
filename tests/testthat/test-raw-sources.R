test_that("get_raw_source_periods() returns all periods for gp_snomed", {
  periods <- get_raw_source_periods("gp_snomed")

  expect_length(periods, 15)
  expect_true(all(vapply(periods, is.character, logical(1))))
  expect_equal(
    periods[["2025to2026"]],
    "https://files.digital.nhs.uk/A6/20567E/SNOMED_code_usage_2025-26.txt"
  )
})

test_that("get_raw_source_periods() returns url/sheet/skip_rows/usage_col for hesapc_icd10", {
  periods <- get_raw_source_periods("hesapc_icd10")

  expect_length(periods, 13)
  expect_named(
    periods[["2024to2025"]],
    c("url", "sheet", "skip_rows", "usage_col")
  )
  expect_equal(periods[["2013to2014"]]$skip_rows, 18)
  expect_equal(periods[["2013to2014"]]$usage_col, 3)
})

test_that("get_raw_source_periods() returns url/sheet/range for the breakdowns datasets", {
  periods <- get_raw_source_periods("hesapc_icd10_breakdowns")

  expect_length(periods, 13)
  expect_named(periods[["2012to2013"]], c("url", "sheet", "range"))
  expect_equal(periods[["2012to2013"]]$range, "A18:AF11400")
})

test_that("period keys use the same <start-year>to<end-year> format across all datasets", {
  key_pattern <- "^\\d{4}to\\d{4}$"

  for (dataset in c(
    "gp_snomed",
    "hesapc_icd10",
    "hesapc_icd10_breakdowns",
    "hesapc_opcs4",
    "hesapc_opcs4_breakdowns"
  )) {
    keys <- names(get_raw_source_periods(dataset))
    expect_true(all(grepl(key_pattern, keys)), info = dataset)
  }
})

test_that("get_raw_source_periods() has matching period counts for hesapc_opcs4 datasets", {
  expect_length(get_raw_source_periods("hesapc_opcs4"), 13)
  expect_length(get_raw_source_periods("hesapc_opcs4_breakdowns"), 13)
})

test_that("usage and breakdowns datasets share the same url/sheet per period", {
  hesapc_icd10 <- get_raw_source_periods("hesapc_icd10")
  hesapc_icd10_breakdowns <- get_raw_source_periods("hesapc_icd10_breakdowns")
  hesapc_opcs4 <- get_raw_source_periods("hesapc_opcs4")
  hesapc_opcs4_breakdowns <- get_raw_source_periods("hesapc_opcs4_breakdowns")

  for (period in names(hesapc_icd10)) {
    expect_equal(
      hesapc_icd10[[period]]$url,
      hesapc_icd10_breakdowns[[period]]$url,
      info = period
    )
    expect_equal(
      hesapc_icd10[[period]]$sheet,
      hesapc_icd10_breakdowns[[period]]$sheet,
      info = period
    )
  }
  for (period in names(hesapc_opcs4)) {
    expect_equal(
      hesapc_opcs4[[period]]$url,
      hesapc_opcs4_breakdowns[[period]]$url,
      info = period
    )
    expect_equal(
      hesapc_opcs4[[period]]$sheet,
      hesapc_opcs4_breakdowns[[period]]$sheet,
      info = period
    )
  }
})

test_that("get_raw_source_periods() errors for an unknown dataset", {
  expect_error(
    get_raw_source_periods("not_a_real_dataset"),
    class = "opencodecounts_error_dataset_not_found"
  )
})
