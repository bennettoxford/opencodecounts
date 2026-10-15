# These tests call the get_*() accessors, which read from the local cache
# (downloading first if the cache is empty - see R/get-data.R). They stay in
# the fast unit suite rather than tests/integration.R because a populated
# cache makes them just as fast as reading a lazy-loaded data object; only a
# cold cache adds a one-off download.

gp_snomed <- get_gp_snomed()
hesapc_icd10 <- get_hesapc_icd10()
hesapc_icd10_breakdowns <- get_hesapc_icd10_breakdowns()
hesapc_opcs4 <- get_hesapc_opcs4()
hesapc_opcs4_breakdowns <- get_hesapc_opcs4_breakdowns()

# SNOMED tests

test_that("Test gp_snomed column names", {
  test_names <- names(gp_snomed)
  expect_equal(
    test_names,
    c(
      "start_date",
      "end_date",
      "snomed_code",
      "description",
      "usage",
      "active_at_start",
      "active_at_end"
    )
  )
})


test_that("Test gp_snomed column types", {
  expect_s3_class(gp_snomed$start_date, "Date")
  expect_s3_class(gp_snomed$end_date, "Date")
  expect_type(gp_snomed$snomed_code, "character")
  expect_type(gp_snomed$description, "character")
  expect_type(gp_snomed$usage, "integer")
  expect_type(gp_snomed$active_at_start, "logical")
  expect_type(gp_snomed$active_at_end, "logical")
})


test_that("Test gp_snomed rows", {
  test_nrow <- nrow(gp_snomed)
  expect_equal(test_nrow, 1857443L)
})

test_that("Test gp_snomed date range", {
  test_range_start_date <- range(gp_snomed$start_date)
  test_range_end_date <- range(gp_snomed$end_date)

  expect_equal(
    test_range_start_date,
    c(as.Date("2011-08-01"), as.Date("2025-08-01"))
  )
  expect_equal(
    test_range_end_date,
    c(as.Date("2012-07-31"), as.Date("2026-07-31"))
  )
})

test_that("Test sum of usage", {
  test_usage_sum <- sum(gp_snomed$usage)
  expect_equal(test_usage_sum, 54409755375)
})

test_that("Minimum SNOMED usage", {
  min_gp_snomed <- min(gp_snomed$usage)
  expect_equal(min_gp_snomed, 5)
})

test_that("Test no non-alphanumeric characters in SNOMED codes", {
  non_alphanumeric_codes_snomed <- gp_snomed$snomed_code[grep(
    "\\s?[^[:alnum:]]+\\s?",
    gp_snomed$snomed_code
  )]
  expect_equal(length(non_alphanumeric_codes_snomed), 0)
})

test_that("Test SNOMEDCT missing description", {
  test_sum_missing_description <- sum(is.na(gp_snomed$description))
  expect_equal(test_sum_missing_description, 0)
})

# This tests if we read in the codes correctly. We might want to improve this to test more than one case in the future.
# This illustrates the problem:
# $ python -c 'print(int(float("39733011000001106")))'
# 39733011000001104
test_that("Atorvastatin code is correct", {
  atorvastatin_code <- gp_snomed[
    gp_snomed$description == "Atorvastatin 20mg tablets (product)",
  ]$snomed_code
  expect_identical(unique(atorvastatin_code), "39733011000001106")
})

# ICD-10 Tests

test_that("Test hesapc_icd10 column types", {
  expect_s3_class(hesapc_icd10$start_date, "Date")
  expect_s3_class(hesapc_icd10$end_date, "Date")
  expect_type(hesapc_icd10$icd10_code, "character")
  expect_type(hesapc_icd10$description, "character")
  expect_type(hesapc_icd10$usage, "integer")
})

test_that("Test hesapc_icd10 rows", {
  test_nrow <- nrow(hesapc_icd10)
  expect_equal(test_nrow, 158849L)
})

test_that("Test hesapc_icd10 date range", {
  test_range_start_date <- range(hesapc_icd10$start_date)
  test_range_end_date <- range(hesapc_icd10$end_date)

  expect_equal(
    test_range_start_date,
    c(as.Date("2012-04-01"), as.Date("2025-04-01"))
  )
  expect_equal(
    test_range_end_date,
    c(as.Date("2013-03-31"), as.Date("2026-03-31"))
  )
})

test_that("Test hesapc_icd10 minimum usage", {
  test_min_usage <- min(hesapc_icd10$usage)
  expect_equal(test_min_usage, 1)
})

test_that("Test cummulative ICD-10 usage", {
  test_sum_usage <- sum(hesapc_icd10$usage)
  expect_equal(test_sum_usage, 1672014332)
})

test_that("Test ICD-10 usage are all integers", {
  test_sum_non_integers <- sum(!is.integer(hesapc_icd10$usage))
  expect_equal(test_sum_non_integers, 0)
})

test_that("Test no non-alphanumeric characters in ICD-10 codes", {
  non_alphanumeric_codes_icd10 <- hesapc_icd10$icd10_code[grep(
    "\\s?[^[:alnum:]]+\\s?",
    hesapc_icd10$icd10_code
  )]
  expect_equal(length(non_alphanumeric_codes_icd10), 0)
})

test_that("Test ICD10 missing description", {
  test_sum_missing_description <- sum(is.na(hesapc_icd10$description))
  expect_equal(test_sum_missing_description, 0)
})

# ICD-10 Breakdowns Tests
test_that("Test hesapc_icd10_breakdowns column names", {
  test_names <- names(hesapc_icd10_breakdowns)
  expect_equal(
    test_names,
    c(
      "start_date",
      "end_date",
      "icd10_code",
      "description",
      "breakdown",
      "usage"
    )
  )
})

test_that("Test hesapc_icd10_breakdowns column types", {
  expect_s3_class(hesapc_icd10_breakdowns$start_date, "Date")
  expect_s3_class(hesapc_icd10_breakdowns$end_date, "Date")
  expect_type(hesapc_icd10_breakdowns$icd10_code, "character")
  expect_type(hesapc_icd10_breakdowns$description, "character")
  expect_type(hesapc_icd10_breakdowns$breakdown, "character")
  expect_type(hesapc_icd10_breakdowns$usage, "integer")
})

test_that("Test hesapc_icd10_breakdowns date range", {
  test_range_start_date <- range(hesapc_icd10_breakdowns$start_date)
  test_range_end_date <- range(hesapc_icd10_breakdowns$end_date)

  expect_equal(
    test_range_start_date,
    c(as.Date("2012-04-01"), as.Date("2025-04-01"))
  )
  expect_equal(
    test_range_end_date,
    c(as.Date("2013-03-31"), as.Date("2026-03-31"))
  )
})

test_that("Test hesapc_icd10_breakdowns breakdown values", {
  expected_breakdowns <- c(
    "all_diagnoses",
    "main_diagnosis",
    "male",
    "female",
    "gender_unknown",
    "age_0",
    "age_1_4",
    "age_5_9",
    "age_10_14",
    "age_15",
    "age_16",
    "age_17",
    "age_18",
    "age_19",
    "age_20_24",
    "age_25_29",
    "age_30_34",
    "age_35_39",
    "age_40_44",
    "age_45_49",
    "age_50_54",
    "age_55_59",
    "age_60_64",
    "age_65_69",
    "age_70_74",
    "age_75_79",
    "age_80_84",
    "age_85_89",
    "age_90plus"
  )
  actual_breakdowns <- unique(hesapc_icd10_breakdowns$breakdown)
  expect_setequal(actual_breakdowns, expected_breakdowns)
})

test_that("Test no non-alphanumeric characters in ICD-10 breakdown codes", {
  non_alphanumeric_codes <- hesapc_icd10_breakdowns$icd10_code[grep(
    "\\s?[^[:alnum:]]+\\s?",
    hesapc_icd10_breakdowns$icd10_code
  )]
  expect_equal(length(non_alphanumeric_codes), 0)
})

test_that("Test ICD-10 breakdowns missing description", {
  test_sum_missing_description <- sum(is.na(
    hesapc_icd10_breakdowns$description
  ))
  expect_equal(test_sum_missing_description, 0)
})

test_that("Test ICD-10 breakdowns totals match main dataset", {
  # Get first 3 unique codes
  test_codes <- unique(hesapc_icd10$icd10_code)[1:3]

  for (code in test_codes) {
    # Get usage from main dataset
    main_usage <- hesapc_icd10 |>
      dplyr::filter(icd10_code == code) |>
      dplyr::select(start_date, end_date, usage) |>
      dplyr::arrange(start_date)

    # Get all_diagnoses breakdown usage
    breakdown_usage <- hesapc_icd10_breakdowns |>
      dplyr::filter(icd10_code == code, breakdown == "all_diagnoses") |>
      dplyr::select(start_date, end_date, usage) |>
      dplyr::arrange(start_date)

    expect_equal(
      main_usage,
      breakdown_usage,
      info = paste("Mismatch for ICD-10 code:", code)
    )
  }
})

# OPCS-4 Tests

test_that("Test hesapc_opcs4 column types", {
  expect_s3_class(hesapc_opcs4$start_date, "Date")
  expect_s3_class(hesapc_opcs4$end_date, "Date")
  expect_type(hesapc_opcs4$opcs4_code, "character")
  expect_type(hesapc_opcs4$description, "character")
  expect_type(hesapc_opcs4$usage, "integer")
})

test_that("Test hesapc_opcs4 rows", {
  test_nrow <- nrow(hesapc_opcs4)
  expect_equal(test_nrow, 125925L)
})

test_that("Test hesapc_opcs4 date range", {
  test_range_start_date <- range(hesapc_opcs4$start_date)
  test_range_end_date <- range(hesapc_opcs4$end_date)

  expect_equal(
    test_range_start_date,
    c(as.Date("2012-04-01"), as.Date("2025-04-01"))
  )
  expect_equal(
    test_range_end_date,
    c(as.Date("2013-03-31"), as.Date("2026-03-31"))
  )
})

test_that("Test hesapc_opcs4 minimum usage", {
  test_min_usage <- min(hesapc_opcs4$usage)
  expect_equal(test_min_usage, 1)
})

test_that("Test OPCS-4 usage are all integers", {
  test_sum_non_integers <- sum(!is.integer(hesapc_opcs4$usage))
  expect_equal(test_sum_non_integers, 0)
})

test_that("Test no non-alphanumeric characters in OPCS-4 codes", {
  non_alphanumeric_codes_opcs4 <- hesapc_opcs4$opcs4_code[grep(
    "\\s?[^[:alnum:]]+\\s?",
    hesapc_opcs4$opcs4_code
  )]
  expect_equal(length(non_alphanumeric_codes_opcs4), 0)
})

test_that("Test OPCS-4 missing description", {
  test_sum_missing_description <- sum(is.na(hesapc_opcs4$description))
  expect_equal(test_sum_missing_description, 0)
})


# OPCS-4 Breakdowns Tests

test_that("Test hesapc_opcs4_breakdowns column names", {
  test_names <- names(hesapc_opcs4_breakdowns)
  expect_equal(
    test_names,
    c(
      "start_date",
      "end_date",
      "opcs4_code",
      "description",
      "breakdown",
      "usage"
    )
  )
})

test_that("Test hesapc_opcs4_breakdowns column types", {
  expect_s3_class(hesapc_opcs4_breakdowns$start_date, "Date")
  expect_s3_class(hesapc_opcs4_breakdowns$end_date, "Date")
  expect_type(hesapc_opcs4_breakdowns$opcs4_code, "character")
  expect_type(hesapc_opcs4_breakdowns$description, "character")
  expect_type(hesapc_opcs4_breakdowns$breakdown, "character")
  expect_type(hesapc_opcs4_breakdowns$usage, "integer")
})

test_that("Test hesapc_opcs4_breakdowns date range", {
  test_range_start_date <- range(hesapc_opcs4_breakdowns$start_date)
  test_range_end_date <- range(hesapc_opcs4_breakdowns$end_date)

  expect_equal(
    test_range_start_date,
    c(as.Date("2012-04-01"), as.Date("2025-04-01"))
  )
  expect_equal(
    test_range_end_date,
    c(as.Date("2013-03-31"), as.Date("2026-03-31"))
  )
})

test_that("Test hesapc_opcs4_breakdowns breakdown values", {
  expected_breakdowns <- c(
    "all_procedures",
    "main_procedure",
    "male",
    "female",
    "gender_unknown",
    "age_0",
    "age_1_4",
    "age_5_9",
    "age_10_14",
    "age_15",
    "age_16",
    "age_17",
    "age_18",
    "age_19",
    "age_20_24",
    "age_25_29",
    "age_30_34",
    "age_35_39",
    "age_40_44",
    "age_45_49",
    "age_50_54",
    "age_55_59",
    "age_60_64",
    "age_65_69",
    "age_70_74",
    "age_75_79",
    "age_80_84",
    "age_85_89",
    "age_90plus"
  )
  actual_breakdowns <- unique(hesapc_opcs4_breakdowns$breakdown)
  expect_setequal(actual_breakdowns, expected_breakdowns)
})

test_that("Test no non-alphanumeric characters in OPCS-4 breakdown codes", {
  non_alphanumeric_codes <- hesapc_opcs4_breakdowns$opcs4_code[grep(
    "\\s?[^[:alnum:]]+\\s?",
    hesapc_opcs4_breakdowns$opcs4_code
  )]
  expect_equal(length(non_alphanumeric_codes), 0)
})

test_that("Test OPCS-4 breakdowns missing description", {
  test_sum_missing_description <- sum(is.na(
    hesapc_opcs4_breakdowns$description
  ))
  expect_equal(test_sum_missing_description, 0)
})

test_that("Test OPCS-4 breakdowns totals match main dataset", {
  # Get first 3 unique codes
  test_codes <- unique(hesapc_opcs4$opcs4_code)[1:3]

  for (code in test_codes) {
    # Get usage from main dataset
    main_usage <- hesapc_opcs4 |>
      dplyr::filter(opcs4_code == code) |>
      dplyr::select(start_date, end_date, usage) |>
      dplyr::arrange(start_date)

    # Get all_procedures breakdown usage
    breakdown_usage <- hesapc_opcs4_breakdowns |>
      dplyr::filter(opcs4_code == code, breakdown == "all_procedures") |>
      dplyr::select(start_date, end_date, usage) |>
      dplyr::arrange(start_date)

    expect_equal(
      main_usage,
      breakdown_usage,
      info = paste("Mismatch for OPCS-4 code:", code)
    )
  }
})
