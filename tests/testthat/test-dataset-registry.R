test_that("load_dataset_registry() parses the package's shiny_app_datasets.yml", {
  registry <- load_dataset_registry()

  expect_named(registry, c("snomedct", "icd10", "opcs4"))
  expect_equal(registry$snomedct$dataset, "gp_snomed")
  expect_equal(registry$snomedct$get_function, "get_gp_snomed")
})

test_that("dataset_choices() returns dataset ids named by their label", {
  choices <- dataset_choices()

  expect_equal(
    choices,
    c(
      "General practice (SNOMED CT)" = "snomedct",
      "Hospital admissions (ICD-10)" = "icd10",
      "Hospital admissions (OPCS-4)" = "opcs4"
    )
  )
})

test_that("get_dataset_by_id() returns the full config for a known dataset", {
  cfg <- get_dataset_by_id("icd10")

  expect_equal(cfg$get_function, "get_hesapc_icd10")
  expect_equal(cfg$code_column, "icd10_code")
  expect_true(cfg$has_code_pattern_search)
  expect_equal(cfg$code_pattern_label, "ICD-10 category")
})

test_that("get_dataset_by_id() reports snomedct as not having a code pattern search", {
  cfg <- get_dataset_by_id("snomedct")

  expect_false(cfg$has_code_pattern_search)
  expect_null(cfg$code_pattern_label)
})

test_that("get_dataset_by_id() errors for an unknown dataset id", {
  expect_error(
    get_dataset_by_id("not_a_real_dataset"),
    class = "opencodecounts_error_app_dataset_not_found"
  )
})

test_that("icd10 and opcs4 registry entries reuse the same source url", {
  icd10 <- get_dataset_by_id("icd10")
  opcs4 <- get_dataset_by_id("opcs4")

  expect_equal(icd10$source_url, opcs4$source_url)
})

test_that("every registry entry's get_function is an exported opencodecounts function", {
  registry <- load_dataset_registry()

  for (dataset_id in names(registry)) {
    get_fn_name <- registry[[dataset_id]]$get_function
    expect_true(
      exists(
        get_fn_name,
        where = asNamespace("opencodecounts"),
        inherits = FALSE
      ),
      info = dataset_id
    )
  }
})

test_that("every registry entry's dataset field is a known tidy_data_sources.yml dataset", {
  registry <- load_dataset_registry()
  known_datasets <- names(load_tidy_sources_config())

  for (dataset_id in names(registry)) {
    expect_true(
      registry[[dataset_id]]$dataset %in% known_datasets,
      info = dataset_id
    )
  }
})
