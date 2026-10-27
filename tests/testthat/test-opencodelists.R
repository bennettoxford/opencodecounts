test_that("Get codelist slug from opencodelists URL", {
  codelist_url_org_clean <- "https://www.opencodelists.org/codelist/phc/2ww-referral-colorectal/7eac259d/"
  codelist_url_user_full_list <- "https://www.opencodelists.org/codelist/nhsd-primary-care-domain-refsets/cpeptide_cod/20200812/#full-list"
  codelist_url_org_search_results <- "https://www.opencodelists.org/codelist/user/milanwiedemann/diastolic-blood-pressure-qof/697e3433/#search-results"

  test_codelist_url_org_clean <- extract_codelist_slug(codelist_url_org_clean)
  test_codelist_url_user_full_list <- extract_codelist_slug(
    codelist_url_user_full_list
  )
  test_codelist_url_org_search_results <- extract_codelist_slug(
    codelist_url_org_search_results
  )

  expect_equal(
    test_codelist_url_org_clean,
    "phc/2ww-referral-colorectal/7eac259d"
  )
  expect_equal(
    test_codelist_url_user_full_list,
    "nhsd-primary-care-domain-refsets/cpeptide_cod/20200812"
  )
  expect_equal(
    test_codelist_url_org_search_results,
    "user/milanwiedemann/diastolic-blood-pressure-qof/697e3433"
  )
})

test_that("Get codelist organisation", {
  codelist_slug_user <- "user/milanwiedemann/diastolic-blood-pressure-qof/697e3433"
  codelist_slug_org <- "nhsd-primary-care-domain-refsets/cpeptide_cod/20200812/"

  test_codelist_user <- get_codelist_organisation(codelist_slug_user)
  test_codelist_org <- get_codelist_organisation(codelist_slug_org)

  expect_equal(test_codelist_user, "user/milanwiedemann")
  expect_equal(test_codelist_org, "nhsd-primary-care-domain-refsets")
})

test_that("get_codelist_versions() lists tagged versions by tag and hash", {
  httr2::local_mocked_responses(function(req) {
    httr2::response_json(
      body = list(
        codelists = list(
          list(
            coding_system_id = "snomedct",
            versions = list(
              list(
                hash = "6c696a17",
                tag = "v1.8",
                full_slug = "org/test/v1.8"
              ),
              list(hash = "117d1ec5", full_slug = "org/test/117d1ec5")
            )
          )
        )
      )
    )
  })

  expect_equal(
    get_codelist_versions("org")$full_slug,
    c("org/test/v1.8", "org/test/6c696a17", "org/test/117d1ec5")
  )
})

test_that("get_coding_systems() looks up each organisation once", {
  orgs_requested <- character()
  local_mocked_bindings(
    get_codelist_versions = function(codelist_org) {
      orgs_requested <<- c(orgs_requested, codelist_org)
      tibble::tibble(
        coding_system_id = c("snomedct", "dmd"),
        full_slug = c("user/test/a/1", "user/test/b/2"),
        hash = c("aaaaaaaa", "bbbbbbbb")
      )
    }
  )

  expect_equal(
    get_coding_systems(c("user/test/a/1", "user/test/b/2")),
    c("snomedct", "dmd")
  )
  expect_equal(orgs_requested, "user/test")
  expect_equal(get_coding_systems("user/test/missing/1"), NA_character_)
  expect_equal(orgs_requested, c("user/test", "user/test", ""))
})

test_that("get_coding_systems() finds moved or renamed codelists by hash", {
  local_mocked_bindings(
    get_codelist_versions = function(codelist_org) {
      if (codelist_org != "") {
        return(tibble::tibble(
          coding_system_id = character(),
          full_slug = character(),
          hash = character()
        ))
      }
      tibble::tibble(
        coding_system_id = c("opcs4", "snomedct", "dmd", "dmd"),
        full_slug = c(
          "new-org/a/1a2b3c4d",
          "user/old/renamed/5e6f7a8b",
          "org/tagged/v1",
          "org/tagged/9c0d1e2f"
        ),
        hash = c("1a2b3c4d", "5e6f7a8b", "9c0d1e2f", "9c0d1e2f")
      )
    }
  )

  expect_message(
    coding_systems <- get_coding_systems(c(
      "old-org/a/1a2b3c4d",
      "user/old/original-name/5e6f7a8b",
      "old-org/tagged/v1",
      "old-org/missing/00000001"
    )),
    "old-org/a/1a2b3c4d is now new-org/a/1a2b3c4d"
  )
  # Versions referred to by tag are not matched
  expect_equal(coding_systems, c("opcs4", "snomedct", NA, NA))
})

test_that("Get codelist stops for invalid slug format", {
  invalid_slug_too_few_parts <- "nhsd-primary-care/cpeptide_cod"
  invalid_slug_empty_part <- "nhsd-primary-care-domain-refsets//20200812"
  invalid_slug_random <- "not-a-valid-format"

  expect_error(
    get_codelist(invalid_slug_too_few_parts),
    class = "opencodecounts_error_codelist_url"
  )

  expect_error(
    get_codelist(invalid_slug_empty_part),
    class = "opencodecounts_error_codelist_url"
  )

  expect_error(
    get_codelist(invalid_slug_random),
    class = "opencodecounts_error_codelist_url"
  )
})

test_that("read_codelists_txt() follows the opensafely codelists rules", {
  codelists_txt <- read_codelists_txt(test_codelists_dir())

  expect_equal(
    codelists_txt$codelist_id,
    c("user/test/phq-snomed/abc12345", "opensafely/test-dmd/def67890")
  )
  expect_equal(
    codelists_txt$codelist,
    c("user-test-phq-snomed", "opensafely-test-dmd")
  )
  expect_equal(
    basename(codelists_txt$file),
    c("user-test-phq-snomed.csv", "opensafely-test-dmd.csv")
  )
})

test_that("read_codelists_txt() errors when codelists.txt is missing", {
  expect_error(
    read_codelists_txt(withr::local_tempdir()),
    class = "opencodecounts_error_codelists_txt"
  )
})

test_that("read_codelists() returns named tibbles with a code column", {
  codelists <- read_codelists(test_codelists_dir())

  expect_named(codelists, c("user-test-phq-snomed", "opensafely-test-dmd"))
  expect_equal(nrow(codelists[["user-test-phq-snomed"]]), 3)
  expect_equal(names(codelists[["opensafely-test-dmd"]])[[1]], "code")
})

test_that("read_codelists() errors when CSV files are missing", {
  dir <- withr::local_tempdir()
  file.copy(file.path(test_codelists_dir(), "codelists.txt"), dir)

  expect_snapshot(read_codelists(dir), error = TRUE)
})

test_that("read_codelists() handles CSV files with an unnamed index column", {
  dir <- withr::local_tempdir()
  writeLines("opensafely/indexed/abc12345", file.path(dir, "codelists.txt"))
  writeLines(
    c(",code,term", "0,Q071,Hysterocolpectomy", "1,Q072,Hysterectomy"),
    file.path(dir, "opensafely-indexed.csv")
  )

  codelist <- read_codelists(dir)[["opensafely-indexed"]]

  expect_named(codelist, c("code", "term"))
  expect_equal(codelist$code, c("Q071", "Q072"))
})

test_that("rename_code_column() drops an index column before the code column", {
  codelist <- tibble::tibble(...1 = c("0", "1"), ctv3_id = c("X1", "X2"))

  expect_equal(
    rename_code_column(codelist),
    tibble::tibble(code = c("X1", "X2"))
  )
})

test_that("get_codelist_versions() returns no versions for an unknown owner", {
  httr2::local_mocked_responses(function(req) httr2::response(404))

  expect_equal(
    get_codelist_versions("user/unknown"),
    tibble::tibble(
      coding_system_id = character(),
      full_slug = character(),
      hash = character()
    )
  )
  expect_true(is.na(get_coding_systems("user/unknown/x/1")))
})

test_that("get_codelist_versions() handles an empty listing and missing hashes", {
  httr2::local_mocked_responses(function(req) {
    httr2::response_json(
      body = list(
        codelists = list(
          list(
            coding_system_id = "snomedct",
            versions = list(list(full_slug = "org/a/1"))
          )
        )
      )
    )
  })
  expect_equal(get_codelist_versions("org")$full_slug, "org/a/1")

  httr2::local_mocked_responses(function(req) {
    httr2::response_json(body = list(codelists = list()))
  })
  expect_equal(nrow(get_codelist_versions("org")), 0)
  expect_true(is.na(get_coding_systems("org/a/1")))
})

test_that("read_codelists_txt() handles Windows line endings and comments", {
  dir <- withr::local_tempdir()
  writeBin(
    charToRaw("# comment\r\n\r\n  user/a/x/1/  \r\nopensafely/y/2\r\n"),
    file.path(dir, "codelists.txt")
  )

  expect_equal(
    read_codelists_txt(dir)$codelist_id,
    c("user/a/x/1", "opensafely/y/2")
  )
})
