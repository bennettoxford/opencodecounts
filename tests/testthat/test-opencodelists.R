test_that("Get codelist slug from opencodelists URL", {
  codelist_url_org_clean <- "https://www.opencodelists.org/codelist/phc/2ww-referral-colorectal/7eac259d/"
  codelist_url_user_full_list <- "https://www.opencodelists.org/codelist/nhsd-primary-care-domain-refsets/cpeptide_cod/20200812/#full-list"
  codelist_url_org_search_results <- "https://www.opencodelists.org/codelist/user/milanwiedemann/diastolic-blood-pressure-qof/697e3433/#search-results"

  test_codelist_url_org_clean <- extract_codelist_slug(codelist_url_org_clean)
  test_codelist_url_user_full_list <- extract_codelist_slug(codelist_url_user_full_list)
  test_codelist_url_org_search_results <- extract_codelist_slug(codelist_url_org_search_results)

  expect_equal(test_codelist_url_org_clean, "phc/2ww-referral-colorectal/7eac259d")
  expect_equal(test_codelist_url_user_full_list, "nhsd-primary-care-domain-refsets/cpeptide_cod/20200812")
  expect_equal(test_codelist_url_org_search_results, "user/milanwiedemann/diastolic-blood-pressure-qof/697e3433")
})

test_that("Get codelist organisation", {
  codelist_slug_user <- "user/milanwiedemann/diastolic-blood-pressure-qof/697e3433"
  codelist_slug_org <- "nhsd-primary-care-domain-refsets/cpeptide_cod/20200812/"

  test_codelist_user <- get_codelist_organisation(codelist_slug_user)
  test_codelist_org <- get_codelist_organisation(codelist_slug_org)

  expect_equal(test_codelist_user, "user/milanwiedemann")
  expect_equal(test_codelist_org, "nhsd-primary-care-domain-refsets")
})

test_that("Get codelist from OpenCodelists URL", {
  codelist_slug_user <- "https://www.opencodelists.org/codelist/user/milanwiedemann/diastolic-blood-pressure-qof/697e3433/"
  codelist_slug_org <- "https://www.opencodelists.org/codelist/nhsd-primary-care-domain-refsets/cpeptide_cod/20200812/"

  test_codelist_user <- get_codelist(codelist_slug_user)
  test_codelist_org <- get_codelist(codelist_slug_org)

  expect_equal(nrow(test_codelist_user), 17L)
  expect_equal(nrow(test_codelist_org), 6L)

  expect_equal(test_codelist_user@coding_system, "snomedct")
  expect_equal(test_codelist_org@coding_system, "snomedct")

  expect_equal(test_codelist_user@full_slug, "user/milanwiedemann/diastolic-blood-pressure-qof/697e3433")
  expect_equal(test_codelist_org@full_slug, "nhsd-primary-care-domain-refsets/cpeptide_cod/20200812")

  expect_equal(test_codelist_user$code, c(
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
  ))
  expect_equal(test_codelist_org$code, c(
    "1106701000000107",
    "1106721000000103",
    "271227006",
    "401124003",
    "88705004",
    "999351000000102"
  ))
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

  expect_equal(test_codelist_user@full_slug, "user/milanwiedemann/diastolic-blood-pressure-qof/697e3433")
  expect_equal(test_codelist_org@full_slug, "nhsd-primary-care-domain-refsets/cpeptide_cod/20200812")

  expect_equal(test_codelist_user$code, c(
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
  ))
  expect_equal(test_codelist_org$code, c(
    "1106701000000107",
    "1106721000000103",
    "271227006",
    "401124003",
    "88705004",
    "999351000000102"
  ))
})

test_that("Get codelist stops for invalid slug format", {

  invalid_slug_too_few_parts <- "nhsd-primary-care/cpeptide_cod"
  invalid_slug_empty_part <- "nhsd-primary-care-domain-refsets//20200812"
  invalid_slug_random <- "not-a-valid-format"
  
  expect_error(
    get_codelist(invalid_slug_too_few_parts),
    "Invalid format. Please use full OpenCodelists URL or ensure slug follows 'org/name/version' or 'user/username/name/version' pattern."
  )
  
  expect_error(
    get_codelist(invalid_slug_empty_part),
    "Invalid format. Please use full OpenCodelists URL or ensure slug follows 'org/name/version' or 'user/username/name/version' pattern."
  )
  
  expect_error(
    get_codelist(invalid_slug_random),
    "Invalid format. Please use full OpenCodelists URL or ensure slug follows 'org/name/version' or 'user/username/name/version' pattern."
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