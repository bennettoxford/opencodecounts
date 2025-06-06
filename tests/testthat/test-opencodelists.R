test_that("Get codelist organisation", {
  codelist_slug_user <- "user/milanwiedemann/diastolic-blood-pressure-qof/697e3433"
  codelist_slug_org <- "nhsd-primary-care-domain-refsets/cpeptide_cod/20200812/"

  test_codelist_user <- get_codelist_organisation(codelist_slug_user)
  test_codelist_org <- get_codelist_organisation(codelist_slug_org)

  expect_equal(test_codelist_user, "user/milanwiedemann")
  expect_equal(test_codelist_org, "nhsd-primary-care-domain-refsets")
})

test_that("Get codelist from OpenCodelists", {
  codelist_slug_user <- "user/milanwiedemann/diastolic-blood-pressure-qof/697e3433"
  codelist_slug_org <- "nhsd-primary-care-domain-refsets/cpeptide_cod/20200812/"

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
