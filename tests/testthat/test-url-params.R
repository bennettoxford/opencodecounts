test_that("parse_url_params() returns an empty list without a query", {
  expect_equal(parse_url_params(NULL), list())
  expect_equal(parse_url_params(""), list())
  expect_length(parse_url_params("?"), 0)
})

test_that("parse_url_params() parses each parameter", {
  params <- parse_url_params(
    "?dataset=icd10&codes=F41,F41.1&description=anxiety%7Cpanic&pattern=F4&from=2014-15&to=2019-20"
  )

  expect_equal(
    params,
    list(
      dataset = "icd10",
      codes = c("F41", "F41.1"),
      description = "anxiety|panic",
      pattern = "F4",
      from = 2015L,
      to = 2020L
    )
  )
})

test_that("parse_url_params() accepts a codelist slug or URL", {
  slug <- "opensafely/anxiety-disorders/6aef605a"

  expect_equal(parse_url_params(paste0("?codelist=", slug))$codelist, slug)
  expect_equal(
    parse_url_params(paste0("?codelist=", slug, "/"))$codelist,
    slug
  )
  expect_equal(
    parse_url_params(
      "?codelist=https://www.opencodelists.org/codelist/opensafely/anxiety-disorders/6aef605a/"
    )$codelist,
    slug
  )
  expect_equal(
    parse_url_params("?codelist=user/someone/my-list/abc123")$codelist,
    "user/someone/my-list/abc123"
  )
})

test_that("parse_url_params() drops values that are not valid", {
  params <- parse_url_params(
    "?dataset=not_a_dataset&codelist=not-a-slug&codes=,,&from=2015&to=2014-16&description=&other=1"
  )

  expect_length(params, 0)
})

test_that("parse_url_params() ignores the code search when a codelist is given", {
  params <- parse_url_params(
    "?dataset=snomedct&codelist=opensafely/anxiety-disorders/6aef605a&codes=123&description=anxiety&from=2014-15"
  )

  expect_named(params, c("dataset", "codelist", "from"))
})

test_that("reporting years are read and written as their labels", {
  expect_equal(parse_reporting_year("2014-15"), 2015L)
  expect_equal(parse_reporting_year("1999-00"), 2000L)
  expect_null(parse_reporting_year("2015"))
  expect_null(parse_reporting_year("2014-2015"))
  expect_null(parse_reporting_year("2014-16"))

  expect_equal(format_reporting_year(2015L), "2014-15")
  expect_equal(format_reporting_year(2000L), "1999-00")
})

test_that("parse_url_params() drops a pattern for datasets without a pattern search", {
  expect_null(parse_url_params("?dataset=snomedct&pattern=F4")$pattern)
  expect_null(parse_url_params("?pattern=F4")$pattern)
  expect_equal(parse_url_params("?dataset=opcs4&pattern=K4")$pattern, "K4")
})

test_that("format_url_params() returns an empty string without values", {
  expect_equal(format_url_params(list()), "")
  expect_equal(
    format_url_params(list(codes = character(0), description = "")),
    ""
  )
})

test_that("format_url_params() keeps slashes and commas readable", {
  expect_equal(
    format_url_params(list(
      dataset = "snomedct",
      codelist = "opensafely/anxiety-disorders/6aef605a"
    )),
    "?dataset=snomedct&codelist=opensafely/anxiety-disorders/6aef605a"
  )
  expect_equal(
    format_url_params(list(codes = c("123", "456"))),
    "?codes=123,456"
  )
})

test_that("format_url_params() writes parameters in spec order", {
  expect_equal(
    format_url_params(list(to = 2020, dataset = "icd10")),
    "?dataset=icd10&to=2019-20"
  )
})

test_that("parse_url_params() reads what format_url_params() writes", {
  params <- list(
    dataset = "opcs4",
    codes = c("K40.1", "K40.2"),
    description = "bypass & graft|stent + 50%",
    pattern = "K4|K5",
    from = 2014L,
    to = 2022L
  )

  expect_equal(parse_url_params(format_url_params(params)), params)
})

test_that("select_end_dates() selects end dates by year", {
  end_dates <- as.Date(c(
    "2019-07-31",
    "2020-07-31",
    "2021-07-31",
    "2022-07-31"
  ))

  expect_equal(select_end_dates(end_dates), range(end_dates))
  expect_equal(
    select_end_dates(end_dates, from = 2020L, to = 2021L),
    as.Date(c("2020-07-31", "2021-07-31"))
  )
  expect_equal(
    select_end_dates(end_dates, from = 2021L),
    as.Date(c("2021-07-31", "2022-07-31"))
  )
  # Years outside the data are clamped to the data
  expect_equal(
    select_end_dates(end_dates, from = 2000L, to = 2050L),
    range(end_dates)
  )
  expect_equal(
    select_end_dates(end_dates, from = 2030L, to = 2040L),
    range(end_dates)
  )
})
