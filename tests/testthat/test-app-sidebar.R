# Mocks the app data and records the calls that update the browser, which
# the mock session does not keep. Returns an environment with `selectize`
# (the arguments of each updateSelectizeInput() call) and `urls`.
local_sidebar_mocks <- function(env = parent.frame()) {
  calls <- new.env()
  calls$selectize <- list()
  calls$urls <- character()

  usage <- test_snomed_usage() |>
    dplyr::mutate(description = paste("Term", snomed_code)) |>
    dplyr::mutate(icd10_code = snomed_code, opcs4_code = snomed_code)

  local_mocked_bindings(
    get_app_dataset = function(dataset, version = NULL) usage,
    updateSelectizeInput = function(session, inputId, ...) {
      calls$selectize <- c(calls$selectize, list(list(...)))
    },
    updateQueryString = function(queryString, ...) {
      calls$urls <- c(calls$urls, queryString)
    },
    .env = env
  )
  calls
}

test_that("the sidebar selects codes from the URL that are in the data", {
  calls <- local_sidebar_mocks()

  testServer(
    mod_sidebar_server,
    args = list(params = list(codes = c("1359723003", "not_a_code"))),
    {
      session$setInputs(dataset = "snomedct")
      expect_equal(calls$selectize[[1]]$selected, "1359723003")
      expect_null(initial_codes())
    }
  )
})

test_that("the sidebar starts the date range and pattern from the URL", {
  local_sidebar_mocks()

  testServer(
    mod_sidebar_server,
    args = list(params = list(dataset = "icd10", pattern = "13", from = 2013L)),
    {
      session$setInputs(dataset = "icd10")
      # 2013-07-31 in milliseconds, the slider's date format
      expect_match(
        output$dynamic_date_slider$html,
        'data-from="1375228800000" data-to="1375228800000"'
      )
      expect_match(output$dynamic_code_pattern_input$html, 'value="13"')

      # Later renders start from defaults
      session$setInputs(
        date_range = as.Date(c("2013-07-31", "2013-07-31")),
        code_pattern_search = "13"
      )
      expect_null(initial_years())
      expect_null(initial_pattern())
    }
  )
})

test_that("the sidebar writes the selection to the URL", {
  calls <- local_sidebar_mocks()

  testServer(mod_sidebar_server, {
    session$setInputs(dataset = "icd10")
    expect_equal(calls$urls, "/mockpath?dataset=icd10")

    session$setInputs(
      code_specific_search = c("1359723003", "1359726006"),
      date_range = as.Date(c("2013-07-31", "2013-07-31"))
    )
    session$elapse(1000)
    expect_equal(
      dplyr::last(calls$urls),
      "/mockpath?dataset=icd10&codes=1359723003,1359726006&from=2012-13&to=2012-13"
    )

    # The full date range is left out
    session$setInputs(date_range = as.Date(c("2012-07-31", "2013-07-31")))
    expect_equal(
      dplyr::last(calls$urls),
      "/mockpath?dataset=icd10&codes=1359723003,1359726006"
    )
  })
})

test_that("the sidebar loads a codelist from the URL before writing the URL", {
  calls <- local_sidebar_mocks()
  local_mocked_bindings(
    get_codelist = function(url) {
      Codelist(
        test_codelist(),
        coding_system = "snomedct",
        full_slug = "user/test/phq/abc123"
      )
    }
  )

  testServer(
    mod_sidebar_server,
    args = list(
      params = list(dataset = "snomedct", codelist = "user/test/phq/abc123")
    ),
    {
      expect_false(url_ready())
      session$setInputs(dataset = "snomedct")

      expect_equal(rv_search_method(), "codelist")
      expect_equal(rv_codelist()$code, test_codelist()$code)
      expect_equal(
        calls$urls,
        "/mockpath?dataset=snomedct&codelist=user/test/phq/abc123"
      )
    }
  )
})
