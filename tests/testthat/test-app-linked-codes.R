# Sidebar module returns, so app_server() can be tested without the sidebar
# inputs. Pass a reactiveVal as code_specific_search to change it in a test.
local_sidebar <- function(
  search_method = "codelist",
  dataset = "snomedct",
  codelist = test_codelist(),
  code_specific_search = NULL,
  description_search = NULL,
  env = parent.frame()
) {
  if (!is.function(code_specific_search)) {
    code_specific_search <- reactiveVal(code_specific_search)
  }

  local_mocked_bindings(
    mod_sidebar_server = function(id) {
      list(
        dataset = reactive(dataset),
        selected_data = reactive(test_app_usage()),
        date_range = reactive(as.Date(c("2012-07-31", "2013-07-31"))),
        search_method = reactive(search_method),
        codelist = reactive(codelist),
        code_specific_search = code_specific_search,
        code_pattern_search = reactive(NULL),
        description_search = reactive(description_search),
        reset = reactive(0)
      )
    },
    get_app_dataset = function(dataset, version = NULL) test_snomed_history(),
    .env = env
  )
}

test_that("app_server() finds linked codes for a loaded SNOMED CT codelist", {
  local_sidebar()

  testServer(app_server, {
    session$flushReact()
    expect_true(has_linked_codes())
    expect_equal(linked_codes()$code, "836541000000100")
    expect_equal(linked_codes()$usage, 150)
    expect_equal(unique(linked_data()$code), "836541000000100")
  })
})

test_that("app_server() finds linked codes for codes selected by code", {
  local_sidebar(search_method = "search", code_specific_search = "1359723003")

  testServer(app_server, {
    session$flushReact()
    expect_true(has_linked_codes())
    expect_equal(linked_codes()$code, "836541000000100")
    expect_equal(linked_codes()$linked_code, "1359723003")
  })
})

test_that("app_server() does not check description searches", {
  local_sidebar(search_method = "search", description_search = "PHQ")
  testServer(app_server, {
    session$flushReact()
    expect_false(has_linked_codes())
    expect_null(linked_codes())
  })

  local_sidebar(
    search_method = "search",
    code_specific_search = "1359723003",
    description_search = "PHQ"
  )
  testServer(app_server, {
    session$flushReact()
    expect_false(has_linked_codes())
    expect_null(linked_codes())
  })
})

test_that("app_server() only checks SNOMED CT codes", {
  local_sidebar(dataset = "icd10")
  testServer(app_server, {
    session$flushReact()
    expect_false(has_linked_codes())
    expect_null(linked_codes())
  })
})

test_that("app_server() keeps the switch when selected codes change", {
  codes <- reactiveVal("1359723003")
  local_sidebar(search_method = "search", code_specific_search = codes)

  testServer(app_server, {
    n_updates <- 0
    observe({
      has_linked_codes()
      n_updates <<- n_updates + 1
    })
    session$flushReact()

    codes(c("1359723003", "1359726006"))
    session$flushReact()

    expect_true(has_linked_codes())
    expect_equal(n_updates, 1)
  })
})

test_that("app_server() does not need the history table to show the switch", {
  local_sidebar()
  local_mocked_bindings(
    get_app_dataset = function(dataset, version = NULL) stop("404")
  )

  testServer(app_server, {
    session$flushReact()
    expect_true(has_linked_codes())
    expect_error(linked_codes(), class = "shiny.silent.error")
  })
})

test_that("app_server() skips empty codelists", {
  local_sidebar(
    codelist = tibble::tibble(code = character(), term = character())
  )

  testServer(app_server, {
    expect_error(linked_codes(), class = "shiny.silent.error")
  })
})

test_that("the linked codes switch only shows when the codes can be checked", {
  for (has_linked in c(TRUE, FALSE)) {
    testServer(
      mod_trends_server,
      args = list(
        filtered_data = reactive(test_app_usage()),
        linked_data = reactive(test_app_usage()[0, ]),
        has_linked_codes = reactive(has_linked),
        dataset = reactive("snomedct"),
        reset = reactive(0)
      ),
      {
        if (has_linked) {
          expect_match(output$linked_codes_switch$html, "show_linked_codes")
        } else {
          expect_error(output$linked_codes_switch, class = "shiny.silent.error")
        }
      }
    )
  }
})

test_that("the linked codes switch is not reset when the linked data changes", {
  linked_data <- reactiveVal(test_app_usage()[1, ])

  testServer(
    mod_trends_server,
    args = list(
      filtered_data = reactive(test_app_usage()),
      linked_data = linked_data,
      has_linked_codes = reactive(TRUE),
      dataset = reactive("snomedct"),
      reset = reactive(0)
    ),
    {
      first <- output$linked_codes_switch
      linked_data(test_app_usage()[2, ])
      session$flushReact()
      expect_identical(output$linked_codes_switch, first)
    }
  )
})

test_that("the trends plot shows linked codes with the total and individual codes", {
  # More than 500 codes cannot be shown individually, so the total is shown
  many_codes <- tibble::tibble(
    start_date = as.Date("2012-08-01"),
    end_date = as.Date("2013-07-31"),
    code = as.character(1:501),
    description = "Term",
    usage = 10L
  )
  few_codes <- dplyr::filter(test_app_usage(), code == "1359723003")

  for (selected in list(few_codes, many_codes)) {
    testServer(
      mod_trends_server,
      args = list(
        filtered_data = reactive(selected),
        linked_data = reactive(dplyr::filter(
          test_app_usage(),
          code == "836541000000100"
        )),
        has_linked_codes = reactive(TRUE),
        dataset = reactive("snomedct"),
        reset = reactive(0)
      ),
      {
        # The text aesthetic is only used by plotly for tooltips
        suppressWarnings({
          session$setInputs(
            show_individual_codes = FALSE,
            show_linked_codes = TRUE
          )
          expect_match(output$usage_plot, "Linked codes not included")

          session$setInputs(show_individual_codes = TRUE)
          expect_match(output$usage_plot, "Linked codes not included")

          session$setInputs(show_linked_codes = FALSE)
          expect_no_match(output$usage_plot, "Linked codes not included")
        })
      }
    )
  }
})

test_that("the linked codes table explains why it is empty", {
  for (linked in list(NULL, empty_linked_codes())) {
    testServer(
      mod_linked_codes_server,
      args = list(
        linked_codes = reactive(linked),
        dataset = reactive("snomedct")
      ),
      {
        message <- if (is.null(linked)) {
          "Load a SNOMED CT codelist or select SNOMED CT codes"
        } else {
          "The selected codes include all linked codes with usage"
        }
        expect_match(output$linked_codes_table, message)
      }
    )
  }
})

test_that("the linked codes download writes the table", {
  linked <- find_linked_codes(
    dplyr::mutate(test_codelist(), codelist = "selected codes"),
    test_usage_total(),
    test_snomed_history()
  ) |>
    missing_linked_codes()

  testServer(
    mod_linked_codes_server,
    args = list(
      linked_codes = reactive(linked),
      dataset = reactive("snomedct")
    ),
    {
      path <- output$download_linked_codes
      expect_equal(
        readr::read_csv(path, col_types = readr::cols(.default = "c"))$code,
        "836541000000100"
      )
    }
  )

  testServer(
    mod_linked_codes_server,
    args = list(linked_codes = reactive(NULL), dataset = reactive("snomedct")),
    {
      path <- output$download_linked_codes
      expect_equal(nrow(readr::read_csv(path, show_col_types = FALSE)), 0)
    }
  )
})

test_that("the value box counts linked codes only when the codes are checked", {
  linked <- tibble::tibble(
    codelist = "selected codes",
    code = c("a", "a", "b"),
    term = "Term",
    side = "old code",
    usage = 10,
    share = 0.5,
    linked_code = c("x", "y", "x")
  )

  testServer(
    mod_value_boxes_server,
    args = list(
      filtered_data = reactive(test_app_usage()),
      linked_codes = reactive(linked)
    ),
    {
      # A code linked to two selected codes is counted once
      expect_equal(output$linked_codes, "2")
    }
  )

  testServer(
    mod_value_boxes_server,
    args = list(
      filtered_data = reactive(test_app_usage()),
      linked_codes = reactive(NULL)
    ),
    {
      expect_equal(output$linked_codes, "Not checked")
    }
  )
})

test_linked_pairs <- function() {
  tibble::tibble(
    codelist = "selected codes",
    code = c("836541000000100", "1359726006", "111111111", "222222222"),
    term = paste("Term", c("A", "B", "C", "D")),
    side = c("old code", "new code", "old code", "old code"),
    usage = c(150, 5, 1, 1),
    share = c(0.75, 0.1, 0.01, 0.01),
    linked_code = c("1359723003", "1104961000000104", "1359723003", "333")
  )
}

test_timeline_usage <- function() {
  test_app_usage() |>
    dplyr::mutate(
      active_at_start = code != "836541000000100",
      active_at_end = code != "836541000000100"
    )
}

test_that("linked_timeline_data() puts each old code above its new code", {
  data <- linked_timeline_data(
    test_linked_pairs()[1:2, ],
    test_timeline_usage()
  )

  expect_equal(
    levels(data$panel),
    c("Term 1359723003", "Term 1359726006")
  )
  # The y axis starts at the bottom, so new codes come first
  expect_equal(
    levels(data$code_label),
    c(
      "1359723003\n(new, selected)",
      "1359726006\n(new, missing)",
      "836541000000100\n(old, missing)",
      "1104961000000104\n(old, selected)"
    )
  )
  shares <- dplyr::summarise(data, share = sum(share), .by = c(panel, end_date))
  expect_equal(shares$share, rep(1, nrow(shares)))
})

test_that("linked_timeline_data() shows a new code once when pairs share it", {
  data <- linked_timeline_data(
    test_linked_pairs()[c(1, 3), ],
    test_timeline_usage()
  )

  expect_equal(levels(data$panel), "Term 1359723003")
  expect_equal(sum(data$code == "1359723003"), 0)
  expect_equal(sum(data$snomed_code == "1359723003"), 2)
})

test_that("linked_timeline_data() uses the missing code's term without new code usage", {
  data <- linked_timeline_data(test_linked_pairs()[4, ], test_timeline_usage())
  expect_equal(nrow(data), 0)

  pairs <- test_linked_pairs()[4, ] |>
    dplyr::mutate(code = "836541000000100")
  data <- linked_timeline_data(pairs, test_timeline_usage())
  expect_equal(levels(data$panel), "Term D")
})

test_that("linked_timeline_height() gives every code row the same height", {
  expect_equal(linked_timeline_height(NULL), 300)

  one <- linked_timeline_data(test_linked_pairs()[1, ], test_timeline_usage())
  two <- linked_timeline_data(test_linked_pairs()[1:2, ], test_timeline_usage())
  expect_equal(
    linked_timeline_height(two) - linked_timeline_height(one),
    45 + 2 * 60
  )
})

test_that("plot_linked_timeline() colours tiles by code status", {
  plot <- linked_timeline_data(
    test_linked_pairs()[1, ],
    test_timeline_usage()
  ) |>
    plot_linked_timeline()
  built <- ggplot2::ggplot_build(plot)$data[[1]]

  expect_setequal(unique(built$fill), c("#5DC863", "#FDE725"))
})

test_that("the timeline plots all linked codes", {
  linked <- reactiveVal(test_linked_pairs()[1:2, ])

  testServer(
    mod_linked_codes_server,
    args = list(
      linked_codes = linked,
      dataset = reactive("snomedct"),
      usage_data = reactive(test_timeline_usage())
    ),
    {
      expect_equal(nrow(dplyr::distinct(timeline_data(), code_label)), 4)
      expect_true(is.list(output$timeline_plot))

      linked(test_linked_pairs()[0, ])
      session$flushReact()
      expect_error(output$timeline_plot, "include all linked codes")

      linked(NULL)
      session$flushReact()
      expect_error(output$timeline_plot, "Load a SNOMED CT codelist")
    }
  )
})
