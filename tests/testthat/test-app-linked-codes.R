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
          session$setInputs(show_individual_codes = TRUE)
          expect_error(output$linked_codes_switch, class = "shiny.silent.error")
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

test_that("the trends plot shows linked codes only with the total usage", {
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
          expect_match(output$usage_plot, "Linked codes not selected")

          # The switch is hidden with individual codes, also when more than
          # 500 codes fall back to the total
          session$setInputs(show_individual_codes = TRUE)
          expect_no_match(output$usage_plot, "Linked codes not selected")

          session$setInputs(show_individual_codes = FALSE)

          session$setInputs(show_linked_codes = FALSE)
          expect_no_match(output$usage_plot, "Linked codes not selected")
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
          "Load a codelist or select codes by code"
        } else {
          "All linked codes with usage are selected"
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
    linked_codes_not_selected()

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
    side = "old",
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
    pair = c(
      "836541000000100 1359723003",
      "1104961000000104 1359726006",
      "111111111 1359723003",
      "222222222 333"
    ),
    code = c("836541000000100", "1359726006", "111111111", "222222222"),
    term = paste("Term", c("A", "B", "C", "D")),
    side = c("old", "new", "old", "old"),
    usage = c(150, 5, 1, 1),
    share = c(0.75, 0.1, 0.01, 0.01),
    linked_code = c("1359723003", "1104961000000104", "1359723003", "333"),
    linked_term = paste("Linked term", c("A", "B", "C", "D")),
    linked_usage = c(50, 45, 99, 0)
  )
}

test_timeline_usage <- function() {
  test_app_usage() |>
    dplyr::mutate(
      active_at_start = code != "836541000000100",
      active_at_end = code != "836541000000100"
    )
}

test_that("linked_timeline_data() puts the old code above the new code", {
  data <- linked_timeline_data(test_linked_pairs()[1, ], test_timeline_usage())

  expect_equal(unique(data$old_title), "Term A (836541000000100)")
  expect_equal(unique(data$new_title), "Linked term A (1359723003)")
  # The y axis starts at the bottom, so the new code comes first
  expect_equal(
    levels(data$code_label),
    c("1359723003\n(new, selected)", "836541000000100\n(old, not selected)")
  )
  shares <- dplyr::summarise(data, share = sum(share), .by = end_date)
  expect_equal(shares$share, rep(1, nrow(shares)))
})

test_that("linked_timeline_data() labels a new code that is not selected", {
  data <- linked_timeline_data(test_linked_pairs()[2, ], test_timeline_usage())

  expect_equal(unique(data$old_title), "Linked term B (1104961000000104)")
  expect_equal(
    levels(data$code_label),
    c("1359726006\n(new, not selected)", "1104961000000104\n(old, selected)")
  )
})

test_that("linked_timeline_data() keeps both codes of a pair without usage", {
  data <- linked_timeline_data(test_linked_pairs()[4, ], test_timeline_usage())

  expect_equal(nrow(data), 2)
  expect_true(all(is.na(data$end_date)))
  expect_equal(unique(data$new_title), "Linked term D (333)")
})

test_that("plot_linked_timeline() titles the plot with the old and new code", {
  pair <- test_linked_pairs()[1, ]
  pair$linked_term <- 'Term with "quotes" and \\ backslash'
  plot <- linked_timeline_data(pair, test_timeline_usage()) |>
    plot_linked_timeline()

  expect_match(
    paste(deparse(plot$labels$title), collapse = ""),
    "Term A (836541000000100)",
    fixed = TRUE
  )
  expect_match(
    paste(deparse(plot$labels$subtitle), collapse = ""),
    "1359723003",
    fixed = TRUE
  )
  png <- withr::local_tempfile(fileext = ".png")
  expect_no_error(ggplot2::ggsave(png, plot, width = 8, height = 4))
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

test_that("the timeline shows one linked code at a time", {
  linked <- reactiveVal(test_linked_pairs())

  testServer(
    mod_linked_codes_server,
    args = list(
      linked_codes = linked,
      dataset = reactive("snomedct"),
      usage_data = reactive(test_timeline_usage())
    ),
    {
      # Ordered by code
      # Ordered by the selected code
      expect_equal(
        unname(pair_choices()),
        c(
          "1104961000000104 1359726006",
          "111111111 1359723003",
          "836541000000100 1359723003",
          "222222222 333"
        )
      )
      expect_equal(
        names(pair_choices()),
        c(
          "Linked term B (1104961000000104)",
          "Linked term C (1359723003)",
          "Linked term A (1359723003)",
          "Linked term D (333)"
        )
      )

      session$setInputs(timeline_pair = "836541000000100 1359723003")
      expect_setequal(
        unique(timeline_data()$snomed_code),
        c("1359723003", "836541000000100")
      )
      expect_true(is.list(output$timeline_plot))

      linked(test_linked_pairs()[0, ])
      session$flushReact()
      expect_equal(pair_choices(), character(0))
      expect_error(
        output$timeline_plot,
        "All linked codes with usage are selected"
      )

      linked(NULL)
      session$flushReact()
      expect_error(output$timeline_plot, "Load a codelist")
    }
  )
})

test_that("linked_codes_table_data() gives each pair a not selected and a selected row", {
  table <- linked_codes_table_data(test_linked_pairs()[1:2, ])

  expect_equal(
    table$pair,
    rep(
      c("836541000000100 1359723003", "1104961000000104 1359726006"),
      each = 2
    )
  )
  expect_equal(table$group, rep(c("Linked term A", "Linked term B"), each = 2))
  # The old code comes first in each pair
  expect_equal(
    table$code,
    c("836541000000100", "1359723003", "1104961000000104", "1359726006")
  )
  expect_equal(
    table$code_type,
    c(
      "old, not selected",
      "new, selected",
      "old, selected",
      "new, not selected"
    )
  )
  expect_equal(table$usage, c(150, 50, 45, 5))
  expect_equal(table$share, c(0.75, 0.25, 0.9, 0.1))

  expect_equal(nrow(linked_codes_table_data(empty_linked_codes())), 0)
})
