#' Linked codes module: linked codes that are not selected
#'
#' Shows old and new SNOMED CT codes with usage that are linked to the
#' selected codes but not selected, as a table and a timeline. Returns a
#' [bslib::nav_panel()] for use inside the main navset.
#'
#' @param id Module id.
#' @noRd
#' @import shiny
#' @import bslib
#' @import bsicons
#' @importFrom DT DTOutput
mod_linked_codes_ui <- function(id) {
  ns <- NS(id)

  nav_panel(
    "Linked codes not selected",
    icon = bs_icon("link-45deg"),
    navset_underline(
      id = ns("linked_codes_view"),
      nav_panel(
        "Table of all code pairs",
        icon = bs_icon("table"),
        br(),
        download_link(
          ns("download_linked_codes"),
          "All pairs of selected codes and linked codes not selected."
        ),
        DTOutput(ns("linked_codes_table"))
      ),
      nav_panel(
        "Timeline of one pair",
        icon = bs_icon("clock-history"),
        br(),
        selectInput(
          ns("timeline_pair"),
          tooltip_label(
            "Select linked code pair",
            "Each pair is a selected code and a linked code that is not selected.",
            placement = "right"
          ),
          choices = NULL,
          width = "100%"
        ),
        # A fixed height keeps the plot from shrinking with the card
        plotOutput(ns("timeline_plot"), height = "340px", fill = FALSE)
      )
    )
  )
}

#' Message shown when the selected codes include every linked code with usage
#' @noRd
all_linked_selected_message <- "All linked codes with usage are selected."

#' Empty linked codes table
#'
#' Used for both the displayed table and the CSV download when the selected
#' codes are not checked for linked codes.
#' @noRd
empty_linked_codes <- function() {
  tibble::tibble(
    codelist = character(0),
    pair = character(0),
    code = character(0),
    term = character(0),
    side = character(0),
    usage = numeric(0),
    share = numeric(0),
    linked_code = character(0),
    linked_term = character(0),
    linked_usage = numeric(0)
  )
}

#' Linked codes as two rows per pair for the grouped table
#'
#' Pairs are ordered by the usage of the code not selected, with the old
#' code first in each pair.
#'
#' @param linked_codes Tibble from `linked_codes_not_selected()`
#'
#' @return Tibble with the `pair` key, the selected code's description as
#'   `group`, and one row for the code not selected and one for the selected
#'   code of each pair
#' @noRd
linked_codes_table_data <- function(linked_codes) {
  pairs <- linked_codes |>
    arrange(desc(usage)) |>
    mutate(
      pair = factor(pair, levels = unique(pair)),
      group = linked_term,
      is_old_not_selected = side == "old"
    )

  not_selected <- pairs |>
    transmute(
      pair,
      group,
      code,
      description = term,
      code_type = paste0(side, ", not selected"),
      usage,
      share,
      is_old = is_old_not_selected
    )
  selected <- pairs |>
    transmute(
      pair,
      group,
      code = linked_code,
      description = linked_term,
      code_type = if_else(
        is_old_not_selected,
        "new, selected",
        "old, selected"
      ),
      usage = linked_usage,
      share = 1 - share,
      is_old = !is_old_not_selected
    )

  bind_rows(not_selected, selected) |>
    arrange(pair, desc(is_old)) |>
    mutate(pair = as.character(pair)) |>
    select(-is_old)
}

#' Linked codes module server
#'
#' @param id Module id.
#' @param linked_codes Reactive returning the tibble from
#'   `linked_codes_not_selected()`, or `NULL` when the selected codes are not
#'   checked.
#' @param dataset Reactive returning the selected dataset id (used in the
#'   download filename).
#' @param usage_data Reactive returning the usage data of all codes in the
#'   selected date range, for the timeline.
#' @noRd
#' @import shiny
#' @import dplyr
#' @importFrom DT renderDT datatable formatPercentage formatRound
#' @importFrom data.table fwrite
mod_linked_codes_server <- function(id, linked_codes, dataset, usage_data) {
  moduleServer(id, function(input, output, session) {
    not_checked_message <- "Load a codelist or select codes by code to find linked codes that are not selected."

    # The table and the download show an empty table when the codes are not
    # checked
    codes <- reactive({
      if (is.null(linked_codes())) {
        return(empty_linked_codes())
      }
      linked_codes()
    })

    # One choice per pair, labelled with the selected code and ordered by it
    pair_choices <- reactive({
      if (is.null(linked_codes()) || nrow(linked_codes()) == 0) {
        return(character(0))
      }
      pairs <- arrange(linked_codes(), linked_code, code)
      stats::setNames(
        pairs$pair,
        paste0(pairs$linked_term, " (", pairs$linked_code, ")")
      )
    })

    # Keep the user's choice when the date range changes, otherwise select
    # the first linked code
    observe({
      choices <- pair_choices()
      selected <- intersect(isolate(input$timeline_pair), choices)
      updateSelectInput(
        session,
        "timeline_pair",
        choices = choices,
        selected = if (length(selected) > 0) selected else choices[1]
      )
    })

    timeline_data <- reactive({
      req(linked_codes(), input$timeline_pair)
      linked_codes() |>
        filter(pair == input$timeline_pair) |>
        linked_timeline_data(usage_data())
    })

    output$timeline_plot <- renderPlot(
      {
        validate(need(!is.null(linked_codes()), not_checked_message))
        validate(need(nrow(linked_codes()) > 0, all_linked_selected_message))
        plot_linked_timeline(timeline_data())
      },
      res = 96
    )

    output$linked_codes_table <- renderDT({
      empty_message <- if (is.null(linked_codes())) {
        not_checked_message
      } else {
        all_linked_selected_message
      }

      codes() |>
        linked_codes_table_data() |>
        datatable(
          colnames = c(
            "Pair",
            "Group",
            "Code",
            "Description",
            "Old or new",
            "Usage",
            "Share of pair"
          ),
          rownames = FALSE,
          extensions = "RowGroup",
          options = list(
            language = list(emptyTable = empty_message),
            # Group by pair, so pairs that share a selected code stay apart,
            # and label each group with the selected code's description
            rowGroup = list(
              dataSrc = 0,
              startRender = DT::JS(
                "function(rows, group) { return rows.data()[0][1]; }"
              )
            ),
            columnDefs = list(list(visible = FALSE, targets = c(0, 1))),
            # Sorting by a column would split the pairs
            ordering = FALSE,
            pageLength = 10,
            searching = TRUE
          ),
          width = "100%",
          fillContainer = TRUE
        ) |>
        formatRound("usage", digits = 0) |>
        formatPercentage("share", digits = 1)
    })

    output$download_linked_codes <- downloadHandler(
      filename = function() {
        paste0(dataset(), "_linked_codes_not_selected.csv")
      },
      content = function(file) {
        fwrite(codes(), file)
      }
    )
  })
}
