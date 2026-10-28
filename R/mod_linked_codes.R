#' Linked codes module: linked codes missing from the selected codes
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
    "Linked codes not included",
    icon = bs_icon("link-45deg"),
    navset_underline(
      id = ns("linked_codes_view"),
      nav_panel(
        "Table",
        icon = bs_icon("table"),
        br(),
        download_link(
          ns("download_linked_codes"),
          "Linked codes with usage that are not selected."
        ),
        DTOutput(ns("linked_codes_table"))
      ),
      nav_panel(
        "Timeline",
        icon = bs_icon("clock-history"),
        br(),
        # The plot height grows with the number of codes, so it must not
        # fill and shrink with the card
        plotOutput(ns("timeline_plot"), height = "auto", fill = FALSE)
      )
    )
  )
}

#' Empty linked codes table
#'
#' Used for both the displayed table and the CSV download when the selected
#' codes are not checked for linked codes.
#' @keywords internal
empty_linked_codes <- function() {
  tibble::tibble(
    codelist = character(0),
    code = character(0),
    term = character(0),
    side = character(0),
    usage = numeric(0),
    share = numeric(0),
    linked_code = character(0)
  )
}

#' Linked codes module server
#'
#' @param id Module id.
#' @param linked_codes Reactive returning the tibble from
#'   `missing_linked_codes()`, or `NULL` when the selected codes are not
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
    not_checked_message <- "Load a SNOMED CT codelist or select SNOMED CT codes by code to find linked codes that are not included."
    all_included_message <- "The selected codes include all linked codes with usage."

    timeline_data <- reactive({
      req(linked_codes())
      linked_timeline_data(linked_codes(), usage_data())
    })

    output$timeline_plot <- renderPlot(
      {
        validate(need(!is.null(linked_codes()), not_checked_message))
        validate(need(nrow(linked_codes()) > 0, all_included_message))
        plot_linked_timeline(timeline_data())
      },
      height = function() {
        linked_timeline_height(tryCatch(timeline_data(), error = \(e) NULL))
      },
      res = 96
    )

    output$linked_codes_table <- renderDT({
      empty_message <- if (is.null(linked_codes())) {
        not_checked_message
      } else {
        all_included_message
      }

      codes <- if (is.null(linked_codes())) {
        empty_linked_codes()
      } else {
        linked_codes()
      }

      codes |>
        select(code, term, side, usage, share, linked_code) |>
        arrange(desc(usage)) |>
        datatable(
          colnames = c(
            "Code",
            "Description",
            "Old or new code",
            "Usage",
            "Share of pair",
            "Linked selected code"
          ),
          rownames = FALSE,
          options = list(
            language = list(emptyTable = empty_message),
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
        paste0(dataset(), "_linked_codes_not_included.csv")
      },
      content = function(file) {
        if (is.null(linked_codes())) {
          fwrite(empty_linked_codes(), file)
          return()
        }

        fwrite(linked_codes(), file)
      }
    )
  })
}
