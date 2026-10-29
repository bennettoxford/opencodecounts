#' Value boxes module: unique codes, linked codes and total activity
#'
#' @param id Module id.
#' @noRd
#' @import shiny
#' @import bslib
#' @import bsicons
#' @importFrom plotly plotlyOutput
mod_value_boxes_ui <- function(id) {
  ns <- NS(id)

  layout_columns(
    height = "20%",
    value_box(
      title = "Selected codes with usage data in England",
      value = textOutput(ns("unique_codes")),
      showcase = bs_icon("file-earmark-medical")
    ),
    value_box(
      title = "Linked codes with usage data not selected",
      value = textOutput(ns("linked_codes")),
      showcase = bs_icon("link-45deg")
    ),
    value_box(
      title = "Total number of recorded events in England",
      value = textOutput(ns("total_activity")),
      showcase = plotlyOutput(ns("sparkline"))
    )
  )
}

#' Value boxes module server
#'
#' @param id Module id.
#' @param filtered_data Reactive returning the filtered usage data.
#' @param linked_codes Reactive returning the tibble from
#'   `linked_codes_not_selected()`, or `NULL` when the selected codes are not
#'   checked for linked codes.
#' @noRd
#' @import shiny
#' @importFrom scales comma
#' @importFrom plotly renderPlotly
mod_value_boxes_server <- function(id, filtered_data, linked_codes) {
  moduleServer(id, function(input, output, session) {
    output$unique_codes <- renderText({
      scales::comma(length(unique(filtered_data()$code)))
    })

    output$linked_codes <- renderText({
      if (is.null(linked_codes())) {
        return("Not checked")
      }
      scales::comma(length(unique(linked_codes()$code)))
    })

    output$total_activity <- renderText({
      scales::comma(sum(filtered_data()$usage, na.rm = TRUE))
    })

    output$sparkline <- renderPlotly({
      filtered_data() |>
        plot_sparkline()
    })
  })
}
