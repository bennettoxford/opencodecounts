#' Usage table module: summarised code usage with CSV download
#'
#' Returns a [bslib::nav_panel()] for use inside the main navset.
#'
#' @param id Module id.
#' @noRd
#' @import shiny
#' @import bslib
#' @import bsicons
#' @importFrom DT DTOutput
mod_usage_table_ui <- function(id) {
  ns <- NS(id)

  nav_panel(
    "Usage table",
    icon = bs_icon("file-earmark-spreadsheet"),
    download_link(
      ns("download_usage_table"),
      "Total usage of each code in the selected date range."
    ),
    DTOutput(ns("usage_table"))
  )
}

#' Summarise total usage per code
#'
#' Shared by the usage table and its CSV download so both stay identical.
#' @keywords internal
#' @import dplyr
summarise_usage <- function(data) {
  data |>
    group_by(code, description) |>
    summarise(total_usage = sum(usage, na.rm = TRUE)) |>
    ungroup() |>
    mutate(total_pct = total_usage / sum(total_usage, na.rm = TRUE))
}

#' Usage table module server
#'
#' @param id Module id.
#' @param filtered_data Reactive returning the filtered usage data.
#' @param dataset Reactive returning the selected dataset id (used in the
#'   download filename).
#' @noRd
#' @import shiny
#' @import dplyr
#' @importFrom DT renderDT
#' @importFrom data.table fwrite
mod_usage_table_server <- function(id, filtered_data, dataset) {
  moduleServer(id, function(input, output, session) {
    output$usage_table <- renderDT({
      filtered_data() |>
        summarise_usage() |>
        arrange(desc(total_usage)) |>
        datatable_usage()
    })

    output$download_usage_table <- downloadHandler(
      filename = function() {
        paste0(
          dataset(),
          "_selected_codes_usage_",
          "from_",
          min(filtered_data()$start_date),
          "_to_",
          max(filtered_data()$end_date),
          ".csv"
        )
      },
      content = function(file) {
        fwrite(
          filtered_data() |>
            summarise_usage(),
          file
        )
      }
    )
  })
}
