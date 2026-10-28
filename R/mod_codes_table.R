#' Selected codes module: codes table with CSV download
#'
#' Returns a [bslib::nav_panel()] for use inside the main navset.
#'
#' @param id Module id.
#' @noRd
#' @import shiny
#' @import bslib
#' @import bsicons
#' @importFrom DT DTOutput
mod_codes_table_ui <- function(id) {
  ns <- NS(id)

  nav_panel(
    "Selected codes",
    icon = bs_icon("file-earmark-medical"),
    download_link(
      ns("download_codes_table"),
      "Selected codes and whether they have usage data."
    ),
    DTOutput(ns("codes_table"))
  )
}

#' Empty selected codes table
#'
#' Used for both the displayed table and the CSV download when no codes or
#' codelist are selected.
#' @keywords internal
empty_selected_codes <- function() {
  tibble::tibble(
    code = character(0),
    description = character(0),
    usage_data_available = factor(
      character(0),
      levels = c("Usage data available", "No usage data reported")
    )
  )
}

#' Selected codes module server
#'
#' @param id Module id.
#' @param filtered_data Reactive returning the filtered usage data.
#' @param search_method Reactive returning the current search method
#'   ("none", "search", or "codelist").
#' @param codelist Reactive returning the loaded codelist data (or `NULL`).
#' @param dataset Reactive returning the selected dataset id (used in the
#'   download filename and table rendering).
#' @noRd
#' @import shiny
#' @import dplyr
#' @importFrom DT renderDT
#' @importFrom data.table fwrite
mod_codes_table_server <- function(
  id,
  filtered_data,
  search_method,
  codelist,
  dataset
) {
  moduleServer(id, function(input, output, session) {
    output$codes_table <- renderDT({
      if (search_method() == "none") {
        # Return an empty table if no codes or codelist are selected
        selected_codes <- empty_selected_codes()
      } else if (search_method() == "codelist") {
        # Get all codes with usage data
        codes_with_usage_data <- filtered_data() |>
          pull(code)

        selected_codes <- codelist() |>
          mutate(usage_data_available = code %in% codes_with_usage_data)
      } else if (search_method() == "search") {
        # All codes will have usage data, otherwise they wouldn't be in the data sets
        # we can therefore just assign TRUE for all these codes
        selected_codes <- filtered_data() |>
          select(code, description) |>
          distinct() |>
          mutate(usage_data_available = TRUE)
      }

      selected_codes |>
        mutate(
          usage_data_available = factor(
            usage_data_available,
            levels = c(TRUE, FALSE),
            labels = c("Usage data available", "No usage data reported")
          )
        ) |>
        arrange(desc(usage_data_available)) |>
        datatable_codelist(data_desc = dataset())
    })

    output$download_codes_table <- downloadHandler(
      filename = function() {
        paste0(
          dataset(),
          "_selected_codes_",
          "from_",
          min(filtered_data()$start_date),
          "_to_",
          max(filtered_data()$end_date),
          ".csv"
        )
      },
      content = function(file) {
        if (search_method() == "none") {
          # Write an empty file with column headers
          fwrite(empty_selected_codes(), file)
          return()
        }

        selected_codes <- if (search_method() == "codelist") {
          codes_with_usage_data <- filtered_data() |> pull(code)
          codelist() |>
            mutate(usage_data_available = code %in% codes_with_usage_data)
        } else if (search_method() == "search") {
          filtered_data() |>
            select(code, description) |>
            distinct() |>
            mutate(usage_data_available = TRUE)
        }

        fwrite(selected_codes, file)
      }
    )
  })
}
