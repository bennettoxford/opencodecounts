#' The application server-side
#'
#' Wires the sidebar module's returns into the central `filtered_data()`
#' reactive and passes it to the display modules.
#'
#' @param input,output,session Internal parameters for {shiny}.
#' @noRd
#' @import shiny
#' @import dplyr
app_server <- function(input, output, session) {
  sidebar <- mod_sidebar_server(
    "sidebar",
    params = parse_url_params(isolate(session$clientData$url_search))
  )

  # Usage data in the selected date range, for all codes
  date_filtered_data <- reactive({
    req(sidebar$selected_data(), sidebar$date_range())

    sidebar$selected_data() |>
      filter(
        end_date >= sidebar$date_range()[1] &
          end_date <= sidebar$date_range()[2]
      )
  })

  # Filtered usage data
  filtered_data <- reactive({
    withProgress(message = "Filtering data ...", {
      data <- date_filtered_data()

      # Apply filters based on the current filtering method
      if (sidebar$search_method() == "search") {
        if (
          !is.null(sidebar$code_specific_search()) &&
            length(sidebar$code_specific_search()) > 0
        ) {
          data <- data |>
            filter(code %in% sidebar$code_specific_search())
        }

        if (
          !is.null(sidebar$code_pattern_search()) &&
            sidebar$code_pattern_search() != ""
        ) {
          data <- data |>
            filter(grepl(
              paste("^", sidebar$code_pattern_search(), sep = ""),
              code,
              ignore.case = TRUE
            ))
        }

        if (
          !is.null(sidebar$description_search()) &&
            sidebar$description_search() != ""
        ) {
          data <- data |>
            filter(grepl(
              sidebar$description_search(),
              description,
              ignore.case = TRUE
            ))
        }
      } else if (sidebar$search_method() == "codelist") {
        req(sidebar$codelist())
        data <- data |>
          filter(code %in% sidebar$codelist()$code)
      }

      if (nrow(data) == 0) {
        showNotification(
          "No data matches your current filters.",
          type = "warning"
        )
      }
      data
    })
  })

  # SNOMED CT codes to check for linked codes: a loaded codelist or codes
  # selected by code. A description search is not checked, because it
  # selects codes by their text rather than by code.
  codes_to_check <- reactive({
    if (sidebar$dataset() != "snomedct") {
      return(NULL)
    }

    if (sidebar$search_method() == "codelist") {
      return(sidebar$codelist()$code)
    }

    has_code_search <- length(sidebar$code_specific_search()) > 0
    has_description_search <- !is.null(sidebar$description_search()) &&
      sidebar$description_search() != ""
    if (
      sidebar$search_method() == "search" &&
        has_code_search &&
        !has_description_search
    ) {
      return(sidebar$code_specific_search())
    }

    NULL
  })

  # A reactiveVal only notifies the trends switch when the value changes, so
  # the switch is not reset each time a code is added
  has_linked_codes <- reactiveVal(FALSE)
  observe(has_linked_codes(!is.null(codes_to_check())))

  snomed_history <- reactive(get_app_dataset("snomed_history"))

  # Linked codes with usage that the selected SNOMED CT codes do not
  # include, or NULL when the selected codes are not checked
  linked_codes <- reactive({
    if (is.null(codes_to_check())) {
      return(NULL)
    }
    req(length(codes_to_check()) > 0)

    history <- tryCatch(snomed_history(), error = function(e) {
      showNotification(
        "Linked codes are not available because the history table could not be loaded.",
        type = "error"
      )
      NULL
    })
    req(history)

    usage_total <- date_filtered_data() |>
      summarise(usage = sum(as.numeric(usage)), .by = code) |>
      rename(snomed_code = code)

    find_linked_codes(
      tibble::tibble(codelist = "selected codes", code = codes_to_check()),
      usage_total,
      history
    ) |>
      linked_codes_not_selected()
  })

  # Usage data of the linked codes not selected
  linked_data <- reactive({
    req(linked_codes())
    date_filtered_data() |>
      filter(code %in% linked_codes()$code)
  })

  mod_value_boxes_server("value_boxes", filtered_data, linked_codes)
  mod_trends_server(
    "trends",
    filtered_data,
    linked_data = linked_data,
    has_linked_codes = has_linked_codes,
    dataset = sidebar$dataset,
    reset = sidebar$reset
  )
  mod_usage_table_server(
    "usage_table",
    filtered_data,
    dataset = sidebar$dataset
  )
  mod_codes_table_server(
    "codes_table",
    filtered_data,
    search_method = sidebar$search_method,
    codelist = sidebar$codelist,
    dataset = sidebar$dataset
  )
  mod_linked_codes_server(
    "linked_codes",
    linked_codes,
    dataset = sidebar$dataset,
    usage_data = date_filtered_data
  )
}
