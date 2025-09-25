#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#' @noRd
#' @import shiny
#' @import bslib
#' @import bsicons
#' @import dplyr
#' @import ggplot2
#' @importFrom data.table fwrite
#' @importFrom scales comma label_date_short label_comma
#' @importFrom lubridate month year
#' @importFrom DT renderDT
#' @importFrom plotly renderPlotly ggplotly plot_ly config add_lines layout
#' @import here

app_server <- function(input, output, session) {
  # Reactive values for search method (1) none, (2) code/desc or (3) codelist) and codelist data
  rv_search_method <- reactiveVal("none")
  rv_codelist <- reactiveVal(NULL)

  # Reset search inputs when dataset changes
  observe({
    rv_search_method("none")
    updateSelectizeInput(session, "code_specific_search", selected = character(0))
    updateSelectizeInput(session, "code_pattern_search", selected = character(0))
    updateTextInput(session, "description_search", value = "")
  }) |>
    bindEvent(input$dataset)

  # Selected code usage dataset
  selected_data <- reactive({
    updateCheckboxInput(session, "show_individual_codes", value = FALSE)

    if (input$dataset == "snomedct") {
      opencodecounts::snomed_usage |>
        select(start_date, end_date, code = snomed_code, description, usage)
    } else if (input$dataset == "icd10") {
      opencodecounts::icd10_usage |>
        select(start_date, end_date, code = icd10_code, description, usage)
    } else if (input$dataset == "opcs4") {
      opencodecounts::opcs4_usage |>
        select(start_date, end_date, code = opcs4_code, description, usage)
    }
  })

  output$dynamic_date_slider <- renderUI({
    req(selected_data())

    available_end_dates <- sort(unique(selected_data()$end_date))

    sliderInput(
      "date_range",
      label = tooltip(
        span("Date range", bs_icon("info-circle")),
        "Filter available data by selecting end dates of yearly reporting intervals",
        options = list(customClass = "left-align-tooltip")
      ),
      min = min(available_end_dates),
      max = max(available_end_dates),
      value = range(available_end_dates, available_end_dates),
      step = 365,
      timeFormat = "%Y",
      ticks = FALSE
    )
  })

  output$dynamic_code_pattern_input <- renderUI({
    req(input$dataset)
    label_text <- if (input$dataset == "icd10") {
      "ICD-10 category"
    } else if (input$dataset == "opcs4") {
      "OPCS-4 category"
    } else {
      NULL
    }

    textInput(
      "code_pattern_search",
      tooltip(
        span(
          label_text,
          bs_icon("info-circle")
        ),
        "Enter the beginning of a code to search by chapter or subchapter. Multiple chapters can be combined using '|'.",
        options = list(customClass = "left-align-tooltip")
      )
    )
  })

  # Update code search choices depending on selected dataset
  observe({
    updateSelectizeInput(
      session, "code_specific_search",
      choices = unique(selected_data()$code),
      server = TRUE
    )
  })

  # Load codelist
  observe({
    req(input$codelist_url, input$load_codelist)

    withProgress(message = "Loading codelist ...", {
      tryCatch(
        {
          codelist_s7 <- get_codelist(input$codelist_url)

          if (codelist_s7@coding_system == input$dataset) {
            showNotification(
              paste0("Successfully loaded ", codelist_s7@coding_system, " codelist."),
              type = "default"
            )

            # Store the codelist data
            rv_codelist(codelist_s7 |>
              tibble::as_tibble() |>
              dplyr::select(1:2))

            # Set filtering method to codelist
            rv_search_method("codelist")

            # Reset search inputs
            updateSelectizeInput(session, "code_specific_search", selected = character(0))
            updateTextInput(session, "code_pattern_search", value = "")
            updateTextInput(session, "description_search", value = "")
          } else {
            showNotification(
              paste0("Loaded codelist (", codelist_s7@coding_system, ") does not match selected data (", input$dataset, ")."),
              type = "error"
            )
          }
        },
        error = function(e) {
          showNotification(
            sprintf("Error loading Codelist: %s", conditionMessage(e)),
            type = "error"
          )
        }
      )
    })
  }) |>
    bindEvent(input$load_codelist)

  # Reset all search methods when reset button is clicked
  observe({
    req(input$reset_search_methods)
    rv_codelist(NULL)
    rv_search_method("none")
    updateCheckboxInput(session, "show_individual_codes", value = FALSE)
    updateSelectizeInput(session, "code_specific_search", selected = character(0))
    updateTextInput(session, "code_pattern_search", value = "")
    updateTextInput(session, "description_search", value = "")
    updateTextInput(session, "codelist_url", value = "")
    showNotification("The code selection has been reset.", type = "default")
  }) |>
    bindEvent(input$reset_search_methods)

  # Create a debounced version of the description search input
  # Currently 500 milliseconds (0.5 seconds) delay 
  description_search_debounced <- reactive(input$description_search) |> 
    debounce(500)
  
  # Set filtering method to search when search inputs change
  observe({
    # If a codelist is loaded AND the user is not entering a new search, do nothing
    if (rv_search_method() == "codelist" &&
      (is.null(input$code_specific_search) || length(input$code_specific_search) == 0) &&
      (is.null(input$code_pattern_search) || input$code_pattern_search == "") &&
      (is.null(description_search_debounced()) || description_search_debounced() == "")) {
      return()
    }

    # If any search input is used, switch to "search"
    if (!is.null(input$code_specific_search) && length(input$code_specific_search) > 0 ||
      !is.null(input$code_pattern_search) && input$code_pattern_search != "" ||
      !is.null(description_search_debounced()) && description_search_debounced() != "") {
      rv_search_method("search")
    } else {
      rv_search_method("none")
    }
  }) |> bindEvent(input$code_specific_search, input$code_pattern_search, description_search_debounced())

  # Filtered usage data
  filtered_data <- reactive({
    req(selected_data(), input$date_range)

    withProgress(message = "Filtering data ...", {
      data <- selected_data()

      data <- data |>
        filter(end_date >= input$date_range[1] & end_date <= input$date_range[2])

      # Apply filters based on the current filtering method
      if (rv_search_method() == "search") {
        if (!is.null(input$code_specific_search) && length(input$code_specific_search) > 0) {
          data <- data |>
            filter(code %in% input$code_specific_search)
        }

        if (!is.null(input$code_pattern_search) && input$code_pattern_search != "") {
          data <- data |>
            filter(grepl(paste("^", input$code_pattern_search, sep = ""), code, ignore.case = TRUE))
        }

        if (!is.null(description_search_debounced()) && description_search_debounced() != "") {
          data <- data |>
            filter(grepl(description_search_debounced(), description, ignore.case = TRUE))
        }
      } else if (rv_search_method() == "codelist") {
        req(rv_codelist())
        data <- data |>
          filter(code %in% rv_codelist()$code)
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

  # VALUE BOXES: Unique codes and total activity
  output$unique_codes <- renderText({
    scales::comma(length(unique(filtered_data()$code)))
  })

  output$total_activity <- renderText({
    scales::comma(sum(filtered_data()$usage, na.rm = TRUE))
  })

  # TABLE: Code usage
  output$usage_table <- renderDT({
    filtered_data() |>
      group_by(code, description) |>
      summarise(total_usage = sum(usage, na.rm = TRUE)) |>
      ungroup() |>
      mutate(total_pct = total_usage / sum(total_usage, na.rm = TRUE)) |>
      arrange(desc(total_usage)) |>
      datatable_usage()
  })

  # TABLE: Selected codes / Codelist
  output$codes_table <- renderDT({
    if (rv_search_method() == "none") {
      # Return an empty table if no codes or codelist are selected
      selected_codes <- tibble::tibble(
        code = character(0),
        description = character(0),
        usage_data_available = factor(character(0), levels = c("Usage data available", "No usage data reported"))
      )
    } else if (rv_search_method() == "codelist") {
      # Get all codes with usage data
      codes_with_usage_data <- filtered_data() |>
        pull(code)

      selected_codes <- rv_codelist() |>
        mutate(usage_data_available = code %in% codes_with_usage_data)
    } else if (rv_search_method() == "search") {
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
      datatable_codelist(data_desc = input$dataset)
  })

  output$download_usage_table <- downloadHandler(
    filename = function() {
      paste0(
        input$dataset,
        "_selected_codes_usage_",
        "from_", min(filtered_data()$start_date),
        "_to_", max(filtered_data()$end_date),
        ".csv"
      )
    },
    content = function(file) {
      fwrite(
        filtered_data() |>
          group_by(code, description) |>
          summarise(total_usage = sum(usage, na.rm = TRUE)) |>
          ungroup() |>
          mutate(total_pct = total_usage / sum(total_usage, na.rm = TRUE)),
        file
      )
    }
  )

  output$download_codes_table <- downloadHandler(
    filename = function() {
      paste0(
        input$dataset,
        "_selected_codes_",
        "from_", min(filtered_data()$start_date),
        "_to_", max(filtered_data()$end_date),
        ".csv"
      )
    },
    content = function(file) {
      if (rv_search_method() == "none") {
        # Write an empty file with column headers
        fwrite(
          tibble::tibble(
            code = character(0),
            description = character(0),
            usage_data_available = factor(character(0), levels = c("Usage data available", "No usage data reported"))
          ),
          file
        )
        return()
      }

      selected_codes <- if (rv_search_method() == "codelist") {
        codes_with_usage_data <- filtered_data() |> pull(code)
        rv_codelist() |> mutate(usage_data_available = code %in% codes_with_usage_data)
      } else if (rv_search_method() == "search") {
        filtered_data() |>
          select(code, description) |>
          distinct() |>
          mutate(usage_data_available = TRUE)
      }

      fwrite(selected_codes, file)
    }
  )

  # PLOT: Trends over time
  output$usage_plot <- renderPlotly({
    withProgress(message = "Plotting data ...", {
      
      # Check if filtered_data is empty
      # As a workaround we are adding a plot with text only if the
      # search criteria match no data. At some point in the future we
      # should reconsider if this is the best approach.
      # text if there are no codes
      if (nrow(filtered_data()) == 0) {
        # Create a text-only plot for empty data
        p <- ggplot() +
          geom_text(
            aes(
              x = 1,
              y = 1,
              label = "No data matches the search criteria."
            ),
            size = 6
          ) +
          theme_void() +
          theme(
            axis.line = element_blank(),
            panel.grid = element_blank()
          )
        
        return(ggplotly(p, tooltip = "text") |>
                 plotly::config(displayModeBar = FALSE))
      }
      
      unique_codes <- length(unique(filtered_data()$code))
      
      # When there are 500 or less selected codes, impute 0 usage 
      # in the annual usage gaps. We do this because the absence of data for
      # any particular year between two available years implies that the usage 
      # for that year is 0. However, the plots do not show zero but interpolate
      # a line between the two last available data points, which is misleading.
      # We impute 0 usage in annual usage gaps in the summary plot 
      # when the number of selected codes is =< 500 and in the individual
      # codes plot (which, by default, can only be displayed with =< 500
      # selected codes). This cut-off was selected for efficiency, since if the user
      # selects >500 codes, the imputation will take longer, but the total usage 
      # shown on the summary plot for any year is unlikely to be zero, making
      # imputation redundant.
      if (unique_codes <= 500) {
        df_plot <- complete_usage_gaps_with_zeros(filtered_data())
      } else {
        df_plot <- filtered_data()
      }

      # Handle individual code display based on number of unique codes
      if (input$show_individual_codes & unique_codes <= 500) {
        p <- df_plot |>
          plot_individual()
      } else {
        if (input$show_individual_codes & unique_codes > 500) {
          showNotification(
            "Too many codes to show individually. To show individual code usage reduce to 500 or fewer selected codes.",
            type = "error"
          )
        }

        p <- df_plot |>
          group_by(start_date, end_date) |>
          summarise(total_usage = sum(usage, na.rm = TRUE)) |>
          plot_summary()
      }

      ggplotly(p, tooltip = "text") |>
        plotly::config(displayModeBar = FALSE)
    })
  })

  # PLOT: Sparkline overview
  output$sparkline <- renderPlotly({
    data_spark <- filtered_data() |>
      plot_sparkline()
  })
}
