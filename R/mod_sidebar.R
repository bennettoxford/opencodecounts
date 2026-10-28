#' Sidebar module: dataset choice, date range, and code selection
#'
#' Owns the "Analyse" controls (dataset radio buttons, dynamic date slider,
#' code search and codelist loading) and the static "More" panel.
#'
#' @param id Module id.
#' @noRd
#' @import shiny
#' @import bslib
mod_sidebar_ui <- function(id) {
  ns <- NS(id)

  sidebar(
    width = "23%",
    navset_card_underline(
      nav_panel(
        "Analyse",
        card(
          card_header("Select data"),
          radioButtons(
            ns("dataset"),
            tooltip_label(
              "Dataset",
              "SNOMED CT (Systematized Nomenclature of Medicine Clinical Terms); ICD-10 (International Classification of Diseases); OPCS-4 Classification of Interventions and Procedures"
            ),
            choices = dataset_choices()
          ),
          uiOutput(ns("dynamic_date_slider"))
        ),
        card(
          card_header("Select codes"),
          navset_tab(
            nav_panel(
              "Search",
              br(),
              selectizeInput(
                ns("code_specific_search"),
                tooltip_label(
                  "Specific code(s)",
                  "Select specific clinical codes. Start typing to see a selection of available codes."
                ),
                choices = NULL,
                multiple = TRUE,
                options = list(maxOptions = 15)
              ),
              textInput(
                ns("description_search"),
                tooltip_label(
                  "Description",
                  "Enter search term(s). Multiple terms can be combined by using '|'."
                )
              ),
              uiOutput(ns("dynamic_code_pattern_input"))
            ),
            nav_panel(
              "Load OpenCodelist",
              br(),
              textInput(
                ns("codelist_url"),
                tooltip_label(
                  "Codelist URL",
                  "Enter codelist URL, e.g., 'https://www.opencodelists.org/codelist/opensafely/anxiety-disorders/6aef605a/'"
                ),
                placeholder = "https://www.opencodelists.org/codelist/opensafely/anxiety-disorders/6aef605a/",
                NULL
              ),
              actionButton(
                ns("load_codelist"),
                "Load codelist",
                class = "btn-outline-primary",
                style = "width: 100%;"
              )
            )
          ),
          actionButton(
            ns("reset_search_methods"),
            "Reset code selection",
            class = "btn-outline-dark"
          )
        )
      ),
      nav_panel(
        "More",
        about_card(),
        guides_card(),
        sources_card()
      )
    )
  )
}

#' Sidebar module server
#'
#' Returns the reactives the rest of the app needs to filter and label data.
#'
#' @param id Module id.
#' @return A list of reactives: `dataset`, `selected_data`, `date_range`,
#'   `search_method`, `codelist`, `code_specific_search`,
#'   `code_pattern_search`, `description_search` (debounced), and `reset`
#'   (the reset button value, for other modules to react to).
#' @noRd
#' @import shiny
#' @import bslib
#' @import bsicons
#' @import dplyr
mod_sidebar_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Reactive values for search method (1) none, (2) code/desc or (3) codelist) and codelist data
    rv_search_method <- reactiveVal("none")
    rv_codelist <- reactiveVal(NULL)

    # Reset search inputs when dataset changes
    observe({
      # Stops the old date_range briefly pairing with the new dataset
      # (wrong total flashes) while the slider rebuilds for the new range.
      freezeReactiveValue(input, "date_range")
      rv_search_method("none")
      updateSelectizeInput(
        session,
        "code_specific_search",
        selected = character(0)
      )
      updateSelectizeInput(
        session,
        "code_pattern_search",
        selected = character(0)
      )
      updateTextInput(session, "description_search", value = "")
    }) |>
      bindEvent(input$dataset)

    # Selected code usage dataset
    selected_data <- reactive({
      dataset_cfg <- get_dataset_by_id(input$dataset)

      get_app_dataset(dataset_cfg$dataset) |>
        select(
          start_date,
          end_date,
          code = all_of(dataset_cfg$code_column),
          description = all_of(dataset_cfg$description_column),
          usage,
          # Only SNOMED CT usage data has these, for the linked codes timeline
          any_of(c("active_at_start", "active_at_end"))
        )
    })

    output$dynamic_date_slider <- renderUI({
      req(selected_data())

      available_end_dates <- sort(unique(selected_data()$end_date))

      sliderInput(
        session$ns("date_range"),
        label = tooltip_label(
          "Date range",
          "Filter available data by selecting end dates of yearly reporting intervals"
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
      dataset_cfg <- get_dataset_by_id(input$dataset)
      req(dataset_cfg$has_code_pattern_search)

      textInput(
        session$ns("code_pattern_search"),
        tooltip_label(
          dataset_cfg$code_pattern_label,
          "Enter the beginning of a code to search by chapter or subchapter. Multiple chapters can be combined using '|'."
        )
      )
    })

    # Update code search choices depending on selected dataset
    observe({
      updateSelectizeInput(
        session,
        "code_specific_search",
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
                paste0(
                  "Successfully loaded ",
                  codelist_s7@coding_system,
                  " codelist."
                ),
                type = "default"
              )

              # Store the codelist data
              rv_codelist(
                codelist_s7 |>
                  tibble::as_tibble() |>
                  dplyr::select(1:2)
              )

              # Set filtering method to codelist
              rv_search_method("codelist")

              # Reset search inputs
              updateSelectizeInput(
                session,
                "code_specific_search",
                selected = character(0)
              )
              updateTextInput(session, "code_pattern_search", value = "")
              updateTextInput(session, "description_search", value = "")
            } else {
              showNotification(
                paste0(
                  "Loaded codelist (",
                  codelist_s7@coding_system,
                  ") does not match selected data (",
                  input$dataset,
                  ")."
                ),
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
      updateSelectizeInput(
        session,
        "code_specific_search",
        selected = character(0)
      )
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
      if (
        rv_search_method() == "codelist" &&
          (is.null(input$code_specific_search) ||
            length(input$code_specific_search) == 0) &&
          (is.null(input$code_pattern_search) ||
            input$code_pattern_search == "") &&
          (is.null(description_search_debounced()) ||
            description_search_debounced() == "")
      ) {
        return()
      }

      # If any search input is used, switch to "search"
      if (
        !is.null(input$code_specific_search) &&
          length(input$code_specific_search) > 0 ||
          !is.null(input$code_pattern_search) &&
            input$code_pattern_search != "" ||
          !is.null(description_search_debounced()) &&
            description_search_debounced() != ""
      ) {
        rv_search_method("search")
      } else {
        rv_search_method("none")
      }
    }) |>
      bindEvent(
        input$code_specific_search,
        input$code_pattern_search,
        description_search_debounced()
      )

    list(
      dataset = reactive(input$dataset),
      selected_data = selected_data,
      date_range = reactive(input$date_range),
      search_method = rv_search_method,
      codelist = rv_codelist,
      code_specific_search = reactive(input$code_specific_search),
      code_pattern_search = reactive(input$code_pattern_search),
      description_search = description_search_debounced,
      reset = reactive(input$reset_search_methods)
    )
  })
}
