#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#' @noRd
#' @import shiny
#' @import bslib
#' @import bsicons
#' @importFrom tibble as_tibble
#' @importFrom DT DTOutput
#' @importFrom plotly plotlyOutput


app_ui <- function(request) {
  page_sidebar(
    theme = bs_theme(version = 5, bootswatch = "lumen"),
    title = NULL,
    sidebar = sidebar(
      width = "23%",

      # Select data
      card(
        card_header("Select data"),
        radioButtons("dataset",
          tooltip(
            span("Dataset", bs_icon("info-circle")),
            "SNOMED CT (Systematized Nomenclature of Medicine Clinical Terms); ICD-10 (International Classification of Diseases); OPCS-4 Classification of Interventions and Procedures",
            options = list(customClass = "left-align-tooltip")
          ),
          choices = c(
            "SNOMED-CT" = "snomedct",
            "ICD-10" = "icd10",
            "OPCS-4" = "opcs4"
          )
        ),
        uiOutput("dynamic_date_slider")
      ),

      # Select codes
      card(
        card_header("Select codes"),
        navset_tab(
          nav_panel(
            "Search",
            br(),
            selectizeInput(
              "code_specific_search",
              tooltip(
                span(
                  "Specific code",
                  bs_icon("info-circle")
                ),
                "Select specific clinical codes. Start typing to see a selection of available codes.",
                options = list(
                  customClass = "left-align-tooltip"
                )
              ),
              choices = NULL,
              multiple = TRUE,
              options = list(maxOptions = 15)
            ),
            textInput(
              "description_search",
              tooltip(
                span(
                  "Description",
                  bs_icon("info-circle")
                ),
                "Enter search term(s). Multiple terms can be combined by using '|'.",
                options = list(
                  customClass = "left-align-tooltip"
                )
              )
            ),
            conditionalPanel(
              condition = "input.dataset == 'icd10' || input.dataset == 'opcs4'",
              uiOutput("dynamic_code_pattern_input")
            )
          ),
          nav_panel(
            "Load OpenCodelist",
            br(),
            textInput(
              "codelist_slug",
              tooltip(
                span(
                  "Codelist ID / Version Tag",
                  bs_icon("info-circle")
                ),
                "Enter <codelist_id>/<version_id>, e.g., 'opensafely/anxiety-disorders/6aef605a'",
                options = list(
                  customClass = "left-align-tooltip"
                )
              ),
              placeholder = "opensafely/anxiety-disorders/6aef605a",
              NULL
            ),
            actionButton("load_codelist", "Load codelist", class = "btn-outline-primary", style = "width: 100%;")
          )
        ),
        actionButton("reset_search_methods", "Reset code selection", class = "btn-outline-dark")
      )
    ),
    # Main page
    # Value boxes
    layout_columns(
      height = "20%",
      value_box(
        title = "Number of selected codes with usage data",
        value = textOutput("unique_codes"),
        showcase = bs_icon("file-earmark-medical")
      ),
      value_box(
        title = "Total number of recorded events",
        value = textOutput("total_activity"),
        showcase = plotlyOutput("sparkline")
      )
    ),
    # Plots and tables
    navset_card_tab(
      height = "80%",
      nav_panel(
        p(bs_icon("graph-up"), "Trends over time"),
        input_switch(
          "show_individual_codes",
          tooltip(
            span(
              "Show individual codes",
              bs_icon("info-circle")
            ),
            "This is only supported for up to 500 selected codes.",
            placement = "right"
          ),
          value = FALSE
        ),
        plotlyOutput("usage_plot")
      ),
      nav_panel(
        p(bs_icon("file-earmark-spreadsheet"), "Usage table"),
        downloadButton("download_usage_table", "Download CSV"),
        DTOutput("usage_table")
      ),
      nav_panel(
        p(bs_icon("file-earmark-medical"), "Selected codes"),
        downloadButton("download_codes_table", "Download CSV"),
        DTOutput("codes_table")
      )
    ),
    tags$style(HTML("
      .left-align-tooltip .tooltip-inner {
        text-align: left;
        max-width: 500px;
      }
      .card-header {
        font-weight: bold;
      }
      .btn {
        text-transform: none !important;
        font-weight: bold !important;
      }
    "))
  )
}
