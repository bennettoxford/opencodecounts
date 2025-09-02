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
  tagList(
    page_sidebar(
      theme = bs_theme(version = 5, bootswatch = "lumen"),
      title = NULL,
      sidebar = sidebar(
        width = "23%",
        navset_card_tab(
          nav_panel(
            p("Analyse"),
            card(
              card_header("Select data"),
              radioButtons("dataset",
                tooltip(
                  span("Dataset", bs_icon("info-circle")),
                  "SNOMED CT (Systematized Nomenclature of Medicine Clinical Terms); ICD-10 (International Classification of Diseases); OPCS-4 Classification of Interventions and Procedures",
                  options = list(customClass = "left-align-tooltip")
                ),
                choices = c(
                  "Primary care (SNOMED CT)" = "snomedct",
                  "Secondary care (ICD-10)" = "icd10",
                  "Secondary care (OPCS-4)" = "opcs4"
                )
              ),
              uiOutput("dynamic_date_slider")
            ),
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
                    "codelist_url",
                    tooltip(
                      span(
                        "Codelist URL",
                        bs_icon("info-circle")
                      ),
                      "Enter codelist URL, e.g., 'https://www.opencodelists.org/codelist/opensafely/anxiety-disorders/6aef605a/'",
                      options = list(
                        customClass = "left-align-tooltip"
                      )
                    ),
                    placeholder = "https://www.opencodelists.org/codelist/opensafely/anxiety-disorders/6aef605a/",
                    NULL
                  ),
                  actionButton("load_codelist", "Load codelist", class = "btn-outline-primary", style = "width: 100%;")
                )
              ),
              actionButton("reset_search_methods", "Reset code selection", class = "btn-outline-dark")
            )
          ),
          nav_panel(
            p("More"),
            card(
              card_header("About"),
              p(
                "This Shiny app was developed to support healthcare researchers in exploring clinical coding data.",
                "It builds on methods described in Tamborska et al. (",
                a("2025", href = "https://www.medrxiv.org/", target = "_blank"),
                ") and the underlying opencodecounts R package.",
              ),
              p(
                "This project was designed and built by the Bennett Institute for Applied Data Science, Department of Primary Care Health Sciences, University of Oxford.",
                "For fruther documentation and support, visit ",
                a("https://bennettoxford.github.io/opencodecounts/", href = "https://bennettoxford.github.io/opencodecounts/", target = "_blank"),
                " or contact the R package maintainer."
              )
            ),
            card(
              card_header("How-to guides"),
              tags$ul(
                tags$li(
                  a("How to use the Shiny app",
                    href = "https://bennettoxford.github.io/opencodecounts/articles/how-to-use-shiny-app.html",
                    target = "_blank"
                  )
                ),
                tags$li(
                  a("How to use the R package",
                    href = "https://bennettoxford.github.io/opencodecounts/articles/how-to-use-R-pkg.html",
                    target = "_blank"
                  )
                ),
                tags$li(
                  a("Available datasets in opencodecounts",
                    href = "https://bennettoxford.github.io/opencodecounts/articles/available-datasets.html",
                    target = "_blank"
                  )
                ),
                tags$li(
                  a("Extract semantic tags from SNOMED CT descriptions",
                    href = "https://bennettoxford.github.io/opencodecounts/articles/extract-snomedct-sem-tag.html",
                    target = "_blank"
                  )
                )
              )
            ),
            card(
              card_header("FAQ")
            )
          )
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
      )
    ),
    div(
      class = "footer",
      div(
        class = "container-fluid",
        div(
          class = "row justify-content-center align-items-center",
          div(
            class = "col-auto",
            a(
              href = "https://www.bennett.ox.ac.uk/",
              target = "_blank",
              img(
                src = "www/bennett-brand-white.png",
                height = "60px",
                alt = "Bennett Institute",
                style = "margin: 0 20px;"
              )
            )
          ),
          div(
            class = "col-auto",
            a(
              href = "https://www.phc.ox.ac.uk/",
              target = "_blank",
              img(
                src = "www/ndpchs-logo.png",
                height = "60px",
                alt = "Nuffield Department of Primary Care Health Sciences",
                style = "margin: 0 20px;"
              )
            )
          ),
          div(
            class = "col-auto",
            a(
              href = "https://www.ox.ac.uk/",
              target = "_blank",
              img(
                src = "www/university-oxford-logo.png",
                height = "60px",
                alt = "University of Oxford",
                style = "margin: 0 20px;"
              )
            )
          )
        )
      )
    ),

    # CSS styles
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
      .footer {
        background-color: #ffffff;
        border-top: 1px solid #dee2e6;
        padding: 15px 0;
        margin-top: 0;
        position: relative;
        bottom: 0;
        width: 100%;
      }
      .footer img {
        max-height: 60px;
        width: auto;
      }
      .footer a {
        text-decoration: none;
        transition: opacity 0.3s ease;
      }
      .footer a:hover {
        opacity: 0.8;
      }
      .footer p a {
        text-decoration: underline !important;
        transition: opacity 0.3s ease;
      }
      .footer p a:hover {
        opacity: 0.8;
        text-decoration: underline !important;
      }

      /* Ensure footer stays at bottom */
      html, body {
        height: 100%;
      }

      body {
        display: flex;
        flex-direction: column;
      }

      .bslib-page-sidebar {
        flex: 1;
      }
    "))
  )
}
