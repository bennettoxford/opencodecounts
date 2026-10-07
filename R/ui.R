#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#' @noRd
#' @import shiny
#' @import bslib
app_ui <- function(request) {
  tagList(
    page_sidebar(
      theme = bs_theme(version = 5, bootswatch = "lumen"),
      title = NULL,
      sidebar = mod_sidebar_ui("sidebar"),
      # Main page
      # Value boxes
      mod_value_boxes_ui("value_boxes"),
      # Plots and tables
      navset_card_tab(
        height = "80%",
        mod_trends_ui("trends"),
        mod_usage_table_ui("usage_table"),
        mod_codes_table_ui("codes_table")
      )
    ),
    footer_ui(),

    # CSS styles
    tags$style(HTML(
      "
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
    "
    ))
  )
}
