#' Run `opencodes` [Shiny App](https://milanwiedemann.shinyapps.io/opencodes/) locally
#'
#' @export
run_app <- function() {
  shiny::shinyApp(
    ui = app_ui,
    server = app_server,
  )
}
