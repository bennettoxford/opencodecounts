#' Run `opencodecounts` [Shiny App](https://milanwiedemann.shinyapps.io/opencodecounts/) locally
#'
#' @export
run_app <- function() {
  shiny::shinyApp(
    ui = app_ui,
    server = app_server,
  )
}
