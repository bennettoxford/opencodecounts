#' Run `opencodecounts` [Shiny App](https://milanwiedemann.shinyapps.io/opencodecounts/) locally
#'
#' @export
run_app <- function() {
  shiny::addResourcePath(
    "www",
    system.file("shiny/www", package = "opencodecounts")
  )
  
  shiny::shinyApp(
    ui = app_ui,
    server = app_server
  )
}
