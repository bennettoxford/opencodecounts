#' Helper function to create code usage table
#' @importFrom DT datatable formatPercentage formatRound formatStyle styleEqual
#' @keywords internal
datatable_usage <- function(data) {
  datatable(
    data,
    colnames = c("Code", "Description", "Usage", "Percentage"),
    rownames = FALSE,
    options = list(
      columnDefs = list(
        list(width = "60px", targets = 0),
        list(width = "350px", targets = 1),
        list(width = "60px", targets = 2),
        list(width = "40px", targets = 3)
      ),
      pageLength = 10,
      scrollY = TRUE,
      scrollX = FALSE,
      searching = TRUE
    ),
    width = "100%",
    fillContainer = TRUE
  ) |>
    formatRound("total_usage", digits = 0) |>
    formatPercentage(
      "total_pct",
      digits = 1
    )
}


#' Helper function to create codelist / selected codes table
#' @importFrom DT datatable
#' @keywords internal
datatable_codelist <- function(data, data_desc) {
  datatable(
    data,
    colnames = c("Code", "Description", "Usage data"),
    rownames = FALSE,
    options = list(
      columnDefs = list(
        list(width = "50px", targets = 0),
        list(width = "400px", targets = 1),
        list(width = "100px", targets = 2)
      ),
      language = list(
        emptyTable = "No codes selected, please select codes or load a codelist."
      ),
      order = list(list(2, "desc")),
      pageLength = 10,
      scrollY = FALSE,
      scrollX = FALSE,
      searching = TRUE
    ),
    width = "100%",
    fillContainer = TRUE
  ) |>
    formatStyle(
      "usage_data_available",
      color = styleEqual(
        c("Usage data available", "No usage data reported"),
        c("#35B779FF", "#ED6925FF")
      ),
      fontWeight = "bold"
    )
}
