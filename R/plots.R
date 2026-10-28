#' Helper function to plot code usage summary
#' @importFrom ggplot2 ggplot aes geom_line geom_point scale_x_date scale_y_continuous scale_colour_manual scale_shape_manual labs theme theme_classic element_text
#' @importFrom lubridate month year
#' @importFrom scales label_date_short label_comma comma
#' @keywords internal
plot_summary <- function(data) {
  scale_x_date_breaks <- unique(data$end_date)

  # A `series` column adds a line for the linked codes not included
  has_series <- "series" %in% names(data)
  if (!has_series) {
    data$series <- "Selected codes"
  }

  ggplot(
    data,
    aes(x = end_date, y = total_usage, colour = series, shape = series)
  ) +
    geom_line(alpha = .4) +
    geom_point(
      size = 2,
      aes(
        text = paste0(
          if (has_series) paste0("<b>Codes:</b> ", series, "<br>"),
          "<b>Timeframe:</b> ",
          lubridate::month(start_date, label = TRUE),
          " ",
          lubridate::year(start_date),
          " to ",
          lubridate::month(end_date, label = TRUE),
          " ",
          lubridate::year(end_date),
          "<br>",
          "<b>Code usage:</b> ",
          scales::comma(total_usage)
        )
      )
    ) +
    scale_x_date(
      breaks = scale_x_date_breaks,
      labels = scales::label_date("%b\n%Y")
    ) +
    scale_y_continuous(
      limits = c(0, NA),
      labels = scales::label_number(accuracy = 1)
    ) +
    scale_colour_manual(
      values = c(
        "Selected codes" = "#239b89ff",
        "Linked codes not included" = "#ED6925FF"
      )
    ) +
    scale_series_shape() +
    labs(
      x = "End date of yearly time interval",
      y = "Recorded events",
      colour = NULL,
      shape = NULL
    ) +
    theme_classic() +
    theme(
      text = element_text(size = 14),
      legend.position = if (has_series) "top" else "none"
    )
}

#' Helper function to plot individual code usage
#' @importFrom ggplot2 ggplot aes geom_line geom_point scale_x_date scale_y_continuous labs theme theme_classic element_text
#' @importFrom dplyr group_by mutate ungroup
#' @importFrom lubridate month year
#' @importFrom scales label_date_short label_comma comma
#' @keywords internal
plot_individual <- function(data) {
  scale_x_date_breaks <- unique(data$end_date)

  # A `series` column adds the linked codes not included, as triangles
  has_series <- "series" %in% names(data)
  if (!has_series) {
    data$series <- "Selected codes"
  }

  data <- data |>
    group_by(start_date, end_date, series) |>
    mutate(annual_proportion = usage / sum(usage, na.rm = TRUE)) |>
    ungroup()

  # Linked codes use the rocket palette so they stand out from the selected
  # codes. Its darkest and lightest ends are left out to keep them visible.
  code_colours <- c(
    series_colours(data, "Selected codes", scales::viridis_pal()),
    series_colours(
      data,
      "Linked codes not included",
      scales::viridis_pal(begin = 0.35, end = 0.85, option = "rocket")
    )
  )

  ggplot(
    data,
    aes(
      x = end_date,
      y = usage,
      colour = code,
      shape = series
    )
  ) +
    geom_line(alpha = .4) +
    geom_point(
      size = 2,
      aes(
        text = paste0(
          "<b>Timeframe:</b> ",
          lubridate::month(start_date, label = TRUE),
          " ",
          lubridate::year(start_date),
          " to ",
          lubridate::month(end_date, label = TRUE),
          " ",
          lubridate::year(end_date),
          "<br>",
          if (has_series) paste0("<b>Codes:</b> ", series, "<br>"),
          "<b>Code:</b> ",
          code,
          "<br>",
          "<b>Description:</b> ",
          description,
          "<br>",
          "<b>Code usage:</b> ",
          scales::comma(usage),
          "<br>",
          "<b>Proportion of annual usage: </b>",
          scales::percent(annual_proportion, accuracy = 0.01)
        )
      )
    ) +
    scale_x_date(
      breaks = scale_x_date_breaks,
      labels = scales::label_date("%b\n%Y")
    ) +
    scale_y_continuous(
      limits = c(0, NA),
      labels = scales::label_number(accuracy = 1)
    ) +
    scale_colour_manual(values = code_colours) +
    scale_series_shape() +
    labs(x = "End date of yearly time interval", y = "Recorded events") +
    theme_classic() +
    theme(
      text = element_text(size = 14),
      legend.position = "none"
    )
}

#' Colours for the codes of one plot series
#'
#' @param data Usage data with `code` and `series` columns
#' @param series String, the series to colour
#' @param palette Function returning `n` colours
#'
#' @return Named character vector of colours, one per code
#' @noRd
series_colours <- function(data, series, palette) {
  codes <- sort(unique(data$code[data$series == series]))
  stats::setNames(palette(length(codes)), codes)
}

#' Shapes for the selected codes and the linked codes not included
#'
#' Linked codes are always triangles, in the summary and individual plots.
#' @noRd
scale_series_shape <- function() {
  ggplot2::scale_shape_manual(
    values = c("Selected codes" = 16, "Linked codes not included" = 17)
  )
}

#' Helper function to plot sparkline
#' @importFrom dplyr group_by summarise
#' @importFrom plotly plot_ly add_lines layout config
#' @keywords internal
plot_sparkline <- function(data) {
  data_spark <- data |>
    group_by(end_date) |>
    summarise(total_usage = sum(usage, na.rm = TRUE))

  plot_ly(data_spark, hoverinfo = "none") |>
    add_lines(
      x = ~end_date,
      y = ~total_usage,
      color = I("black"),
      span = I(1),
      fill = "tozeroy",
      alpha = 0.2
    ) |>
    layout(
      xaxis = list(visible = F, showgrid = F, title = ""),
      yaxis = list(visible = F, showgrid = F, title = ""),
      hovermode = "x",
      margin = list(t = 0, r = 0, l = 0, b = 0),
      font = list(color = "white"),
      paper_bgcolor = "transparent",
      plot_bgcolor = "transparent"
    ) |>
    config(displayModeBar = FALSE) |>
    layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
}

#' Plot the yearly usage of linked code pairs as a timeline
#'
#' One tile per code and reporting period, coloured by whether the code was
#' active at the start and end of the period.
#'
#' @param data Output of `linked_timeline_data()`
#' @keywords internal
plot_linked_timeline <- function(data) {
  status_colours <- c(
    "Active all year" = "#5DC863",
    "Status changed during year" = "gray80",
    "Inactive all year" = "#FDE725"
  )

  data |>
    dplyr::mutate(
      status = dplyr::case_when(
        active_at_start & active_at_end ~ "Active all year",
        !active_at_start & !active_at_end ~ "Inactive all year",
        .default = "Status changed during year"
      ),
      usage_label = scales::label_number(
        accuracy = 1,
        scale_cut = c(0, K = 1e3),
        big.mark = ","
      )(usage),
      share = round(100 * share),
      share_label = dplyr::if_else(share == 0, "<1%", paste0(share, "%"))
    ) |>
    ggplot(aes(
      x = start_date + (end_date - start_date) / 2,
      y = code_label,
      width = as.numeric(end_date - start_date),
      fill = status
    )) +
    ggplot2::geom_tile(colour = "white", height = 0.8) +
    # Right align the labels with some space before the end of each period
    ggplot2::geom_text(
      aes(x = end_date - 45, label = usage_label),
      size = 3.5,
      hjust = 1,
      vjust = -0.2
    ) +
    ggplot2::geom_text(
      aes(x = end_date - 45, label = share_label),
      size = 3.5,
      hjust = 1,
      vjust = 1.2,
      colour = "gray30"
    ) +
    scale_x_date(
      breaks = unique(data$end_date),
      labels = scales::label_date("%b\n%Y")
    ) +
    ggplot2::scale_fill_manual(
      values = status_colours,
      breaks = names(status_colours)
    ) +
    ggplot2::facet_wrap(
      ggplot2::vars(panel),
      ncol = 1,
      scales = "free_y",
      axes = "all_x",
      axis.labels = "margins",
      labeller = ggplot2::label_bquote(
        bold("SNOMED CT description:") ~ .(as.character(panel))
      )
    ) +
    labs(
      x = "End date of yearly reporting period",
      y = NULL,
      fill = NULL
    ) +
    theme_classic(base_size = 15) +
    theme(
      panel.grid.major.x = ggplot2::element_line(
        colour = "gray90",
        linetype = "dotted"
      ),
      strip.background = ggplot2::element_blank(),
      strip.text = element_text(
        hjust = 0,
        margin = ggplot2::margin(2, 0, 2, 0)
      ),
      legend.position = "bottom"
    )
}
