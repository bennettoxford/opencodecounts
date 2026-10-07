#' Input label with an info tooltip
#'
#' Wraps the repeated `tooltip(span(<label>, bs_icon("info-circle")), <text>)`
#' pattern used for input labels across the app.
#'
#' @param label Character, the visible label text.
#' @param text Character, the tooltip body.
#' @param ... Passed on to [bslib::tooltip()] (e.g. `placement`).
#' @keywords internal
tooltip_label <- function(
  label,
  text,
  ...,
  options = list(customClass = "left-align-tooltip")
) {
  bslib::tooltip(
    shiny::span(label, bsicons::bs_icon("info-circle")),
    text,
    ...,
    options = options
  )
}

#' Link that opens in a new tab
#' @keywords internal
external_link <- function(text, href) {
  shiny::a(text, href = href, target = "_blank")
}

#' Footer logo wrapped in a link
#' @keywords internal
footer_logo <- function(href, src, alt, height = "60px") {
  shiny::div(
    class = "col-auto",
    shiny::a(
      href = href,
      target = "_blank",
      shiny::img(
        src = src,
        height = height,
        alt = alt,
        style = "margin: 0 20px;"
      )
    )
  )
}

#' Institutional logo footer
#' @keywords internal
footer_ui <- function() {
  shiny::div(
    class = "footer",
    shiny::div(
      class = "container-fluid",
      shiny::div(
        class = "row justify-content-center align-items-center",
        footer_logo(
          href = "https://bennettoxford.github.io/opencodecounts/",
          src = "www/logo-wordmark.png",
          alt = "opencodecounts website",
          height = "45px"
        ),
        footer_logo(
          href = "https://www.bennett.ox.ac.uk/",
          src = "www/bennett-brand-white.png",
          alt = "Bennett Institute"
        ),
        footer_logo(
          href = "https://www.phc.ox.ac.uk/",
          src = "www/ndpchs-logo.png",
          alt = "Nuffield Department of Primary Care Health Sciences"
        ),
        footer_logo(
          href = "https://www.ox.ac.uk/",
          src = "www/university-oxford-logo.png",
          alt = "University of Oxford"
        )
      )
    )
  )
}

#' Static cards for the "More" sidebar panel
#' @keywords internal
about_card <- function() {
  bslib::card(
    bslib::card_header("About"),
    shiny::p(
      "This Shiny app was developed to support healthcare researchers in exploring clinical coding data in England. ",
      "This project was designed and built by the ",
      external_link(
        "Bennett Institute for Applied Data Science",
        href = "https://www.bennett.ox.ac.uk/"
      ),
      ". For fruther documentation and support, visit the ",
      external_link(
        "opencodecounts",
        href = "https://bennettoxford.github.io/opencodecounts/"
      ),
      "R package website or contact us at ",
      shiny::a(
        "bennett@phc.ox.ac.uk",
        href = "mailto:bennett@phc.ox.ac.uk?subject=opencodecounts%20Feedback"
      ),
      "."
    )
  )
}

#' @rdname about_card
#' @keywords internal
guides_card <- function() {
  bslib::card(
    bslib::card_header("How-to guides"),
    shiny::p("Here is a list of our how-to guides:"),
    shiny::tags$ul(
      shiny::tags$li(
        external_link(
          "How to use the Shiny app",
          href = "https://bennettoxford.github.io/opencodecounts/articles/how-to-use-shiny-app.html"
        )
      ),
      shiny::tags$li(
        external_link(
          "How to use the R package",
          href = "https://bennettoxford.github.io/opencodecounts/articles/how-to-use-R-pkg.html"
        )
      ),
      shiny::tags$li(
        external_link(
          "How to extract semantic tags from SNOMED CT descriptions",
          href = "https://bennettoxford.github.io/opencodecounts/articles/extract-snomedct-sem-tag.html"
        )
      ),
      shiny::tags$li(
        external_link(
          "Learn about the available datasets",
          href = "https://bennettoxford.github.io/opencodecounts/articles/available-datasets.html"
        )
      )
    )
  )
}

#' @rdname about_card
#' @keywords internal
sources_card <- function() {
  sources <- unique(lapply(load_dataset_registry(), function(d) {
    list(d$source_label, d$source_url)
  }))

  bslib::card(
    bslib::card_header("Data sources"),
    shiny::p("The original data is available from NHS Digital at:"),
    shiny::tags$ul(
      lapply(sources, function(source) {
        shiny::tags$li(external_link(source[[1]], href = source[[2]]))
      })
    )
  )
}
