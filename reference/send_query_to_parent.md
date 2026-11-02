# Send the URL query to the page that embeds the app

When the app is in an iframe, e.g. on the pkgdown site, the browser
shows the URL of the page around it. The script in `app_ui()` passes the
query on to that page, which can then show it.

## Usage

``` r
send_query_to_parent(session, query)
```

## Arguments

- session:

  Shiny session

- query:

  String, as returned by
  [`format_url_params()`](https://bennettoxford.github.io/opencodecounts/reference/format_url_params.md)
