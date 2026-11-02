# URL parameters of the Shiny app

Each entry parses one query string parameter into a clean value, or
`NULL` to drop a value that is not valid, and formats a value back into
the URL. To add a parameter, add an entry here and use its value where
the input is created in `mod_sidebar_ui()` or `mod_sidebar_server()`.

## Usage

``` r
url_param_specs()
```

## Value

A named list of parameter specs, in the order they appear in URLs
