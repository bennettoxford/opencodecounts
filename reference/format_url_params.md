# Format app state as a URL query string

Format app state as a URL query string

## Usage

``` r
format_url_params(params)
```

## Arguments

- params:

  Named list of parameter values, as returned by
  [`parse_url_params()`](https://bennettoxford.github.io/opencodecounts/reference/parse_url_params.md)

## Value

String starting with `"?"`, or `""` when no parameter has a value
