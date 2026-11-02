# Parse the app's URL query string

Parse the app's URL query string

## Usage

``` r
parse_url_params(query_string)
```

## Arguments

- query_string:

  String, e.g. `"?dataset=snomedct&codes=123,456"`

## Value

A named list of the valid parameters. Unknown parameters, empty values
and values that are not valid are dropped.
