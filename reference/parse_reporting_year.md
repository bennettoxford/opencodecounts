# Parse a reporting year URL parameter

Reporting years are labelled as in NHS Digital publications, e.g.
`"2014-15"`. The months differ by dataset. GP data runs from August to
July and hospital data from April to March.

## Usage

``` r
parse_reporting_year(x)
```

## Arguments

- x:

  String, e.g. `"2014-15"`

## Value

Integer year in which the reporting year ends, e.g. `2015L`, or `NULL`
when `x` is not a reporting year
