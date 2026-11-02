# Select the end dates of a date range from reporting years

Select the end dates of a date range from reporting years

## Usage

``` r
select_end_dates(end_dates, from = NULL, to = NULL)
```

## Arguments

- end_dates:

  Date vector of the available end dates

- from, to:

  Integer years in which the first and last reporting year end, as
  returned by
  [`parse_reporting_year()`](https://bennettoxford.github.io/opencodecounts/reference/parse_reporting_year.md),
  or `NULL` for the first or last year in the data

## Value

Date vector of length 2, the first and last end date with a year from
`from` to `to`, or the full range when none match
