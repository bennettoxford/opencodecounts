# Check codelists for inactive SNOMED CT codes with usage

**\[experimental\]**

Finds old and new SNOMED CT codes with recorded GP usage that may be
missing from codelists, for review.

- `check_codelists_dir()` checks the codelists in `codelists.txt` in an
  OpenSAFELY study, using the CSV files from
  `opensafely codelists update`.

- `check_codelists_url()` checks codelists from their URLs on
  [OpenCodelists](https://www.opencodelists.org).

- `check_codelists()` calls `check_codelists_url()` when `url` is set
  and `check_codelists_dir()` otherwise.

## Usage

``` r
check_codelists(dir = "codelists", url = NULL, ...)

check_codelists_dir(
  dir = "codelists",
  start_date = NULL,
  end_date = NULL,
  data_version = NULL,
  history_version = NULL
)

check_codelists_url(
  url,
  start_date = NULL,
  end_date = NULL,
  data_version = NULL,
  history_version = NULL
)
```

## Arguments

- dir:

  String, path to the codelists directory. Default `"codelists"`.
  Ignored when `url` is set.

- url:

  Character vector of codelist URLs on OpenCodelists. Default `NULL`
  uses `dir`.

- ...:

  Arguments passed to `check_codelists_dir()` or
  `check_codelists_url()`.

- start_date, end_date:

  Dates or strings, first and last day of the reporting periods to use.
  Default `NULL` uses all periods.

- data_version:

  String, version of the
  [`get_gp_snomed()`](https://bennettoxford.github.io/opencodecounts/reference/get_gp_snomed.md)
  data. Default `NULL` uses the latest.

- history_version:

  String, version of the SNOMED CT History Substitution Table. Default
  `NULL` uses the latest.

## Value

A `CodelistCheck` object with `@summary`, one row per codelist, and
`@results`, one row per old and new code pair.
