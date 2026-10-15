# Get raw NHS Digital source locations for a dataset

Reads `inst/config/raw_<publication>.yml`, named by this dataset's
`publication` field in `tidy_data_sources.yml`, and returns its
`periods` list. To add a new year, add one entry to that file's
`periods:` map.

## Usage

``` r
get_raw_source_periods(dataset)
```

## Arguments

- dataset:

  String, dataset name, e.g. "gp_snomed", "hesapc_icd10"

## Value

A named list, one entry per period. For `gp_snomed` each entry is a url
string; for the others, a list with `url` plus fields like `sheet`,
`range` or `usage_col`.

## Details

A publication can back more than one dataset (e.g. `hesapc_icd10` and
`hesapc_icd10_breakdowns` read the same xlsx file, different sheet
region); `variant` picks which sub-fields to merge in.
