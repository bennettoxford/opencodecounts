# Helper function to name the code column of a codelist `code`

Drops unnamed columns and renames the first column to `code` if there is
no `code` column.

## Usage

``` r
rename_code_column(codelist)
```

## Arguments

- codelist:

  Data frame read from a codelist CSV file

## Value

`codelist` with a `code` column
