# Read codelists listed in codelists.txt

Reads the codelist CSV files from `opensafely codelists update`.

## Usage

``` r
read_codelists(dir = "codelists")
```

## Arguments

- dir:

  String, path to the codelists directory. Default `"codelists"`.

## Value

Named list of tibbles, one per codelist, each with a `code` column.
