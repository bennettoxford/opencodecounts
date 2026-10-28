# Helper function to get the coding system and slug of every codelist version of an organisation or user on OpenCodelists

Helper function to get the coding system and slug of every codelist
version of an organisation or user on OpenCodelists

## Usage

``` r
get_codelist_versions(codelist_org)
```

## Arguments

- codelist_org:

  String, organisation from
  [`get_codelist_organisation()`](https://bennettoxford.github.io/opencodecounts/reference/get_codelist_organisation.md)

## Value

Tibble with columns `coding_system_id`, `full_slug` and `hash`. Tagged
versions are listed by tag and by hash.
