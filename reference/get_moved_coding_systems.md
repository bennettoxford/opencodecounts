# Helper function to get the coding system of codelist versions that moved or were renamed on OpenCodelists

Version hashes are unique across OpenCodelists and do not change when a
codelist moves or is renamed, so versions are matched on their hash.
Versions referred to by tag are not matched.

## Usage

``` r
get_moved_coding_systems(codelist_ids)
```

## Arguments

- codelist_ids:

  Character vector of codelist slugs including the version, not found
  under their owner

## Value

Character vector of coding system IDs, `NA` for codelists not found by
hash
