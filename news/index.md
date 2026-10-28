# Changelog

## opencodecounts (development version)

### Codelist checks

- New
  [`check_codelists()`](https://bennettoxford.github.io/opencodecounts/reference/check_codelists.md)
  to find old or new SNOMED CT codes with GP usage that may be missing
  from the codelists in `codelists/codelists.txt`
- New
  [`read_codelists()`](https://bennettoxford.github.io/opencodecounts/reference/read_codelists.md)
  to read the CSV files from `opensafely codelists update`
- New terminal command `opencodecounts codelists check`
- The Shiny app has a new “Linked codes not included” tab and a switch
  to show their usage over time when a SNOMED CT codelist is loaded
- New
  [`check_codelists_dir()`](https://bennettoxford.github.io/opencodecounts/reference/check_codelists.md)
  and
  [`check_codelists_url()`](https://bennettoxford.github.io/opencodecounts/reference/check_codelists.md),
  called by
  [`check_codelists()`](https://bennettoxford.github.io/opencodecounts/reference/check_codelists.md).
  [`check_codelists_url()`](https://bennettoxford.github.io/opencodecounts/reference/check_codelists.md)
  and `opencodecounts codelists check --url` check codelists from their
  OpenCodelists URLs
- [`get_codelist()`](https://bennettoxford.github.io/opencodecounts/reference/get_codelist.md)
  errors when a codelist version is not found and accepts version hashes
- [`check_codelists()`](https://bennettoxford.github.io/opencodecounts/reference/check_codelists.md)
  downloads the SNOMED CT History Substitution Table from a GitHub
  release, so it does not need a TRUD API key

### Bug fixes

- [`get_codelist()`](https://bennettoxford.github.io/opencodecounts/reference/get_codelist.md)
  and
  [`read_codelists()`](https://bennettoxford.github.io/opencodecounts/reference/read_codelists.md)
  read CSV files with an unnamed index column
  ([\#165](https://github.com/bennettoxford/opencodecounts/issues/165))

## opencodecounts 0.9.1

### New data

`get_hesapc_*()` functions now includes 2025-26 (1 August 2025 to 31
July 2026).

## opencodecounts 0.9.0

### New data

[`get_gp_snomed()`](https://bennettoxford.github.io/opencodecounts/reference/get_gp_snomed.md)
now includes 2025-26 (1 August 2025 to 31 July 2026). The dataset
version is now 0.2.0. Use `get_gp_snomed(version = "0.1.0")` for the
previous release.

## opencodecounts 0.8.0

### Renamed dataset accessors

`get_*()` functions are renamed to `<source>_<coding system>`, so the
name says both where the data comes from and what codes it contains:

| Old name | New name |
|----|----|
| [`get_snomed_usage()`](https://bennettoxford.github.io/opencodecounts/reference/get_snomed_usage.md) | [`get_gp_snomed()`](https://bennettoxford.github.io/opencodecounts/reference/get_gp_snomed.md) |
| [`get_icd10_usage()`](https://bennettoxford.github.io/opencodecounts/reference/get_icd10_usage.md) | [`get_hesapc_icd10()`](https://bennettoxford.github.io/opencodecounts/reference/get_hesapc_icd10.md) |
| [`get_icd10_usage_breakdowns()`](https://bennettoxford.github.io/opencodecounts/reference/get_icd10_usage_breakdowns.md) | [`get_hesapc_icd10_breakdowns()`](https://bennettoxford.github.io/opencodecounts/reference/get_hesapc_icd10_breakdowns.md) |
| [`get_opcs4_usage()`](https://bennettoxford.github.io/opencodecounts/reference/get_opcs4_usage.md) | [`get_hesapc_opcs4()`](https://bennettoxford.github.io/opencodecounts/reference/get_hesapc_opcs4.md) |
| [`get_opcs4_usage_breakdowns()`](https://bennettoxford.github.io/opencodecounts/reference/get_opcs4_usage_breakdowns.md) | [`get_hesapc_opcs4_breakdowns()`](https://bennettoxford.github.io/opencodecounts/reference/get_hesapc_opcs4_breakdowns.md) |

Old names still work but warn once per session.
[`available_versions()`](https://bennettoxford.github.io/opencodecounts/reference/available_versions.md)
now takes the new dataset keys (e.g. `"hesapc_icd10"`).

### Messaging

Errors, warnings, and messages now go through
[cli](https://cli.r-lib.org/), with classed conditions for catching
specific errors (e.g. `opencodecounts_error_dataset_not_found`). The
Shiny app is unaffected.

## opencodecounts 0.7.1

- See GitHub releases for changes prior to this file.
