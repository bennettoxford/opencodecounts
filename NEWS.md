# opencodecounts (development version)

## Codelist checks

- New `check_codelists()` to find old or new SNOMED CT codes with GP usage that are linked to the codes in codelists but not selected
- New `read_codelists()` to read the CSV files from `opensafely codelists update`
- New terminal command `opencodecounts codelists check`
- The Shiny app has a new "Linked codes not selected" tab and a switch to show their usage over time when a SNOMED CT codelist is loaded
- New `check_codelists_dir()` and `check_codelists_url()`, called by `check_codelists()`. `check_codelists_url()` and `opencodecounts codelists check --url` check codelists from their OpenCodelists URLs
- `get_codelist()` errors when a codelist version is not found and accepts version hashes
- `check_codelists()` downloads the SNOMED CT History Substitution Table from a GitHub release, so it does not need a TRUD API key

## Shiny app

- Link to a selection in the app with URL parameters, e.g. `?dataset=snomedct&codelist=opensafely/anxiety-disorders/6aef605a`. The URL updates as the selection changes, so it can be shared (#115)

## Bug fixes

- `get_codelist()` and `read_codelists()` read CSV files with an unnamed index column (#165)

# opencodecounts 0.9.1

## New data

`get_hesapc_*()` functions now includes 2025-26 (1 August 2025 to 31 July 2026).

# opencodecounts 0.9.0

## New data

`get_gp_snomed()` now includes 2025-26 (1 August 2025 to 31 July 2026). The dataset version is now 0.2.0. Use `get_gp_snomed(version = "0.1.0")` for the previous release.

# opencodecounts 0.8.0

## Renamed dataset accessors

`get_*()` functions are renamed to `<source>_<coding system>`, so the name says both where the data comes from and what codes it contains:

| Old name | New name |
|---|---|
| `get_snomed_usage()` | `get_gp_snomed()` |
| `get_icd10_usage()` | `get_hesapc_icd10()` |
| `get_icd10_usage_breakdowns()` | `get_hesapc_icd10_breakdowns()` |
| `get_opcs4_usage()` | `get_hesapc_opcs4()` |
| `get_opcs4_usage_breakdowns()` | `get_hesapc_opcs4_breakdowns()` |

Old names still work but warn once per session. `available_versions()` now takes the new dataset keys (e.g. `"hesapc_icd10"`).

## Messaging

Errors, warnings, and messages now go through [cli](https://cli.r-lib.org/), with classed conditions for catching specific errors (e.g. `opencodecounts_error_dataset_not_found`). The Shiny app is unaffected.

# opencodecounts 0.7.1

* See GitHub releases for changes prior to this file.
