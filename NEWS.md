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
