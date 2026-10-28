# How to check codelists for inactive SNOMED CT codes with usage

SNOMED CT codes can become inactive and be replaced by new codes. GP
records may still use the old codes, so a codelist that includes only
one code of a pair may miss some usage.
[`check_codelists()`](https://bennettoxford.github.io/opencodecounts/reference/check_codelists.md)
finds these pairs for review and shows their usage in English general
practice.

## Set up your codelists

The check uses the same layout as OpenSAFELY studies. List your
codelists in `codelists/codelists.txt`, one OpenCodelists ID per line:

    user/Lola_O/proms-dep/62e34278
    user/Lola_O/proms-anx/0314a9a9

Then download the CSV files with the [OpenSAFELY
CLI](https://docs.opensafely.org/opensafely-cli/):

``` bash
opensafely codelists update
```

Only codelists with the SNOMED CT coding system are checked. Others,
such as dm+d codelists, are skipped.

If a codelist has moved to a new owner on OpenCodelists, it is skipped
and listed as not found. Update its ID in `codelists.txt` to the new
one.

Read the codelists in R with
[`read_codelists()`](https://bennettoxford.github.io/opencodecounts/reference/read_codelists.md):

``` r

library(opencodecounts)

codelists <- read_codelists("codelists")
```

## Where the code pairs come from

The old and new code pairs come from the SNOMED CT History Substitution
Table on NHS TRUD (Technology Reference data Update Distribution), item
276. The package downloads a copy that keeps only pairs whose old code
has GP usage. It is downloaded once and cached. Clear it with
[`cache_clear()`](https://bennettoxford.github.io/opencodecounts/reference/cache_clear.md).

## Run the check in R

``` r

check <- check_codelists("codelists", start_date = "2020-08-01")
check
```

The report counts the codelists with and without missing linked codes
and lists those with missing codes:

    ── Codelist check ──────────────────────────────────────────────────────────────
    SNOMED CT history version 0.1.0 (TRUD release uk_sctqths_43.1.0_20261021000001)
    OpenCodeCounts SNOMED CT data version 0.2.0 from 01 Aug 2020 to 31 Jul 2026
    ✖ 2 codelists miss linked codes with usage:

      Codelist               Included codes             Linked codes not included
      user-Lola_O-proms-anx  68 codes,  9,217,600 uses  2 codes,    13,220 uses,  0.1%
      user-Lola_O-proms-dep  50 codes, 16,400,990 uses  5 codes, 2,584,100 uses, 13.6%

    Missing codes and their share of each old and new code pair:

      user-Lola_O-proms-anx
        836551000000102   old code      7,180 uses   99.4% of pair  Generalised anxiety disorder 2 scale (assessment scale)
        836561000000104   old code      6,040 uses   85.8% of pair  Assessment using generalised anxiety disorder 2 scale (procedure)

      user-Lola_O-proms-dep
        836541000000100   old code  2,444,600 uses   87.0% of pair  Patient health questionnaire 2 score (observable entity)
        1104961000000104  old code    114,220 uses  100.0% of pair  Patient Health Questionnaire-4 score (observable entity)
        1104941000000100  old code     21,160 uses   99.6% of pair  Patient Health Questionnaire-4 (assessment scale)
        836521000000107   old code      2,190 uses   99.5% of pair  Patient health questionnaire 2 (assessment scale)
        836531000000109   old code      1,930 uses   82.8% of pair  Assessment using patient health questionnaire 2 (procedure)

The percentages show the share of usage that the missing codes may
account for, per codelist and per old and new code pair. Pairs that TRUD
flags as ambiguous, often a broad parent code such as “Ethnic group”,
are left out of the report but kept in `check@results`. Review each code
before adding it to a codelist.

Write all pairs to CSV:

``` r

readr::write_csv(check@results, "codelist-check.csv", na = "")
```

To check codelists on OpenCodelists without a codelists folder, use
[`check_codelists_url()`](https://bennettoxford.github.io/opencodecounts/reference/check_codelists.md):

``` r

check_codelists_url("https://www.opencodelists.org/codelist/user/example/phq-9/abc12345/")
```

Set `data_version` and `history_version` to make the results
reproducible. The first two lines of the report show the versions used.

## Run the check in the terminal

Install the `opencodecounts` command once with
[Rapp](https://github.com/r-lib/Rapp):

``` r

install.packages("Rapp")
Rapp::install_pkg_cli_apps("opencodecounts")
```

Then run the check from your study folder:

``` bash
opencodecounts codelists check
opencodecounts codelists check --start-date 2020-08-01 --output codelist-check.csv
```

Use `--output` to save all old and new code pairs to a CSV file. It
exits with status 1 when a codelist misses a linked code with recorded
usage, so you can use it in continuous integration. The command uses
your installed package, so reinstall the package to update it. Run
`opencodecounts codelists check --help` for all options.
