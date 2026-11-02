# How to use the Shiny app

## Overview

The `opencodecounts` Shiny app is an interactive tool for exploring
clinical code usage data in England. It visualises trends over time and
helps you search, filter, and analyse clinical codes from three major
coding systems used in the NHS. It can be launched locally with the
[`run_app()`](https://bennettoxford.github.io/opencodecounts/reference/run_app.md)
function or accessed online at
<https://bennettoxford.github.io/opencodecounts/articles/app>.

## Step 1: Select data

The Shiny app has a sidebar to select one of the available datasets
(i.e., SNOMED CT, ICD-10, or OPCS-4) and search for a specific code or a
collection of codes, including codelists from
[OpenCodelists.org](http://opencodelists.org/).

![\*\*Figure 1.\*\* Select dataset for
anlysis.](img/shiny_app_datasets.png)

**Figure 1.** Select dataset for anlysis.

## Step 2: Select codes

The app offers flexible ways to select codes for analysis:

1.  Search by code: If you know exact codes, start typing and select
    from suggestions
2.  Enter keywords to find codes (use `|` to combine terms, e.g.,
    `diabetes|diabetic`)
3.  Load codelist by URL from
    [OpenCodelists.org](http://opencodelists.org/).

![\*\*Figure 2.\*\* Search codes by specific code or
description.](img/shiny_app_search_description.png)

**Figure 2.** Search codes by specific code or description.

![\*\*Figure 3.\*\* Select codes by loading
codelist.](img/shiny_app_load_codelist.png)

**Figure 3.** Select codes by loading codelist.

## Step 3: Review your results

There are three main tabs to review the results:

**Trends over time**: You can explore visualisations summarising overall
trends and inspect individual code usage over time. You can hover over
data points for detailed information. Toggle “Show individual codes” to
see separate lines for each code (available when ≤500 codes selected).

![\*\*Figure 4.\*\* Main page showing \*Trends over
time\*.](img/shiny_app_trends.png)

**Figure 4.** Main page showing *Trends over time*.

**Usage table**: A table provides key statistics on the frequency and
proportional contribution of selected codes across all years.

![\*\*Figure 5.\*\* Main page showing \*Usage
table\*.](img/shiny_app_usage_table.png)

**Figure 5.** Main page showing *Usage table*.

**Selected codes**: Additionally, the app presents a structured list of
all selected codes along with their descriptions and a column
specifiying if usage data was reported. This list can be downloaded as a
CSV file for use in EHR research or for upload to
[OpenCodelists.org](http://opencodelists.org/).

![\*\*Figure 6.\*\* Main page showing \*Selected
codes\*.](img/shiny_app_selected_codes.png)

**Figure 6.** Main page showing *Selected codes*.

## Share a link to your selection

The app URL holds your current selection. Copy it from the address bar
to share the same view. You can also write a link yourself with these
parameters.

| Parameter | Example | Selects |
|----|----|----|
| `dataset` | `snomedct`, `icd10` or `opcs4` | The dataset |
| `codelist` | `opensafely/anxiety-disorders/6aef605a` | A codelist from [OpenCodelists.org](http://opencodelists.org/), by its slug or full URL |
| `codes` | `F411,F329` | Specific codes, separated by commas |
| `description` | `anxiety%7Cpanic` | A description search. `%7C` is the URL code for `|` |
| `pattern` | `F4` | An ICD-10 or OPCS-4 category |
| `from`, `to` | `2014-15`, `2019-20` | The first and last reporting year of the date range |

For example,
`?dataset=snomedct&codelist=opensafely/anxiety-disorders/6aef605a` opens
the app with this codelist loaded. Add `dataset` when you link to a
codelist, because the app does not switch the dataset for you. A
codelist replaces `codes`, `description` and `pattern`. The app ignores
values it does not recognise.

Reporting years are labelled as in the NHS Digital publications. For GP
data (SNOMED CT), `2014-15` runs from 1 August 2014 to 31 July 2015. For
hospital data (ICD-10 and OPCS-4), it runs from 1 April 2014 to 31 March
2015.

## Find more information

We provide more detailed information as part of the R package
documentation.

![\*\*Figure 7.\*\* Tab with links to more
documentation.](img/shiny_app_more.png)

**Figure 7.** Tab with links to more documentation.
