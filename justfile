set positional-arguments := true

alias help := list

# List available commands
list:
    @just --list --unsorted

# Format R code using air
fix:
    air format .

# Regenerate documentation from the comments in the R code
document:
    Rscript --quiet --vanilla -e 'devtools::document()'

# Build and install package
# inst/app-data/ is only meant to exist during `just deploy`.
# If a previous deploy left it behind, remove it first: a plain.
build:
    rm -rf inst/app-data
    Rscript --quiet --vanilla -e 'pak::local_install()'

# Rebuild all the datasets from NHS Digital's website (slow - downloads years of data)
build-data:
    Rscript --quiet --vanilla -e '\
        source("data-raw/gp_snomed.R"); \
        source("data-raw/hesapc_icd10.R"); \
        source("data-raw/hesapc_icd10_breakdowns.R"); \
        source("data-raw/hesapc_opcs4.R"); \
        source("data-raw/hesapc_opcs4_breakdowns.R")'

# Publish one dataset's file to GitHub as a new release (push your branch first)
# Usage: just release <dataset> <version> [notes]
# Example: just release snomed_usage 0.1.0 "SNOMED CT code usage 2011-12 to 2024-25"
release dataset version notes='':
    #!/usr/bin/env bash
    tag=$(echo "{{dataset}}" | tr '_' '-')-v{{version}}
    gh release create "$tag" data-raw/{{dataset}}.parquet --notes "{{notes}}" --target "$(git branch --show-current)"

# Run fast, network-free tests
test-unit:
    Rscript --quiet --vanilla -e 'devtools::test()'

# Run slow tests that hit NHS Digital and OpenCodelists.org for real
test-integration:
    Rscript tests/integration.R

test: test-unit test-integration

# Run R's full package health check
check:
    Rscript --quiet --vanilla -e 'devtools::check()'

# Render README.Rmd to README.md (README.Rmd loads the package itself)
render-readme:
    Rscript --quiet --vanilla -e 'rmarkdown::render("README.Rmd", output_format = rmarkdown::github_document(html_preview = FALSE), output_file = "README.md", clean = TRUE)'

# Render DEVELOPERS.Rmd to DEVELOPERS.md
render-developers:
    Rscript --quiet --vanilla -e 'devtools::load_all(quiet = TRUE); rmarkdown::render("DEVELOPERS.Rmd", output_format = rmarkdown::github_document(html_preview = FALSE), output_file = "DEVELOPERS.md", clean = TRUE)'

# Build pkgdown site
docs-build:
    Rscript --quiet --vanilla -e 'pkgdown::build_site()'

# Preview the already-built docs website in a browser
docs-serve:
    Rscript --quiet --vanilla -e 'servr::httw("docs", initpath = "index.html", browser = TRUE)'

# Build and preview pkgdown site
docs: docs-build docs-serve

# Deploy the Shiny app to Posit Connect Cloud. target: main (default) or beta
# inst/app-data/ only needs to exist for rsconnect::deployApp()
deploy target='main':
    Rscript --quiet --vanilla -e '\
        devtools::load_all(); \
        unlink("inst/app-data", recursive = TRUE); \
        copy_app_data_for_deploy("inst/app-data")'
    Rscript --quiet --vanilla -e '\
        app_name <- if ("{{target}}" == "beta") "opencodecounts-beta" else "opencodecounts"; \
        on.exit(unlink("inst/app-data", recursive = TRUE), add = TRUE); \
        rsconnect::deployApp(server = "connect.posit.cloud", appName = app_name, appTitle = app_name)'
