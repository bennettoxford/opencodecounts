#!/usr/bin/env Rapp
#| name: opencodecounts
#| title: opencodecounts
#| description: Command line tools from the opencodecounts R package.

# Show only the error message, without a backtrace
options(rlang_backtrace_on_error = "none")

# Empty strings mean "use the default" because NULL would declare a
# positional argument
null_if_empty <- function(x) if (nzchar(x)) x else NULL

switch(
  command <- "",

  #| title: Work with codelists in codelists/codelists.txt
  codelists = {
    switch(
      subcommand <- "",

      #| title: Check codelists for inactive SNOMED CT codes with usage
      #| description: Show codelists that do not select some linked old or new codes with recorded usage. Exits with status 1 when any codelist does.
      #| examples:
      #|   - opencodecounts codelists check
      #|   - opencodecounts codelists check --start-date 2020-08-01 --output codelist-check.csv
      #|   - opencodecounts codelists check --url https://www.opencodelists.org/codelist/user/example/phq-9/abc12345/
      check = {
        #| description: Codelists directory with codelists.txt and CSV files.
        dir <- "codelists"

        #| description: OpenCodelists URL of one codelist to check instead of the codelists directory.
        url <- ""

        #| description: Start of the first reporting period to use (YYYY-MM-DD). Default all periods.
        start_date <- ""

        #| description: End of the last reporting period to use (YYYY-MM-DD). Default all periods.
        end_date <- ""

        #| description: Version of the GP SNOMED CT usage data. Default latest.
        data_version <- ""

        #| description: Version of the SNOMED CT History Substitution Table. Default latest.
        history_version <- ""

        #| description: Write every old and new code pair to this CSV file.
        output <- ""

        check <- opencodecounts::check_codelists(
          dir = dir,
          url = null_if_empty(url),
          start_date = null_if_empty(start_date),
          end_date = null_if_empty(end_date),
          data_version = null_if_empty(data_version),
          history_version = null_if_empty(history_version)
        )
        print(check)

        if (nzchar(output)) {
          readr::write_csv(check@results, output, na = "")
          cli::cli_alert_success("Wrote all code pairs to {.file {output}}.")
        } else {
          cli::cli_alert_info(
            "Save all code pairs with {.code --output codelist-check.csv}."
          )
        }

        if (check@not_selected_usage_found) {
          quit(status = 1)
        }
      }
    )
  }
)
