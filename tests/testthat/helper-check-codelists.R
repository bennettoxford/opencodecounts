test_codelists_dir <- function() {
  test_path("fixtures", "codelists")
}

# Old 836541000000100 is not in the test codelist and has usage.
# Old 1104961000000104 and new 1359726006 are both in the codelist.
# Old 111111111 is not in the test codelist and has no usage.
test_snomed_history <- function() {
  tibble::tibble(
    old_code = c("836541000000100", "1104961000000104", "111111111"),
    old_term = c("PHQ-2 score (old)", "PHQ-4 score (old)", "PHQ-2 (old)"),
    new_code = c("1359723003", "1359726006", "1359723003"),
    new_term = c("PHQ-2 score", "PHQ-4 score", "PHQ-2 score"),
    is_ambiguous = c(FALSE, TRUE, FALSE),
    trud_release = "uk_sctqths_test_release"
  )
}

test_snomed_usage <- function() {
  period_1 <- c(start_date = "2011-08-01", end_date = "2012-07-31")
  period_2 <- c(start_date = "2012-08-01", end_date = "2013-07-31")

  tibble::tibble(
    start_date = as.Date(c(rep(period_1[[1]], 3), rep(period_2[[1]], 3))),
    end_date = as.Date(c(rep(period_1[[2]], 3), rep(period_2[[2]], 3))),
    snomed_code = c(
      "836541000000100",
      "1359723003",
      "1104961000000104",
      "836541000000100",
      "1359723003",
      "1359726006"
    ),
    usage = c(100L, 10L, 30L, 50L, 40L, 5L)
  )
}

local_check_mocks <- function(env = parent.frame()) {
  local_mocked_bindings(
    get_coding_systems = function(codelist_ids) {
      ifelse(grepl("dmd", codelist_ids), "dmd", "snomedct")
    },
    get_gp_snomed = function(version = NULL) test_snomed_usage(),
    get_snomed_history = function(version) test_snomed_history(),
    .env = env
  )
}

local_test_check <- function(..., env = parent.frame()) {
  local_check_mocks(env)
  check_codelists(
    test_codelists_dir(),
    data_version = "0.2.0",
    history_version = "0.1.0",
    ...
  )
}

# Usage data in the format the Shiny app uses
test_app_usage <- function() {
  test_snomed_usage() |>
    dplyr::rename(code = snomed_code) |>
    dplyr::mutate(description = paste("Term", code))
}

test_codelist <- function() {
  readr::read_csv(
    file.path(test_codelists_dir(), "user-test-phq-snomed.csv"),
    col_types = readr::cols(.default = readr::col_character())
  )
}

test_usage_total <- function() {
  test_snomed_usage() |>
    dplyr::summarise(usage = sum(as.numeric(usage)), .by = snomed_code)
}
