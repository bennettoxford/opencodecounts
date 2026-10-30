test_that("Test strip semantic tag - One string with sem tag", {
  snomed_ct_description <- c("Blood Pressure (observable entity)")
  description_short <- strip_semantic_tag(snomed_ct_description)

  expect_equal(
    description_short,
    c("Blood Pressure")
  )
})

test_that("Test strip semantic tag - two string with sem tag", {
  snomed_ct_description <- c(
    "Blood Pressure (observable entity)",
    "Hypertension (finding)"
  )
  description_short <- strip_semantic_tag(snomed_ct_description)

  expect_equal(
    description_short,
    c("Blood Pressure", "Hypertension")
  )
})

test_that("Test strip semantic tag - One string with () and sem tag", {
  snomed_ct_description <- c("Foo (foo_desc) (foo_sem_tag)")
  description_short <- strip_semantic_tag(snomed_ct_description)

  expect_equal(
    description_short,
    c("Foo (foo_desc)")
  )
})

test_that("Test strip semantic tag - One string without sem tag", {
  snomed_ct_description <- c("Blood pressure")
  description_short <- strip_semantic_tag(snomed_ct_description)

  expect_equal(
    description_short,
    c("Blood pressure")
  )
})


test_that("Test extract semantic tag - One string with sem tag", {
  snomed_ct_description <- c("Blood pressure (observable entity)")
  semantic_tag <- extract_semantic_tag(snomed_ct_description)

  expect_equal(
    semantic_tag,
    c("observable entity")
  )
})

test_that("Test extract semantic tag - Two string with sem tag", {
  snomed_ct_description <- c("Foo (foo)", "Boo (boo)")
  semantic_tag <- extract_semantic_tag(snomed_ct_description)

  expect_equal(
    semantic_tag,
    c("foo", "boo")
  )
})

test_that("Test extract semantic tag - One string with () and sem tag", {
  snomed_ct_description <- c("Foo (foo_desc) (foo_sem_tag)")
  semantic_tag <- extract_semantic_tag(snomed_ct_description)

  expect_equal(
    semantic_tag,
    c("foo_sem_tag")
  )
})

test_that("Test extract semantic tag - One string without sem tag", {
  snomed_ct_description <- c("Blood pressure")
  semantic_tag <- extract_semantic_tag(snomed_ct_description)

  expect_equal(
    semantic_tag,
    c(NA_character_)
  )
})

test_that("Test extract semantic tag - Numeric", {
  snomed_ct_description <- c(10)
  semantic_tag <- extract_semantic_tag(snomed_ct_description)

  expect_equal(
    semantic_tag,
    c(NA_character_)
  )
})


test_that("Test get codes with multiple desc", {
  df_test <- tibble::tribble(
    ~start_date  , ~end_date    , ~snomed_code , ~description                 , ~active_at_start , ~active_at_start , ~active_at_end ,
    "2023-08-01" , "2024-07-31" , "100"        , "One code one description"   ,              100 , TRUE             , TRUE           ,
    "2022-08-01" , "2023-07-31" , "100"        , "One code one description"   ,              100 , TRUE             , TRUE           ,
    "2023-08-01" , "2024-07-31" , "200"        , " One code two description"  ,              200 , TRUE             , TRUE           ,
    "2022-08-01" , "2023-07-31" , "200"        , " One code two descriptions" ,              200 , TRUE             , TRUE
  )

  expect_equal(
    get_codes_with_multiple_desc(df_test, snomed_code),
    c("200")
  )
})

test_that("Test get codes with encoding problem", {
  df_test <- tibble::tribble(
    ~start_date  , ~end_date    , ~snomed_code , ~description           , ~active_at_start , ~active_at_start , ~active_at_end ,
    "2023-08-01" , "2024-07-31" , "100"        , "No encoding problems" ,              100 , TRUE             , TRUE           ,
    "2022-08-01" , "2023-07-31" , "200"        , " Encoding problems Ã" ,              200 , TRUE             , TRUE           ,
    "2022-08-01" , "2023-07-31" , "300"        , " Encoding problems â" ,              300 , TRUE             , TRUE
  )

  expect_equal(
    get_codes_with_encoding_problems(df_test, snomed_code),
    c("200", "300")
  )
})

test_that("Test fix a circumflex lowercase encoding", {
  a_circumflex_lower <- c(
    "Provision of information about Infant crying is normal, Comforting methods can help, Itâ€™s OK to walk away, Never, ever shake a baby (procedure)",
    "Breast implantâ€“associated anaplastic large-cell lymphoma (disorder)",
    "Vitamin Bâ,\u0081â,, deficiency anaemia due to intrinsic factor deficiency",
    "Vitamin Bâ,\u0081â,, deficiency anaemia due to selective vitamin Bâ,\u0081â,, malabsorption with proteinuria",
    "Other dietary vitamin Bâ,\u0081â,, deficiency anaemia",
    "Other vitamin Bâ,\u0081â,, deficiency anaemias",
    "Vitamin Bâ,\u0081â,, deficiency anaemia, unspecified",
    "Megavitamin-Bâ,† syndrome",
    "GMâ,, gangliosidosis",
    "Poisoning: Histamine Hâ,,-receptor antagonists",
    "Vitamin Bâ,\u0081â,,, folic acid and other anti-megaloblastic-anaemia preparations",
    "Histamine Hâ,,-receptor antagonists"
  )

  expected_a_circumflex_lower_fix <- c(
    "Provision of information about Infant crying is normal, Comforting methods can help, It's OK to walk away, Never, ever shake a baby (procedure)",
    "Breast implant-associated anaplastic large-cell lymphoma (disorder)",
    "Vitamin B12 deficiency anaemia due to intrinsic factor deficiency",
    "Vitamin B12 deficiency anaemia due to selective vitamin B12 malabsorption with proteinuria",
    "Other dietary vitamin B12 deficiency anaemia",
    "Other vitamin B12 deficiency anaemias",
    "Vitamin B12 deficiency anaemia, unspecified",
    "Megavitamin-B6 syndrome",
    "GM2 gangliosidosis",
    "Poisoning: Histamine H2-receptor antagonists",
    "Vitamin B12, folic acid and other anti-megaloblastic-anaemia preparations",
    "Histamine H2-receptor antagonists"
  )

  expect_equal(
    fix_encoding(a_circumflex_lower),
    expected_a_circumflex_lower_fix
  )
})

test_that("Test fix a tilde uppercase encoding", {
  a_tilde_upper <- c(
    "Serum SjÃ¶gren's syndrome A 60 kiloDalton antibody (Ro) concentration (observable entity)",
    "ChÃ©diak-Higashi syndrome (disorder)",
    "DÃ©jÃ©rine-Sottas disease (disorder)",
    "MÃ©niÃ¨re's disease (disorder)",
    "LasÃ¨gue sign (finding)",
    "DÃ©jÃ\u00A0 pensÃ© (finding)",
    "DÃ©jÃ\u00A0 vu (finding)",
    "KÃ¼mmell disease (disorder)",
    "Hand-SchÃ¼ller-Christian disease (disorder)",
    "Montgomery-Ã…sberg depression rating scale (assessment scale)",
    "Pelger-HuÃ«t anomaly (disorder)",
    "Born in CuraÃ§ao (finding)",
    "Main spoken language Norwegian BokmÃ¥l (finding)",
    "Concentric sclerosis [BalÃ³]",
    "BehÃ§et disease"
  )

  expected_a_tilde_upper_fix <- c(
    "Serum Sjögren's syndrome A 60 kiloDalton antibody (Ro) concentration (observable entity)",
    "Chédiak-Higashi syndrome (disorder)",
    "Déjérine-Sottas disease (disorder)",
    "Ménière's disease (disorder)",
    "Lasègue sign (finding)",
    "Déjà pensé (finding)",
    "Déjà vu (finding)",
    "Kümmell disease (disorder)",
    "Hand-Schüller-Christian disease (disorder)",
    "Montgomery-Åsberg depression rating scale (assessment scale)",
    "Pelger-Huët anomaly (disorder)",
    "Born in Curaçao (finding)",
    "Main spoken language Norwegian Bokmål (finding)",
    "Concentric sclerosis [Baló]",
    "Behçet disease"
  )

  expect_equal(
    fix_encoding(a_tilde_upper),
    expected_a_tilde_upper_fix
  )
})

test_that("Test a circumflex lower singlelow 9 quotation mark encoding problems", {
  a_circumflex_lower_singlelow_9_quotation_mark <- c(
    "Vitamin Bâ‚\u0081â‚‚ deficiency anaemia due to intrinsic factor deficiency",
    "Vitamin Bâ‚\u0081â‚‚ deficiency anaemia due to selective vitamin Bâ‚\u0081â‚‚ malabsorption with proteinuria",
    "Other dietary vitamin Bâ‚\u0081â‚‚ deficiency anaemia",
    "Other vitamin Bâ‚\u0081â‚‚ deficiency anaemias",
    "Vitamin Bâ‚\u0081â‚‚ deficiency anaemia, unspecified",
    "Megavitamin-Bâ‚† syndrome",
    "GMâ‚‚ gangliosidosis",
    "Poisoning: Histamine Hâ‚‚-receptor antagonists",
    "Vitamin Bâ‚\u0081â‚‚, folic acid and other anti-megaloblastic-anaemia preparations",
    "Histamine Hâ‚‚-receptor antagonists"
  )

  expected_a_circumflex_lower_singlelow_9_quotation_mark_fix <- c(
    "Vitamin B12 deficiency anaemia due to intrinsic factor deficiency",
    "Vitamin B12 deficiency anaemia due to selective vitamin B12 malabsorption with proteinuria",
    "Other dietary vitamin B12 deficiency anaemia",
    "Other vitamin B12 deficiency anaemias",
    "Vitamin B12 deficiency anaemia, unspecified",
    "Megavitamin-B6 syndrome",
    "GM2 gangliosidosis",
    "Poisoning: Histamine H2-receptor antagonists",
    "Vitamin B12, folic acid and other anti-megaloblastic-anaemia preparations",
    "Histamine H2-receptor antagonists"
  )

  expect_equal(
    fix_encoding(a_circumflex_lower_singlelow_9_quotation_mark),
    expected_a_circumflex_lower_singlelow_9_quotation_mark_fix
  )
})


test_that("Dont replace anything are no gaps", {
  df_test <- tibble::tribble(
    ~start_date  , ~end_date    , ~code , ~usage , ~description           ,
    "2023-08-01" , "2024-07-31" , "100" ,    100 , "Code 100 description" ,
    "2022-08-01" , "2023-07-31" , "100" ,    200 , "Code 100 description" ,
    "2023-08-01" , "2024-07-31" , "200" ,    100 , "Code 200 description" ,
    "2022-08-01" , "2023-07-31" , "200" ,    200 , "Code 200 description"
  ) |>
    dplyr::mutate(
      dplyr::across(c(start_date, end_date), as.Date)
    )

  df_completed <- complete_usage_gaps_with_zeros(df_test)

  df_expected <- tibble::tribble(
    ~code , ~end_date    , ~start_date  , ~usage , ~description           ,
    "100" , "2023-07-31" , "2022-08-01" ,    200 , "Code 100 description" ,
    "100" , "2024-07-31" , "2023-08-01" ,    100 , "Code 100 description" ,
    "200" , "2023-07-31" , "2022-08-01" ,    200 , "Code 200 description" ,
    "200" , "2024-07-31" , "2023-08-01" ,    100 , "Code 200 description"
  ) |>
    dplyr::mutate(
      dplyr::across(c(start_date, end_date), as.Date)
    )

  expect_equal(df_completed, df_expected)
})

test_that("Replace one gap in one code with zero", {
  df_test <- tibble::tribble(
    ~start_date  , ~end_date    , ~code , ~usage , ~description           ,
    "2023-08-01" , "2024-07-31" , "100" ,    100 , "Code 100 description" ,
    "2021-08-01" , "2022-07-31" , "100" ,    300 , "Code 100 description"
  ) |>
    dplyr::mutate(
      dplyr::across(c(start_date, end_date), as.Date)
    )

  df_completed <- complete_usage_gaps_with_zeros(df_test)

  df_expected <- tibble::tribble(
    ~code , ~end_date    , ~start_date  , ~usage , ~description           ,
    "100" , "2022-07-31" , "2021-08-01" ,    300 , "Code 100 description" ,
    "100" , "2023-07-31" , "2022-08-01" ,      0 , "Code 100 description" ,
    "100" , "2024-07-31" , "2023-08-01" ,    100 , "Code 100 description"
  ) |>
    dplyr::mutate(
      dplyr::across(c(start_date, end_date), as.Date)
    )

  expect_equal(df_completed, df_expected)
})


test_that("Replace two gap in one code with zeros", {
  df_test <- tibble::tribble(
    ~start_date  , ~end_date    , ~code , ~usage , ~description           ,
    "2023-08-01" , "2024-07-31" , "100" ,    100 , "Code 100 description" ,
    "2020-08-01" , "2021-07-31" , "100" ,    200 , "Code 100 description" ,
    "2016-08-01" , "2017-07-31" , "100" ,    300 , "Code 100 description"
  ) |>
    dplyr::mutate(
      dplyr::across(c(start_date, end_date), as.Date)
    )

  df_completed <- complete_usage_gaps_with_zeros(df_test)

  df_expected <- tibble::tribble(
    ~code , ~end_date    , ~start_date  , ~usage , ~description           ,
    "100" , "2017-07-31" , "2016-08-01" ,    300 , "Code 100 description" ,
    "100" , "2018-07-31" , "2017-08-01" ,      0 , "Code 100 description" ,
    "100" , "2019-07-31" , "2018-08-01" ,      0 , "Code 100 description" ,
    "100" , "2020-07-31" , "2019-08-01" ,      0 , "Code 100 description" ,
    "100" , "2021-07-31" , "2020-08-01" ,    200 , "Code 100 description" ,
    "100" , "2022-07-31" , "2021-08-01" ,      0 , "Code 100 description" ,
    "100" , "2023-07-31" , "2022-08-01" ,      0 , "Code 100 description" ,
    "100" , "2024-07-31" , "2023-08-01" ,    100 , "Code 100 description"
  ) |>
    dplyr::mutate(
      dplyr::across(c(start_date, end_date), as.Date)
    )

  expect_equal(df_completed, df_expected)
})

test_that("add_period_dates() parses the snomed_usage 1 Aug - 31 Jul period", {
  df_test <- tibble::tibble(
    nhs_fy = c("2024to2025", "2023to2024"),
    usage = c(100, 200)
  )

  df_dated <- add_period_dates(df_test, "nhs_fy", "08-01", "07-31")

  expect_equal(df_dated$start_date, as.Date(c("2024-08-01", "2023-08-01")))
  expect_equal(df_dated$end_date, as.Date(c("2025-07-31", "2024-07-31")))
  expect_equal(df_dated$usage, c(100, 200))
})

test_that("add_period_dates() parses the icd10/opcs4 1 Apr - 31 Mar financial year", {
  df_test <- tibble::tibble(
    nhs_fy = c("2024to2025", "2012to2013"),
    usage = c(100, 200)
  )

  df_dated <- add_period_dates(df_test, "nhs_fy", "04-01", "03-31")

  expect_equal(df_dated$start_date, as.Date(c("2024-04-01", "2012-04-01")))
  expect_equal(df_dated$end_date, as.Date(c("2025-03-31", "2013-03-31")))
})

test_that("Replace two gaps in two codes with zeros", {
  df_test <- tibble::tribble(
    ~start_date  , ~end_date    , ~code , ~usage , ~description           ,
    "2023-08-01" , "2024-07-31" , "100" ,    100 , "Code 100 description" ,
    "2021-08-01" , "2022-07-31" , "100" ,    200 , "Code 100 description" ,
    "2023-08-01" , "2024-07-31" , "200" ,    100 , "Code 200 description" ,
    "2021-08-01" , "2022-07-31" , "200" ,    200 , "Code 200 description" ,
  ) |>
    dplyr::mutate(
      dplyr::across(c(start_date, end_date), as.Date)
    )

  df_completed <- complete_usage_gaps_with_zeros(df_test)

  df_expected <- tibble::tribble(
    ~code , ~end_date    , ~start_date  , ~usage , ~description           ,
    "100" , "2022-07-31" , "2021-08-01" ,    200 , "Code 100 description" ,
    "100" , "2023-07-31" , "2022-08-01" ,      0 , "Code 100 description" ,
    "100" , "2024-07-31" , "2023-08-01" ,    100 , "Code 100 description" ,
    "200" , "2022-07-31" , "2021-08-01" ,    200 , "Code 200 description" ,
    "200" , "2023-07-31" , "2022-08-01" ,      0 , "Code 200 description" ,
    "200" , "2024-07-31" , "2023-08-01" ,    100 , "Code 200 description"
  ) |>
    dplyr::mutate(
      dplyr::across(c(start_date, end_date), as.Date)
    )

  expect_equal(df_completed, df_expected)
})

test_that("add_linked_summary() fills zeros in periods of either series", {
  summary_data <- tibble::tibble(
    start_date = as.Date(c("2012-08-01", "2013-08-01")),
    end_date = as.Date(c("2013-07-31", "2014-07-31")),
    total_usage = c(10, 20)
  )
  # The old code stopped being used after the first period
  linked_data <- tibble::tibble(
    start_date = as.Date(c("2011-08-01", "2012-08-01", "2012-08-01")),
    end_date = as.Date(c("2012-07-31", "2013-07-31", "2013-07-31")),
    code = c("a", "a", "b"),
    usage = c(100, 50, 5)
  )

  result <- add_linked_summary(summary_data, linked_data)

  expect_equal(
    result,
    tibble::tibble(
      series = rep(c("Linked codes not selected", "Selected codes"), each = 3),
      start_date = rep(as.Date(c("2011-08-01", "2012-08-01", "2013-08-01")), 2),
      end_date = rep(as.Date(c("2012-07-31", "2013-07-31", "2014-07-31")), 2),
      total_usage = c(100, 55, 0, 0, 10, 20)
    )
  )
})

test_that("plot_summary() adds a coloured series for linked codes", {
  summary_data <- tibble::tibble(
    start_date = as.Date("2012-08-01"),
    end_date = as.Date("2013-07-31"),
    total_usage = 10
  )
  linked <- add_linked_summary(
    summary_data,
    dplyr::mutate(summary_data, usage = 5, code = "a")
  )

  # The text aesthetic is only used by plotly for tooltips
  local_plot <- function(data) suppressWarnings(plot_summary(data))
  single <- suppressWarnings(ggplot2::ggplot_build(local_plot(summary_data)))
  both <- suppressWarnings(ggplot2::ggplot_build(local_plot(linked)))

  expect_equal(unique(single$data[[1]]$colour), "#002147")
  expect_setequal(unique(both$data[[1]]$colour), c("#002147", "#ED6925FF"))
  expect_equal(local_plot(summary_data)$theme$legend.position, "none")
  expect_equal(local_plot(linked)$theme$legend.position, "top")
})

test_that("plot_summary() shows linked codes as triangles", {
  usage <- tibble::tibble(
    start_date = as.Date("2012-08-01"),
    end_date = as.Date("2013-07-31"),
    code = c("a", "b"),
    description = "Term",
    usage = c(10, 5)
  )
  summary_data <- dplyr::summarise(
    usage,
    total_usage = sum(usage),
    .by = c(start_date, end_date)
  )
  # The text aesthetic is only used by plotly for tooltips
  point_shapes <- function(plot) {
    suppressWarnings(ggplot2::ggplot_build(plot))$data[[2]]$shape
  }

  expect_equal(point_shapes(plot_summary(summary_data)), 16)
  expect_setequal(
    point_shapes(plot_summary(add_linked_summary(summary_data, usage))),
    c(16, 17)
  )
})
