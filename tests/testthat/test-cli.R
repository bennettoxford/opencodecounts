test_that("the opencodecounts CLI shows help for codelists check", {
  skip_if_not_installed("Rapp")
  app <- system.file("exec", "opencodecounts.R", package = "opencodecounts")

  help <- capture.output(
    Rapp::run(app, c("codelists", "check", "--help"))
  )

  expect_match(help, "opencodecounts codelists check", all = FALSE)
  expect_match(help, "--start-date", all = FALSE)
  expect_match(help, "--output", all = FALSE)
})
