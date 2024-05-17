### testing alert_generator dispatch process and list message formatting process

cli::test_that_cli(
  "List messages are formatted correctly",
  {
    testthat::expect_snapshot(
      error = T,
      x = check_columns_levels(
        df,
        columns = c("visit_number", "ppi_treatment"),
        col_levels = list(visit_number = c("v1", "2", "3"), ppi_treatment = c("yes", "no_treatment"))
      )
    )
  },
  configs = "ansi"
)
