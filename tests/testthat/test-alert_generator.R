### testing alert_generator function.

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



testthat::test_that("alert_generator produce alerts with class", {
  expect_condition(
    alert_generator(type = "message", alert_message = "generic message", quickalert = TRUE),
    class = "quickalert"
  )

  expect_failure(
    suppressMessages({
      expect_condition(
        alert_generator(type = "message", alert_message = "generic message", quickalert = FALSE),
        class = "quickalert"
      )
    })
  )
})
