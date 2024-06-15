### Testing the quickalert class property for functions that use internally impose_accumulation behaviour.

testthat::test_that(
  "Functions that use internally impose_accumulation behaviour launch classed alerts",
  {
    expect_condition(
      check_columns_key(df, columns = c("sex", "visit_number"), quickalert = TRUE),
      class = "quickalert"
    )

    suppressWarnings({
      expect_failure({
        expect_condition(
          check_columns_key(df, columns = c("sex", "visit_number"), raise = "warning", quickalert = FALSE),
          class = "quickalert"
        )
      })
    })
  }
)
