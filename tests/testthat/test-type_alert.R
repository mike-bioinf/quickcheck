### testing if the correct type of alert is used


testthat::test_that("The correct type of alert is raised", {
  expect_warning(check_nrow_dfs(df1 = df, df2 = df_row, raise = "warning"))
  expect_error(check_nrow_dfs(df, df_row, raise = "error"))
  expect_message(check_nrow_dfs(df, df_row, raise = "message"))
})
