### testing if the correct type of alert is used


testthat::test_that("The correct type of alert is raised", {
  expect_warning(check_nrow_dfs(df1 = qadf, df2 = qadf_row, raise = "warning"))
  expect_error(check_nrow_dfs(qadf, qadf_row, raise = "error"))
  expect_message(check_nrow_dfs(qadf, qadf_row, raise = "message"))
})
