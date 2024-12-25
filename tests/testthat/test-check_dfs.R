### test on two dfs


test_that("check_columns_copresence works as intended", {
  expect_error(check_columns_copresence(qadf1, qadf2, columns = "visit_number"), class = "quickalert")
  expect_no_error(check_columns_copresence(qadf1, qadf2, columns = "bmi"))
  expect_error(check_columns_copresence(qadf1, qadf2, columns = "other"), regexp = "other is not found in df1 and df2")
})



test_that("check_presence_dfs works as intented", {
  expect_no_error(check_presence_dfs(qadf1, qadf2, "visit_number"))
  qadf2 <- dplyr::rename(qadf2, visit_number2 = visit_number)
  expect_no_error(check_presence_dfs(qadf1, qadf2, c("visit_number", "visit_number2")))
  expect_error(check_presence_dfs(qadf1, qadf2, c("visit_number", "height")), class = "quickalert")
  expect_error(check_presence_dfs(qadf1, qadf2, c("visit_number", "other")), regexp = "The following column")
  expect_error(check_presence_dfs(qadf1, qadf2, c("visit_number", "bmi"), alert_message = "questi:{missing_values2}"), regexp = "questi:")
})
