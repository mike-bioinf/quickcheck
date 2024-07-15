## tests for impose_logical_behavior


testthat::test_that("impose_logical_behavior returns expected logical value", {
  expect_equal(
    object = impose_logical_behavior(check_columns_presence(df = df, columns = "materasso")),
    expected = TRUE
  )

  expect_equal(
    object = impose_logical_behavior(check_empty_vec(vec = c(1, 2, 3))),
    expected = FALSE
  )

  expect_error(
    object = impose_logical_behavior(check_presence_values(vec = df[["sex"]], values = "men", raise = "messagesssss")),
    regexp = '`type` must be one of "error", "warning", or "message", not "messagesssss".'
  )
})




testthat::test_that("impose_logical_behavior return error when the wrong alert is picked", {
  expect_error(
    object = impose_logical_behavior(expr = check_columns_key(df, columns = c("visit_numb"))),
    regexp = "The following column is missing in df: visit_numb"
  )
})





test_that("impose_logical_behavior force_alert argument works as intended", {
  expect_message(
    object = impose_logical_behavior(expr = check_columns_key(df, columns = c("visit_number"), raise = "message"), force_alert = T),
    class = "quickalert"
  )

  expect_no_condition(impose_logical_behavior(expr = check_columns_presence(df, columns = c("visit_number")), force_alert = T))
})

