### test functionality impose_logical_behavior

testthat::test_that("impose_logical_behavior does its job", {
  expect_equal(
    impose_logical_behavior(check_columns_presence(df = df, columns = "materasso")),
    TRUE
  )

  expect_equal(
    impose_logical_behavior(check_empty_vec(vec = c(1, 2, 3), raise = "message")),
    FALSE
  )

  expect_error(
    object = impose_logical_behavior(check_key(df, key = "sex", raise = "messagesssss")),
    regexp = '`type` must be one of "error", "warning", or "message", not "messagesssss".'
  )
})
