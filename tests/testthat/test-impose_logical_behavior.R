### test functionality impose_logical_behavior

testthat::test_that("impose_logical_behavior does its job", {
  expect_equal(
    impose_logical_behaviour(check_columns_presence(df = df, columns = "materasso")),
    TRUE
  )
})
