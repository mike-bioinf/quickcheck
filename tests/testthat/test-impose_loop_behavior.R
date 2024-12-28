## Testing impose_loop_behavior


test_that("impose_loop_behavior raise errors for incorrect inputs", {
  expect_snapshot_error(impose_loop_behavior(x = qadf, check_func = check_columns_number))
  expect_snapshot_error(impose_loop_behavior(x = list(qadf), check_func = check_columns_na, check_arg_list = 3))
  expect_error(
    impose_loop_behavior(x = list(qadf), check_func = check_columns_na, check_arg_list = list(p1 = 2, 3)),
    regexp = "vec is or contains an empty entity."
  )
  expect_error(
    impose_loop_behavior(x = list(qadf), check_func = check_columns_na, check_arg_list = list(sign = FALSE)),
    regexp = "Incompatible parameters passed in check_arg_list"
  )
})



test_that("impose_loop_behavior works correctly", {
  expect_snapshot_error(
    impose_loop_behavior(
      x = list(df1 = qadf, qadf, df3 = qadf),
      check_func = check_columns_presence,
      check_arg_list = list(columns = "sexxx"),
      element_nameroot = "df"
    )
  )
})



test_that("impose_loop_behavior captures only quickalerts", {
  expect_warning(impose_loop_behavior(list(NA, 1, c(1, 2)), check_func = check_sorted_vec), regexp = "NAs in vec")
  expect_snapshot_error(impose_loop_behavior(list(list(1), c(1, 2)), check_func = check_sorted_vec))
})
