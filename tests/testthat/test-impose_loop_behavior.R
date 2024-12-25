 ## Testing impose_loop_behavior

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
