### tests for impose_accumulation_behavior



testthat::test_that(desc = "impose_accumulation_behavior accumulate alerts", {
  expect_snapshot_error({
    columns <- c("sex", "visit_number")
    col_levels <- list(sex = c("femmina", "male"), visit_number = c("visit1", "2", "vis3"))
    impose_accumulation_behavior(
      header = "this is the header.",
      expr = for(n in columns){
        check_presence_values(
          vec = df[[n]],
          values = col_levels[[n]],
          vec_arg = n,
          alert_message = "{vec_arg} --> {col_magenta(missing_values)}",
          raise = "accumulate_message"
        )
      })
  })
})




testthat::test_that("impose_accumulation_behavior launch unexpected error alert", {
  expect_snapshot_error({
    columns <- c("sex", "visit_number")
    col_levels <- list(sex = c("femmina", "male"))
    impose_accumulation_behavior(
      expr = for(n in columns){
        check_presence_values(
          vec = df[[n]],
          values = col_levels[[n]],
          vec_arg = n,
          alert_message = "{vec_arg} --> {col_magenta(missing_values)}",
          raise = "messagessss"
        )
      })
  })
})




testthat::test_that("impose_accumulation_behavior correctly doesn't raise alert", {
  columns <- c("sex")
  col_levels <- list(sex = c("male"))
  res <- impose_accumulation_behavior(
    expr = for(n in columns){
      check_presence_values(
        vec = df[[n]],
        values = col_levels[[n]],
        vec_arg = n,
        alert_message = "{vec_arg} --> {col_magenta(missing_values)}",
        raise = "message"
      )})
  expect_null(res)
})




test_that("impose_accumulation_behavior launch one time the provided alert message", {
  expect_snapshot(
    check_columns_key(
      df = df,
      columns = c("sex", "visit_number"),
      raise = "warning",
      alert_message = "Just one line !!!",
      header = NULL
    )
  )
})




test_that("The header option works correctly with impose_accumulation behavior", {
  expect_snapshot(
    check_columns_key(
      df = df,
      columns = c("sex", "visit_number"),
      raise = "warning",
      header = "CUSTOM HEADER"
    )
  )
})
