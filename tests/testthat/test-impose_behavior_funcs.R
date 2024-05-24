## testing impose_*_behavior functions

testthat::test_that("impose_logical_behavior returns expcted logical", {
  expect_equal(impose_logical_behavior(check_columns_presence(df = df, columns = "materasso")), TRUE)

  expect_equal(impose_logical_behavior(check_empty_vec(vec = c(1, 2, 3))), FALSE)

  expect_error(
    object = impose_logical_behavior(check_presence_values(vec = df[["sex"]], values = "men", raise = "messagesssss")),
    regexp = '`type` must be one of "error", "warning", "message", or "accumulate_message", not "messagesssss".'
  )
})




testthat::test_that("impose_logical_behavior return error when the wrong alert is picked", {
  expect_error(
    object = impose_logical_behavior(expr = check_columns_key(df, keys = c("visit_numb"))),
    regexp = "The following key is missing in df: visit_numb"
  )
})









#### impose_accumulation_behavior =====================================================================================================

testthat::test_that(desc = "impose_accumulation_behavior accumulate quickalert", {
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





testthat::test_that("impose_acumulation_behavior correctly doesn't raise alert", {
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

