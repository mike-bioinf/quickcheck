### tests for impose_accumulation_behavior


test_that(desc = "impose_accumulation_behavior accumulate alerts", {
  expect_snapshot_error({
    columns <- c("sex", "visit_number")
    col_levels <- list(sex = c("femmina", "male"), visit_number = c(10, 20))
    impose_accumulation_behavior(
      header = "this is the header.",
      expr = for(n in columns){
        internal_check_presence_vec(
          vec = qadf[[n]],
          values = col_levels[[n]],
          vec_arg = n,
          alert_message = "{vec_arg} --> {cli::col_magenta(missing_values)}",
          header = NULL,
          raise = "message",
          sign = FALSE
        )
      })
  })
})



test_that("impose_accumulation_behavior launch unexpected error alert", {
  expect_snapshot_error({
    columns <- c("sex", "visit_number")
    col_levels <- list(sex = c("femmina", "male"))
    impose_accumulation_behavior(
      expr = for(n in columns){
        check_presence_vec(
          vec = qadf[[n]],
          values = col_levels[[n]],
          vec_arg = n,
          alert_message = "{vec_arg} --> {cli::col_magenta(missing_values)}",
          raise = "messagessss"
        )
      })
  })
})



test_that("impose_accumulation_behavior correctly doesn't raise alert", {
  columns <- c("sex")
  col_levels <- list(sex = c("male"))
  res <- impose_accumulation_behavior(
    expr = for(n in columns){
      check_presence_vec(
        vec = qadf[[n]],
        values = col_levels[[n]],
        vec_arg = n,
        alert_message = "{vec_arg} --> {cli::col_magenta(missing_values)}",
        raise = "message"
      )})
  expect_null(res)
})



test_that("impose_accumulation_behavior launch one time the provided alert message", {
  expect_snapshot(
    check_columns_key(
      df = qadf,
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
      df = qadf,
      columns = c("sex", "visit_number"),
      raise = "warning",
      header = "CUSTOM HEADER"
    )
  )
})



test_that("impose_accumulation_behavior does not capture non-quickalerts conditions", {
  expect_warning(impose_accumulation_behavior(check_sorted_vec(c(NA, 1, 2))), regexp = "NAs in vec")
  expect_error(impose_accumulation_behavior({check_sorted_vec(c(1, 2)); stop("forced error")}), regexp = "forced error")
})

