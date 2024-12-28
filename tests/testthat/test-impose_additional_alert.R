### tests for impose_additional_alert


test_that("impose_additional_alert works as intended", {
  expect_snapshot({
    impose_additional_alert(
      expr = check_columns_key(qadf, c("sex", "visit_number"), raise = "message"),
      message = c("i" = "additional_part"),
      margin = 1,
      raise = "message"
    )},
    cnd_class = T
  )

  expect_snapshot({
    impose_additional_alert(
      expr = check_columns_key(qadf, c("sex", "visit_number"), raise = "warning", sign = FALSE),
      message = "additional_part",
      margin = 2,
      raise = "warning"
    )},
    cnd_class = T
  )

  expect_no_condition(impose_additional_alert(check_na_vec(1, raise = "message"), message = "additional"))
})



test_that("impose_additional_alert evalution frame mechanisms works", {
  m <- "additional_part"
  expect_error(
    impose_additional_alert(check_na_vec(NA, raise = "message"), message = "{m}", n_evaluation_frame = 1),
    regexp = "additional_part"
  )
})


