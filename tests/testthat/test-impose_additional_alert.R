### tests for impose_additional_alert



test_that("impose_additional_alert works as intended", {
  expect_snapshot({
    impose_additional_alert(
      expr = check_columns_key(df, c("sex", "visit_number"), raise = "message"),
      message = c("i" = "additional_part"),
      margin = 1,
      raise = "message"
    )},
    cnd_class = T
  )

  expect_snapshot({
    impose_additional_alert(
      expr = check_columns_key(df, c("sex", "visit_number"), raise = "warning"),
      message = "additional_part",
      margin = 2,
      raise = "warning"
    )},
    cnd_class = T
  )
})
