## tests for impose_logical_behavior


test_that("impose_logical_behavior returns the expected boolean", {
  expect_true(impose_logical_behavior(check_columns_presence(df = qadf, columns = "materasso")))
  expect_false(impose_logical_behavior(check_empty_vec(vec = c(1, 2, 3))))
  # works with message and warnings
  expect_true(impose_logical_behavior(check_empty_vec(NULL, raise = "message")))
  expect_true(impose_logical_behavior(check_empty_vec(NULL, raise = "warning")))
})



test_that("impose_logical_behavior raise errors when non-quickalert errors are encountered", {
  expect_error(
    object = impose_logical_behavior(check_presence_vec(vec = qadf[["sex"]], values = "men", raise = "messagesssss")),
    regexp = '`type` must be one of "error", "warning", or "message", not "messagesssss".'
  )
  expect_error(impose_logical_behavior(check_columns_key(qadf, "visit_numb")), regexp = "The following column")
})



test_that("impose_logical_behavior doesn't capture non-quickalert conditions", {
  expect_warning(test_sorted_vec(vec = c(2, 1, NA)), regexp = "NAs in vec, they are not considered in the sort check.")
})



test_that("impose_logical_behavior force_alert and strip_quickalert parameteres works as intended", {
  expect_message(
    object = impose_logical_behavior(check_columns_key(qadf, columns = c("visit_number"), raise = "message"), force_alert = T),
    class = "quickalert"
  )
  expect_no_condition(impose_logical_behavior(check_columns_presence(qadf, columns = c("visit_number")), force_alert = T))
  # strip_quickalert works
  expect_condition(impose_logical_behavior(check_columns_presence(qadf, columns = c("missing")), force_alert = TRUE), class = "quickalert")
  cnd_classes <- class(rlang::catch_cnd(
    impose_logical_behavior(check_columns_presence(qadf, columns = c("missing")), force_alert = TRUE, strip_quickalert = TRUE)
  ))
  expect_false("quickalert" %in% cnd_classes)
})

