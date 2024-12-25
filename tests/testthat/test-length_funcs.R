### Test for check_length_.* funcs


test_that("check_length_vec works as intented", {
  v1 <- c(1, 2, 3, 4, 4, 4, 4, 4)
  expect_error(
    object = check_length_vec(vec = v1, exact_len = 2, vec_arg = "Vector"),
    regexp = "Vector of length 2 expected, 8 detected.",
    class = "quickalert"
  )
  expect_error(
    object = check_length_vec(v1, exact_len = 2, alert_message = "expected {length(vec)}"),
    regexp = "expected 8"
  )
})



test_that("check_length_list works as intented", {
  l1 <- list(1, 2, 3, 4, 4, 4, 4, 4)
  expect_error(
    object = check_length_list(l1, max_len = 5, xarg = "List"),
    regexp = "List of maximum length 5 expected, 8 detected.",
    class = "quickalert"
  )
})



test_that("check_columns_number works as intended", {
  expect_error(check_columns_number(qadf, exact_len = 12, min_len = 4, max_len = 22), class = "quickalert")
})
