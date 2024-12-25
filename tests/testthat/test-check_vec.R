### test on single vector


test_that("check_duplicate_vec works as expected", {
  expect_no_error(check_duplicate_vec(vec = c(1, 2, 4, 5)))
  v1 <- c(NA, NA, 1, 2, 3)
  expect_no_error(check_duplicate_vec(v1, na_rm = TRUE))
  expect_error(check_duplicate_vec(v1, na_rm = FALSE), class = "quickalert")
})



test_that("check_presence_vec works as expected",{
  value <- "home"
  values <- c("home", "temp")
  na_value <- c("home", NA)
  vec <- c("home", "building", "basefloor")
  expect_no_error(check_presence_vec(vec, value))
  expect_snapshot_error(check_presence_vec(vec, values), class = "quickalert")
  # NA are considered and reported in the alert.
  expect_snapshot_error(check_presence_vec(vec, na_value), class = "quickalert")
})



test_that("test_sorted_vec works as expected", {
  v1 <- c(1, 2, 3, 5)
  expect_no_error(check_sorted_vec(v1, decreasing = FALSE))
  expect_error(check_sorted_vec(v1, decreasing = TRUE), class = "quickalert")
  v1 <- c(1, 2, 3, NA)
  expect_warning(check_sorted_vec(v1), regexp = "NAs in vec, they are not considered in the sort check.")
})



test_that("check_predicate_vec works as expected", {
  v1 <- c(1, 2, 3, 4)
  expect_no_error(check_predicate_vec(v1, predicate = is.numeric))
  expect_error(check_predicate_vec(v1, is.character), regexp = "the missing element names", class = "quickalert")
  names(v1) <- c(1, 2, 3, 4)
  expect_error(check_predicate_vec(v1, is.character), regexp = "the predicate in vec", class = "quickalert")
  expect_no_error(check_predicate_vec(v1, is.character, inverse = TRUE))
})
