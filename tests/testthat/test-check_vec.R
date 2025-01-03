### test on single vector


test_that("check_vector works as expected", {
  l <- list(1, 2)
  v <- c("banana", "kiwi", "apple", "strawberry", "ananas")
  v2 <- c(NA, NA, 2, 1, 0)
  expect_error(check_vector(l), class = "quickalert")
  expect_no_error(check_vector(v, predicate = is.character, unique = TRUE, exact_len = 5, min_len = 1, max_len = 6, include = "apple"))
  expect_error(check_vector(v, sorted = TRUE), class = "quickalert")
  expect_error(check_vector(v, include = "home"), class = "quickalert")
  suppressWarnings(expect_no_error(check_vector(v2, sorted = TRUE, decreasing = TRUE, unique = TRUE, na_rm_unique = TRUE)))
  expect_error(check_vector(v2, unique = TRUE, na_rm_unique = FALSE), class = "quickalert")
})



test_that("test_vector works as expected", {
  l <- list(1, 2)
  v <- c("banana", "kiwi", "apple", "strawberry", "ananas")
  v2 <- c(NA, NA, 2, 1, 0)
  expect_true(test_vector(l))
  expect_false(test_vector(v, predicate = is.character, unique = TRUE, exact_len = 5, min_len = 1, max_len = 6, include = "apple"))
  expect_true(test_vector(v, sorted = TRUE))
  expect_true(test_vector(v, include = "home"))
  expect_true(test_vector(v2, unique = TRUE, na_rm_unique = FALSE))
  expect_false(suppressWarnings(test_vector(v2, sorted = TRUE, decreasing = TRUE, unique = TRUE, na_rm_unique = TRUE)))
})



test_that("check_atomic_vec works as expected", {
  expect_no_error(check_atomic_vec(c(1, 2)))
  expect_error(check_atomic_vec(list(1, 2)), class = "quickalert")
  expect_error(check_atomic_vec(factor(1, 2)), class = "quickalert")
  expect_error(check_atomic_vec(matrix(1)), class = "quickalert")
  expect_error(check_atomic_vec(qadf), class = "quickalert")
})



test_that("check_duplicate_vec works as expected", {
  expect_no_error(check_duplicate_vec(vec = c(1, 2, 4, 5)))
  v1 <- c(NA, NA, 1, 2, 3)
  expect_no_error(check_duplicate_vec(v1, na_rm = TRUE))
  expect_error(check_duplicate_vec(v1, na_rm = FALSE), class = "quickalert")
  # evaluation frame mechanisms works
  mes <- "right_frame"
  expect_error(check_duplicate_vec(v1, na_rm = FALSE, alert_message = "{mes}", header = NULL, n_evaluation_frame = 1), regexp = mes)
})



test_that("check_presence_vec works as expected", {
  value <- "home"
  values <- c("home", "temp")
  na_value <- c("home", NA)
  vec <- c("home", "building", "basefloor")
  expect_no_error(check_presence_vec(vec, value))
  expect_snapshot_error(check_presence_vec(vec, values), class = "quickalert")
  # NA are considered and reported in the alert.
  expect_snapshot_error(check_presence_vec(vec, na_value), class = "quickalert")
  # coercion is prevented
  suppressWarnings(expect_snapshot_error(check_presence_vec(vec = c("1", "2"), values = c(1, 2))))
})



test_that("check_sorted_vec works as expected", {
  v1 <- c(1, 2, 3, 5)
  expect_no_error(check_sorted_vec(v1, decreasing = FALSE))
  expect_error(check_sorted_vec(v1, decreasing = TRUE), class = "quickalert")
  v1 <- c(1, 2, 3, NA)
  expect_warning(check_sorted_vec(v1), regexp = "NAs in vec, they are not considered in the sort check.")
  # NULL vector is considered sorted
  expect_no_error(check_sorted_vec(NULL))
  # floats instability is correctly addressed
  v <- c(1.9 - 0.900000000001, 1)
  expect_error(check_sorted_vec(v, decreasing = TRUE, strict = TRUE), class = "quickalert")
  expect_no_error(check_sorted_vec(v, decreasing = TRUE, strict = FALSE))
  # the vector attributes are ignored
  v2 <- stats::na.omit(v1)
  expect_no_error(check_sorted_vec(v2, strict = FALSE))
  # evaluation frame mechanisms works
  mes <- "right_frame"
  suppressWarnings(expect_error(check_sorted_vec(v1, decreasing = TRUE, alert_message = "{mes}", n_evaluation_frame = 1), regexp = mes))
})



test_that("check_predicate_vec works as expected", {
  v1 <- c(1, 2, 3, 4)
  expect_no_error(check_predicate_vec(v1, predicate = is.numeric))
  expect_error(check_predicate_vec(v1, is.character), regexp = "the missing element names", class = "quickalert")
  names(v1) <- c(1, 2, 3, 4)
  expect_error(check_predicate_vec(v1, is.character), regexp = "the predicate in vec", class = "quickalert")
  expect_no_error(check_predicate_vec(v1, is.character, inverse = TRUE))
  # evaluation frame mechanisms works
  mes <- "right_frame"
  expect_error(check_predicate_vec(v1, is.character, header = NULL, alert_message = "{mes}", n_evaluation_frame = 1), regexp = mes)
})



test_that("check_length_vec works as intented", {
  v1 <- c(1, 2, 3, 4, 4, 4, 4, 4)
  expect_error(
    object = check_length_vec(vec = v1, exact_len = 2, vec_arg = "Vector"),
    regexp = "Vector of length 2 expected, 8 detected.",
    class = "quickalert"
  )
  expect_error(check_length_vec(v1, exact_len = 2, alert_message = "expected {length(vec)}"), regexp = "expected 8")
  # evaluation frame mechanisms works
  mes <- "right_frame"
  expect_error(check_length_vec(v1, exact_len = 2, alert_message = "{mes}", n_evaluation_frame = 1), regexp = mes)
})



test_that("check_absence_vec works as intented", {
  v <- c(1, 2, 4)
  expect_error(check_absence_vec(v, 1), class = "quickalert")
  expect_true(test_absence_vec(v, 1))
  expect_no_error(check_absence_vec(v, 10))
  expect_false(test_absence_vec(v, 10))
  expect_warning(check_absence_vec(v, "casa"), regexp = "A vector of FALSE is automatically returned")
  expect_false(suppressWarnings(test_absence_vec(v, "casa")))
})
