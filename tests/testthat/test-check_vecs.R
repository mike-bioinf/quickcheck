## test for check functions that works on 2 vecs


test_that("check_length_vecs works as expected", {
  v1 <- c(1, 2, NA)
  v2 <- c(2, 2, 1, NA)
  v3 <- c(1, 1)
  expect_no_error(check_length_vecs(v1, v2, unique = TRUE))
  expect_error(check_length_vecs(v1, v3, unique = TRUE), class = "quickalert")
  expect_error(check_length_vecs(v1, v3, na_rm = TRUE, unique = TRUE), class = "quickalert")
})



test_that("check_identical_vecs works as expected", {
  v1 <- c(1, 3, 2, NA)
  v2 <- c(1, 2, 3, NA)
  expect_error(check_identical_vecs(v1, v2), class = "quickalert")
  expect_error(check_identical_vecs(v1, v2, unique = TRUE), class = "quickalert")
  expect_error(check_identical_vecs(v1, v2, na_rm = TRUE), class = "quickalert")
  expect_no_error(check_identical_vecs(v1, v2, sort = TRUE))
})



test_that("check_equality_vecs works as expected", {
  v1 <- c(1, 1)
  v2 <- c(1)
  v3 <- c(1, 1, NA, 2)
  v4 <- c(1L, 1L, NA, 2L)
  v5 <- c(2, 1, 1)
  expect_no_error(check_equality_vecs(v1, v2, recycle = TRUE))
  expect_no_error(check_equality_vecs(v1, v2, unique = TRUE))
  suppressWarnings(expect_no_error(check_equality_vecs(v3, v4, coerce = TRUE)))
  suppressWarnings(expect_error(check_equality_vecs(v3, v5), class = "quickalert"))
  suppressWarnings(expect_no_error(check_equality_vecs(v3, v5, sort = TRUE)))
  # auxiliary checks works as expected
  mes <- "right_frame"
  suppressWarnings(expect_error(check_equality_vecs(v3, v4, alert_message = "{mes}", n_evaluation_frame = 1), regexp = mes, class = "quickalert"))
  expect_error(check_equality_vecs(v1, v2, alert_message = "{mes}", n_evaluation_frame = 1), regexp = mes, class = "quickalert")
  expect_message(check_equality_vecs(v1, v2, raise = "message"), class = "quickalert")
})



test_that("check_matching_vecs works as expected", {
  v1 <- c(1, NA, 2, 3, 4)
  v2 <- c(1, 3, 4, 2)
  v3 <- c("1", NA, "2", "3", "4")
  expect_error(check_matching_vecs(v1, v2), class = "quickalert")
  expect_no_error(check_matching_vecs(v1, v2, na_rm = TRUE))
  expect_no_error(check_matching_vecs(v1, v3, coerce = TRUE))
  # auxiliary check works as expected
  mes <- "right_frame"
  expect_error(check_matching_vecs(v1, v3, alert_message = "{mes}", n_evaluation_frame = 1), regexp = mes, class = "quickalert")
  expect_message(check_matching_vecs(v1, v3, raise = "message"), class = "quickalert")
})
