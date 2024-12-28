### test on check_names function


test_that("check_names works as expected", {
  v <- 1:5
  names(v) <- c("south", "north", NA, "", "east")
  l <- as.list(v)
  # errors for incorrect what
  expect_error(check_names(l, what = "rownames"), regexp = "rownames is not found as attribute of x")
  expect_error(check_names(v, what = "colnames"), regexp = "colnames is not found as attribute of x.")
  # check_names recognize different object correctly
  expect_no_error(check_names(qadf, null_check = TRUE))
  expect_no_error(check_names(l, null_check = TRUE))
  expect_no_error(check_names(v, null_check = TRUE))
  # check_names works
  expect_error(check_names(qadf, exclude = "sex"), class = "quickalert")
  expect_error(check_names(l, na_check = TRUE), class = "quickalert")
  expect_error(check_names(v, empty_string_check = TRUE), class = "quickalert")
  expect_no_error(check_names(v, unique = TRUE))
  # alert customization works
  expect_error(check_names(l, xarg = "xcustom", na_check = TRUE), regexp = "xcustom")
})



test_that("test_names works as expected", {
  v <- 1:5
  names(v) <- c("south", "north", NA, "", "east")
  l <- as.list(v)
  expect_error(check_names(l, what = "rownames"), regexp = "rownames is not found as attribute of x")
  expect_error(check_names(v, what = "colnames"), regexp = "colnames is not found as attribute of x.")
  expect_false(test_names(qadf, null_check = TRUE))
  expect_false(test_names(l, null_check = TRUE))
  expect_false(test_names(v, null_check = TRUE))
  expect_true(test_names(qadf, exclude = "sex"))
  expect_true(test_names(l, na_check = TRUE))
  expect_true(test_names(v, empty_string_check = TRUE))
  expect_false(test_names(v, unique = TRUE))
})
