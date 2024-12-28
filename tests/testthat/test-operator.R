### test on %IN% operator


test_that("The %IN% operator works as expected", {
  v1 <- c("1", "2")
  v2 <- c(1, 2)
  expect_warning(v1 %IN% v2, regexp = "A vector of FALSE is automatically returned")
  expect_type(suppressWarnings(v1 %IN% v2), type = "logical")
})


test_that("The %IN% operator does not raise warning when NULL is involved", {
  expect_no_warning(1 %IN% NULL)
})
