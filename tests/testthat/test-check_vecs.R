## test for check functions that works on 2 vecs


test_that("coerce and equality parameters works as expected", {
  v1 <- c(1L, 2L, 3L)
  v2 <- c(1, 2, 3)
  v3 <- c(1, 2, 3, 1, 2, 3)
  expect_false(impose_logical_behavior(check_equality_vecs(v1, v2, coerce = TRUE)))
  expect_error(check_equality_vecs(v1, v2), regexp = "vec1 and vec2 classes are different")
  expect_no_error(check_equality_vecs(v2, v3, recycle = TRUE))
  expect_error(check_identical_vecs(v1, v2), class = "quickalert")
})
