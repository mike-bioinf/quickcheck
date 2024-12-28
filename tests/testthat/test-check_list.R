### test for the single list functions.


test_that("check_uniform_list works", {
  numeric_list <- purrr::keep(cancer_list, is.numeric)
  character_list <- cancer_list[c("sex", "patient_id")]
  expect_error(object = check_uniform_list(cancer_list), class = "quickalert")
  expect_no_error(check_uniform_list(numeric_list))
  expect_no_error(check_uniform_list(character_list))
})



test_that("check_predicate_list function works", {
  clist <- cancer_list[c("visit_number", "patient_id")]
  expect_snapshot_error(check_predicate_list(x = clist, predicate = is.character))
  expect_snapshot_error(check_predicate_list(x = clist, predicate = is.character, inverse = TRUE))
  names(clist)[1] <- NA
  expect_snapshot_error(check_predicate_list(x = clist, predicate = is.character))
  expect_snapshot_error(check_predicate_list(x = clist, predicate = is.character, inverse = TRUE))
})



test_that("check_length_list works as intented", {
  l1 <- list(1, 2, 3, 4, 4, 4, 4, 4)
  expect_error(
    object = check_length_list(l1, max_len = 5, xarg = "List"),
    regexp = "List of maximum length 5 expected, 8 detected.",
    class = "quickalert"
  )
})
