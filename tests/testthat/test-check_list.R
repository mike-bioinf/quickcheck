### test for the single list functions.


test_that("check_list works as expected", {
  expect_error(check_list(x = NULL, null_check = TRUE), class = "quickalert")
  expect_error(check_list(x = list(1, "str"), predicate = is.numeric), class = "quickalert")
  expect_error(check_list(x = list(1, 2), exact_len = 3), class = "quickalert")
  expect_no_error(check_list(x = list(1), null_check = TRUE, zero_len_check = TRUE, uniform = TRUE, predicate = is.numeric, exact_len = 1))
  ## refuses dataframe
  expect_snapshot_error(check_list(qadf))
})



test_that("test_list works as expected", {
  expect_true(test_list(x = NULL, null_check = TRUE))
  expect_true(test_list(x = list(1, "str"), predicate = is.numeric))
  expect_true(test_list(x = list(1, 2), exact_len = 3))
  expect_false(test_list(x = list(1), null_check = TRUE, zero_len_check = TRUE, uniform = TRUE, predicate = is.numeric, exact_len = 1))
})



test_that("check_empty_list works as expected", {
  expect_error(check_empty_list(list()), class = "quickalert")
  expect_error(check_empty_list(NULL), class = "quickalert")
  expect_no_error(check_empty_list(x = list(), null = TRUE, len = FALSE))
  expect_no_error(check_empty_list(x = NULL, null = FALSE, len = FALSE))
  expect_error(check_empty_list(x = 1, null = FALSE, len = FALSE), regexp = "The following expectations are not met")
  ## evaluation mechanisms frame works
  mess <- "custom_error"
  expect_error(check_empty_list(NULL, alert_message = "{mess}", n_evaluation_frame = 1), regexp = "custom_error")
  ## refuses dataframe
  expect_snapshot_error(check_empty_list(qadf))
})



test_that("check_uniform_list works", {
  numeric_list <- purrr::keep(cancer_list, is.numeric)
  character_list <- cancer_list[c("sex", "patient_id")]
  expect_error(object = check_uniform_list(cancer_list), class = "quickalert")
  expect_no_error(check_uniform_list(numeric_list))
  expect_no_error(check_uniform_list(character_list, flatten = TRUE))
  ## refuses dataframe
  expect_snapshot_error(check_uniform_list(qadf))
})



test_that("check_uniform_list frame mechanisms works", {
  mess <- "custom_error"
  expect_error(
    object = check_uniform_list(cancer_list, alert_message = "{mess}", n_evaluation_frame = 1),
    regexp = "custom_error"
  )
})



test_that("check_predicate_list function works", {
  clist <- purrr::list_flatten(cancer_list[c("visit_number", "patient_id")], name_spec = "{outer}${inner}")
  expect_snapshot_error(check_predicate_list(x = clist, predicate = is.character))
  expect_snapshot_error(check_predicate_list(x = clist, predicate = is.character, inverse = TRUE))
  names(clist)[1] <- NA
  expect_snapshot_error(check_predicate_list(x = clist, predicate = is.character))
  expect_snapshot_error(check_predicate_list(x = clist, predicate = is.character, inverse = TRUE))
  ## refuses dataframe
  expect_snapshot_error(check_predicate_list(qadf, predicate = is.numeric))
})



test_that("check_predicate_list evaluation frame mechanisms works", {
  mess <- "predicate not satisfied"
  numeric_list <- purrr::keep(cancer_list, is.numeric)
  expect_error(
    object = check_predicate_list(numeric_list, is.character, alert_message = "{mess}", header = NULL, n_evaluation_frame = 1),
    regexp = "predicate not satisfied"
  )
})



test_that("check_length_list works as intented", {
  l1 <- list(1, 2, 3, 4, 4, 4, 4, 4)
  expect_error(
    object = check_length_list(l1, max_len = 5, xarg = "List"),
    regexp = "List of maximum length 5 expected, 8 detected.",
    class = "quickalert"
  )
  ## refuses dataframe
  expect_snapshot_error(check_length_list(qadf))
})



test_that("check_length_list frame mechanisms works", {
  mess <- "custom error"
  l1 <- list(1, 2, 3, 4, 4, 4, 4, 4)
  expect_error(
    object = check_length_list(l1, max_len = 5, xarg = "List", alert_message = "{mess}", n_evaluation_frame = 1),
    regexp = "custom error"
  )
})
