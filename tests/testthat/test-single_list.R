### test for the single list functions.


test_that("check_uniform_list works", {
  numeric_list <- purrr::keep(cancer_list, is.numeric)
  character_list <- cancer_list[c("sex", "patient_id")]

  expect_error(object = check_uniform_list(cancer_list), class = "quickalert")
  expect_no_error(check_uniform_list(numeric_list))
  expect_no_error(check_uniform_list(character_list))
})



test_that("check_predicate_list function works",
  {
    clist <- cancer_list[c("visit_number", "patient_id")]
    expect_snapshot_error(check_predicate_list(x = clist, predicate = is.character))

    names(clist)[1] <- NA
    expect_snapshot_error(check_predicate_list(x = clist, predicate = is.character))
  }
)
