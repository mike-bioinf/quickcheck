### test for the single list functions.


test_that("if check_uniform_list works", {
  numeric_list <- purrr::keep(cancer_list, is.numeric)
  character_list <- cancer_list[c("sex", "patient_id")]

  expect_error(object = check_uniform_list(cancer_list), class = "quickalert")
  expect_no_error(check_uniform_list(numeric_list))
  expect_no_error(check_uniform_list(character_list))
})



cli::test_that_cli(
  desc = "if check_types_list function works",
  {
    clist <- cancer_list[c("visit_number", "patient_id")]
    expect_snapshot_error(x = check_types_list(x = clist, predicate = is.character))

    names(clist)[1] <- NA
    expect_snapshot_error(x = check_types_list(x = clist, predicate = is.character))
  },
  configs = "ansi"
)
