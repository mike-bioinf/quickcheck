### test on single dataframe


test_that("check_columns_presence works as intented", {
  expect_no_error(check_columns_presence(qadf, columns = c("sex", "age")))
  expect_snapshot_error(check_columns_presence(qadf, columns = "agee"), class = "quickalert")
  # check pluralization
  expect_snapshot_error(check_columns_presence(qadf, c("agee", "agee2")), class = "quickalert")
})



test_that("check_columns_key works as intented", {
  expect_no_error(check_columns_key(qadf, columns = "sample_date"))
  expect_snapshot_error(check_columns_key(qadf, c("height", "sex")), class = "quickalert")
})



test_that("check_columns_levels works as intented", {
  expect_error(check_columns_levels(qadf, columns = "sex", col_levels = c(sex = "male")), regexp = "The following argument doesn't have")
  expect_error(check_columns_levels(qadf, columns = "sex", col_levels = list("male")), regexp = "All elements of col_levels must be nominated.")
  expect_error(check_columns_levels(qadf, "sex", list(sexx = "male")), regexp = "All cols specified in columns must be reported in col_levels.")
  expect_no_error(check_columns_levels(qadf, "sex", list(sex = "male")))
  suppressWarnings({
    expect_snapshot_error(check_columns_levels(qadf, c("sex", "visit_number"), col_levels = list(sex = "M", visit_number = "5")))
  })
  expect_snapshot_error(check_columns_levels(qadf, c("sex", "visit_number"), col_levels = list(sex = "M", visit_number = 5)))
})



test_that("check_columns_na works as intented", {
  expect_no_error(check_columns_na(qadf, columns = "sex"))
  qadf$sex[1] <- NA
  expect_error(check_columns_na(qadf, "sex"), class = "quickalert")
})



test_that("check_columns_predicate works as intented", {
  numeric_qadf <- purrr::keep(qadf, is.numeric)
  expect_error(check_columns_predicate(qadf, is.numeric), class = "quickalert")
  expect_no_error(check_columns_predicate(numeric_qadf, is.numeric))
  expect_error(check_columns_predicate(numeric_qadf, is.numeric, inverse = TRUE), class = "quickalert", regexp = "inverse of")
})



test_that("check_columns_number works as intended", {
  expect_error(check_columns_number(qadf, exact_len = 12, min_len = 4, max_len = 22), class = "quickalert")
})

