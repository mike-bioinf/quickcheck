### test on single dataframe


test_that("check_dataframe works as intended", {
  expect_error(check_dataframe(df = NULL, dim_check = TRUE), class = "quickalert")
  expect_error(check_dataframe(df = data.frame(), dim_check = TRUE), class = "quickalert")
  expect_error(check_dataframe(df = qadf, df_arg = "qadf", n_exact_cols = 12), class = "quickalert")
  expect_error(check_dataframe(qadf, predicate = is.numeric), class = "quickalert")
  expect_no_error(check_dataframe(qadf, columns = "sex"))
  expect_no_error(check_dataframe(qadf, columns = "sex", cols_na = FALSE))
})



test_that("test_dataframe works as expected", {
  expect_true(test_dataframe(df = NULL, dim_check = TRUE))
  expect_true(test_dataframe(df = data.frame(), dim_check = TRUE))
  expect_true(test_dataframe(df = qadf, df_arg = "qadf", n_exact_cols = 12))
  expect_true(test_dataframe(qadf, predicate = is.numeric))
  expect_false(test_dataframe(qadf, columns = "sex"))
  expect_false(test_dataframe(qadf, columns = "sex", cols_na = FALSE))
  expect_error(test_dataframe(list()), regexp = "The following expectations are not met")
})



test_that("check_empty_df works as intended", {
  expect_error(check_empty_df(data.frame()), class = "quickalert")
  expect_error(check_empty_df(NULL),class = "quickalert")
  expect_error(check_empty_df(NULL, dim = TRUE, null = FALSE), class = "quickalert")
  expect_no_error(check_empty_df(qadf))
  expect_no_error(check_empty_df(NULL, null = FALSE, dim = FALSE))
  expect_no_error(check_empty_df(data.frame(), dim = FALSE))
  # check evaluation frame mechanisms
  mes <- "custom_error"
  expect_error(check_empty_df(NULL, alert_message = "{mes}", n_evaluation_frame = 1), regexp = mes)
})



test_that("check_columns_presence works as intented", {
  expect_no_error(check_columns_presence(qadf, columns = c("sex", "age")))
  expect_snapshot_error(check_columns_presence(qadf, columns = "agee"), class = "quickalert")
  # check pluralization
  expect_snapshot_error(check_columns_presence(qadf, c("agee", "agee2")), class = "quickalert")
  # check evaluation frame mechanisms
  mes <- "custom_error"
  expect_error(
    object = check_columns_presence(qadf, "ageee", alert_message = "{mes}", header = NULL, n_evaluation_frame = 1),
    regexp = mes
  )
})



test_that("check_columns_key works as intented", {
  expect_no_error(check_columns_key(qadf, columns = "sample_date"))
  expect_snapshot_error(check_columns_key(qadf, c("height", "sex")), class = "quickalert")
  # check evaluation frame mechanisms
  mes <- "custom_error"
  expect_error(
    object = check_columns_key(qadf, "height", alert_message = "{mes}", header = NULL, n_evaluation_frame = 1),
    regexp = "custom_error"
  )
})



test_that("check_columns_levels works as intented", {
  expect_error(check_columns_levels(qadf, columns = "sex", col_levels = c(sex = "male")), regexp = "col_levels) ACTUAL = character | EXPECTED = list")
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
  # check evaluation frame mechanisms
  mes <- "custom_error"
  expect_error(check_columns_na(qadf, "sex", alert_message = "{mes}", n_evaluation_frame = 1), regexp = mes)
})



test_that("check_columns_predicate works as intented", {
  numeric_qadf <- purrr::keep(qadf, is.numeric)
  expect_error(check_columns_predicate(qadf, is.numeric), class = "quickalert")
  expect_no_error(check_columns_predicate(numeric_qadf, is.numeric))
  expect_error(check_columns_predicate(numeric_qadf, is.numeric, inverse = TRUE), class = "quickalert", regexp = "inverse of")
  # check evaluation frame mechanisms
  mes <- "custom_error"
  expect_error(
    object = check_columns_predicate(qadf, predicate = is.numeric, alert_message = "{mes}", header = NULL, n_evaluation_frame = 1),
    regexp = "custom_error"
  )
})



test_that("check_columns_number works as intended", {
  expect_no_error(check_columns_number(qadf, exact_len = 20, min_len = 10, max_len = 30))
  expect_error(check_columns_number(qadf, exact_len = 11, min_len = 30, max_len = 10), class = "quickalert")
  # check evaluation frame mechanisms
  mes <- "custom_error"
  expect_error(check_columns_number(qadf, exact_len = 10, alert_message = "{mes}", n_evaluation_frame = 1), regexp = "custom_error")
})
