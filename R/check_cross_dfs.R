# Checking functions that works on 2 dataframes.


#' Check equal number of rows between two dataframes
#' @param df1 first dataframe.
#' @param df2 second dataframe.
#' @param df1_arg String specifying how to address first df in the raised messages (default "df1").
#' @param df2_arg String specifying how to address second df in the raised messages (default "df2").
#' @inheritParams check_columns_key
#' @inherit check_atomic_vec return
#' @export
check_nrow_dfs <- function(df1, df2, df1_arg = "df1", df2_arg = "df2", raise = "error", alert_message = NULL, n_evaluation_frame = 0, quickalert = TRUE, ...){
  check_required_all()
  
  check_args(
    args = c("df1", "df2", "df1_arg", "df2_arg"),
    expected_types = c("data.frame", "character"),
    flag = c(FALSE, FALSE, TRUE, TRUE),
    with = "class",
    recycle_expected_types = c(2, 2), 
    quickalert = FALSE
  )

  if(nrow(df1) != nrow(df2)){
    alert_message <- generate_message(alert_message, "{cli::col_red('Different number')} of rows between {df1_arg} and {df2_arg}.")
    alert_generator(raise, alert_message, n_evaluation_frame, quickalert, ...)
  }

  invisible(NULL)
}



#' Check duplicated column names in two dataframes
#' @inheritParams check_nrow_dfs
#' @param columns Character vector of columns to consider. If NULL, the default, all columns are considered. No check is done for not existent columns.
#' @param header String to add at the beginning of the alert message. If "default" the default header is used, otherwise the string passed in.
#' @details 
#' The function does not check for the existence of the passed columns value. 
#' Therefore is possible to pass names not found in both dataframes and no error is raised.
#' @inherit check_atomic_vec return
#' @export
check_columns_copresence <- function(df1, df2, columns = NULL, df1_arg = "df1", df2_arg = "df2", raise = "error",
                                     alert_message = NULL, header = "default", n_evaluation_frame = 0, quickalert = TRUE, ...){
  check_required_all()
  
  check_args(
    args = c("df1", "df2", "columns", "df1_arg", "df2_arg"),
    expected_types = c("data.frame", "character"),
    flag = c(FALSE, FALSE, FALSE, TRUE, TRUE),
    null = c(FALSE, FALSE, TRUE, FALSE, FALSE),
    with = "class",
    recycle_expected_types = c(2, 3),
    quickalert = FALSE
  )

  if(!is.null(columns)){
    df1 <- dplyr::select(df1, dplyr::any_of(columns))
    df2 <- dplyr::select(df2, dplyr::any_of(columns))
    if(length(df1) == 0 || length(df2) == 0) return(invisible(NULL))
  }

  all_cols <- c(colnames(df1), colnames(df2))
  duplicated_cols <- all_cols[duplicated(all_cols)]
  n_duplicated_cols <- length(duplicated_cols)

  if(n_duplicated_cols > 0){
    header <- generate_header(header, "The following {cli::qty(n_duplicated_cols)} column{?s} {?is/are} present in both dataframes:")
    alert_message <- generate_message(alert_message, "{cli::col_red(duplicated_cols)}.")
    alert_generator(raise, alert_message, n_evaluation_frame, quickalert, header = header, ...)
  }

  invisible(NULL)
}



#' Perform an oriented check on the values of two columns of two dataframes
#' @description
#' The function allows to check the presence of all values of the selected column of one dataframe
#' in the other. If direction equal 'bidirectional' a bidirectional check is performed.
#' @inheritParams check_nrow_dfs
#' @param columns
#'  Character vector with the names of the columns to consider. If of length one
#'  the single name is assumed for both dataframes. If of length two the first name is assumed
#'  for the first dataframe and the other for the second.
#' @param direction
#'  String equal to one of 'first_in_second', 'second_in_first' or 'bidirectional'. Set the direction of the comparison.
#' @param coerce
#'  Boolean indicating whether the 2 columns can be coerced during the check (default FALSE).
#' @inheritParams check_columns_key
#' @inherit check_atomic_vec return
#' @export
check_presence_dfs <- function(df1, df2, columns, coerce = FALSE, direction = "first_in_second", df1_arg = "df1", df2_arg = "df2", raise = "error",
                               alert_message = NULL, n_evaluation_frame = 0, quickalert = TRUE, ...){
  check_required_all()
  
  check_args(
    args = c("df1", "df2", "columns", "coerce", "df1_arg", "df2_arg"),
    expected_types = c("data.frame", "character", "logical", "character"),
    flag = c(rep(FALSE, 3), rep(TRUE, 3)),
    with = "class",
    recycle_expected_types = c(2, 1, 1, 2),
    quickalert = FALSE
  )

  rlang::arg_match(arg = direction, values = c("first_in_second", "second_in_first", "bidirectional"), multiple = FALSE)
  check_length_vec(columns, min_len = 1, max_len = 2, quickalert = FALSE)
  check_col_arg(df1, df2, columns)

  if(length(columns) == 2){
    df2 <- dplyr::rename(df2, !!columns[1] := dplyr::all_of(columns[2]))
  }

  col <- columns[1]

  if(!coerce){
    internal_check_identical_vecs(
      vec1 = class(df1[[col]]),
      vec2 = class(df2[[col]]),
      n_evaluation_frame = 1,
      quickalert = FALSE,
      alert_message = c(
        "{columns[1]} and {columns[2]} are of {cli::col_red('different classes')}!",
        "i" = " Set the 'coerce' argument to TRUE to enable coercion."
      )
    )
  }

  missing_values1 <- header1 <- final_alert1 <- NULL
  missing_values2 <- header2 <- final_alert2 <- NULL

  if(direction == "first_in_second" || direction == "bidirectional"){
    missing_values2 <- setdiff(x = df1[[col]], y = df2[[col]])
  }

  if(direction == "second_in_first" || direction == "bidirectional"){
    missing_values1 <- setdiff(x = df2[[col]], y = df1[[col]])
  }

  if(!is_empty_vec(missing_values2, na = FALSE, empty_string = FALSE)){
    number_missing2 <- length(missing_values2)
    header2 <- "The following {cli::qty(length(number_missing2))} value{?s} {?is/are} {cli::col_red('missing')} in {df2_arg}:"
    final_alert2 <- c(header2, paste0(missing_values2, collapse = ", "))
  }

  if(!is_empty_vec(missing_values1, na = FALSE, empty_string = FALSE)){
    number_missing1 <- length(missing_values1)
    header1 <- "The following {cli::qty(length(number_missing1))} value{?s} {?is/are} {cli::col_red('missing')} in {df1_arg}:"
    final_alert1 <- c(header1, paste0(missing_values1, collapse = ", "))
  }

  if(!is_empty_vec(c(missing_values1, missing_values2))){
    alert_message <- generate_message(alert_message, c(final_alert1, final_alert2))
    alert_generator(raise, alert_message, n_evaluation_frame, quickalert, ...)
  }

  invisible(NULL)
}





### HELPER -----------------------------------------------------------------------------------------------------------------------------------------

#' Helper of 2 dfs checking functions that checks the different columns argument scenario.
#' @inheritParams check_presence_dfs
check_col_arg <- function(df1, df2, columns){
  if(length(columns) == 1){
    impose_loop_behavior(
      x = list(df1 = df1, df2 = df2),
      check_func = check_columns_presence,
      check_arg_list = list(columns = columns),
      header = NULL,
      alert_message = "{check_arg_list[['columns']]} is {cli::col_red('not found')} in {cli::col_magenta(failed_elements)}.",
      quickalert = FALSE
    )
  } else {
    check_columns_presence(df1, columns = columns[1], df_arg = "df1", quickalert = FALSE)
    check_columns_presence(df2, columns = columns[2], df_arg = "df2", quickalert = FALSE)
  }
}
