# Checking functions that works on 2 dataframes.



#' Checks equal number of rows between two dataframes
#' @param df1 first dataframe.
#' @param df2 second dataframe.
#' @inheritParams check_columns_key
#' @return NULL
#' @export
check_nrow_dfs <- function(df1, df2, raise = "error", alert_message = NULL, n.evaluation_frame = 0, quickalert = TRUE, ...){
  check_required_all()
  check_args_classes(c("df1", "df2"), "data.frame", 2, quickalert = FALSE)

  if(nrow(df1) != nrow(df2)){
    alert_message <- generate_message(alert_message, "{cli::col_red('Different number')} of rows between df1 and df2.")
    alert_generator(raise, alert_message, n.evaluation_frame, quickalert, ...)
  }

  invisible(NULL)
}




#' Checks duplicated column names in 2 dataframes
#' @inheritParams check_nrow_dfs
#' @param columns Character vector of columns to consider. If NULL all columns are considered (default NULL).
#' @param header String to add at the beginning of the alert message. If "default" the default header is used, otherwise the string passed in.
#' @returns NULL
#' @export
check_columns_copresence <- function(df1, df2, columns = NULL, raise = "error", alert_message = NULL, header = "default", n.evaluation_frame = 0, quickalert = TRUE, ...){
  check_required_all()
  check_args_classes(c("df1", "df2"), "data.frame", 2, quickalert = FALSE)
  check_args_primitive_types("columns", "character", null = T, quickalert = FALSE)

  if(!is.null(columns)){
    df1 <- dplyr::select(df1, dplyr::any_of(columns))
    df2 <- dplyr::select(df2, dplyr::any_of(columns))
    if(length(df1) == 0 && length(df2) == 0) cli::cli_abort("{cli::qty(columns)} {? /None of} {columns} {?is not/is} found in df1 and df2.")
  }

  all_cols <- c(colnames(df1), colnames(df2))
  duplicated_cols <- all_cols[duplicated(all_cols)]

  if(length(duplicated_cols) > 0){
    header <- generate_header(header, "The following columns are present in both dataframes:")
    alert_message <- generate_message(alert_message, "{cli::col_red(duplicated_cols)}.")
    alert_generator(raise, alert_message, n.evaluation_frame, quickalert, header = header)
  }

  invisible(NULL)
}




#' Perform an ordered cross-checking between the values of two columns of two dataframes.
#' @description
#' The function allows to check the presence of all values of the selected column of one dataframe
#' in the other. If direction equal 'bidirectional' a bidirectional check is performed.
#' @inheritParams check_nrow_dfs
#' @param columns character vector with the names of the columns to consider. If of length one
#'  the single name is assumed for both dataframes. If of length two the first name is assumed
#'  for the first dataframe and the other for the second.
#' @param direction string equal to one of 'first_in_second', 'second_in_first' or 'bidirectional'.
#'  Set the direction of the comparison.
#' @inheritParams check_columns_key
#' @return NULL
#' @export
check_presence_dfs <- function(df1, df2, columns, direction = "first_in_second", raise = "error", alert_message = NULL, n.evaluation_frame = 0, quickalert = TRUE, ...){
  check_required_all()
  check_args_classes(c("df1", "df2"), "data.frame", 2, quickalert = FALSE)
  rlang::arg_match(arg = direction, values = c("first_in_second", "second_in_first", "bidirectional"), multiple = FALSE)
  check_col_arg(df1, df2, columns)

  colnames(df2)[colnames(df2) == columns[2]] <- columns[1]
  col <- columns[1]
  missing_values1 <- NULL
  missing_values2 <- NULL
  header1 <- NULL
  header2 <- NULL

  if(direction == "first_in_second" || direction == "bidirectional"){
    missing_values2 <- setdiff(x = df1[[col]], y = df2[[col]])
  }

  if(direction == "second_in_first" || direction == "bidirectional"){
    missing_values1 <- setdiff(x = df2[[col]], y = df1[[col]])
  }

  if(!is.null(missing_values2)){
    header2 <- "The following {qty(missing_values2)} value{?s} {?is/are} {cli::col_red('missing')} in df2:"
  }

  if(!is.null(missing_values1)){
    header1 <- "The following {qty(missing_values1)} value{?s} {?is/are} {cli::col_red('missing')} in df1:"
    missing_values1 <- c(missing_values1, "\n")
  }

  if(!is.null(missing_values1) || !is.null(missing_values2)){
    alert_message <- c(header1, missing_values1, header2, missing_values2)
    alert_generator(raise, alert_message, n.evaluation_frame, quickalert, ...)
  }

  invisible(NULL)
}






### HELPER ====================================================================================================================================================

#' Helper of 2 dfs checking functions that checks the different columns argument scenario.
#' @inheritParams check_presence_dfs
check_col_arg <- function(df1, df2, columns){
  if(length(columns) > 2){
    cli::cli_abort(c("x" = "columns argument must be of length 1 or 2."))
  }

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

  invisible(NULL)
}

