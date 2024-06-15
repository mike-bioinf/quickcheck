# Checking functions that works on 2 dataframes.


#' Checks equal number of rows between two dataframes
#' @param df1 first dataframe.
#' @param df2 second dataframe.
#' @param df1_arg string specifying how to address df1 in the raised messages (default "df1").
#' @param df2_arg string specifying how to address df2 in the raised messages (default "df2").
#' @inheritParams check_columns_presence
#' @return NULL
#' @export
check_nrow_dfs <- function(df1, df2, df1_arg = "df1", df2_arg = "df2", raise = "error", alert_message = NULL, n.evaluation_frame = 2, quickalert = TRUE){
  if(nrow(df1) != nrow(df2)){
    alert_message <- generate_message(alert_message, "{col_red('Different number')} of rows between {df1_arg} and {df2_arg}")
    alert_generator(raise, alert_message, n.evaluation_frame, quickalert)
  }
  invisible(NULL)
}






#' Perform an ordered cross-checking between the values of two columns of two dataframes.
#' @description
#' The function allows to check the presence of all values of the selected column of one dataframe
#' in the other. If direction equal 'bidirectional' a bidirectional check is performed.
#'
#' @param df1 first dataframe.
#' @param df2 second dataframe.
#' @param col character vector referring the name of the columns to consider. If of length one
#'  the single name is assumed for both dataframes. If of length two the first name is assumed
#'  for the first dataframe and the other for the second.
#' @param direction string equal to one of 'first_in_second', 'second_in_first' or 'bidirectional'.
#'  Set the direction of the comparison.
#' @inheritParams check_columns_presence
#' @return NULL
#' @export
check_copresence_dfs <- function(df1, df2, col, direction = "first_in_second", raise = "error", alert_message = NULL, n.evaluation_frame = 2){
  check_required_all()
  check_args_classes(args = c("df1", "df2"), expected_classes = c("data.frame"), 2)
  rlang::arg_match(arg = direction, values = c("first_in_second", "second_in_first", "bidirectional"), multiple = FALSE)

  if(length(col) > 2){
    cli::cli_abort(c("x" = "col argument must be of length 1 or 2"))
  }

  if(length(col) == 2){
    colnames(df2)[colnames(df2) == col[2]] <- col[1]
    col <- col[1]
  }

  if(direction == "second_in_first"){
    new_df1 <- df2
    new_df2 <- df1
    df1_ori <- "df2"
    df2_ori <- "df1"
  } else {
    new_df1 <- df1
    new_df2 <- df2
    df1_ori <- "df1"
    df2_ori <- "df2"
  }


  if(direction != "bidirectional"){
    log_comparisons1 <- new_df1[[col]] %in% new_df2[[col]]
    log_comparisons2 <- TRUE
  } else {
    log_comparisons1 <- new_df1[[col]] %in% new_df2[[col]]
    log_comparisons2 <- new_df2[[col]] %in% new_df1[[col]]
  }

  if(!all(log_comparisons1) || !all(log_comparisons2)){
    miss_values1 <- new_df1[[col]][!log_comparisons1]
    if(is.null(alert_message)){
      alert_message <- c(
        "The following {qty(miss_values1)} value{?s} of {col} {qty(miss_values1)} {?is/are} {col_red('missing')} in {df2_ori} but present in {df1_ori}: ",
        "{col_magenta(miss_values1)}"
      )
    }

    if(direction == "bidirectional"){
      miss_values2 <- new_df2[[col]][!log_comparisons2]
      if(is.null(alert_message)){
        alert_message2 <- c(
          "The following {qty(miss_values2)} value{?s} of {col} {qty(miss_values2)} {?is/are} {col_red('missing')} in {df1_ori} but present in {df2_ori}: ",
          "{col_magenta(miss_values2)}"
        )
      } else {alert_message2 <- NULL}
    } else {
      miss_values2 <- NULL
    }

    if(length(miss_values2) > 0){
      alert_message <- c(alert_message, "\n", alert_message2)
    }

    alert_generator(raise, alert_message, n.evaluation_frame)
  }

  invisible(NULL)
}

