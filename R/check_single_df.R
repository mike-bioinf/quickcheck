# Checking functions that work on single dataframe.


#' Checks the presence of one or multiple columns in a dataframe
#' @description
#' Checks the presence of a column in a dataframe base on its name and raises error, warning or a message.
#' The default alert message can be modified and the cli syntax can be used
#' (the character vector is passed to cli_bullets).
#'
#' @param df dataframe.
#' @param columns character string reporting the column/s name.
#' @param df_arg string specifying how to address df in the raised messages (default "df").
#' @param raise character string equal to one of "error", "warning" or "message" (default error).
#'  Set the type of alert that is created.
#' @param alert_message string reporting the alert message. Its formatted by cli_bullets function.
#'  Default NULL, in this case a standard message with the appropriate bullet sign is used.
#' @return NULL.
#' @export
check_columns_presence <- function(df, columns, df_arg = "df", raise = "error", alert_message = NULL){
  rlang::arg_match(arg = raise, values = c("error", "warning", "message"), multiple = F)
  absent_cols <- columns[!columns %in% colnames(df)]

  if(length(absent_cols) == 0){
    return(NULL)
  }

  if(is.null(alert_message)){
    alert_message <- c(
      "The following {qty(absent_cols)} column{?s} {?is/are} {col_red('not found')} in {df_arg}: ",
      "{col_magenta(absent_cols)}"
    )
  }

  alert_generator(type = raise, alert_message = alert_message)
  return(NULL)
}





#' Checks if the specified column of the dataframe is suitable as key with only unique values.
#' @param key character reporting the column name.
#' @inheritParams check_columns_presence
#' @return NULL.
#' @export
check_key <- function(df, key, raise = "error", alert_message = NULL){
  value_freqs <- table(df[[key]])
  if(!all(value_freqs == 1)){
    err_value <- names(value_freqs[value_freqs != 1])
    if(is.null(alert_message)){
      alert_message <-  c(
        "x" = "The following {qty(err_value)} value{?s} {?is/are} present {col_red('multiple times')} in {key}: ",
        "{col_magenta(err_value)}",
        "i" = "Use a different key or filter the duplicated rows."
      )
    }
    alert_generator(raise, alert_message)
  }
  return(NULL)
}
