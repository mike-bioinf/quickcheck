# Checking functions that work on single dataframe.


#' Checks the presence of one or multiple columns in a dataframe
#' @description
#' Checks the presence of a column in a dataframe base on its name and raises error, warning or a message.
#' The default alert message can be modified and the cli syntax can be used
#' (the character vector is passed to cli_bullets).
#'
#' @param df Dataframe passed in the outer function.
#' @param columns Character string reporting the column/s name.
#' @param df_arg String specifying how to address df in the raised messages (default "df").
#' @param raise Character string equal to one of "error", "warning" or "message" (default error).
#'  Set the type of alert that is created.
#' @param alert_message String reporting the alert message. Its formatted by cli_bullets function.
#'  Default NULL, in this case a standard message with the appropriate bullet sign is used.
#' @param n.evaluation_frame numeric, defines the number of calling frame to look up for the evaluation
#'  of the alert message in respect to where the function calling the alert is run.
#'  The default value points to the frame below the function frame (to not modify
#'  if the default alert is desired).
#'  So it's easy to points to upper frames (as well as to below frames but is not recommended).
#'
#' @return NULL.
#' @export
check_columns_presence <- function(df, columns, df_arg = "df", raise = "error", alert_message = NULL, n.evaluation_frame = 2){
  if(is.null(alert_message)){
    alert_message <- c(
      "The following {qty(missing_values)} column{?s} {?is/are} missing in {vec_arg}",
      "{col_magenta(missing_values)}"
    )
  }

  check_presence_values(
    vec = colnames(df),
    values = columns,
    vec_arg = df_arg,
    raise = raise,
    alert_message = alert_message,
    n.evaluation_frame = n.evaluation_frame
  )

  invisible(NULL)
}





#' Checks if the specified column of the dataframe is suitable as key with only unique values.
#' @param key character reporting the column name.
#' @param na.rm logical (default TRUE), indicating if NA must be excluded prior computations.
#' @inheritParams check_columns_presence
#' @return NULL.
#' @export
check_key <- function(df, key, raise = "error", alert_message = NULL, na.rm = TRUE, n.evaluation_frame = 2){
  if(is.null(alert_message)){
    alert_message <- c(
      "x" = "The following {qty(err_value)} value{?s} {?is/are} present {col_red('multiple times')} in {vec_arg}: ",
      "{col_magenta(err_value)}",
      "i" = "Use a different key or filter the duplicated rows."
    )
  }
  check_unique_values(vec = df[[key]], vec_arg = key, raise, alert_message, na.rm, n.evaluation_frame)
  invisible(NULL)
}










#### DA MODIFICARE CREANDO DAPPRIMA LA CONTROPARTE VETTORIALE.=====================================


#' Checks the presence of the specified values in the selected column
#' @inheritParams check_columns_presence
#' @param columns character vector of columns names to check
#' @param col_levels list of character vector reporting the expected levels for each column
#'  specified in columns. The element of the list must be nominated according to the columns values.
#' @return NULL
#' @export
check_columns_levels <- function(df, columns, col_levels, raise = "error", alert_message = NULL, n.evaluation_frame = 2){
  check_required_all()
  check_args_primitive_types("col_levels", "list")
  check_empty_vec(vec = names(col_levels), alert_message = "All elements of col_levels must be nominated")
  check_length_vecs(columns, names(col_levels), vec1_arg = "columns", vec2_arg = "col_levels")

  check_equality_vecs(
    sort(columns),
    sort(names(col_levels)),
    alert_message = c(
      "columns and col_levels names {col_red('not coincide')}: ",
      "i" = "all cols specified in columns must be reported as element names in col_levels"
    )
  )

  err_list <- list()
  for(n in columns){
    miss_values <- dplyr::setdiff(x = col_levels[[n]], y = unique(df[[n]]))
    if(length(miss_values) > 0){
      err_list[[n]] <- miss_values
    }
  }

  if(length(err_list) > 0){
    alert_message <- c("The following levels are missing: \n", err_list)
    alert_generator(raise, alert_message, n.evaluation_frame)
  }

  invisible(NULL)
}




