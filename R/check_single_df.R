# Checking functions that work on single dataframe.


#' Checks the presence of one or multiple columns in a dataframe
#'
#' @description
#' Checks the presence of columns in a dataframe base on strings and raises an error, warning or a message.
#'
#' @param df Dataframe passed in the outer function.
#' @param columns Character string reporting the column/s name.
#' @param df_arg String specifying how to address df in the raised messages (default "df").
#' @param raise Character string equal to one of "error", "warning", "message" or "accumulate_message"
#' (default error). Set the type of alert that is created. Note: 'accumulate_message' can be most of
#' times ignored because it is used internally in combination with impose_accumulate_behavior.
#' Either the case it raises a message without the "i" bullet.
#' @param alert_message String reporting the alert message. Its formatted by cli_bullets function.
#'  Default NULL, in this case a standard message with the appropriate bullet sign is used.
#' @param n.evaluation_frame numeric, defines the number of calling frame to look up for the evaluation
#'  of the alert message in respect to where the function calling the alert is run.
#'  The default value points to the frame below the function frame (to not modify
#'  if the default alert is desired). So to point to the calling frame of this function you have to set 4.
#' @param ... not of direct use.
#'
#' @return NULL.
#' @export
check_columns_presence <- function(df, columns, df_arg = "df", raise = "error", alert_message = NULL, n.evaluation_frame = 2,...){
  if(is.null(alert_message)){
    alert_message <- c(
      "The following {qty(missing_values)} column{?s} {?is/are} {col_red('missing')} in {vec_arg}:",
      "{col_magenta(missing_values)}"
    )
  }

  check_presence_values(
    vec = colnames(df),
    values = columns,
    vec_arg = df_arg,
    raise = raise,
    alert_message = alert_message,
    n.evaluation_frame = n.evaluation_frame,
    ...
  )

  invisible(NULL)
}





#' Checks if the specified dataframe columns are suitable as keys of only unique values.
#' @param keys character vector reporting the names of the columns to test as key.
#' @param na.rm logical (default TRUE), indicating if NA must be excluded prior computations.
#' @inheritParams check_columns_presence
#' @return NULL.
#' @export
check_columns_key <- function(df, keys, raise = "error", alert_message = NULL, na.rm = TRUE, n.evaluation_frame = 2){
  check_required_all()

  check_presence_values(
    vec = colnames(df),
    values = keys,
    vec_arg = "df",
    alert_message = c("The following {qty(missing_values)} key{?s} {?is/are} {col_red('missing')} in {vec_arg}:", "{col_magenta(missing_values)}"),
    quickalert = FALSE
  )

  if(is.null(alert_message)){
    alert_message <- "{vec_arg} --> {col_magenta(err_value)}"
  }

  impose_accumulation_behavior(
    header = "The following values occur {col_red('multiple times')} for the following columns:",
    type = raise,
    expr = {
      for(n in keys){
        check_unique_values(
          vec = df[[n]],
          raise = "accumulate_message",
          vec_arg = n,
          alert_message = alert_message,
          na.rm = na.rm,
          n.evaluation_frame = n.evaluation_frame
      )}
    }
  )

  invisible(NULL)
}






#' Checks the presence of the specified values in the selected columns.
#' @inheritParams check_columns_presence
#' @param columns character vector of columns names to check
#' @param col_levels list of character vector reporting the expected levels for each column
#'  specified in columns. The element of the list must be nominated according to the columns values.
#' @return NULL
#' @export
check_columns_levels <- function(df, columns, col_levels, raise = "error", alert_message = NULL, n.evaluation_frame = 2){
  check_required_all()
  check_args_primitive_types("col_levels", "list")
  check_columns_presence(df, columns, quickalert = FALSE)
  check_empty_vec(vec = names(col_levels), alert_message = "All elements of col_levels must be nominated", quickalert = FALSE)
  check_length_vecs(columns, names(col_levels), vec1_arg = "columns", vec2_arg = "col_levels", quickalert = FALSE)

  check_equality_vecs(
    vec1 = sort(columns),
    vec2 = sort(names(col_levels)),
    alert_message = c(
      "columns and col_levels names {col_red('not coincide')}: ",
      "i" = "All cols specified in columns must be reported as element names in col_levels."
    ),
    quickalert = FALSE
  )

  impose_accumulation_behavior(
    type = raise,
    header = "The following levels are {col_red('missing')} from the reported columns:",
    expr = for(n in columns){
      check_presence_values(
        vec = df[[n]],
        values = col_levels[[n]],
        vec_arg = n,
        alert_message = "{vec_arg} --> {col_magenta(missing_values)}",
        raise = "accumulate_message",
        n.evaluation_frame = n.evaluation_frame
      )
    }
  )

  invisible(NULL)
}





#' Checks if all columns satisfy the predicate.
#' @inheritParams check_columns_presence
#' @param predicate function that works on vectors and return a single logical value.
#' @param inverse logical, whether to invert the check direction in the sense that the predicate
#'  must be not satisfied for all columns (default FALSE).
#' @return NULL
#' @export
check_columns_predicate <- function(df, predicate, inverse = FALSE, raise = "error", alert_message = NULL, n.evaluation_frame = 2){
  check_required_all()
  check_args_classes(args = c("df", "predicate"), expected_classes = c("data.frame", "function"))
  logical_vec <- purrr::map_lgl(df, ~ predicate(.x))

  if(inverse){
    logical_vec <- !logical_vec
  }

  false_col <- colnames(df)[!logical_vec]

  if(length(false_col) > 0){
    if(is.null(alert_message)){
      alert_message <- c(
        "The predicate {col_red('returned FALSE')} for the following {qty(false_col)} column{?s}:", "{col_magenta(false_col)}")
    }
    alert_generator(raise, alert_message, n.evaluation_frame)
  }

  invisible(NULL)
}

