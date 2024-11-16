### Checking functions that work on a single dataframe.


#' Checks the presence of one or multiple columns in a dataframe
#' @param df Dataframe passed in the outer function.
#' @param columns Character string reporting the column/s name.
#' @param df_arg String specifying how to address df in the raised messages (default "df").
#' @param raise
#'  Character string equal to one of "error", "warning" or "message" (default error). Set the type of alert that is created.
#' @param alert_message
#'  Character vector reporting the alert message. Default NULL, in this case a standard message is used.
#'  It's also possible to pass a list of strings that is displayed as a nominated or numbered list.
#' @param header
#'  Character string to add at the beginning of the alert message. If "default" the default header is used, otherwise the string passed in.
#' @param n.evaluation_frame
#'  numeric, defines the number of stack frame to look down for the evaluation of the glue expressions of the alert message.
#'  The default value (0) points to the frame above this function frame. So to point to the frame below this function frame you have to set 2.
#' @param quickalert logical, whether the raised alert has to be of class "quickalert".
#' @param ... To pass additional argument to alert_generator function.
#' @return NULL.
#' @export
check_columns_presence <- function(df, columns, df_arg = "df", raise = "error", alert_message = NULL, header = "default", n.evaluation_frame = 0, quickalert = TRUE, ...){
  check_required_all()
  header <- generate_header(header, "The following {qty(missing_values)} column{?s} {?is/are} {col_red('missing')} in {vec_arg}:")

  check_presence_values(
    vec = colnames(df),
    values = columns,
    vec_arg = df_arg,
    raise = raise,
    alert_message = alert_message,
    header = header,
    n.evaluation_frame = n.evaluation_frame,
    quickalert = quickalert,
    ...
  )

  invisible(NULL)
}



#' Checks if the specified dataframe columns are suitable as keys of only unique values.
#' @param na.rm logical (default TRUE), indicating if NA must be excluded prior evaluation.
#' @param n.evaluation_frame
#'  Numeric, defines the number of stack frame to look down for the evaluation of the glue expressions of the alert message.
#'  The default (0) points to this function frame.
#' @inheritParams check_columns_presence
#' @return invisible NULL.
#' @export
check_columns_key <- function(df, columns, na.rm = TRUE, raise = "error", alert_message = NULL, header = "default", n.evaluation_frame = 0, quickalert = TRUE, ...){
  check_required_all()
  check_columns_presence(df, columns, quickalert = FALSE)
  header <- generate_header(header, "The following values occur {cli::col_red('multiple times')} for the following columns:")

  impose_accumulation_behavior(
    raise = raise,
    alert_message = alert_message,
    header = header,
    quickalert = quickalert,
    n.evaluation_frame = n.evaluation_frame + 1,
    ...,
    expr = {
      for(n in columns){
        check_unique_values(
          vec = df[[n]],
          vec_arg = n,
          na.rm = na.rm,
          raise = "message",
          alert_message = "{vec_arg} --> {cli::col_magenta(err_value)}",
          header = NULL,
          sign = FALSE
      )}
    }
  )

  invisible(NULL)
}



#' Checks the presence of the specified values in the selected columns.
#' @inheritParams check_columns_key
#' @param col_levels list of character vector reporting the expected levels for each column specified in columns.
#'  The element of the list must be nominated according to the columns names which they refer.
#' @return NULL
#' @export
check_columns_levels <- function(df, columns, col_levels, raise = "error", alert_message = NULL, header = "default", n.evaluation_frame = 0, quickalert = TRUE, ...){
  check_required_all()
  check_args_primitive_types("col_levels", "list", quickalert = FALSE)
  check_columns_presence(df, columns, quickalert = FALSE)
  names_col_levels <- names(col_levels)
  check_empty_vec(names_col_levels, alert_message = "All elements of col_levels must be nominated.", quickalert = FALSE)
  check_length_vecs(columns, names_col_levels, vec1_arg = "columns", vec2_arg = "col_levels", quickalert = FALSE)

  check_equality_vecs(
    vec1 = sort(columns),
    vec2 = sort(names_col_levels),
    alert_message = c(
      "columns and col_levels names {cli::col_red('are not the same')}.",
      "i" = "All cols specified in columns must be reported in col_levels."
    ),
    quickalert = FALSE
  )

  header <- generate_header(header, "The following levels are {cli::col_red('missing')} from the reported columns:")

  impose_accumulation_behavior(
    raise = raise,
    alert_message = alert_message,
    header = header,
    quickalert = quickalert,
    n.evaluation_frame = n.evaluation_frame + 1,
    ...,
    expr = for(n in columns){
      check_presence_values(
        vec = df[[n]],
        values = col_levels[[n]],
        vec_arg = n,
        alert_message = "{vec_arg} --> {cli::col_magenta(missing_values)}",
        header = NULL,
        raise = "message",
        sign = FALSE
      )
    }
  )

  invisible(NULL)
}



#' Checks the presence of NAs in the specified columns
#' @inheritParams check_columns_key
#' @return invisible NULL
#' @export
check_columns_na <- function(df, columns, raise = "error", alert_message = NULL, header = "default", n.evaluation_frame = 0, quickalert = TRUE, ...){
  check_required_all()
  check_args_classes("columns", "character", quickalert = FALSE)
  check_columns_presence(df, columns, quickalert = FALSE)

  header <- generate_header(header, "The following columns {cli::col_red('present NAs')}:")

  impose_accumulation_behavior(
    raise = raise,
    alert_message = alert_message,
    header = header,
    quickalert = quickalert,
    n.evaluation_frame = n.evaluation_frame + 1,
    ...,
    expr = for(col in columns){
      check_na_vec(
        vec = df[[col]],
        vec_arg = col,
        raise = "message",
        alert_message = "{vec_arg}",
        header = NULL,
        sign = FALSE
      )
    }
  )

  invisible(NULL)
}



#' Checks if all columns satisfy the predicate.
#' @inheritParams check_columns_key
#' @param predicate function that works on vectors and return a single logical value.
#' @param inverse logical, whether to invert the check direction in the sense that the predicate
#'  must be not satisfied for all columns (default FALSE).
#' @return invisible NULL
#' @export
check_columns_predicate <- function(df, predicate, inverse = FALSE, raise = "error", alert_message = NULL, header = "default", n.evaluation_frame = 0, quickalert = TRUE, ...){
  check_required_all()
  check_args_classes("predicate", "function", quickalert = FALSE)
  logical_vec <- purrr::map_lgl(df, ~ predicate(.x))

  if(inverse){logical_vec <- !logical_vec}
  false_cols <- colnames(df)[!logical_vec]

  if(length(false_cols) > 0){
    alert_message <- generate_message(alert_message, "{col_magenta(false_cols)}")
    header <- generate_header(header, "The predicate function {cli::col_red('returned FALSE')} for the following {qty(false_cols)} column{?s}:")
    alert_generator(raise, alert_message, n.evaluation_frame, quickalert, header = header, ...)
  }

  invisible(NULL)
}



#' Check if the dataframe is empty
#' @inheritParams check_columns_key
#' @param df_arg String specifying how to address df in the raised messages (default "df").
#' @return invisible NULL
#' @export
check_empty_df <- function(df, df_arg = "df", raise = "error", alert_message = NULL, n.evaluation_frame = 0, quickalert = TRUE, ...){
  rlang::check_required(df)
  if(nrow(df) == 0L){
    alert_message <- generate_message(alert_message, "{df_arg} is empty.")
    alert_generator(raise, alert_message, n.evaluation_frame, quickalert, ...)
  }
  invisible(NULL)
}
