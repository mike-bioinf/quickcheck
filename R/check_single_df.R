### Checking functions that work on a single dataframe.


#' Check the presence of one or multiple columns in a dataframe
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
#' @param n_evaluation_frame
#'  numeric, defines the number of stack frame to look down for the evaluation of the glue expressions of the alert message.
#'  The default value (0) points to the frame above this function frame. So to point to the frame below this function frame you have to set 2.
#' @param quickalert logical, whether the raised alert has to be of class "quickalert".
#' @param ... To pass additional argument to alert_generator function.
#' @return NULL.
#' @export
check_columns_presence <- function(df, columns, df_arg = "df", raise = "error", alert_message = NULL, header = "default", n_evaluation_frame = 0, quickalert = TRUE, ...){
  check_required_all()
  header <- generate_header(header, "The following {qty(missing_values)} column{?s} {?is/are} {col_red('missing')} in {vec_arg}:")

  check_presence_vec(
    vec = colnames(df),
    values = columns,
    vec_arg = df_arg,
    raise = raise,
    alert_message = alert_message,
    header = header,
    n_evaluation_frame = n_evaluation_frame,
    quickalert = quickalert,
    ...
  )

  invisible(NULL)
}



#' Check whether the specified dataframe columns are suitable as keys of only unique values
#' @param na_rm
#'  Boolean, indicating if NA must be excluded prior evaluation (default TRUE).
#' @param n_evaluation_frame
#'  Numeric, defines the number of stack frame to look down for the evaluation of the glue expressions of the alert message.
#'  The default (0) points to this function frame.
#' @inheritParams check_columns_presence
#' @return invisible NULL.
#' @export
check_columns_key <- function(df, columns, na_rm = TRUE, raise = "error", alert_message = NULL, header = "default", n_evaluation_frame = 0, quickalert = TRUE, ...){
  check_required_all()
  check_columns_presence(df, columns, quickalert = FALSE)
  header <- generate_header(header, "The following values occur {cli::col_red('multiple times')} for the following columns:")

  impose_accumulation_behavior(
    raise = raise,
    alert_message = alert_message,
    header = header,
    quickalert = quickalert,
    n_evaluation_frame = n_evaluation_frame + 1,
    ...,
    expr = {
      for(n in columns){
        check_duplicate_vec(
          vec = df[[n]],
          vec_arg = n,
          na_rm = na_rm,
          raise = "message",
          alert_message = "{vec_arg} --> {cli::col_magenta(dup_values)}",
          header = NULL,
          sign = FALSE
      )}
    }
  )

  invisible(NULL)
}



#' Check the presence of the specified values in the selected columns.
#' @inheritParams check_columns_key
#' @param col_levels
#'  List of character vectors reporting the expected levels for each column specified in columns.
#'  The element of the list must be nominated according to the columns names which they refer.
#' @return NULL
#' @export
check_columns_levels <- function(df, columns, col_levels, raise = "error", alert_message = NULL, header = "default", n_evaluation_frame = 0, quickalert = TRUE, ...){
  check_required_all()
  check_args_primitive_types(c("columns", "col_levels"), c("character", "list"), quickalert = FALSE)
  check_columns_presence(df, columns, quickalert = FALSE)
  names_col_levels <- names(col_levels)
  check_empty_vec(names_col_levels, alert_message = "All elements of col_levels must be nominated.", quickalert = FALSE)

  check_equality_vecs(
    vec1 = sort(columns),
    vec2 = sort(names_col_levels),
    vec1_arg = "columns",
    vec2_arg = "col_levels names",
    quickalert = FALSE,
    alert_message = c(
      "columns and col_levels names {cli::col_red('are not the same')}.",
      "i" = "All cols specified in columns must be reported in col_levels."
    )
  )

  header <- generate_header(header, "The following levels are {cli::col_red('missing')} from the reported columns:")

  impose_accumulation_behavior(
    raise = raise,
    alert_message = alert_message,
    header = header,
    quickalert = quickalert,
    n_evaluation_frame = n_evaluation_frame + 1,
    ...,
    expr = for(n in columns){
      check_presence_vec(
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



#' Check the presence of NAs in the specified columns
#' @inheritParams check_columns_key
#' @return invisible NULL
#' @export
check_columns_na <- function(df, columns, raise = "error", alert_message = NULL, header = "default", n_evaluation_frame = 0, quickalert = TRUE, ...){
  check_required_all()
  check_args_classes("columns", "character", quickalert = FALSE)
  check_columns_presence(df, columns, quickalert = FALSE)

  header <- generate_header(header, "The following columns {cli::col_red('present NAs')}:")

  impose_accumulation_behavior(
    raise = raise,
    alert_message = alert_message,
    header = header,
    quickalert = quickalert,
    n_evaluation_frame = n_evaluation_frame + 1,
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



#' Check whether dataframe columns satisfy a predicate
#' @inheritParams check_columns_key
#' @param predicate function that works on vectors and return a single logical value.
#' @param inverse logical, whether to invert the check direction in the sense that the predicate
#'  must be not satisfied for all columns (default FALSE).
#' @return invisible NULL
#' @export
check_columns_predicate <- function(df, predicate, inverse = FALSE, raise = "error", alert_message = NULL, header = "default", n_evaluation_frame = 0, quickalert = TRUE, ...){
  check_required_all()
  check_args_classes("predicate", "function", quickalert = FALSE)
  logical_vec <- purrr::map_lgl(df, ~ predicate(.x))

  if(inverse) logical_vec <- !logical_vec
  false_cols <- colnames(df)[!logical_vec]

  if(length(false_cols) > 0){
    header <- generate_header(header, "The predicate function {cli::col_red('returned FALSE')} for the following {qty(false_cols)} column{?s}:")
    alert_message <- generate_message(alert_message, "{col_magenta(false_cols)}")
    alert_generator(raise, alert_message, n_evaluation_frame, quickalert, header = header, ...)
  }

  invisible(NULL)
}



#' Check whether the dataframe is empty
#' @inheritParams check_columns_key
#' @param dim Boolean, indicating whether to perform the check for 0 length dimensions (rows and columns) (default TRUE).
#' @param null Boolean, indicating whether to perform the check for NULL values (default TRUE).
#' @param df_arg String specifying how to address df in the raised messages (default "df").
#' @return invisible NULL
#' @export
check_empty_df <- function(df, dim = TRUE, null = TRUE, df_arg = "df", raise = "error", alert_message = NULL, n_evaluation_frame = 0, quickalert = TRUE, ...){
  rlang::check_required(df)

  if((dim && (nrow(df) == 0 || ncol(df) == 0)) || (null && is.null(df))){
    alert_message <- generate_message(alert_message, "{df_arg} is {cli::col_red('empty')}!")
    alert_generator(raise, alert_message, n_evaluation_frame, quickalert, ...)
  }

  invisible(NULL)
}
