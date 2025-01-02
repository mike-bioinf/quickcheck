### Checking functions that work on a single dataframe.


#' Check the presence of one or multiple columns in a dataframe
#' @param df Dataframe passed in the outer function.
#' @param columns Character string reporting the column/s name.
#' @param df_arg String specifying how to address df in the raised messages (default "df").
#' @param header String added at the beginning of the alert message. If "default" the default header is used, otherwise the string passed in.
#' @inheritParams check_atomic_vec
#' @inherit check_atomic_vec return
#' @export
check_columns_presence <- function(df, columns, df_arg = "df", raise = "error", alert_message = NULL, header = "default", n_evaluation_frame = 0, quickalert = TRUE, ...){
  check_required_all()
  check_args_classes("df", "data.frame", quickalert = FALSE)
  header <- generate_header(header, "The following {cli::qty(length(missing_values))} column{?s} {?is/are} {cli::col_red('missing')} in {vec_arg}:")

  internal_check_presence_vec(
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
#' @inherit check_atomic_vec return
#' @export
check_columns_key <- function(df, columns, na_rm = TRUE, raise = "error", alert_message = NULL, header = "default", n_evaluation_frame = 0, quickalert = TRUE, ...){
  check_required_all()
  check_args_classes("df", "data.frame", quickalert = FALSE)
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
#' @inherit check_atomic_vec return
#' @export
check_columns_levels <- function(df, columns, col_levels, raise = "error", alert_message = NULL, header = "default", n_evaluation_frame = 0, quickalert = TRUE, ...){
  check_required_all()
  check_args_classes(c("df","columns", "col_levels"), c("data.frame", "character", "list"), quickalert = FALSE)
  check_columns_presence(df, columns, quickalert = FALSE)
  names_col_levels <- names(col_levels)
  check_empty_vec(names_col_levels, alert_message = "All elements of col_levels must be nominated.", quickalert = FALSE)

  check_equality_vecs(
    vec1 = columns,
    vec2 = names_col_levels,
    sort = TRUE,
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
      internal_check_presence_vec(
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
#' @inherit check_atomic_vec return
#' @export
check_columns_na <- function(df, columns, raise = "error", alert_message = NULL, header = "default", n_evaluation_frame = 0, quickalert = TRUE, ...){
  check_required_all()
  check_args_classes(c("df", "columns"), c("data.frame","character"), quickalert = FALSE)
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
#' @inheritParams check_columns_presence
#' @param predicate Function that works on vectors and returns a single logical value.
#' @param inverse Boolean, whether to invert the check direction in the sense that the predicate must be not satisfied for all columns (default FALSE).
#' @inherit check_atomic_vec return
#' @export
check_columns_predicate <- function(df, predicate, inverse = FALSE, raise = "error", alert_message = NULL, header = "default", n_evaluation_frame = 0, quickalert = TRUE, ...){
  check_required_all()
  check_args_classes(c("df", "predicate"), c("data.frame", "function"), quickalert = FALSE)
  logical_vec <- purrr::map_lgl(df, predicate)

  if(inverse) {
    logical_vec <- !logical_vec
    inverse_string <- "inverse of the"
  } else {
    inverse_string <- ""
  }

  false_cols <- colnames(df)[!logical_vec]

  if(length(false_cols) > 0){
    default_header <- paste0("The ", inverse_string, " predicate function {cli::col_red('returned FALSE')} for the following {cli::qty(length(false_cols))} column{?s}:")
    header <- generate_header(header, default_header)
    alert_message <- generate_message(alert_message, "{cli::col_magenta(false_cols)}")
    alert_generator(raise, alert_message, n_evaluation_frame, quickalert, header, ...)
  }

  invisible(NULL)
}



#' Check the number of columns of a dataframe
#' @inheritParams check_columns_presence 
#' @param exact_len Integer indicating the exact expected number of columns (default NULL).
#' @param min_len Integer indicating the minimum expected number of columns (default NULL).
#' @param max_len Integer indicating the maximum expected number of columns (default NULL).
#' @inherit check_atomic_vec return
#' @export
check_columns_number <- function(df, exact_len = NULL, min_len = NULL, max_len = NULL, df_arg = "df", raise = "error", alert_message = NULL, n_evaluation_frame = 0, quickalert = TRUE, ...){
  rlang::check_required(df)
  check_args_classes("df", "data.frame", quickalert = FALSE)
  check_len_args(exact_len, min_len, max_len)
  ncols <- length(df)
  combined_message <- core_length_test(ncols, exact_len, min_len, max_len, df_arg)
  raise_length_alert(raise, combined_message, alert_message, n_evaluation_frame+1, quickalert, ...)
  invisible(NULL)
}



#' Check whether the dataframe is empty
#' @inheritParams check_columns_key
#' @param dim Boolean, indicating whether to perform the check for 0 length dimensions (rows and columns) (default TRUE).
#' @param null Boolean, indicating whether to perform the check for NULL values (default TRUE).
#' @param df_arg String specifying how to address df in the raised messages (default "df").
#' @inherit check_atomic_vec return
#' @export
check_empty_df <- function(df, dim = TRUE, null = TRUE, df_arg = "df", raise = "error", alert_message = NULL, n_evaluation_frame = 0, quickalert = TRUE, ...){
  rlang::check_required(df)
  check_args_classes("df", "data.frame", quickalert = FALSE)

  if((dim && (nrow(df) == 0 || ncol(df) == 0)) || (null && is.null(df))){
    alert_message <- generate_message(alert_message, "{df_arg} is {cli::col_red('empty')}!")
    alert_generator(raise, alert_message, n_evaluation_frame, quickalert, ...)
  }

  invisible(NULL)
}
