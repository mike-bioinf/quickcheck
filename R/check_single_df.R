### Checking functions that work on a single dataframe.


#' Ensemble check for a dataframe
#' @description
#' Allow to test for different proprieties of a dataframe.
#' The properties are tested in the order they compare as parameters function.
#' @param df Dataframe to check.
#' @param df_arg String reporting how to address the dataframe in the alerts (default "df").
#' @param null_check Boolean, indicating whether to check for a NULL value (default FALSE).
#' @param dim_check Boolean, indicating whether to check for 0 dimensional dataframe (both columns and rows) (default FALSE).
#' @param n_exact_cols Integer(ish), indicating the exact expected number of columns (default NULL).
#' @param n_min_cols Integer(ish), indicating the minimum expected number of columns (default NULL).
#' @param n_max_cols Integer(ish), indicating the maximum expected number of columns (default NULL).
#' @param predicate Predicate function that all df columns have to satisfy (default NULL).
#' @param inverse Boolean, whether the predicate must be NOT satisfied for all the elements for a successful check (default FALSE).
#' @param columns Character vector reporting the columns names on which the presence check and the additional optional specific check are perfomed.
#' @param cols_key Boolean, should be the columns specified in "columns" made of only unique values (default FALSE).
#' @param rm_na_key Boolean, should the NAs not considered during the "key" check (default FALSE).
#' @param cols_na Boolean, specifying whether the columns specified in "columns" can have NAs (default TRUE).
#' @export
check_dataframe <- function(df, df_arg = "df", null_check = FALSE, dim_check = FALSE, n_exact_cols = NULL, n_min_cols = NULL, n_max_cols = NULL, predicate = NULL, inverse = FALSE,
                             columns = NULL, cols_key = FALSE, rm_na_key = FALSE, cols_na = TRUE){
  check_args(
    args = c(
      "df","df_arg", "null_check", "dim_check", "n_exact_cols", "n_min_cols", "n_max_cols", "predicate", "inverse",
      "columns", "cols_key", "rm_na_key", "cols_na"
    ),
    expected_types = c("data.frame", "character", "logical", "integerish", "function", "logical", "character", "logical"),
    flag = c(FALSE, rep(TRUE, 6), FALSE, TRUE, FALSE, rep(TRUE, 3)),
    null = c(TRUE, rep(FALSE, 3), rep(TRUE, 4), FALSE, TRUE, rep(FALSE, 3)),
    with = c("class", rep("typeof", 3), rep("check_integerish", 3), "class", rep("typeof", 5)),
    recycle_expected_types = c(1, 1, 2, 3, 1, 1, 1, 3),
    quickalert = FALSE
  )

  internal_check_empty_df(df, dim_check, null_check, df_arg)
  internal_check_columns_number(df, n_exact_cols, n_min_cols, n_max_cols)
  if(!is.null(predicate)) internal_check_columns_predicate(df, predicate, inverse)

  if(!is.null(columns)){
    internal_check_columns_presence(df, columns, df_arg)
    if(cols_key) internal_check_columns_key(df, columns, rm_na_key)
    if(!cols_na) internal_check_columns_na(df, columns)
  }

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

  check_args(
    args = c("df", "dim", "null", "df_arg"),
    expected_types = c("data.frame", "logical", "logical", "character"),
    flag = c(FALSE, TRUE, TRUE, TRUE),
    null = c(TRUE, FALSE, FALSE, FALSE),
    with = "class",
    quickalert = FALSE
  )

  n_evaluation_frame <- raise_custom_frame(n_evaluation_frame, 1)
  internal_check_empty_df(df, dim, null, df_arg, raise, alert_message, n_evaluation_frame, quickalert, ...)
  invisible(NULL)
}



#' Check the presence of one or multiple columns in a dataframe
#' @param df Dataframe passed in the outer function.
#' @param columns Character vector reporting the column/s name.
#' @param df_arg String specifying how to address df in the alerts (default "df").
#' @param header String added at the beginning of the alert message. If "default" the default header is used, otherwise the string passed in.
#' @inheritParams check_atomic_vec
#' @inherit check_atomic_vec return
#' @export
check_columns_presence <- function(df, columns, df_arg = "df", raise = "error", alert_message = NULL, header = "default", n_evaluation_frame = 0, quickalert = TRUE, ...){
  check_required_all()
  check_args(c("df", "columns", "df_arg"), c("data.frame", "character", "character"), flag = c(FALSE, FALSE, TRUE), with = "class", quickalert = FALSE)
  n_evaluation_frame <- raise_custom_frame(n_evaluation_frame, 2)
  internal_check_columns_presence(df, columns, df_arg, raise, alert_message, header, n_evaluation_frame, quickalert, ...)
  invisible(NULL)
}



#' Check whether the specified dataframe columns are suitable as keys of only unique values
#' @param na_rm Boolean, indicating if NA must be excluded prior evaluation (default TRUE).
#' @inheritParams check_columns_presence
#' @inherit check_atomic_vec return
#' @export
check_columns_key <- function(df, columns, na_rm = TRUE, raise = "error", alert_message = NULL, header = "default", n_evaluation_frame = 0, quickalert = TRUE, ...){
  check_required_all()
  check_args(c("df", "columns", "na_rm"), c("data.frame", "character", "logical"), flag = c(FALSE, FALSE, TRUE), with = "class", quickalert = FALSE)
  internal_check_columns_presence(df, columns, quickalert = FALSE)
  n_evaluation_frame <- raise_custom_frame(n_evaluation_frame, 1)
  internal_check_columns_key(df, columns, na_rm, raise, alert_message, header, n_evaluation_frame, quickalert, ...)
  invisible(NULL)
}



#' Check the presence of the specified values in the selected columns.
#' @inheritParams check_columns_key
#' @param col_levels List of character vectors reporting the expected levels for each column specified in columns.
#' The element of the list must be nominated according to the columns names which they refer.
#' @inherit check_atomic_vec return
#' @export
check_columns_levels <- function(df, columns, col_levels, raise = "error", alert_message = NULL, header = "default", n_evaluation_frame = 0, quickalert = TRUE, ...){
  check_required_all()
  check_args(c("df","columns", "col_levels"), c("data.frame", "character", "list"), with = "class", quickalert = FALSE)

  internal_check_columns_presence(df, columns, quickalert = FALSE)
  names_col_levels <- names(col_levels)
  internal_check_empty_vec(names_col_levels, alert_message = "All elements of col_levels must be nominated.", quickalert = FALSE)

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
  check_args(c("df", "columns"), c("data.frame","character"), with = "class", quickalert = FALSE)
  internal_check_columns_presence(df, columns, quickalert = FALSE)
  n_evaluation_frame <- raise_custom_frame(n_evaluation_frame, 1)
  internal_check_columns_na(df, columns, raise, alert_message, header, n_evaluation_frame, quickalert, ...)
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
  check_args(c("df", "predicate", "inverse"), c("data.frame", "function", "logical"), with = "class", quickalert = FALSE)
  n_evaluation_frame <- raise_custom_frame(n_evaluation_frame, 1)
  internal_check_columns_predicate(df, predicate, inverse, raise, alert_message, header, n_evaluation_frame, quickalert, ...)
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
  check_args(c("df", "df_arg"), c("data.frame", "character"), with = "class", quickalert = FALSE)
  check_len_args(exact_len, min_len, max_len)
  n_evaluation_frame <- raise_custom_frame(n_evaluation_frame, 1)
  internal_check_columns_number(df, exact_len, min_len, max_len, df_arg, raise, alert_message, n_evaluation_frame, quickalert, ...)
  invisible(NULL)
}