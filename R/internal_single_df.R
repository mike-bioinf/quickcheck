#' Internal version of check_empty_df
#' @inheritParams check_empty_df
internal_check_empty_df <- function(df, dim = TRUE, null = TRUE, df_arg = "df", raise = "error", alert_message = NULL, n_evaluation_frame = 0, quickalert = TRUE, ...){
	if((!null && dim && is.null(df)) || (null && is.null(df)) || (dim && (nrow(df) == 0 || ncol(df) == 0))){
    alert_message <- generate_message(alert_message, "{df_arg} is {cli::col_red('empty')}!")
    alert_generator(raise, alert_message, n_evaluation_frame, quickalert, ...)
  }
}



#' Internal version of check_columns_presence
#' @inheritParams check_columns_presence
internal_check_columns_presence <- function(df, columns, df_arg = "df", raise = "error", alert_message = NULL, header = "default", n_evaluation_frame = 0, quickalert = TRUE, ...){
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
}



#' Internal version of check_columns_key
#' @inheritParams check_columns_key
internal_check_columns_key <- function(df, columns, na_rm = TRUE, raise = "error", alert_message = NULL, header = "default", n_evaluation_frame = 0, quickalert = TRUE, ...){
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
}



#' Internal version of check_columns_na
#' @inheritParams check_columns_na
internal_check_columns_na <- function(df, columns, raise = "error", alert_message = NULL, header = "default", n_evaluation_frame = 0, quickalert = TRUE, ...){
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
}



#' Internal version of check_columns_predicate
#' @inheritParams check_columns_predicate
internal_check_columns_predicate <- function(df, predicate, inverse = FALSE, raise = "error", alert_message = NULL, header = "default", n_evaluation_frame = 0, quickalert = TRUE, ...){
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
}



#' Internal version of check_columns_number
#' @inheritParams check_columns_number
internal_check_columns_number <- function(df, exact_len = NULL, min_len = NULL, max_len = NULL, df_arg = "df", raise = "error", alert_message = NULL, n_evaluation_frame = 0, quickalert = TRUE, ...){
	ncols <- ncol(df)
  combined_message <- core_length_test(ncols, exact_len, min_len, max_len, df_arg)
  raise_length_alert(raise, combined_message, alert_message, n_evaluation_frame+1, quickalert, ...)
}


