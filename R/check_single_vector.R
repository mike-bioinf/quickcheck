### Checking functions that works on single vector.


#' Checks the presence of "empty" values in one vector. Empty here is declined in its
#' broader meaning indicating NAs, "", NULL and vector of length zero.
#' @inheritParams check_columns_presence
#' @param vec Vector to check.
#' @param vec_arg String indicating how to address vec in the alert message (default 'vec').
#' @param n.evaluation_frame numeric, defines the number of calling frame to look up for the evaluation of the alert message.
#' @param ... To pass additional argument to alert_generator function.
#' @return invisible NULL
#' @export
check_empty_vec <- function(vec, vec_arg = "vec", raise = "error", alert_message = NULL, n.evaluation_frame = 0, quickalert = TRUE, ...){
  if(is_empty_vec(vec)){
    alert_message <- generate_message(alert_message, "There are empty values in {vec_arg}.")
    alert_generator(raise, alert_message, n.evaluation_frame, quickalert, ...)
  }
  invisible(NULL)
}




#' Checks the presence of NAs in a vector.
#' @inheritParams check_empty_vec
#' @return invisible NULL
#' @export
check_na_vec <- function(vec, vec_arg = "vec", raise = "error", alert_message = NULL, n.evaluation_frame = 0, quickalert = TRUE, ...){
  if(any(is.na(vec))){
    alert_message <- generate_message(alert_message, "There are NAs in {vec_arg}.")
    alert_generator(raise, alert_message, n.evaluation_frame, quickalert, ...)
  }
  invisible(NULL)
}




#' Checks the presence of duplicated values in a vector.
#' @inheritParams check_empty_vec
#' @param header Character string to add at the beginning of the alert message.
#' @return invisible NULL
#' @export
check_duplicate_vec <- function(vec, vec_arg = "vec", raise = "error", alert_message = NULL, header = "default", n.evaluation_frame = 0, quickalert = TRUE, ...){
  vec <- stats::na.omit(vec)
  dup <- duplicated(vec)

  if(any(dup)){
    dup_values <- vec[dup] |> unique() |> sort()
    alert_message <- generate_message(alert_message, "{cli::col_magenta(dup_values)}")
    header <- generate_header(header, "The following {qty(length(dup_values))} value{?s} {?is/are} {cli::col_red('duplicated')} in {vec_arg}:")
    alert_generator(raise, alert_message, n.evaluation_frame, quickalert, header = header, ...)
  }
  invisible(NULL)
}




#' Checks if the number of unique values of a vector is equal to the expected.
#' @inheritParams check_empty_vec
#' @param expected_number numeric indicating the expected number of vec unique values.
#' @param na.rm logical (default TRUE), indicating if NA must be excluded prior computations.
#' @return invisible NULL
#' @export
check_number_values <- function(vec, expected_number, vec_arg = "vec", na.rm = TRUE, raise = "error", alert_message = NULL, n.evaluation_frame = 0, quickalert = TRUE, ...){
  check_required_all()
  unique_values <- unique(vec)
  if(na.rm){
    unique_values <- stats::na.omit(unique_values)
  }
  if(length(unique_values) != expected_number){
    alert_message <- generate_message(alert_message, "{expected_number} unique value{?s} expected but {length(unique_values)} detected.")
    alert_generator(raise, alert_message, n.evaluation_frame, quickalert, ...)
  }
  invisible(NULL)
}




#' Checks the presence of the specified values in a vector.
#' @inheritParams check_empty_vec
#' @param values character vector of values searched in vec.
#' @param header Character string to add at the beginning of the alert message.
#' @return invisible NULL
#' @export
check_presence_values <- function(vec, values, vec_arg = "vec", raise = "error", alert_message = NULL, header = "default", n.evaluation_frame = 0, quickalert = TRUE, ...){
  check_required_all()
  unique_vec <- stats::na.omit(unique(vec))
  if(!all(values %in% vec)){
    missing_values <- setdiff(values, unique_vec)
    alert_message <- generate_message(alert_message, "{col_magenta(missing_values)}")
    header <- generate_header(header, "The following {qty(missing_values)} value{?s} {?is/are} {cli::col_red('missing')} in {vec_arg}:")
    alert_generator(raise, alert_message, n.evaluation_frame, quickalert, header = header, ...)
  }
  invisible(NULL)
}




#' Checks if a vector has all unique values.
#' @inheritParams check_number_values
#' @param header Character string to add at the beginning of the alert message.
#' @return invisible NULL
#' @export
check_unique_values <- function(vec, vec_arg = "vec", na.rm = TRUE, raise = "error", alert_message = NULL, header = "default", n.evaluation_frame = 0, quickalert = TRUE, ...){
  if(na.rm) vec <- stats::na.omit(vec)
  value_freqs <- table(vec)
  unique_log <- value_freqs == 1
  if(!all(unique_log)){
    err_value <- names(value_freqs[!unique_log])
    alert_message <- generate_message(alert_message, "{col_magenta(err_value)}")
    header <- generate_header(header, "The following {qty(err_value)} value{?s} {?is/are} present {col_red('multiple times')} in {vec_arg}:")
    alert_generator(raise, alert_message, n.evaluation_frame, quickalert, header = header, ...)
  }
  invisible(NULL)
}




#' Checks if a vector is sorted or not.
#' @inheritParams check_number_values
#' @param decreasing Logical indicating whether the expect sorted order is decreasing or not (default FALSE).
#' @return invisible NULL
#' @export
check_sorted_vec <- function(vec, vec_arg = "vec", decreasing = F, raise = "error", alert_message = NULL, n.evaluation_frame = 0, quickalert = TRUE, ...){
  check_na_vec(vec, raise = "warning", alert_message = "NAs in vec, they are not considered in the sort check.")
  vec <- stats::na.omit(vec)
  if(any(vec != sort(vec, decreasing))){
    alert_message <- generate_message(alert_message, "{vec_arg} is not sorted.")
    alert_generator(raise, alert_message, n.evaluation_frame, quickalert, ...)
  }
  invisible(NULL)
}
