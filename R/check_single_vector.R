### Checking functions that works on single vector.


#' Checks the presence of "empty" values in one vector. Empty here is declined in its
#' broader meaning indicating NAs, "", NULL and vector of length zero.
#' @inheritParams check_columns_presence
#' @param vec Vector to check.
#' @param vec_arg String indicating how to address vec in the alert message (default 'vec').
#' @param n.evaluation_frame numeric, defines the number of calling frame to look up for the evaluation
#'  of the alert message in respect to where the function calling the alert is run.
#'  The default value points to the function frame. So it's possible to simply points
#'  to upper frames (as well as to below frames but is not recommended).
#' @param ... To pass additional argument to alert_generator function.
#' @return invisible NULL
#' @export
check_empty_vec <- function(vec, vec_arg = "vec", raise = "error", alert_message = NULL, n.evaluation_frame = 2, quickalert = TRUE, ...){
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
check_na_vec <- function(vec, vec_arg = "vec", raise = "error", alert_message = NULL, n.evaluation_frame = 2, quickalert = TRUE, ...){
  if(any(is.na(vec))){
    alert_message <- generate_message(alert_message, "There are NAs in {vec_arg}.")
    alert_generator(raise, alert_message, n.evaluation_frame, quickalert, ...)
  }
  invisible(NULL)
}





#' Checks the presence of duplicated values in a vector.
#' @inheritParams check_empty_vec
#' @return invisible NULL
#' @export
check_duplicate_vec <- function(vec, vec_arg = "vec", raise = "error", alert_message = NULL, n.evaluation_frame = 2, quickalert = TRUE, ...){
  vec <- stats::na.omit(vec)
  dup <- duplicated(vec)
  if(any(dup)){
    dup_values <- vec[dup] |> unique() |> sort()
    alert_message <- generate_message(
      alert_message,
      c("The following {qty(length(dup_values))} value{?s} {?is/are} {cli::col_red('duplicated')} in {vec_arg}:",
        "{cli::col_magenta(dup_values)}")
    )
    alert_generator(raise, alert_message, n.evaluation_frame, quickalert, ...)
  }
  invisible(NULL)
}





#' Checks if the number of unique values of a vector is equal to the expected.
#' @inheritParams check_empty_vec
#' @param expected_number_levels numeric indicating the expected number of unique values for vec.
#' @param na.rm logical (default TRUE), indicating if NA must be excluded prior computations.
#' @return invisible NULL
#' @export
check_number_values <- function(vec, expected_number_levels, vec_arg = "vec", na.rm = TRUE, raise = "error", alert_message = NULL, n.evaluation_frame = 2, quickalert = TRUE, ...){
  check_required_all()
  unique_levels <- unique(vec)
  if(na.rm) unique_levels <- stats::na.omit(unique_levels)

  if(length(unique_levels) != expected_number_levels){
    alert_message <- generate_message(alert_message, "{expected_number_levels} level{?s} expected but {length(unique_levels)} detected.")
    alert_generator(raise, alert_message, n.evaluation_frame, quickalert, ...)
  }

  invisible(NULL)
}





#' Checks the presence of the specified values in a vector.
#' @inheritParams check_empty_vec
#' @param values character vector of values searched in vec.
#' @return invisible NULL
#' @export
check_presence_values <- function(vec, values, vec_arg = "vec", raise = "error", alert_message = NULL, n.evaluation_frame = 2, quickalert = TRUE, ...){
  check_required_all()
  unique_vec <- stats::na.omit(unique(vec))

  if(!all(values %in% vec)){
    missing_values <- setdiff(values, unique_vec)
    alert_message <- generate_message(
      alert_message,
      c("The following {qty(missing_values)} value{?s} {?is/are} missing in {vec_arg}:", "{col_magenta(missing_values)}")
    )
    alert_generator(raise, alert_message, n.evaluation_frame, quickalert, ...)
  }

  invisible(NULL)
}





#' Checks if a vector has all unique values.
#' @inheritParams check_number_values
#' @return invisible NULL
#' @export
check_unique_values <- function(vec, vec_arg = "vec", na.rm = TRUE, raise = "error", alert_message = NULL, n.evaluation_frame = 2, quickalert = TRUE, ...){
  if(na.rm){
    value_freqs <- table(vec)
  } else {
    value_freqs <- table(vec, useNA = "ifany")
  }

  if(!all(value_freqs == 1)){
    err_value <- names(value_freqs[value_freqs != 1])
    alert_message <- generate_message(
      alert_message,
      c("The following {qty(err_value)} value{?s} {?is/are} present {col_red('multiple times')} in {vec_arg}: ", "{col_magenta(err_value)}")
    )
    alert_generator(raise, alert_message, n.evaluation_frame, quickalert, ...)
  }

  invisible(NULL)
}

