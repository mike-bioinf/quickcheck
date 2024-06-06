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
#' @param ... not of direct use.
#' @return invisible NULL
#' @export
check_empty_vec <- function(vec, vec_arg = "vec", raise = "error", alert_message = NULL, n.evaluation_frame = 2, ...){
  if(is_empty_vec(vec)){
    alert_message <- generate_message(alert_message, "There are empty values in {vec_arg}")
    alert_generator(raise, alert_message, n.evaluation_frame, ...)
  }
  invisible(NULL)
}




#' Checks the presence of NAs in a vector.
#' @inheritParams check_empty_vec
#' @return invisible NULL
#' @export
check_na_vec <- function(vec, vec_arg = "vec", raise = "error", alert_message = NULL, n.evaluation_frame = 2, ...){
  if(any(is.na(vec))){
    alert_message <- generate_message(alert_message, "There are NAs in {vec_arg}")
    alert_generator(raise, alert_message, n.evaluation_frame, ...)
  }
}





#' Checks if the number of unique values of a vector is equal to the expected.
#' @inheritParams check_empty_vec
#' @param expected_number_levels numeric indicating the expected number of unique values for vec.
#' @param na.rm logical (default TRUE), indicating if NA must be excluded prior computations.
#' @return invisible NULL
#' @export
check_number_values <- function(vec, expected_number_levels, vec_arg = "vec", raise = "error", alert_message = NULL, na.rm = TRUE, n.evaluation_frame = 2, ...){
  check_required_all()
  unique_levels <- unique(vec)

  if(na.rm){unique_levels <- stats::na.omit(unique_levels)}

  if(length(unique_levels) != expected_number_levels){
    alert_message <- generate_message(alert_message, "{expected_number_levels} level{?s} expected but {length(unique_levels)} detected")
    alert_generator(raise, alert_message, n.evaluation_frame, ...)
  }

  invisible(NULL)
}





#' Checks the presence of the specified values in a vector.
#' @inheritParams check_empty_vec
#' @param values character vector of values searched in vec.
#' @return invisible NULL
#' @export
check_presence_values <- function(vec, values, vec_arg = "vec", raise = "error", alert_message = NULL, n.evaluation_frame = 2, ...){
  check_required_all()
  unique_vec <- unique(stats::na.omit(vec))

  if(!all(values %in% vec)){
    missing_values <- setdiff(values, unique_vec)
    alert_message <- generate_message(
      alert_message,
      c("The following {qty(missing_values)} value{?s} {?is/are} missing in {vec_arg}:", "{col_magenta(missing_values)}")
    )
    alert_generator(raise, alert_message, n.evaluation_frame, ...)
  }

  invisible(NULL)
}





#' Checks if a vector has all unique values.
#' @inheritParams check_number_values
#' @return invisible NULL
#' @export
check_unique_values <- function(vec, vec_arg = "vec", raise = "error", alert_message = NULL, na.rm = TRUE, n.evaluation_frame = 2, ...){
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
    alert_generator(raise, alert_message, n.evaluation_frame, ...)
  }

  invisible(NULL)
}




#' Check the length of the input vector through an expression
#' @description
#' If the expression argument returns FALSE then the alert is raised. This system has been
#' choose in order to easily conduct test for combinations of equality, greater than and less than.
#' However no check is done on the provided expression and its return value.
#' Therefore if a whatsoever expression returning a boolean or not returning a boolean is passed,
#' then the function will continue to work or not in unexpected ways.
#'
#' @inheritParams check_empty_vec
#' @param na.rm logical (default FALSE), indicating if NA must be excluded prior computations.
#' @param expr expression returning a boolean and evaluation the length of the vector
#' (the nature and the actual meaning of the expression is not checked by this function).
#'
#' @return invisible NULL
#' @export
check_length_vec <- function(vec, expr, vec_arg = "vec", raise = "error", alert_message = NULL, na.rm = FALSE, n.evaluation_frame = 2, ...){
  if(na.rm) {vec <- stats::na.omit(vec)}
  alert_message <- generate_message(alert_message, "The length of {vec_arg} is {col_red('different from expected')}.")
  if(!expr) alert_generator(raise, alert_message,  n.evaluation_frame, ...)
  invisible(NULL)
}






### HELPERS ======================================================================================================================

#' Helper of check_empty_vec in which all "empty possibilities" are tested for each value.
#' Return a single boolean (FALSE even if one value is empty, TRUE otherwise).
#' @inheritParams check_empty_vec
is_empty_vec <- function(vec){
  if("" %in% vec){
    return(TRUE)
  }

  if(is.null(vec)){
    return(TRUE)
  }

  if(length(vec) == 0){
    return(TRUE)
  }

  if(any(is.na(vec))){
    return(TRUE)
  }

  return(FALSE)
}



#' Helper that is used to construct a default alert message if not provided (if NULL).
#' @param alert_message takes in the alert_message argument of the checking function.
#' @param default_message character vector that is used as default message. Follow cli intax rules.
generate_message <- function(alert_message, default_message){
  if(is.null(alert_message)){
    alert_message <- default_message
  }
  return(alert_message)
}
