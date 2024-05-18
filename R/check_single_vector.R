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
#'
#' @return invisible NULL
#' @export
check_empty_vec <- function(vec, vec_arg = "vec", raise = "error", alert_message = NULL, n.evaluation_frame = 2){
  if(is_empty_vec(vec)){
    if(is.null(alert_message)){
      alert_message <- "There are empty values in {vec_arg}"
    }
    alert_generator(raise, alert_message, n.evaluation_frame)
  }
  invisible(NULL)
}





#' Checks if the number of unique values of a vector is equal to the expected.
#' @inheritParams check_empty_vec
#' @param expected_number_levels numeric indicating the expected number of expected
#' unique values for vec.
#' @param na.rm logical (default TRUE), indicating if NA must be excluded prior computations.
#' @return NULL
#' @export
check_number_values <- function(vec, expected_number_levels, vec_arg = "vec", raise = "error", alert_message = NULL, na.rm = TRUE, n.evaluation_frame = 2){
  check_required_all()
  unique_levels <- unique(vec)

  if(na.rm){
    unique_levels <- stats::na.omit(unique_levels)
  }

  if(length(unique_levels) != expected_number_levels){
    if(is.null(alert_message)){
      alert_message <- c("{expected_number_levels} level{?s} expected but {length(unique_levels)} detected")
    }
    alert_generator(raise, alert_message, n.evaluation_frame)
  }

  invisible(NULL)
}





#' Checks the presence of the specified values in a vector.
#' @inheritParams check_empty_vec
#' @param values character vector of values searched in vec.
check_presence_values <- function(vec, values, vec_arg = "vec", raise = "error", alert_message = NULL, n.evaluation_frame = 2){
  check_required_all()
  unique_vec <- unique(stats::na.omit(vec))

  if(!all(values %in% vec)){
    missing_values <- dplyr::setdiff(values, unique_vec)
    if(is.null(alert_message)){
      alert_message <- c(
        "The following {qty(missing_values)} value{?s} {?is/are} missing in {vec_arg}",
        "{col_magenta(missing_values)}"
      )
    }
    alert_generator(raise, alert_message, n.evaluation_frame)
  }

  invisible(NULL)
}





#' Checks if a vector has all unique values.
#' @inheritParams check_number_values
#' @return NULL
#' @export
check_unique_values <- function(vec, vec_arg = "vec", raise = "error", alert_message = NULL, na.rm = TRUE, n.evaluation_frame = 2){
  if(na.rm){
    value_freqs <- table(vec)
  } else {
    value_freqs <- table(vec, useNA = "ifany")
  }

  if(!all(value_freqs == 1)){
    err_value <- names(value_freqs[value_freqs != 1])
    if(is.null(alert_message)){
      alert_message <- c(
        "x" = "The following {qty(err_value)} value{?s} {?is/are} present {col_red('multiple times')} in {vec_arg}: ",
        "{col_magenta(err_value)}"
      )
    }
    alert_generator(raise, alert_message, n.evaluation_frame)
  }

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
