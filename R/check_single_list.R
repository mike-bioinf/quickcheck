### Checking functions that work on a single list.


#' Ensemble check for a sigle list
#' @description
#' Allow to test for different proprieties of a single list. The properties are tested in the order they compare as parameters function.
#' @param x List to check.
#' @param xarg String, indicating how to refer x in the alert messages (default "list").
#' @param flatten Single boolean, whether to flat out the list before doing the check (default TRUE).
#' @param null_check Boolean, indicating whether to check for a NULL value (default FALSE).
#' @param zero_len_check Boolean, indicating whether to check for a zero-length list (default FALSE).
#' @param uniform Boolean, indicating whether to check for identical classed list elements (default FALSE).
#' @param predicate A predicate function, usually an "is.something" function (default NULL).
#' @param inverse Boolean, whether the predicate must not be satisfied for all the elements (default FALSE).
#' @param exact_len Integer(ish), indicating the exact expected list length (default NULL).
#' @param min_len Integer(ish), indicating the minimum expected list length (default NULL).
#' @param max_len Integer(ish), indicating the maximum expected list length (default NULL).
#' @inherit check_atomic_vec return
#' @export 
check_list <- function(x, xarg = "list", flatten = FALSE, null_check = FALSE, zero_len_check = FALSE, uniform = FALSE,
                        predicate = NULL, inverse = FALSE, exact_len = NULL, min_len = NULL, max_len = NULL){
  rlang::check_required(x)

  check_args(
    args = c("x", "xarg", "flatten", "null_check", "zero_len_check", "uniform", "predicate", "inverse", "exact_len", "min_len", "max_len"),
    expected_types = c("list", "character", "logical", "logical", "logical", "logical", "function", "logical", "integerish", "integerish", "integerish"),
    null = c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE),
    flag = c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE),
    with = c(rep("typeof", 6), "class", "typeof", rep("check_integerish", 3)),
    quickalert = FALSE
  )

  internal_check_list(x, xarg, flatten, null_check, zero_len_check, uniform, predicate, inverse, exact_len, min_len, max_len)
  invisible(NULL)
}



#' Check the emptiness of a list
#' @param x List to check.
#' @param null Boolean, indicating whether to perform the check for NULL value (default TRUE).
#' @param len Boolean, indicating whether to perform the check for 0 length (default TRUE).
#' @param xarg String, indicating how to refer x in the alert messages (default "list").
#' @inheritParams check_atomic_vec
#' @inherit check_atomic_vec return
#' @export
check_empty_list <- function(x, null = TRUE, len = TRUE, xarg = "list", raise = "error", alert_message = NULL, n_evaluation_frame = 0, quickalert = TRUE, ...){
  rlang::check_required(x)

  check_args(
    args = c("x", "null", "len", "xarg"),
    expected_types = c("list", "logical", "logical", "character"),
    flag = c(FALSE, TRUE, TRUE, TRUE),
    null = c(TRUE, FALSE, FALSE, FALSE),
    with = "typeof",
    quickalert = FALSE
  )

  n_evaluation_frame <- raise_custom_frame(n_evaluation_frame, 1)
  # changing the default internal_check_empty_vec alert message
  alert_message <- generate_message(alert_message, "{vec_arg} is an empty list.")
  internal_check_empty_vec(x, null, len, FALSE, FALSE, xarg, raise, alert_message = alert_message, n_evaluation_frame, quickalert, ...)
  invisible(NULL)
}



#' Check whether the list elements are of the same class(es)
#' @inheritParams check_empty_list
#' @param flatten Boolean, whether to flat out the list before doing the check (default TRUE).
#' @inherit check_atomic_vec return
#' @export
check_uniform_list <- function(x, flatten = FALSE, xarg = "list", raise = "error", alert_message = NULL, n_evaluation_frame = 0, quickalert = TRUE, ...){
  rlang::check_required(x)
  check_args(args = c("x", "flatten"), expected_types = c("list", "logical"), flag = c(FALSE, TRUE), with = "typeof", quickalert = FALSE)
  n_evaluation_frame <- raise_custom_frame(n_evaluation_frame, 1)
  internal_check_uniform_list(x, flatten, xarg, raise, alert_message, n_evaluation_frame, quickalert, ...)
  invisible(NULL)
}



#' Check list elements through a predicate function
#' @description
#' Checks whether the predicate returns TRUE for all elements of the list.
#' The function doesn't perform any check on the argument provided in predicate.
#' Therefore the correctness of the provided predicate falls on the user.
#' @inheritParams check_uniform_list
#' @param predicate A predicate function, usually an "is.something" function.
#' @param inverse Boolean, whether to invert the check direction in the sense that the predicate must be not satisfied for all the elements (default FALSE).
#' @param header Character string to add at the beginning of the alert message. If "default" the default header is used, otherwise the string passed in.
#' @details
#' The alert will not points the eventual elements in error if no or some elements names are empty, but instead it will raise a more general alert.
#' @inherit check_atomic_vec return
#' @export
check_predicate_list <- function(x, predicate, inverse = FALSE, flatten = FALSE, xarg = "list", raise = "error",
                                  alert_message = NULL, header = "default", n_evaluation_frame = 0, quickalert = TRUE, ...){
  check_required_all()

  check_args(
    args = c("x", "flatten", "predicate"),
    expected_types = c("list", "logical", "function"),
    flag = c(FALSE, TRUE, FALSE),
    with = c("typeof", "typeof", "class"),
    quickalert = FALSE
  )

  n_evaluation_frame <- raise_custom_frame(n_evaluation_frame, 1)
  internal_check_predicate_list(x, predicate, inverse, flatten, xarg, raise, alert_message, header, n_evaluation_frame, quickalert, ...)
  invisible(NULL)
}



#' Check the length of a list
#' @description
#' Checks whether the expectations on the list length hold.
#' @inheritParams check_uniform_list
#' @inheritParams check_length_vec
#' @inherit check_atomic_vec return
#' @export
check_length_list <- function(x, exact_len = NULL, min_len = NULL, max_len = NULL, flatten = FALSE, xarg = "list",
                              raise = "error", alert_message = NULL, n_evaluation_frame = 0, quickalert = TRUE, ...){
  rlang::check_required(x)
  check_args(args = c("x", "flatten"), expected_types = c("list", "logical"), flag = c(FALSE, TRUE), with = "typeof", quickalert = FALSE)
  n_evaluation_frame <- raise_custom_frame(n_evaluation_frame, 1)
  internal_check_length_list(x, exact_len, min_len, max_len, flatten, xarg, raise, alert_message, n_evaluation_frame, quickalert, ...)
  invisible(NULL)
}




### HELPERS --------------------------------------------------------------------------------------------------------------------------------

#' Helper of check_uniform_list.
#' @param x first element.
#' @param y second element.
custom_identical <- function(x, y){
  if(identical(x, y)) return(y) else return("not_identical_class")
}
