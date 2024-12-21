### Checking functions that work on a single list.


#' Check whether the list elements are of the same class(es)
#' @inheritParams check_columns_key
#' @param x list.
#' @param flatten Boolean, whether to flat out the list before doing the check (default TRUE).
#' @param xarg String, indicating how to refer x in the alert messages (default "list").
#' @return invisible NULL.
#' @export
check_uniform_list <- function(x, flatten = TRUE, xarg = "list", raise = "error", alert_message = NULL, n_evaluation_frame = 0, quickalert = TRUE, ...){
  check_args_primitive_types("x", "list", quickalert = FALSE)

  if(flatten) x <- rec_flatten_list(x, till_flat = TRUE)
  types <- lapply(x, class)
  identity <- purrr::reduce(types, custom_identical)

  if(identity == "NotEqualClass"){
    alert_message <- generate_message(alert_message, "{xarg} is {cli::col_red('not uniform')}.")
    alert_generator(raise, alert_message, n_evaluation_frame, quickalert, ...)
  }

  invisible(NULL)
}



#' Check list elements through a predicate function
#' @description
#' Checks whether the predicate returns TRUE for all elements of the list.
#' The function doesn't perform any check on the argument provided in predicate.
#' Therefore the correctness of the provided function falls on the user.
#' @inheritParams check_uniform_list
#' @param predicate A predicate function, usually an "is.something" function.
#' @param inverse Boolean, whether to invert the check direction in the sense that the predicate must be not satisfied for all the elements (default FALSE).
#' @param header Character string to add at the beginning of the alert message. If "default" the default header is used, otherwise the string passed in.
#' @details
#' The alert will not points the eventual elements in error if no or some elements names are empty, but instead it will raise a more general alert.
#' @return invisible NULL.
#' @export
check_predicate_list <- function(x, predicate, inverse = FALSE, flatten = TRUE, xarg = "list", raise = "error",
                                  alert_message = NULL, header = "default", n_evaluation_frame = 0, quickalert = TRUE, ...){
  check_required_all()
  check_args_classes(c("x", "predicate"), c("list", "function"), quickalert = FALSE)

  if(flatten) x <- rec_flatten_list(x, till_flat = TRUE)
  predicate_log <- apply_predicate(x, predicate, inverse)
  empty_log <- is_empty_vec(names(x))
  errors <- get_errors_predicate(x, empty_log, predicate_log)
  raise_predicate_error(empty_log, errors, xarg, inverse, raise, alert_message, n_evaluation_frame+1, quickalert, header, ...)
  invisible(NULL)
}



#' Check list elements names
#' @description
#' Checks whether the names attibutes or some list element names are empty.
#' In particular it checks for NULL attribute names, and NA and "" erroneous names.
#' @inheritParams check_uniform_list
#' @return invisible NULL.
#' @export
check_names_list <- function(x, flatten = TRUE, xarg = "list", raise = "error", alert_message = NULL, n_evaluation_frame = 0, quickalert = TRUE, ...){
  rlang::check_required(x)
  check_args_primitive_types("x", "list", quickalert = FALSE)
  if(flatten) x <- rec_flatten_list(x, till_flat = TRUE)

  if(is_empty_vec(names(x))){
    alert_message <- generate_message(alert_message, "{xarg} present {cli::col_red('missing element names')}.")
    alert_generator(raise, alert_message, n_evaluation_frame, quickalert, ...)
  }

  invisible(NULL)
}



#' Check the length of a list
#' @description
#' Checks whether the expectations on the list lenght hold.
#' @inheritParams check_uniform_list
#' @inheritParams check_length_vec
#' @return invisible NULL.
#' @export
check_length_list <- function(x, exact_len = NULL, min_len = NULL, max_len = NULL, flatten = TRUE, xarg = "list",
                                raise = "error", alert_message = NULL, n_evaluation_frame = 0, quickalert = TRUE, ...){
  rlang::check_required(x)
  check_args_primitive_types("x", "list", quickalert = FALSE)
  if(flatten) x <- rec_flatten_list(x, till_flat = TRUE)
  len_list <- length(x)
  combined_message <- core_length_test(len_list, exact_len, min_len, max_len, xarg)
  raise_length_alert(raise, combined_message, alert_message, n_evaluation_frame+1, quickalert, ...)
  invisible(NULL)
}




#### HELPERS --------------------------------------------------------------------------------------------------------------------------------

#' Helper of check_uniform_list.
#' @param x first element.
#' @param y second element.
custom_identical <- function(x, y){
  if(identical(x, y)) return(y) else return("NotEqualClass")
}
