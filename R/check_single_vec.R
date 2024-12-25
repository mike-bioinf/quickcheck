### Checking functions that works on single vector.


#' Check the emptiness of a vector
#' @description
#' Checks the emptiness of a vector and or the presence of "empty" values in it.
#' Empty here is declined in its broader meaning indicating NAs, "", NULL and vector of length zero.
#' This interpretation can be fine tuned using the parameters function.
#' @inheritParams check_columns_key
#' @inheritParams is_empty_vec
#' @param vec Vector to check.
#' @param vec_arg String indicating how to address vec in the alert message (default 'vec').
#' @return
#' Depending on the function prefix: the "check" function returns the condition otherwise NULL invisibly,
#' the "test" function returns TRUE if the condition would be raised and FALSE otherwise.
#' @export
check_empty_vec <- function(vec, na = TRUE, empty_string = TRUE, len = TRUE, null = TRUE, vec_arg = "vec",
                             raise = "error", alert_message = NULL, n_evaluation_frame = 0, quickalert = TRUE, ...){
  rlang::check_required(vec)
  if(is_empty_vec(vec, na, empty_string, len, null)){
    alert_message <- generate_message(alert_message, "{vec_arg} is or contains an empty entity.")
    alert_generator(raise, alert_message, n_evaluation_frame, quickalert, ...)
  }
  invisible(NULL)
}



#' Check the presence of NAs in a vector
#' @inheritParams check_empty_vec
#' @inherit check_empty_vec return
#' @export
check_na_vec <- function(vec, vec_arg = "vec", raise = "error", alert_message = NULL, n_evaluation_frame = 0, quickalert = TRUE, ...){
  rlang::check_required(vec)
  if(any(is.na(vec))){
    alert_message <- generate_message(alert_message, "There are NAs in {vec_arg}.")
    alert_generator(raise, alert_message, n_evaluation_frame, quickalert, ...)
  }
  invisible(NULL)
}



#' Check the presence of duplicated values in a vector
#' @inheritParams check_empty_vec
#' @param header Character string to add at the beginning of the alert message.
#' @param na_rm Boolean indicating if NA must be excluded prior checking (default TRUE).
#' @inherit check_empty_vec return
#' @export
check_duplicate_vec <- function(vec, na_rm = TRUE, vec_arg = "vec", raise = "error", alert_message = NULL, header = "default", n_evaluation_frame = 0, quickalert = TRUE, ...){
  rlang::check_required(vec)
  if(na_rm) vec <- stats::na.omit(vec)
  dup <- duplicated(vec)

  if(any(dup)){
    dup_values <- vec[dup] |> unique() |> sort(na.last = FALSE)
    alert_message <- generate_message(alert_message, "{cli::col_magenta(dup_values)}")
    header <- generate_header(header, "The following {cli::qty(length(dup_values))} value{?s} {?is/are} {cli::col_red('duplicated')} in {vec_arg}:")
    alert_generator(raise, alert_message, n_evaluation_frame, quickalert, header = header, ...)
  }

  invisible(NULL)
}



#' Check on vector length
#' @details
#' The function does not check the logical validity of the values passed in the *_len arguments.
#' So for example it's possible to pass a minimun length of 10 and a maximimum of 3.
#' @inheritParams check_empty_vec
#' @param exact_len Integer indicating the exact expected vector length (default NULL).
#' @param min_len Integer indicating the minimum expected vector length (default NULL).
#' @param max_len Integer indicating the maximum expected vector length (default NULL).
#' @param na_rm Boolean indicating if NA must be excluded prior checking (default TRUE).
#' @param unique Boolean indicating whether to perform the check only on unique values (default FALSE).
#' @inherit check_empty_vec return
#' @export
check_length_vec <- function(vec, exact_len = NULL, min_len = NULL, max_len = NULL, na_rm = TRUE, unique = FALSE, vec_arg = "vec",
                              raise = "error", alert_message = NULL, n_evaluation_frame = 0, quickalert = TRUE, ...){
  rlang::check_required(vec)
  check_len_args(exact_len, min_len, max_len)

  if(na_rm) vec <- stats::na.omit(vec)
  if(unique) vec <- unique(vec)

  vec_len <- length(vec)
  combined_message <- core_length_test(vec_len, exact_len, min_len, max_len, vec_arg)
  raise_length_alert(raise, combined_message, alert_message, n_evaluation_frame+1, quickalert, ...)
  invisible(NULL)
}



#' Check the presence of specified values in a vector
#' @inheritParams check_empty_vec
#' @param values Character vector of values searched in vec.
#' @param header Character string to add at the beginning of the alert message. If "default" the default header is used, otherwise the string passed in.
#' @inherit check_empty_vec return
#' @export
check_presence_vec <- function(vec, values, vec_arg = "vec", raise = "error", alert_message = NULL, header = "default", n_evaluation_frame = 0, quickalert = TRUE, ...){
  check_required_all()
  unique_vec <- unique(vec)

  if(!all(values %in% unique_vec)){
    missing_values <- setdiff(values, unique_vec)
    alert_message <- generate_message(alert_message, "{cli::col_magenta(missing_values)}")
    header <- generate_header(header, "The following {cli::qty(length(missing_values))} value{?s} {?is/are} {cli::col_red('missing')} in {vec_arg}:")
    alert_generator(raise, alert_message, n_evaluation_frame, quickalert, header = header, ...)
  }

  invisible(NULL)
}



#' Check whether a vector is sorted or not
#' @inheritParams check_length_vec
#' @param decreasing Boolean indicating whether the expect sorted order is decreasing or not (default FALSE).
#' @inherit check_empty_vec return
#' @export
check_sorted_vec <- function(vec, decreasing = FALSE, vec_arg = "vec", raise = "error", alert_message = NULL, n_evaluation_frame = 0, quickalert = TRUE, ...){
  rlang::check_required(vec)
  check_na_vec(vec, raise = "warning", alert_message = "NAs in vec, they are not considered in the sort check.", quickalert = FALSE)
  vec <- stats::na.omit(vec)

  if(any(vec != sort(vec, decreasing))){
    alert_message <- generate_message(alert_message, "{vec_arg} is not sorted.")
    alert_generator(raise, alert_message, n_evaluation_frame, quickalert, ...)
  }

  invisible(NULL)
}



#' Check vector elements through a predicate function
#' @description
#' Checks whether the predicate returns TRUE for all elements of the vector.
#' The function doesn't perform any check on the argument provided in predicate.
#' Therefore the correctness of the provided function falls on the user.
#' @inheritParams check_predicate_list
#' @inheritParams check_empty_vec
#' @inherit check_empty_vec return
#' @export
check_predicate_vec <- function(vec, predicate, inverse = FALSE, vec_arg = "vec", raise = "error", alert_message = NULL, header = "default", n_evaluation_frame = 0, quickalert = TRUE, ... ){
  check_required_all()
  check_args_classes("predicate", "function", quickalert = FALSE)
  predicate_log <- apply_predicate(vec, predicate, inverse)
  empty_log <- is_empty_vec(names(vec))
  errors <- get_errors_predicate(vec, empty_log, predicate_log)
  raise_predicate_error(empty_log, errors, vec_arg, inverse, raise, alert_message, n_evaluation_frame+1, quickalert, header, ...)
  invisible(NULL)
}
