#' Internal version of check_list
#' @inheritParams check_list
internal_check_list <- function(x, xarg = "list", flatten = FALSE, null_check = FALSE, zero_len_check = FALSE, uniform = FALSE, 
                                predicate = NULL, inverse = FALSE, exact_len = NULL, min_len = NULL, max_len = NULL){
  if(flatten) x <- rec_flatten_list(x, till_flat = TRUE)

  if(null_check || zero_len_check){
    internal_check_empty_vec(x, null_check, zero_len_check, FALSE, FALSE, xarg, alert_message = "{vec_arg} is empty.")
  }

  if(uniform) internal_check_uniform_list(x, flatten = FALSE, xarg = xarg)
  if(!is.null(predicate)) internal_check_predicate_list(x, predicate, inverse, flatten = FALSE, xarg = xarg)

  if(!is.null(exact_len) || !is.null(min_len) || !is.null(max_len)){
    internal_check_length_list(x, exact_len, min_len, max_len, flatten = FALSE, xarg = xarg)
  }
}



#' Internal version of check_uniform_list
#' @inheritParams check_uniform_list
internal_check_uniform_list <- function(x, flatten = FALSE, xarg = "list", raise = "error", alert_message = NULL, n_evaluation_frame = 0, quickalert = TRUE, ...){
  if(flatten) x <- rec_flatten_list(x, till_flat = TRUE)
  types <- lapply(x, class)
  identity <- purrr::reduce(types, custom_identical)

  if(identity == "not_identical_class"){
    alert_message <- generate_message(alert_message, "{xarg} is {cli::col_red('not uniform')}.")
    alert_generator(raise, alert_message, n_evaluation_frame, quickalert, ...)
  }
}



#' Internal version of check_predicate_list
#' @inheritParams check_predicate_list
internal_check_predicate_list <- function(x, predicate, inverse = FALSE, flatten = FALSE, xarg = "list", raise = "error",
                                          alert_message = NULL, header = "default", n_evaluation_frame = 0, quickalert = TRUE, ...){
  if(flatten) x <- rec_flatten_list(x, till_flat = TRUE)
  predicate_log <- apply_predicate(x, predicate, inverse)
  empty_log <- is_empty_vec(names(x))
  errors <- get_errors_predicate(x, empty_log, predicate_log)
  raise_predicate_error(empty_log, errors, xarg, inverse, raise, alert_message, n_evaluation_frame+1, quickalert, header, ...)  
}



#' Internal version of check_length_list
#' @inheritParams check_length_list
internal_check_length_list <- function(x, exact_len = NULL, min_len = NULL, max_len = NULL, flatten = FALSE, xarg = "list",
                                        raise = "error", alert_message = NULL, n_evaluation_frame = 0, quickalert = TRUE, ...){
  if(flatten) x <- rec_flatten_list(x, till_flat = TRUE)
  len_list <- length(x)
  combined_message <- core_length_test(len_list, exact_len, min_len, max_len, xarg)
  raise_length_alert(raise, combined_message, alert_message, n_evaluation_frame+1, quickalert, ...)
}