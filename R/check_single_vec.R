### Checking functions that works on single vector.


#' Checks the presence of "empty" values in a vector
#' @description
#' Checks the presence of "empty" values in a vector. Empty here is declined in its
#' broader meaning indicating NAs, "", NULL and vector of length zero.
#' @inheritParams check_columns_key
#' @inheritParams is_empty_vec
#' @param vec Vector to check.
#' @param vec_arg String indicating how to address vec in the alert message (default 'vec').
#' @return invisible NULL
#' @export
check_empty_vec <- function(vec, na = TRUE, empty_string = TRUE, len = TRUE, null = TRUE, vec_arg = "vec", raise = "error", alert_message = NULL, n_evaluation_frame = 0, quickalert = TRUE, ...){
  rlang::check_required(vec)
  if(is_empty_vec(vec, na, empty_string, len, null)){
    alert_message <- generate_message(alert_message, "There are empty values in {vec_arg}.")
    alert_generator(raise, alert_message, n_evaluation_frame, quickalert, ...)
  }
  invisible(NULL)
}



#' Checks the presence of NAs in a vector.
#' @inheritParams check_empty_vec
#' @return invisible NULL
#' @export
check_na_vec <- function(vec, vec_arg = "vec", raise = "error", alert_message = NULL, n_evaluation_frame = 0, quickalert = TRUE, ...){
  rlang::check_required(vec)
  if(any(is.na(vec))){
    alert_message <- generate_message(alert_message, "There are NAs in {vec_arg}.")
    alert_generator(raise, alert_message, n_evaluation_frame, quickalert, ...)
  }
  invisible(NULL)
}



#' Checks the presence of duplicated values in a vector.
#' @inheritParams check_empty_vec
#' @param header Character string to add at the beginning of the alert message.
#' @param na_rm Boolean indicating if NA must be excluded prior checking (default TRUE).
#' @return invisible NULL
#' @export
check_duplicate_vec <- function(vec, vec_arg = "vec", na_rm = TRUE, raise = "error", alert_message = NULL, header = "default", n_evaluation_frame = 0, quickalert = TRUE, ...){
  rlang::check_required(vec)
  if(na_rm) vec <- stats::na.omit(vec)
  dup <- duplicated(vec)

  if(any(dup)){
    dup_values <- vec[dup] |> unique() |> sort()
    alert_message <- generate_message(alert_message, "{cli::col_magenta(dup_values)}")
    header <- generate_header(header, "The following {qty(length(dup_values))} value{?s} {?is/are} {cli::col_red('duplicated')} in {vec_arg}:")
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
#' @param unique Boolean indicating whether to perform the check on the unique value of vec (default FALSE).
#' @return invisible NULL
#' @export
check_length_vec <- function(vec, exact_len = NULL, min_len = NULL, max_len = NULL, vec_arg = "vec", na_rm = TRUE, unique = FALSE, raise = "error", alert_message = NULL, n_evaluation_frame = 0, quickalert = TRUE, ...){
  rlang::check_required(vec)

  if(is.null(exact_len) && is.null(min_len) && is.null(max_len)){
    cli::cli_abort(c("x" = "At least one of exact_len, min_len and max_len arguments must be set!"))
  }

  check_integerish_args(c("exact_len", "min_len", "max_len"), null = TRUE, quickalert = FALSE)
  if(na_rm) vec <- stats::na.omit(vec)
  if(unique) vec <- unique(vec)

  alert <- alert1 <- alert2 <- NULL

  if(!is.null(exact_len) && length(vec) != exact_len){
    alert <- "Vector of length {exact_len} expected, {length(vec)} detected."
  }

  if(!is.null(min_len) && length(vec) < min_len){
    alert1 <- "Vector of minumum length {min_len} expected, {length(vec)} detected."
  }

  if(!is.null(max_len) && length(vec) > max_len){
    alert2 <- "Vector of maximimum length {max_len} expected, {length(vec)} detected."
  }

  combined_message <- c(alert, alert1, alert2)

  if(!is.null(combined_message)){
    if(raise == "error") names(combined_message) <- rep("x", length(combined_message))
    final_message <- generate_message(alert_message, combined_message)
    alert_generator(raise, final_message, n_evaluation_frame, quickalert, ...)
  }

  invisible(NULL)
}



#' Checks the presence of the specified values in a vector.
#' @inheritParams check_empty_vec
#' @param values Character vector of values searched in vec.
#' @param na_rm Boolean indicating if NAs must be excluded prior checking from both vec and values (default TRUE).
#' @param header Character string to add at the beginning of the alert message. If "default" the default header is used, otherwise the string passed in.
#' @return invisible NULL
#' @export
check_presence_vec <- function(vec, values, vec_arg = "vec", na_rm = TRUE, raise = "error", alert_message = NULL, header = "default", n_evaluation_frame = 0, quickalert = TRUE, ...){
  check_required_all()

  if(na_rm) {
    vec <- stats::na.omit(vec)
    values <- stats::na.omit(values)
  }

  unique_vec <- unique(vec)

  if(!all(values %in% vec)){
    missing_values <- setdiff(values, unique_vec)
    alert_message <- generate_message(alert_message, "{col_magenta(missing_values)}")
    header <- generate_header(header, "The following {qty(missing_values)} value{?s} {?is/are} {cli::col_red('missing')} in {vec_arg}:")
    alert_generator(raise, alert_message, n_evaluation_frame, quickalert, header = header, ...)
  }

  invisible(NULL)
}



#' Checks if a vector is sorted or not.
#' @inheritParams check_length_vec
#' @param decreasing Logical indicating whether the expect sorted order is decreasing or not (default FALSE).
#' @return invisible NULL
#' @export
check_sorted_vec <- function(vec, vec_arg = "vec", decreasing = FALSE, raise = "error", alert_message = NULL, n_evaluation_frame = 0, quickalert = TRUE, ...){
  rlang::check_required(vec)
  check_na_vec(vec, raise = "warning", alert_message = "NAs in vec, they are not considered in the sort check.", quickalert = FALSE)
  vec <- stats::na.omit(vec)

  if(any(vec != sort(vec, decreasing))){
    alert_message <- generate_message(alert_message, "{vec_arg} is not sorted.")
    alert_generator(raise, alert_message, n_evaluation_frame, quickalert, ...)
  }

  invisible(NULL)
}
