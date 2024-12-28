#' Internal check of check_vector
#' @inheritParams check_vector
internal_check_vector <- function(vec, vec_arg = "vec", null_check = FALSE, zero_len_check = FALSE, na_check = FALSE, empty_string_check = FALSE,
                          predicate = NULL, inverse = FALSE, sorted = FALSE, decreasing = FALSE, unique = FALSE, na_rm_unique = FALSE,
                          exact_len = NULL, min_len = NULL, max_len = NULL, unique_len = FALSE, na_rm_len = FALSE, include = NULL, exclude = NULL){
  check_atomic_vec(vec, vec_arg)
  
  if(null_check || zero_len_check || na_check || empty_string_check){
    internal_check_empty_vec(vec, null_check, zero_len_check, na_check, empty_string_check, vec_arg)
  }

  if(!is.null(predicate)) internal_check_predicate_vec(vec, predicate, inverse, vec_arg)
  if(sorted) internal_check_sorted_vec(vec, decreasing, vec_arg)
  if(unique) internal_check_duplicate_vec(vec, na_rm_unique, vec_arg)

  if(!is.null(exact_len) || !is.null(min_len) || !is.null(max_len)){
    internal_check_length_vec(vec, exact_len, min_len, max_len, na_rm_len, unique_len)
  }

  if(!is.null(include)) internal_check_presence_vec(vec, include, vec_arg)
  if(!is.null(exclude)) internal_check_absence_vec(vec, exclude, vec_arg)
}



#' Internal check of check_empty_vec
#' @inheritParams check_empty_vec
internal_check_empty_vec <- function(vec, null = TRUE, len = TRUE, na = TRUE, empty_string = TRUE, vec_arg = "vec",
                                    raise = "error", alert_message = NULL, n_evaluation_frame = 0, quickalert = TRUE, ...){
   if(is_empty_vec(vec, null, len, na, empty_string)){
    alert_message <- generate_message(alert_message, "{vec_arg} is or contains an empty entity.")
    alert_generator(raise, alert_message, n_evaluation_frame, quickalert, ...)
  }
}



#' Internal check of check_predicate_vec
#' @inheritParams check_predicate_vec
internal_check_predicate_vec <- function(vec, predicate, inverse = FALSE, vec_arg = "vec",
                                        raise = "error", alert_message = NULL, header = "default", n_evaluation_frame = 0, quickalert = TRUE, ...){
  predicate_log <- apply_predicate(vec, predicate, inverse)
  empty_log <- is_empty_vec(names(vec))
  errors <- get_errors_predicate(vec, empty_log, predicate_log)
  raise_predicate_error(empty_log, errors, vec_arg, inverse, raise, alert_message, n_evaluation_frame+1, quickalert, header, ...)
}



#' Internal check of check_sorted_vec
#' @inheritParams check_sorted_vec
internal_check_sorted_vec <- function(vec, decreasing = FALSE, vec_arg = "vec", raise = "error", alert_message = NULL, n_evaluation_frame = 0, quickalert = TRUE, ...){
  check_na_vec(vec, raise = "warning", alert_message = "NAs in vec, they are not considered in the sort check.", quickalert = FALSE)
  vec_known <- stats::na.omit(vec)

  # full-NA vectors, vectors with only one not-NA value and with 0 or 1 elements are considered sorted
  if(length(vec_known) < 2) return(NULL)
  sorted_vec_known <- sort(vec_known, decreasing)

  if(!isTRUE(all.equal(vec_known, sorted_vec_known, check.attributes = FALSE, check.names = FALSE))){
    alert_message <- generate_message(alert_message, "{vec_arg} is not sorted.")
    alert_generator(raise, alert_message, n_evaluation_frame, quickalert, ...)
  }
}



#' Internal check of check_duplicate_vec
#' @inheritParams check_duplicate_vec
internal_check_duplicate_vec <- function(vec, na_rm = TRUE, vec_arg = "vec", raise = "error",
                                        alert_message = NULL, header = "default", n_evaluation_frame = 0, quickalert = TRUE, ...){
  if(na_rm) vec <- stats::na.omit(vec)
  dup <- duplicated(vec)

  if(any(dup)){
    dup_values <- vec[dup] |> unique() |> sort(na.last = FALSE)
    alert_message <- generate_message(alert_message, "{cli::col_magenta(dup_values)}")
    header <- generate_header(header, "The following {cli::qty(length(dup_values))} value{?s} {?is/are} {cli::col_red('duplicated')} in {vec_arg}:")
    alert_generator(raise, alert_message, n_evaluation_frame, quickalert, header = header, ...)
  }
}



#' Internal check of check_length_vec
#' @inheritParams check_length_vec
internal_check_length_vec <- function(vec, exact_len = NULL, min_len = NULL, max_len = NULL, na_rm = TRUE, unique = FALSE, vec_arg = "vec",
                                      raise = "error", alert_message = NULL, n_evaluation_frame = 0, quickalert = TRUE, ...){
  if(na_rm) vec <- stats::na.omit(vec)
  if(unique) vec <- unique(vec)
  vec_len <- length(vec)
  combined_message <- core_length_test(vec_len, exact_len, min_len, max_len, vec_arg)
  raise_length_alert(raise, combined_message, alert_message, n_evaluation_frame+1, quickalert, ...)
}



#' Internal check of check_presence_vec
#' @inheritParams check_presence_vec
internal_check_presence_vec <- function(vec, values, vec_arg = "vec", raise = "error", alert_message = NULL, header = "default", n_evaluation_frame = 0, quickalert = TRUE, ...){
  missing_values <- values[!values %IN% unique(vec)]

  if(length(missing_values) > 0){
    alert_message <- generate_message(alert_message, "{cli::col_magenta(missing_values)}")
    header <- generate_header(header, "The following {cli::qty(length(missing_values))} value{?s} {?is/are} {cli::col_red('missing')} in {vec_arg}:")
    alert_generator(raise, alert_message, n_evaluation_frame, quickalert, header = header, ...)
  }
}



#' Internal check of check_absence_vec
#' @inheritParams check_absence_vec
internal_check_absence_vec <- function(vec, values, vec_arg = "vec", raise = "error", alert_message = NULL, header = "default", n_evaluation_frame = 0, quickalert = TRUE, ...){
  unexpected_values <- values[values %IN% unique(vec)]

  if(length(unexpected_values) > 0){
    alert_message <- generate_message(alert_message, "{cli::col_magenta(unexpected_values)}")
    header <- generate_header(header, "The following {cli::qty(length(unexpected_values))} value{?s} {?is/are} {cli::col_red('not absent')} in {vec_arg}:")
    alert_generator(raise, alert_message, n_evaluation_frame, quickalert, header = header, ...)
  }
}
