### internal functions for check_length_* functions -------------------------------------------------------------


#' Internal check for .*_len arguments of check_length_vec and check_length_list
#' @inheritParams check_length_vec
check_len_args <- function(exact_len, min_len, max_len){
  if(is.null(exact_len) && is.null(min_len) && is.null(max_len)){
    cli::cli_abort(c("x" = "At least one of exact_len, min_len and max_len arguments must be set!"))
  }
  check_args(
    args = c("exact_len", "min_len", "max_len"),
    expected_types = "integerish",
    flag = TRUE,
    null = TRUE,
    with = "check_integerish",
    recycle_expected_types = 3,
    quickalert = FALSE
  )
}


#' Internal function that does the checking for the check_length_.* funcs and builds the alert_message
#' @param len_object Length of the object to check and report in the alert messages.
#' @inheritParams check_length_vec
#' @param string_object String to adress the object in the alert messages.
#' @return The alert message or NULL if no alert must be raised.
core_length_test <- function(len_object, exact_len, min_len, max_len, string_object){
  exact_alert <- min_alert <- max_alert <- NULL

  if(!is.null(exact_len) && len_object != exact_len){
    exact_alert <- paste0(string_object, " of length {exact_len} expected, ", len_object, " detected.")
  }

  if(!is.null(min_len) && len_object < min_len){
    min_alert <- paste0(string_object, " of minimum length {min_len} expected, ", len_object, " detected.")
  }

  if(!is.null(max_len) && len_object > max_len){
    max_alert <- paste0(string_object, " of maximum length {max_len} expected, ", len_object, " detected.")
  }

  combined_message <- c(exact_alert, min_alert, max_alert)
  return(combined_message)
}


#' Helper to raise alerts of check_length_* funcs.
#' @inheritParams check_empty_vec
#' @param combined_message alert build by core_length_test function.
raise_length_alert <- function(raise, combined_message, alert_message, n_evaluation_frame, quickalert, ...){
  if(!is.null(combined_message)){
    if(raise == "error") names(combined_message) <- rep("x", length(combined_message))
    final_message <- generate_message(alert_message, combined_message)
    alert_generator(raise, final_message, n_evaluation_frame, quickalert, ...)
  }
}




### internal functions for check_predicate_* funcs ---------------------------------------------------------------

#' Apply The predicate function on the object and returns the resulting logical vector.
#' @param object Vector or list.
#' @inheritParams check_predicate_list
apply_predicate <- function(object, predicate, inverse){
  predicate_log <- purrr::map_lgl(object, predicate)
  if(inverse) predicate_log <- !predicate_log
  return(predicate_log)
}


#' Retrieve and returns the elements in error.
#' Wants in input a boolena indicating if the vector/list has empty names.
#' In this last case it returns a value 1 as error.
#' @param object Vector or list.
#' @param log_empty_names Boolean indicating if the object on which the predicate is checked has empty names.
#' @param predicate_log Boolean vector with the result of the application of the predicate.
get_errors_predicate <- function(object, log_empty_names, predicate_log){
  if(log_empty_names && !all(predicate_log)){
    errors <- 1
  } else {
    errors <- names(object)[!predicate_log]
  }
  return(errors)
}


#' Helper to raise alert for check_predicate_* funcs.
#' @inheritParams check_predicate_list
#' @inheritParams get_errors_predicate
#' @param errors Vector with the names of the error elements or 1.
#' @param string_object String with the name of the object to use inside the alerts.
raise_predicate_error <- function(log_empty_names, errors, string_object, inverse, raise, alert_message, n_evaluation_frame, quickalert, header, ...){
  if(length(errors) > 0){
    inverse_string <- ""
    if(log_empty_names){
      header <- NULL
      if(inverse) inverse_string <- "NOT "
      alert_message <- c(
        paste0("{cli::col_red('Not all elements')} ", inverse_string, "satisfy the predicate in ", string_object, "."),
        "i" = "{cli::col_blue('Set or fill')} the missing element names for a more informative alert."
      )
    } else {
      if(inverse) inverse_string <- "inverse of the "
      header <- generate_header(header, paste0("The following {cli::qty(length(errors))} element{?s} {?doesn't/don't} satisfy the ", inverse_string, "predicate in ", string_object, ":"))
      alert_message <- generate_message(alert_message, "{cli::col_magenta(errors)}.")
    }
    alert_generator(raise, alert_message, n_evaluation_frame, quickalert, header = header, ...)
  }

  invisible(NULL)
}




### internal functions for check_*_vec functions ----------------------------------------------------------------------------

#' Manage sorting, NAs and duplicates removal for a vector.
#' @param vec Input vector.
#' @param decreasing Boolean indicating the sorting order.
#' @param na_rm Boolean indicating if NA must be removed.
#' @inheritParams check_equality_vecs
#' @return The cleaned vector.
clean_vec <- function(vec, na_rm = FALSE, unique = FALSE, sort = FALSE, decreasing = FALSE){
  if(na_rm) vec <- stats::na.omit(vec)
  if(unique) vec <- unique(vec)
  if(sort) vec <- sort(vec, decreasing, na.last = FALSE)
  return(vec)
}




### General ---------------------------------------------------------------------------------------------------------------------

#' Alternative version of the %in% operator that checks on identical set of classes for vec1 and vec2.
#' If the set of classes is different (not identical) then returns a vector of FALSE raising a warning.
#' @param vec1 First vector.
#' @param vec2 Second vector.
#' @return A logical vector.
`%IN%` <- function(vec1, vec2){
  if(is.null(vec1) || is.null(vec2)) force_alert <- FALSE else force_alert <- TRUE

  unequal_class <- impose_logical_behavior(
    {
      internal_check_identical_vecs(
        vec1 = class(vec1),
        vec2 = class(vec2),
        raise = "warning",
        alert_message = c("The two vectors have {.strong  DIFFERENT} set of classes", "A vector of FALSE is automatically returned.")
      )
    },
    force_alert = force_alert,
    strip_quickalert = TRUE
  )
  
  if(unequal_class) return(rep(FALSE, length(vec1)))
  return(vec1 %in% vec2)
}