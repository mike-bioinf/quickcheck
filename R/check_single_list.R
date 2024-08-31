### Checking functions that work on a single list.



#' Checks if the list elements are of the same class.
#' @inheritParams check_columns_key
#' @param x list.
#' @param flatten logical, whether to flat out the list before doing the check (default TRUE).
#' @return invisible NULL.
#' @export
check_uniform_list <- function(x, flatten = TRUE, raise = "error", alert_message = NULL, n.evaluation_frame = 0, quickalert = TRUE, ...){
  check_args_primitive_types(c("x", "flatten"), c("list", "logical"), quickalert = FALSE)

  if(flatten) x <- rec_flatten_list(x, till_flat = T)
  types <- lapply(x, class)
  identity <- purrr::reduce(types, custom_identical)

  if(identity == "NotEqualClass"){
    alert_message <- generate_message(alert_message, "The list is {cli::col_red('not uniform')}.")
    alert_generator(raise, alert_message, n.evaluation_frame, quickalert, ...)
  }

  invisible(NULL)
}




#' Checks all the elements of the list through a predicate function.
#' @description
#' The function doesn't perform any check on the argument provided in predicate.
#' Therefore the correctness of the provided function fall on the user.
#' @inheritParams check_uniform_list
#' @param predicate A predicate function, usually an "is.something" function.
#' @param header Character string to add at the beginning of the alert message.
#'  If "default" the default header is used, otherwise the string passed in.
#' @details
#'  The alert will not points the eventual elements in error if no or some elements names are missing,
#'  but instead it will raise a more general alert.
#' @return invisible NULL.
#' @export
check_predicate_list <- function(x, predicate, flatten = TRUE, raise = "error", alert_message = NULL, header = "default", n.evaluation_frame = 0, quickalert = TRUE, ...){
  check_required_all()
  check_args_primitive_types(c("x", "flatten"), c("list", "logical"), quickalert = FALSE)
  check_args_classes("predicate", "function", quickalert = FALSE)

  if(flatten) x <- rec_flatten_list(x, till_flat = T)

  pred_log <- purrr::map_lgl(x, predicate)
  empty_log <- is_empty_vec(names(x))

  if(empty_log && !all(pred_log)){
    errors <- 1
  } else {
    errors <- names(x)[!pred_log]
  }

  if(length(errors) > 0){
    if(empty_log){
      header <- NULL
      alert_message <- c(
        "{cli::col_red('Not all elements')} satisfy the predicate.",
        "i" = "{cli::col_blue('Set or fill')} the missing list element names for a more informative alert."
      )
    } else {
      header <- generate_header(header, "The following {qty(errors)} element{?s} {?doesn't/don't} satisfy the predicate:")
      alert_message <- generate_message(alert_message, "{cli::col_magenta(errors)}.")
    }
    alert_generator(raise, alert_message, n.evaluation_frame, quickalert, header = header, ...)
  }

  invisible(NULL)
}




#' Checks that the list have all elements with defined names
#' @description The function checks for NULL, NA and "".
#' @inheritParams check_uniform_list
#' @return invisible NULL.
#' @export
check_names_list <- function(x, flatten = TRUE, raise = "error", alert_message = NULL, n.evaluation_frame = 0, quickalert = TRUE, ...){
  rlang::check_required(x)
  check_args_primitive_types("x", "list", quickalert = FALSE)

  if(flatten) x <- rec_flatten_list(x, till_flat = T)

  if(is_empty_vec(names(x))){
    alert_message <- generate_message(alert_message, "The provided list present {cli::col_red('missing element names')}.")
    alert_generator(raise, alert_message, n.evaluation_frame, quickalert, ...)
  }

  invisible(NULL)
}






#### HELPERS =======================================================================================================================================0

#' Helper of check_uniform_list.
#' @param x first element.
#' @param y second element.
custom_identical <- function(x, y){
  if(identical(x, y)){
    return(y)
  } else {
    return("NotEqualClass")
  }
}
