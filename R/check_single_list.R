### Checking functions that work on a single list.



#' Checks if the elements of the list are of the same class.
#' @inheritParams check_columns_presence
#' @param x list.
#' @param flatten logical, whether to flat out the list before before doing the check (default TRUE).
#' @return invisible NULL.
#' @export
check_uniform_list <- function(x, flatten = TRUE, raise = "error", alert_message = NULL, n.evaluation_frame = 2, quickalert = TRUE){
  check_args_primitive_types("x", "list")

  if(flatten) {x <- rec_flatten_list(x, till_flat = T)}
  types <- lapply(x, class)
  res_identity <- purrr::reduce(types, custom_identical)

  if(res_identity == "NotExistingCLASS"){
    alert_message <- generate_message(alert_message, "The list is {cli::col_red('not uniform')}.")
    alert_generator(raise, alert_message, n.evaluation_frame, quickalert)
  }

  invisible(NULL)
}





#' Checks the types of all elements of the list through a predicate function.
#' @description
#' The function doesn't perform any check on the argument provided in predicate.
#' Therefore the correctness of the provided function fall on the user.
#' @inheritParams check_uniform_list
#' @param predicate a predicate function, usually an "is.something" function type.
#' @details The alert will not points the eventual elements in error if no or some elements names are missing.
#' @return invisible NULL.
#' @export
check_types_list <- function(x, predicate, flatten = TRUE, raise = "error", alert_message = NULL, n.evaluation_frame = 2, quickalert = TRUE){
  check_required_all()
  check_args_primitive_types(c("x", "flatten"), c("list", "logical"))
  check_args_classes("predicate", "function")

  if(flatten){
    x <- rec_flatten_list(x, till_flat = T, nam_spec = "{outer}${inner}")
  }

  all_types <- purrr::map_lgl(x, predicate)
  errors <- names(x)[!all_types]

  if(length(errors) > 0){
    if(is_empty_vec(names(x))){
      alert_message <- c(
        "{cli::col_red('Not all elements')} are of the expected type.",
        "i" = "{cli::col_blue('Set or fill')} the missing list element names for a more informative alert."
      )
    } else {
      alert_message <- generate_message(
        alert_message,
        c("The following {qty(errors)} element{?s} {?is/are} not of the expected type:", "{cli::col_magenta(errors)}")
      )
    }
    alert_generator(raise, alert_message, n.evaluation_frame, quickalert)
  }

  invisible(NULL)
}






#### HELPERS =======================================================================================================================

#' Helper of check_uniform_list.
#' @param x first element.
#' @param y second element.
custom_identical <- function(x, y){
  if(identical(x, y)){
    return(y)
  } else {
    return("NotExistingCLASS")
  }
}
