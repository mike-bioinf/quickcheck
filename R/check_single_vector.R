### Checking functions that works on single vector.


#' Checks the presence of "empty" values in one vector. Empty here is declined in its
#' broader meaning indicating NAs, "", NULL and vector of length zero.
#' @param vec vector to check
#' @param vec_arg string indicating how to address vec in the alert message (default 'vec')
#' @inheritParams check_columns_presence
#' @return invisible NULL
#' @export
check_empty_vec <- function(vec, vec_arg = "vec", raise = "error", alert_message = NULL){
  if(is_empty_vec(vec)){
    if(is.null(alert_message)){
      alert_message <- "There are empty values in {vec_arg}"
    }
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
