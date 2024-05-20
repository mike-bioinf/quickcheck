# Checking functions that works on 2 vectors.



#' Checks if two vectors have the same length
#' @param vec1 First vector.
#' @param vec2 Second vector.
#' @param vec1_arg string indicating how to address vec1 in the raised message (default 'vec1').
#' @param vec2_arg string indicating how to address vec2 in the raised message (default 'vec2)'.
#' @inheritParams check_columns_presence
#' @return invisible NULL
#' @export
check_length_vecs <- function(vec1, vec2, vec1_arg = "vec1", vec2_arg = "vec2", raise = "error", alert_message = NULL, n.evaluation_frame = 2, ...){
  if(length(vec1) != length(vec2)){
    if(is.null(alert_message)){
      alert_message <- "{vec1_arg} and {vec2_arg} have {col_red('different length')}"
    }
    alert_generator(raise, alert_message, n.evaluation_frame, ...)
  }
  invisible(NULL)
}




#' Checks the equality of two vectors
#' @inheritParams check_length_vecs
#' @return invisible NULL
#' @export
check_equality_vecs <- function(vec1, vec2, vec1_arg = "vec1", vec2_arg = "vec2", raise = "error", alert_message = NULL, n.evaluation_frame = 2, ...){
  if(any(vec1 != vec2)){
    if(is.null(alert_message)){
      alert_message <- "{vec1_arg} and {vec2_arg} {col_red('are not equal')}"
    }
    alert_generator(raise, alert_message, n.evaluation_frame, ...)
  }
  invisible(NULL)
}



