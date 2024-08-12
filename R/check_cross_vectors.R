# Checking functions that works on 2 vectors.


#' Checks if two vectors have the same length (works with all object on which length can be called).
#' @param vec1 First vector.
#' @param vec2 Second vector.
#' @param vec1_arg string indicating how to address vec1 in the raised message (default 'vec1').
#' @param vec2_arg string indicating how to address vec2 in the raised message (default 'vec2)'.
#' @param ... not of direct use.
#' @inheritParams check_columns_presence
#' @return invisible NULL
#' @export
check_length_vecs <- function(vec1, vec2, vec1_arg = "vec1", vec2_arg = "vec2", raise = "error", alert_message = NULL, quickalert = TRUE, n.evaluation_frame = 0, ...){
  check_required_all()
  if(length(vec1) != length(vec2)){
    alert_message <- generate_message(alert_message, "{vec1_arg} and {vec2_arg} have {col_red('different length')}.")
    alert_generator(raise, alert_message, n.evaluation_frame, quickalert, ...)
  }
  invisible(NULL)
}





#' Checks the ordered equality of two vectors
#' @details
#' The equality is checked thought "==", so the order of the values matter as well as the presence of repeated values.
#' If the order or the repeats must not be considered see "check_unordered_equality_vecs" function.
#' @inheritParams check_length_vecs
#' @return invisible NULL
#' @export
check_equality_vecs <- function(vec1, vec2, vec1_arg = "vec1", vec2_arg = "vec2", raise = "error", alert_message = NULL, quickalert = TRUE, n.evaluation_frame = 0, ...){
  check_required_all()
  if(any(vec1 != vec2)){
    alert_message <- generate_message(alert_message, "{vec1_arg} and {vec2_arg} {cli::col_red('are not equal')}.")
    alert_generator(raise, alert_message, n.evaluation_frame, quickalert, ...)
  }
  invisible(NULL)
}





#' Checks the unordered equality of two vectors.
#' @description
#' The equality is checked thought the %in% operator in both senses.
#' The vectors orders as well as the presence of repeated values have no importance.
#' If the order and the repetitions have a role see "check_equality_vecs" function.
#' @inheritParams check_length_vecs
#' @return invisible NULL
#' @export
check_unordered_equality_vecs <- function(vec1, vec2, vec1_arg = "vec1", vec2_arg = "vec2", raise = "error", alert_message = NULL, quickalert = TRUE, n.evaluation_frame = 0, ...){
  check_required_all()
  missing12 <- unique(vec1[!vec1 %in% vec2])
  missing21 <- unique(vec2[!vec2 %in% vec1])
  message1 <- NULL
  message2 <- NULL

  if(length(missing12) > 0){
    message1 <- c(
      "The following {qty(length(missing12))} value{?s} {?is/are} present in {vec1_arg} but missing in {vec2_arg}:",
      "{missing12}",
      "\n"
    )
  }

  if(length(missing21) > 0){
    message2 <- c(
      "The following {qty(length(missing21))} value{?s} {?is/are} present in {vec2_arg} but missing in {vec1_arg}:",
      "{missing21}"
    )
  }

  final_message <- c(message1, message2)

  if(!is.null(final_message)){
    final_message <- c("{cli::col_red('Detected differences')}:", final_message)
    alert_message <- generate_message(alert_message, final_message)
    alert_generator(raise, alert_message, n.evaluation_frame, quickalert, ...)
  }

  invisible(NULL)
}
