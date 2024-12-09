# Checking functions that works on 2 vectors.


#' Checks whether two vectors have the same length
#' @param vec1 First vector.
#' @param vec2 Second vector.
#' @param vec1_arg string indicating how to address vec1 in the raised message (default 'vec1').
#' @param vec2_arg string indicating how to address vec2 in the raised message (default 'vec2)'.
#' @param ... not of direct use.
#' @inheritParams check_columns_key
#' @return invisible NULL
#' @export
check_length_vecs <- function(vec1, vec2, vec1_arg = "vec1", vec2_arg = "vec2", raise = "error", alert_message = NULL, quickalert = TRUE, n_evaluation_frame = 0, ...){
  check_required_all()
  if(length(vec1) != length(vec2)){
    alert_message <- generate_message(alert_message, "{vec1_arg} and {vec2_arg} have {col_red('different lengths')}.")
    alert_generator(raise, alert_message, n_evaluation_frame, quickalert, ...)
  }
  invisible(NULL)
}



#' Checks whether two vectors are identical
#' @details
#' The function checks for **identical** vectors using the identical function.
#' @inheritParams check_length_vecs
#' @export
check_identical_vecs <- function(vec1, vec2, vec1_arg = "vec1", vec2_arg = "vec2", raise = "error", alert_message = NULL, quickalert = TRUE, n_evaluation_frame = 0, ...){
  check_required_all()
  if(!identical(vec1, vec2)){
    alert_message <- generate_message(alert_message, "{vec1_arg} and {vec2_arg} are {cli::col_red('not identical')}.")
    alert_generator(raise, alert_message, n_evaluation_frame, quickalert)
  }
  invisible(NULL)
}



#' Checks the ordered equality of two vectors
#' @description
#' Equality is verified using "==", meaning that both the order of values and the presence of duplicates are taken into account.
#' To ignore order or duplicates, consider using the "check_unordered_equality_vecs" function.
#' In addition the equality operator allows for automating recycling and coercion, behaviors that can be indirectly controlled
#' performing preliminary checks using the recycle and coerce parameters.
#' @inheritParams check_length_vecs
#' @param recycle
#' Boolean indicating whether to allow vector recycling (default FALSE).
#' @param coerce
#' Boolean indicating whether the two vectors can be coerced during the check (default FALSE).
#' If FALSE a check on vector classes is performed before the main check. This check is performed using the identical function.
#' Therefore the vector classes must be perfectly identical in order to pass the check.
#' @return invisible NULL
#' @export
check_equality_vecs <- function(vec1, vec2, vec1_arg = "vec1", vec2_arg = "vec2", recycle = FALSE, coerce = FALSE, raise = "error", alert_message = NULL, quickalert = TRUE, n_evaluation_frame = 0, ...){
  check_required_all()
  if(!recycle) check_length_vecs(vec1, vec2, vec1_arg, vec2_arg, quickalert = FALSE)
  if(!coerce) check_identical_vecs(class(vec1), class(vec2), vec1_arg, vec2_arg, alert_message = "{vec1_arg} and {vec2_arg} classes are different!", quickalert = FALSE)

  if(any(vec1 != vec2)){
    alert_message <- generate_message(alert_message, "{vec1_arg} and {vec2_arg} {cli::col_red('are not equal')}.")
    alert_generator(raise, alert_message, n_evaluation_frame, quickalert, ...)
  }

  invisible(NULL)
}



#' Checks the unordered equality of two vectors
#' @description
#' The equality is checked thought the %in% operator in both senses.
#' The "%in%" operator alloes for automatic coercion. To control this behaviour see the coerce parameter.
#' The vectors orders as well as the presence of repeated values have no importance.
#' If the order and the repetitions have a role see "check_equality_vecs" function.
#' @inheritParams check_length_vecs
#' @inheritParams check_equality_vecs
#' @return invisible NULL
#' @export
check_unordered_equality_vecs <- function(vec1, vec2, vec1_arg = "vec1", vec2_arg = "vec2", coerce = FALSE, raise = "error", alert_message = NULL, quickalert = TRUE, n_evaluation_frame = 0, ...){
  check_required_all()
  if(!coerce) check_identical_vecs(class(vec1), class(vec2), vec1_arg, vec2_arg, alert_message = "{vec1_arg} and {vec2_arg} classes are different!", quickalert = FALSE)

  missing12 <- unique(vec1[!vec1 %in% vec2])
  missing21 <- unique(vec2[!vec2 %in% vec1])
  message1 <- NULL
  message2 <- NULL

  if(length(missing12) > 0){
    message1 <- c(
      "The following {qty(length(missing12))} value{?s} {?is/are} present in {vec1_arg} but missing in {vec2_arg}:",
      "{missing12}"
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
    alert_generator(raise, alert_message, n_evaluation_frame, quickalert, ...)
  }

  invisible(NULL)
}
