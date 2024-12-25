# Checking functions that works on 2 vectors.


#' Check whether two vectors have the same length
#' @param vec1 First vector.
#' @param vec2 Second vector.
#' @param vec1_arg string indicating how to address vec1 in the raised message (default 'vec1').
#' @param vec2_arg string indicating how to address vec2 in the raised message (default 'vec2)'.
#' @param na_rm Boolean indicating if NA must be excluded prior checking (default FALSE).
#' @inheritParams check_columns_key
#' @inheritParams check_length_vec
#' @inherit check_empty_vec return
#' @export
check_length_vecs <- function(vec1, vec2, na_rm = FALSE, unique = FALSE, vec1_arg = "vec1", vec2_arg = "vec2",
                              raise = "error", alert_message = NULL, quickalert = TRUE, n_evaluation_frame = 0, ...){
  check_required_all()
  vec1 <- clean_vec(vec1, na_rm, unique)
  vec2 <- clean_vec(vec2, na_rm, unique)

  if(length(vec1) != length(vec2)){
    alert_message <- generate_message(alert_message, "{vec1_arg} and {vec2_arg} have {cli::col_red('different length')}.")
    alert_generator(raise, alert_message, n_evaluation_frame, quickalert, ...)
  }
  invisible(NULL)
}



#' Check whether two vectors are identical
#' @details
#' The function checks for identical vectors using the "identical" function.
#' @inheritParams check_length_vecs
#' @param sort Boolean, whether to sort the vectors before performing the check (default FALSE).
#' @inherit check_empty_vec return
#' @export
check_identical_vecs <- function(vec1, vec2, na_rm = FALSE, unique = FALSE, sort = FALSE, vec1_arg = "vec1", vec2_arg = "vec2",
                                 raise = "error", alert_message = NULL, quickalert = TRUE, n_evaluation_frame = 0, ...){
  check_required_all()
  vec1 <- clean_vec(vec1, na_rm, unique, sort)
  vec2 <- clean_vec(vec2, na_rm, unique, sort)

  if(!identical(vec1, vec2)){
    alert_message <- generate_message(alert_message, "{vec1_arg} and {vec2_arg} are {cli::col_red('not identical')}.")
    alert_generator(raise, alert_message, n_evaluation_frame, quickalert, ...)
  }

  invisible(NULL)
}



#' Check the ordered equality of two vectors
#' @description
#' Equality is verified using "==", meaning that both the order of values and the presence of duplicates are taken into account.
#' To ignore order or duplicates, consider using the "check_matching_vecs" function.
#' In addition the equality operator allows for automating recycling and coercion, behaviors that can be indirectly controlled
#' performing preliminary checks using the recycle and coerce parameters (check on equality of vector lengths and classes, respectively).
#' Note that unique and sort args have precedence over the recycle one, meaning the reciclying is done after sorting and duplicates removal.
#' @inheritParams check_identical_vecs
#' @param recycle
#' Boolean indicating whether to allow vector recycling (default FALSE).
#' @param coerce
#' Boolean indicating whether the two vectors can be coerced during the check (default FALSE).
#' If FALSE a check on vector classes is performed before the main check. This check is performed using the identical function.
#' Therefore the vector classes must be perfectly identical in order to pass the check.
#' @details NAs are always removed since comparisons involving them result always in NAs using the equality operator (a warning is raised).
#' @inherit check_empty_vec return
#' @export
check_equality_vecs <- function(vec1, vec2, unique = FALSE, sort = FALSE, recycle = FALSE, coerce = FALSE, vec1_arg = "vec1", vec2_arg = "vec2",
                                 raise = "error", alert_message = NULL, quickalert = TRUE, n_evaluation_frame = 0, ...){
  check_required_all()
  
  if(any(is.na(c(vec1, vec2)))){
    cli::cli_warn(c("!" = "NAs in input vectors are removed prior checking."))
  }

  vec1 <- clean_vec(vec1, na_rm = TRUE, unique, sort)
  vec2 <- clean_vec(vec2, na_rm = TRUE, unique, sort)

  if(!recycle) {
    check_length_vecs(
      vec1 = vec1,
      vec2 = vec2,
      na_rm = TRUE,
      vec1_arg = vec1_arg,
      vec2_arg = vec2_arg,
      quickalert = FALSE
    )
  }

  if(!coerce){
    check_identical_vecs(
      vec1 = class(vec1),
      vec2 = class(vec2),
      vec1_arg = vec1_arg,
      vec2_arg = vec2_arg,
      alert_message = "{vec1_arg} and {vec2_arg} classes are different!",
      quickalert = FALSE
    )
  }

  if(any(vec1 != vec2)){
    alert_message <- generate_message(alert_message, "{vec1_arg} and {vec2_arg} {cli::col_red('are not equal')}.")
    alert_generator(raise, alert_message, n_evaluation_frame, quickalert, ...)
  }

  invisible(NULL)
}



#' Check the unordered equality of two vectors
#' @description
#' The equality is checked thought the matching %in% operator in both senses.
#' The "%in%" operator uses automatic coercion. To control this behavior see the coerce parameter.
#' The vectors orders as well as the presence of repeated values have no importance.
#' If the order and the repetitions have a role see "check_equality_vecs" function.
#' @inheritParams check_equality_vecs
#' @inheritParams check_length_vecs
#' @inherit check_empty_vec return
#' @export
check_matching_vecs <- function(vec1, vec2, na_rm = FALSE, coerce = FALSE, vec1_arg = "vec1", vec2_arg = "vec2", 
                                raise = "error", alert_message = NULL, quickalert = TRUE, n_evaluation_frame = 0, ...){
  check_required_all()

  if(!coerce){
    check_identical_vecs(
      vec1 = class(vec1),
      vec2 = class(vec2),
      vec1_arg = vec1_arg,
      vec2_arg = vec2_arg,
      alert_message = "{vec1_arg} and {vec2_arg} classes are different!",
      quickalert = FALSE
    )
  }

  vec1 <-  clean_vec(vec1, na_rm)
  vec2 <-  clean_vec(vec2, na_rm)

  missing12 <- unique(vec1[!vec1 %in% vec2])
  missing21 <- unique(vec2[!vec2 %in% vec1])

  message1 <- NULL
  message2 <- NULL

  if(length(missing12) > 0){
    message1 <- c(
      "The following {cli::qty(length(missing12))} value{?s} {?is/are} present in {vec1_arg} but missing in {vec2_arg}:",
      "{missing12}"
    )
  }

  if(length(missing21) > 0){
    message2 <- c(
      "The following {cli::qty(length(missing21))} value{?s} {?is/are} present in {vec2_arg} but missing in {vec1_arg}:",
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
