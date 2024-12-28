#' Internal check of check_length_vecs
#' @inheritParams check_length_vecs
internal_check_length_vecs <- function(vec1, vec2, na_rm = FALSE, unique = FALSE, vec1_arg = "vec1", vec2_arg = "vec2",
                                      raise = "error", alert_message = NULL, quickalert = TRUE, n_evaluation_frame = 0, ...){
  vec1 <- clean_vec(vec1, na_rm, unique)
  vec2 <- clean_vec(vec2, na_rm, unique)

  if(length(vec1) != length(vec2)){
    alert_message <- generate_message(alert_message, "{vec1_arg} and {vec2_arg} have {cli::col_red('different length')}.")
    alert_generator(raise, alert_message, n_evaluation_frame, quickalert, ...)
  }
}



#' Internal check of check_identical_vecs
#' @inheritParams check_identical_vecs
internal_check_identical_vecs <- function(vec1, vec2, na_rm = FALSE, unique = FALSE, sort = FALSE, vec1_arg = "vec1", vec2_arg = "vec2",
                                          raise = "error", alert_message = NULL, quickalert = TRUE, n_evaluation_frame = 0, ...){
  vec1 <- clean_vec(vec1, na_rm, unique, sort)
  vec2 <- clean_vec(vec2, na_rm, unique, sort)

  if(!identical(vec1, vec2)){
    alert_message <- generate_message(alert_message, "{vec1_arg} and {vec2_arg} are {cli::col_red('not identical')}.")
    alert_generator(raise, alert_message, n_evaluation_frame, quickalert, ...)
  }
}
