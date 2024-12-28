### Checking functions that works on single vector.


#' Ensemble check for an atomic vector
#' @description
#' Allow to test for different proprieties of an atomic vector. For the definition of “atomic”, see \link[base]{is.atomic}.
#' The properties are tested in the order they compare as parameters function.
#' @param vec Vector to check. Must be an atomic vector (see function description).
#' @param vec_arg String reporting how to adress the vec in the alerts (default "vec").
#' @param null_check Boolean, indicating whether to check for a NULL vec (default FALSE).
#' @param zero_len_check Boolean, indicating whether to check for a zero-length vec (default FALSE).
#' @param na_check Boolean, indicating whether to check for absence of NAs (default FALSE).
#' @param empty_string_check Boolean, indicating whether to check for absence of empty strings "" (default FALSE).
#' @param predicate A predicate function to apply to all the elements of vec (default NULL).
#' @param inverse Boolean, whether the predicate must be NOT satisfied for all the elements for a successful check (default FALSE).
#' @param sorted Boolean, is vec sorted? (default FALSE).
#' @param decreasing Boolean, should the sort check be performed in decreasing order? (default FALSE).
#' @param unique Boolean, should be vec a vector of unique values (default FALSE).
#' @param na_rm_unique Boolean, should the NAs be removed for the unique-check (default FALSE).
#' @param exact_len Integer, indicating the exact expected vector length (default NULL).
#' @param min_len Integer, indicating the minimum expected vector length (default NULL).
#' @param max_len Integer, indicating the maximum expected vector length (default NULL).
#' @param unique_len Boolean, should the length-check be performed on only the unique values of vec (default FALSE).
#' @param na_rm_len Boolean, should the NAs be removed for the length-check (default FALSE).
#' @param include Vector of values wich presence is checked (default NULL).
#' @param exclude Vector of values wich absence is checked (default NULL).
#' @inherit check_atomic_vec return
#' @export
check_vector <- function(vec, vec_arg = "vec", null_check = FALSE, zero_len_check = FALSE, na_check = FALSE, empty_string_check = FALSE,
                          predicate = NULL, inverse = FALSE, sorted = FALSE, decreasing = FALSE, unique = FALSE, na_rm_unique = FALSE,
                          exact_len = NULL, min_len = NULL, max_len = NULL, unique_len = FALSE, na_rm_len = FALSE, include = NULL, exclude = NULL){
  rlang::check_required(vec)

  check_args_classes(
    args = c("vec_arg", "null_check", "zero_len_check", "na_check", "empty_string_check", "inverse", "sorted", "decreasing", "unique", "na_rm_unique", "unique_len", "na_rm_len"),
    expected_classes = c("character", "logical"),
    numeric_correspondence = c(1, 11),
    quickalert = FALSE
  )

  check_args_classes("predicate", "function", null = TRUE, quickalert = FALSE)
  check_integerish_args(c("exact_len", "min_len", "max_len"), null = TRUE, quickalert = FALSE)
  check_atomic_vec(include, vec_arg = "include")
  check_atomic_vec(exclude, vec_arg = "exclude")
  
  internal_check_vector(
    vec, vec_arg, null_check, zero_len_check, na_check, empty_string_check, predicate, inverse, sorted, decreasing,
    unique, na_rm_unique, exact_len, min_len, max_len, unique_len, na_rm_len, include, exclude
  )

  invisible(NULL)
}



#' Checks wheter a vector is atomic
#' @description
#' An atomic vector is defined slightly different from specifications in \link[base]{is.atomic},
#' in the sense that it can logical, integer, numeric, complex, character, or NULL, but it cannot be
#' a matrix or a factor. In short atomic here is declined as: is.atomic(vec) && !is.matrix(vec) && !is.factor(vec).
#' @param vec 
#' Vector to check.
#' @param vec_arg 
#' String indicating how to address vec in the alert message (default 'vec').
#' @param raise 
#' Character string equal to one of "error", "warning" or "message" (default error). Set the type of alert that is created.
#' @param alert_message
#' Character vector reporting the alert message. Default NULL, in this case a standard message is used.
#' It's also possible to pass a list of strings that is displayed as a nominated or numbered list.
#' @param n_evaluation_frame
#' numeric, defines the number of stack frame to look down for the evaluation of the glue expressions of the alert message.
#' The default value (0) points to the frame above this function frame. So to point to the frame below this function frame you have to set 2.
#' @param quickalert logical, whether the raised alert has to be of class "quickalert".
#' @param ... To pass the additional parameters sign, list_format and header (if not in the formals parameters). 
#' @return
#' Depending on the function prefix: the "check" function returns the condition otherwise NULL invisibly,
#' the "test" function returns TRUE if the condition would be raised and FALSE otherwise.
#' @export
check_atomic_vec <- function(vec, vec_arg = "vec", raise = "error", alert_message = NULL, n_evaluation_frame = 0, quickalert = TRUE, ...){
  rlang::check_required(vec)
  if(!is.atomic(vec) || is.matrix(vec) || is.factor(vec)){
    alert_message <- generate_message(alert_message, "{vec_arg} is not an {cli::col_red('atomic vector')}.")
    alert_generator(raise, alert_message, n_evaluation_frame, quickalert, ...)
  }
  invisible(NULL)
}



#' Checks the presence of null values in a vector in a broader sense.
#' @param vec vector to test.
#' @param na Boolean, indicating whether to perform the check for NAs (default TRUE).
#' @param empty_string Boolean, indicating whether to perform the check for empty string "" (default TRUE).
#' @param len Boolean, indicating whether to perform the check for 0 length (default TRUE).
#' @param null Boolean, indicating whether to perform the check for NULL value (default TRUE).
#' @return A single boolean, FALSE even if one value is empty, TRUE otherwise.
#' @export
is_empty_vec <- function(vec, null = TRUE, len = TRUE, na = TRUE, empty_string = TRUE){
  if(null && is.null(vec)) return(TRUE)
  if(len && length(vec) == 0) return(TRUE)
  if(empty_string && "" %in% vec) return(TRUE)
  if(na && any(is.na(vec))) return(TRUE)
  return(FALSE)
}



#' Check the emptiness of a vector
#' @description
#' Checks the emptiness of a vector and or the presence of "empty" values in it.
#' Empty here is declined in its broader meaning indicating NAs, "", NULL and vector of length zero.
#' This interpretation can be fine tuned using the parameters function.
#' @inheritParams check_atomic_vec
#' @inheritParams is_empty_vec
#' @inherit check_atomic_vec return
#' @export
check_empty_vec <- function(vec, null = TRUE, len = TRUE, na = TRUE, empty_string = TRUE, vec_arg = "vec",
                             raise = "error", alert_message = NULL, n_evaluation_frame = 0, quickalert = TRUE, ...){
  rlang::check_required(vec)
  check_atomic_vec(vec, vec_arg, quickalert = FALSE)
  n_evaluation_frame <- raise_custom_frame(n_evaluation_frame, 1)
  internal_check_empty_vec(vec, null, len, na, empty_string, vec_arg, raise, alert_message, n_evaluation_frame, quickalert, ...)
  invisible(NULL)
}



#' Check the presence of NAs in a vector
#' @inheritParams check_atomic_vec
#' @inherit check_atomic_vec return
#' @export
check_na_vec <- function(vec, vec_arg = "vec", raise = "error", alert_message = NULL, n_evaluation_frame = 0, quickalert = TRUE, ...){
  rlang::check_required(vec)
  check_atomic_vec(vec, vec_arg, quickalert = FALSE)
  if(any(is.na(vec))){
    alert_message <- generate_message(alert_message, "There are NAs in {vec_arg}.")
    alert_generator(raise, alert_message, n_evaluation_frame, quickalert, ...)
  }
  invisible(NULL)
}



#' Check vector elements through a predicate function
#' @description
#' Checks whether the predicate returns TRUE for all elements of the vector.
#' The function doesn't perform any check on the argument provided in predicate.
#' Therefore the correctness of the provided function falls on the user.
#' @inheritParams check_predicate_list
#' @inheritParams check_atomic_vec
#' @inherit check_atomic_vec return
#' @export
check_predicate_vec <- function(vec, predicate, inverse = FALSE, vec_arg = "vec", raise = "error", 
                                alert_message = NULL, header = "default", n_evaluation_frame = 0, quickalert = TRUE, ...){
  check_required_all()
  check_atomic_vec(vec, vec_arg, quickalert = FALSE)
  check_args_classes("predicate", "function", quickalert = FALSE)
  n_evaluation_frame <- raise_custom_frame(n_evaluation_frame, 1)
  internal_check_predicate_vec(vec, predicate, inverse, vec_arg, raise, alert_message, header, n_evaluation_frame, quickalert, ...)
  invisible(NULL)
}



#' Check whether a vector is sorted
#' @inheritParams check_length_vec
#' @param decreasing Boolean indicating whether the expect sorted order is decreasing (default FALSE).
#' @details
#' NAs in the vector are ignored during the check. A zero-length vector is treated as sorted.
#' Internally, the check is performed by comparing the vector to its sorted version.
#' The comparison is conducted using the `all.equal` function to mitigate issues with floating-point precision, utilizing the default tolerance.
#' @inherit check_atomic_vec return
#' @export
check_sorted_vec <- function(vec, decreasing = FALSE, vec_arg = "vec", raise = "error", alert_message = NULL, n_evaluation_frame = 0, quickalert = TRUE, ...){
  rlang::check_required(vec)
  check_atomic_vec(vec, vec_arg, quickalert = FALSE)
  n_evaluation_frame <- raise_custom_frame(n_evaluation_frame, 1)
  internal_check_sorted_vec(vec, decreasing, vec_arg, raise, alert_message, n_evaluation_frame, quickalert, ...)
  invisible(NULL)
}



#' Check the presence of duplicated values in a vector
#' @inheritParams check_atomic_vec
#' @param header Character string to add at the beginning of the alert message.
#' @param na_rm Boolean indicating if NA must be excluded prior checking (default TRUE).
#' @inherit check_atomic_vec return
#' @export
check_duplicate_vec <- function(vec, na_rm = TRUE, vec_arg = "vec", raise = "error", alert_message = NULL, header = "default", n_evaluation_frame = 0, quickalert = TRUE, ...){
  rlang::check_required(vec)
  check_atomic_vec(vec, vec_arg, quickalert = FALSE)
  n_evaluation_frame <- raise_custom_frame(n_evaluation_frame, 1)
  internal_check_duplicate_vec(vec, na_rm, vec_arg, raise, alert_message, header, n_evaluation_frame, quickalert, ...)
  invisible(NULL)
}



#' Check on vector length
#' @details
#' The function does not check the logical validity of the values passed in the *_len arguments.
#' So for example it's possible to pass a minimun length of 10 and a maximimum of 3.
#' @inheritParams check_atomic_vec
#' @param exact_len Integer indicating the exact expected vector length (default NULL).
#' @param min_len Integer indicating the minimum expected vector length (default NULL).
#' @param max_len Integer indicating the maximum expected vector length (default NULL).
#' @param na_rm Boolean indicating if NA must be excluded prior checking (default TRUE).
#' @param unique Boolean indicating whether to perform the check only on unique values (default FALSE).
#' @inherit check_atomic_vec return
#' @export
check_length_vec <- function(vec, exact_len = NULL, min_len = NULL, max_len = NULL, na_rm = TRUE, unique = FALSE, vec_arg = "vec",
                              raise = "error", alert_message = NULL, n_evaluation_frame = 0, quickalert = TRUE, ...){
  rlang::check_required(vec)
  check_atomic_vec(vec, vec_arg, quickalert = FALSE)
  check_len_args(exact_len, min_len, max_len)
  n_evaluation_frame <- raise_custom_frame(n_evaluation_frame, 1)
  internal_check_length_vec(vec, exact_len, min_len, max_len, na_rm, unique, vec_arg, raise, alert_message, n_evaluation_frame, quickalert, ...)
  invisible(NULL)
}



#' Check the presence of specified values in a vector
#' @inheritParams check_atomic_vec
#' @param values Character vector of values searched in vec.
#' @param header Character string to add at the beginning of the alert message. If "default" the default header is used, otherwise the string passed in.
#' @details vec and values vectors must share the same set of classes (this is enforced via "identical" function).
#' @inherit check_atomic_vec return
#' @export
check_presence_vec <- function(vec, values, vec_arg = "vec", raise = "error", alert_message = NULL, header = "default", n_evaluation_frame = 0, quickalert = TRUE, ...){
  check_required_all()
  check_atomic_vec(vec, vec_arg, quickalert = FALSE)
  n_evaluation_frame <- raise_custom_frame(n_evaluation_frame, 1)
  internal_check_presence_vec(vec, values, vec_arg, raise, alert_message, header, n_evaluation_frame, quickalert, ...)
  invisible(NULL)
}



#' Check the absence of specified values in a vector
#' @inheritParams check_presence_vec
#' @inherit check_atomic_vec return
#' @export
check_absence_vec <- function(vec, values, vec_arg = "vec", raise = "error", alert_message = NULL, header = "default", n_evaluation_frame = 0, quickalert = TRUE, ...){
  check_required_all()
  check_atomic_vec(vec, vec_arg, quickalert = FALSE)
  n_evaluation_frame <- raise_custom_frame(n_evaluation_frame, 1)
  internal_check_absence_vec(vec, values, vec_arg, raise, alert_message, header, n_evaluation_frame, quickalert, ...)
  invisible(NULL)
}