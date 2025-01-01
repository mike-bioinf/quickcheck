#' Perform various checks on object names
#' @param x Object with one of the attributes accepted by the param "what".
#' @param xarg String reporting how to adress the object in the alerts (default "x").
#' @param what Type of name attribute to check, one of "names", "colnames" or "rownames" (default "names").
#' @inheritParams check_vector
#' @inherit check_atomic_vec return
#' @export
check_names <- function(x, xarg = "x", null_check = FALSE, zero_len_check = FALSE, na_check = FALSE, empty_string_check = FALSE,
                        unique = FALSE, na_rm_unique = FALSE, include = NULL, exclude = NULL, what = "names"){
  rlang::check_required(x)

  check_args_classes(
    args = c("xarg", "null_check", "zero_len_check", "na_check", "empty_string_check", "unique", "na_rm_unique"),
    expected_classes = c("character", "logical"),
    numeric_correspondence = c(1, 6),
    quickalert = FALSE
  )

  check_atomic_vec(include, vec_arg = "include")
  check_atomic_vec(exclude, vec_arg = "exclude")
  rlang::arg_match(what, c("names", "colnames", "rownames"), multiple = FALSE)

  if(!what %in% names(attributes(x))){
    cli::cli_abort(c("x" = "{what} is not found as attribute of {xarg}."))
  }

  if(what %in% c("names", "colnames")) get_names <- names else get_names <- rownames
  vec_name <- get_names(x)

  internal_check_vector(
    vec = vec_name,
    vec_arg = xarg,
    null_check = null_check,
    zero_len_check = zero_len_check,
    na_check = na_check,
    empty_string_check = empty_string_check,
    unique = unique,
    na_rm_unique = na_rm_unique,
    include = include,
    exclude = exclude
  )

  invisible(NULL)
}
