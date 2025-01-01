#' Check that arguments are supplied
#' @description
#' Checks the presence of all arguments that have no default values in the upper calling function
#' and stops if find some of them, indicating their names. Ensemble version of rlang 'check_required' function.
#' @return invisible NULL.
#' @export
check_required_all <- function(){
  envlist <- as.list(rlang::caller_env(n = 1))
  envmiss <- purrr::keep(envlist, rlang::is_missing)
  if(length(envmiss) > 0){
    cli::cli_abort(c("x" = "{cli::col_red(names(envmiss))} {?is/are} absent but must be supplied."))
  }
  invisible(NULL)
}



#' Check incompatible arguments
#' @description
#' In rare occasion when one argument is set, others must remain NULL.
#' This function checks whether not compatible arguments are set in a caller function.
#' @inheritParams check_args_primitive_types
#' @return invisible NULL.
#' @export
check_args_incompatible <- function(args, alert_message = NULL, n_evaluation_frame = 0, quickalert = TRUE, ...){
  rlang::check_required(args)
  if(!is.character(args)) cli::cli_abort(c("x" = "args must be a character vector."))

  calling_list <- as.list(rlang::caller_env(1))

  internal_check_presence_vec(
    vec = names(calling_list),
    values = args,
    header = "The following {cli::qty(length(missing_values))} argument{?s} {?is/are} {cli::col_red('not found')} in the caller function:",
    quickalert = FALSE
  )

  calling_args <- calling_list[args]
  lgl_null_args <- purrr::map_lgl(calling_args, is.null)

  if(sum(!lgl_null_args) > 1){
    incompatible_args <- names(lgl_null_args[!lgl_null_args])
    alert_message <- generate_message(alert_message, "Incompatible arguments detected: {incompatible_args}")
    alert_generator("error", alert_message, n_evaluation_frame, quickalert, ...)
  }

  invisible(NULL)
}



#' Check caller function arguments
#' @description
#' Check basic properties of a calling function arguments. The function search the strings passed in args in the calling function enviroment.
#' Therefore one must use this function before modifying the caller function arguments.
#' Optimally one should use this function before setting any variable in the caller function.
#' It's not possible to check arguments passed via dots.
#' @param args
#' Character vector reporting the calling function arguments to check (parameter names).
#' @param expected_types
#' Character vector reporting the expected type/classes of args. Follow the order of args.
#' It's possible to report only one class for arg, in this case the expected type must be contained in the vector class of arg.
#' In addition is possible to use "integerish" to set an expectation of a integerish object (a defualt tolerance is used).
#' @param null
#' Logical vector of the same length of args or of length 1, indicating whether the correspondent args can be null.
#' If of length one the vector is recycled to args length.
#' @param flag
#' Logical vector of the same length of args or of length 1, indicating whether the correspondent args have to be of length 1.
#' If of length one the vector is recycled to args length
#' @param with
#' Character vector indicating which functions to use to get the actual types/class on which the expectation must be checked.
#' Must be of the same length of args or of length 1. If of length one the vector is recycled to args length.
#' Possible values are:
#' 'typeof': declare the use of the "typeof" function,
#' 'class': declare the use of the "class" function,
#' 'integerish': declare a check internally performed via class_integerish returning "integerish" for rlang-defined-integerish objects.
#' @param recycle_expected_types
#' Numeric vector that allows to recycle expected_types following its order and the numbers specified in this vector.
#' @inheritParams check_columns_key
#' @return invisible NULL.
#' @export
check_args <- function(args, expected_types, null = FALSE, flag = FALSE, with = "typeof", recycle_expected_types = NULL,
                        raise = "error", alert_message = NULL, header = "default", n_evaluation_frame = 0, quickalert = TRUE, ...){
  check_required_all()
  if(!is.character(args)) cli::cli_abort(c("x" = "args must be a character vector."))
  if(!is.character(expected_types)) cli::cli_abort(c("x" = "expected_types must be a character vector."))

  rlang::arg_match(with, c("typeof", "class", "check_integerish"), multiple = TRUE)

  if(!is.null(recycle_expected_types) && !is.numeric(recycle_expected_types)){
    cli::cli_abort(c("x" = "recycle_expected_types must be a numeric vector."))
  }

  if(!is.logical(null)) cli::cli_abort(c("x" = "null must be a logical vector."))
  if(!is.logical(flag)) cli::cli_abort(c("x" = "flag must be a logical vector."))

  len_args <- length(args)
  if(length(null) != 1L) internal_check_length_vec(null, exact_len = len_args, vec_arg = "null", quickalert = FALSE)
  if(length(flag) != 1L) internal_check_length_vec(flag, exact_len = len_args, vec_arg = "flag", quickalert = FALSE)

  if(!is.null(recycle_expected_types)){
    expected_types <- control_recycle(args, expected_types, recycle_expected_types)
  } else {
    internal_check_length_vecs(args, expected_types, alert_message = "args and expected_types have different lengths.", quickalert = FALSE)
  }

  calling_list <- as.list(rlang::caller_env(n = 1))

  internal_check_presence_vec(
    vec = names(calling_list),
    values = args,
    header = "The following {cli::qty(length(missing_values))} argument{?s} {?is/are} {cli::col_red('not found')} in the caller function:",
    quickalert = FALSE
  )

  calling_args <- calling_list[args]
  params <- names(calling_args)

  list_check <- list(calling_args, expected_types, null, flag, with, params)
  header <- generate_header(header, "The following expectations are not met:")

  impose_accumulation_behavior(
    expr = purrr::pmap(list_check, check_arg),
    raise = raise,
    alert_message = alert_message,
    header = header,
    quickalert = quickalert,
    n_evaluation_frame = n_evaluation_frame + 1
  )

  invisible(NULL)
}




### HELPERS -----------------------------------------------------------------------------------------------------------------------------------------

#' Helper to control the recycling of expected types/classes.
#' Vector2 is recycled according to numeric_correspondence vector.
#' @param vector1 first vector.
#' @param vector2 second vector.
#' @param numeric_correspondence number of times the elements of vector2 must be repeated in order.
control_recycle <- function(vector1, vector2, numeric_correspondence){
  if(length(vector1) != sum(numeric_correspondence)){
    cli::cli_abort(c("x" = "The sum of numbers specified in numeric_correspondence must be equal to args length"))
  }
  vector2 <- rep(vector2, numeric_correspondence)
  return(vector2)
}


#' Check function used to check the single arg.
#' @inheritParams check_args
#' @param arg Argument to check.
#' @param exp_type String reporting the type expectation on that arg.
#' @param param  Name of the parameter.
#' @inheritParams check_args
check_arg <- function(arg, exp_type, null, flag, with, param){
  if(null && is.null(arg)) {
    return(NULL)
  }
  param_bullet <- paste0(param, ")")

  if(flag && !is.null(arg)){
    internal_check_length_vec(
      vec = arg,
      exact_len = 1,
      raise = "message",
      alert_message = "{cli::col_cyan(param_bullet)} {param} is not a flag.",
      n_evaluation_frame = 1,
      sign = FALSE
    )
  }

  check_func <- select_check_func(with)
  actual_type <- check_func(arg)

  if(!exp_type %in% actual_type){
    cli::cli_inform(
      message = "{cli::col_cyan(param_bullet)} ACTUAL = {actual_type} | EXPECTED = {exp_type}",
      class = "quickalert"
    )
  }
}


#' Select the correct check func based on with value.
#' @inheritParams check_args
select_check_func <- function(with){
  switch(
    with,
    "typeof" = typeof,
    "class" = class,
    "check_integerish" = check_integerish
  )
}


#' Helper to convert the output of is_integerish to strings.
#' In detail if x is integerish, the function return "integerish", otherwise "non_integerish".
#' @param x object on wich is_integerish is called.
check_integerish <- function(x){
  if(rlang::is_integerish(x)) return("integerish") else return("non_integerish")
}
