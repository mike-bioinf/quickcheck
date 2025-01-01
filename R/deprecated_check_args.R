#' Check the internal types of selected arguments of a calling function
#' @description
#' Checks if the "primitive" or "internal" types obtained by applying the typeof function on the specified arguments are
#' in concordance with the expected ones.
#' @param args character vector reporting the arguments of the outer function to check.
#' @param expected_types character vector with the expected types.
#' @param numeric_correspondence numeric vector that allows to recycle expected_types following
#'  its order and the numbers specified in this vector.
#' @param null Single logical, whether args can be NULL or not in addiction to the "normal" expected type (default FALSE).
#' @inheritParams check_columns_key
#' @return invisible NULL.
#' @export
check_args_primitive_types <- function(args, expected_types, numeric_correspondence = NULL, null = FALSE, alert_message = NULL, header = "default", n_evaluation_frame = 0, quickalert = TRUE, ...){
  check_required_all()
  if(!is.character(args)) cli::cli_abort(c("x" = "args must be a character vector."))
  if(!is.character(expected_types)) cli::cli_abort(c("x" = "expected_types must be a character vector."))

  if(!is.null(numeric_correspondence) && !is.numeric(numeric_correspondence)){
    cli::cli_abort(c("x" = "numeric_correspondence must be a numeric vector."))
  }

  if(!is.null(numeric_correspondence)){
    expected_types <- control_recycle(args, expected_types, numeric_correspondence)
  } else if(length(args) != length(expected_types)){
    cli::cli_abort(c("x" = "args and expected_types have {cli::col_red('different lengths')}."))
  }

  calling_list <- as.list(rlang::caller_env(n = 1))

  internal_check_presence_vec(
    vec = names(calling_list),
    values = args,
    header = "The following {cli::qty(length(missing_values))} argument{?s} {?is/are} {cli::col_red('not found')} in the caller function:",
    quickalert = FALSE
  )

  calling_args <- calling_list[args]
  type_args <- purrr::map_chr(calling_args, typeof)

  if(null){
    err_args <- args[type_args != expected_types & type_args != "NULL"]
  } else {
    err_args <- args[type_args != expected_types]
  }

  if(length(err_args) > 0){
    alert <- generate_message(alert_message, "{cli::col_magenta(err_args)}.")
    header <- generate_header(header, "The following {cli::qty(err_args)} argument{?s} {?is/are} of {cli::col_red('wrong type')}:")
    alert_generator("error", alert, n_evaluation_frame, quickalert, header = header, ...)
  }

  invisible(NULL)
}



#' Check the classes of selected arguments of a calling function
#' @inheritParams check_args_primitive_types
#' @param expected_classes character vector with the expected classes (one for argument).
#' @return invisible NULL.
#' @export
check_args_classes <- function(args, expected_classes, numeric_correspondence = NULL, null = FALSE, alert_message = NULL, header = "default", n_evaluation_frame = 0, quickalert = TRUE, ...){
  check_required_all()
  if(!is.character(args)) cli::cli_abort(c("x" = "args must be of character type"))
  if(!is.character(expected_classes)) cli::cli_abort(c("x" = "expected_classes must be a character vector."))

  if(!is.null(numeric_correspondence) && !is.numeric(numeric_correspondence)){
    cli::cli_abort(c("x" = "numeric_correspondence must be a numeric vector."))
  }

  if(!is.null(numeric_correspondence)){
    expected_classes <- control_recycle(args, expected_classes, numeric_correspondence)
  } else if(length(args) != length(expected_classes)){
    cli::cli_abort(c("x" = "args and expected_classes have different lengths."))
  }

  calling_env <- rlang::caller_env(n = 1)
  class_args <- list()

  for(arg in args){
    class_args[[arg]] <- class(calling_env[[arg]])
  }

  err_args <- c()

  for(i in seq_along(class_args)){
    if(null){
      if(!expected_classes[i] %in% class_args[[i]] && class_args[[i]] != "NULL"){
        err_args <- c(err_args, args[i])
      }
    } else {
      if(!expected_classes[i] %in% class_args[[i]]){
        err_args <- c(err_args, args[i])
      }
    }
  }

  if(length(err_args) > 0){
    alert <- generate_message(alert_message, "{cli::col_magenta(err_args)}.")
    header <- generate_header(header,"The following {cli::qty(err_args)} argument{?s} {?doesn't/don't} have the {cli::col_red('expected class')}:")
    alert_generator("error", alert, n_evaluation_frame, quickalert, header = header, ...)
  }

  invisible(NULL)
}



#' Check the numeric nature of selected arguments of a calling function
#' @description
#' Allow to easily check for numeric arguments without relying on classes or primitive types.
#' @inheritParams check_args_primitive_types
#' @return invisible NULL.
#' @export
check_numeric_args <- function(args, null = FALSE, alert_message = NULL, header = "default", n_evaluation_frame = 0, quickalert = TRUE, ...){
  rlang::check_required(args)
  if(!is.character(args)) cli::cli_abort(c("x" = "args must be of character type"))

  calling_env <- as.list(rlang::caller_env(n = 1))
  calling_args <- calling_env[args]

  if(null){
    err_args <- purrr::discard(calling_args, \(a) is.numeric(a) || is.null(a))
  } else {
    err_args <- purrr::discard(calling_args, \(a) is.numeric(a))
  }

  if(length(err_args) > 0){
    header <- generate_header(header, "The following {cli::qty(err_args)} argument{?s} {?is/are} {cli::col_red('not numeric')}:")
    alert <- generate_message(alert_message, "{cli::col_magenta(names(err_args))}.")
    alert_generator("error", alert, n_evaluation_frame, quickalert, header = header, ...)
  }

  invisible(NULL)
}



#' Check selected arguments of a calling function to be integer-like
#' @description
#' Allows to check for integer-like arguments according to R tolerance.
#' Integer like numbers are either integer typed value (e.g. 10L) or integer like double (e.g. 10.0).
#' @details
#' Double values with small imprecision like 10.00000000000001 are considered integer due to R's numerical tolerance.
#' Therefore this function is not optimal in such extreme cases.
#' @inheritParams check_args_primitive_types
#' @return invisible NULL.
#' @export
check_integerish_args <- function(args, null = FALSE, alert_message = NULL, header = "default", n_evaluation_frame = 0, quickalert = TRUE, ...){
  if(!is.character(args)) cli::cli_abort(c("x" = "args must be of character type"))
  calling_env <- as.list(rlang::caller_env(n = 1))
  calling_args <- calling_env[args]

  if(null){
    err_args <- purrr::discard(calling_args, \(a) rlang::is_integerish(a) || is.null(a))
  } else {
    err_args <- purrr::discard(calling_args, \(a) rlang::is_integerish(a))
  }

  if(length(err_args) > 0){
    header <- generate_header(header, "The following {cli::qty(err_args)} argument{?s} {?is/are} {cli::col_red('not integer-like')}:")
    alert <- generate_message(alert_message, "{cli::col_magenta(names(err_args))}.")
    alert_generator("error", alert, n_evaluation_frame, quickalert, header = header, ...)
  }

  invisible(NULL)
}



### Helpers of check_args for dots managing (NOT IMPLEMENTED) -------------------------------------------------------------------------------------------------------

#' Helper to convert the names of args in a vector of "dots" and "non_dots"
#' @inheritParams check_args
#' @return Returns the args vector.
adjust_args_names <- function(args){
  if(is.null(names(args))){
    names(args) <- rep("non_dots", length(args))
  } else {
    names(args)[is.na(names(args)) | names(args) != "dots"] <- "non_dots"
  }
  return(args)
}


#' Helper to add the missing dots marked argumeyns to the calling_args list with the "missing" value.
#' @param calling_args list of calling parameters-arguments couples.
#' @param dotted_args Character vector or dotted-marked parameters.
add_missing_dots <- function(calling_args, dotted_args){
  calling_params <- names(calling_args)
  for(n in dotted_args){
    if(!n %in% calling_params){
      calling_args[[n]] <- rlang::missing_arg()
    }
  }
  return(calling_args)
}
