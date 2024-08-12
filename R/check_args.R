#' @name check_args
#' @title function argument checks
#' @description A set of functions useful to check the arguments provided in input by outer functions


#' Checks for not provided arguments with no default value in the calling function.
#' @description
#' Checks the presence of all arguments that have no default values in the upper calling function
#' and stops if find some of them indicating their names. Ensemble version of rlang 'check_required'.
#' @export
check_required_all <- function(){
  envlist <- as.list(rlang::caller_env(n = 1))
  envmiss <- purrr::keep(envlist, rlang::is_missing)

  if(length(envmiss) > 0){
    cli::cli_abort(c("x" = "{col_red(names(envmiss))} {?is/are} absent but must be supplied."))
  }
  invisible(NULL)
}





#' Checks the primitive types of the specified arguments of a calling function.
#' @param args character vector reporting the arguments of the outer function to check.
#' @param expected_types character vector with the expected types.
#' @param numeric_correspondence numeric vector that allows to recycle expected_types following
#'  its order and the numbers specified in this vector.
#' @param null logical, whether args can be NULL or not in addiction to the "normal" expected type (default FALSE).
#' @param alert_message String reporting the alert message. Its formatted by cli_bullets function.
#' @inheritParams check_columns_presence
#' @return NULL
#' @export
check_args_primitive_types <- function(args, expected_types, numeric_correspondence = NULL, null = FALSE, alert_message = NULL, header = "default", n.evaluation_frame = 0, quickalert = TRUE, ...){
  if(!is.null(numeric_correspondence)){
    expected_types <- control_recycle(args, expected_types, numeric_correspondence)
  } else if(length(args) != length(expected_types)){
    cli::cli_abort(c("x" = "args and expected_types have {col_red('different lengths')}."))
  }

  calling_env <- rlang::caller_env(n = 1)
  type_args <- c()

  for(arg in args){
    type_args <- c(type_args, typeof(calling_env[[arg]]))
  }

  if(null){
    err_args <- args[type_args != expected_types & type_args != "NULL"]
  } else {
    err_args <- args[type_args != expected_types]
  }

  if(length(err_args) > 0){
    alert <- generate_message(alert_message, "{col_magenta(err_args)}.")
    header <- generate_header(header, "The following {qty(err_args)} argument{?s} {?is/are} of {col_red('wrong type')}:")
    alert_generator("error", alert, n.evaluation_frame, quickalert, header = header, ...)
  }

  invisible(NULL)
}




#' Checks the classes of selected arguments of a calling function.
#' @inheritParams check_args_primitive_types
#' @param expected_classes character vector with the expected classes (one for argument).
#' @return NULL
#' @export
check_args_classes <- function(args, expected_classes, numeric_correspondence = NULL, null = FALSE, alert_message = NULL, header = "default", n.evaluation_frame = 0, quickalert = TRUE, ...){
  if(!is.null(numeric_correspondence)){
    expected_classes <- control_recycle(args, expected_classes, numeric_correspondence)
  } else if(length(args) != length(expected_classes)){
    cli::cli_abort(c("x" = "args and expected_classes have different lengths."))
  }

  calling_env <- rlang::caller_env(n = 1)
  class_args <- list()

  for(a in args){
    class_args[[a]] <- class(calling_env[[a]])
  }

  err_args <- c()

  for(i in seq_along(class_args)){
    if(null){
      if(!expected_classes[i] %in% class_args[[i]] && expected_classes[i] != "NULL"){
        err_args <- c(err_args, args[i])
      }
    } else {
     if(!expected_classes[i] %in% class_args[[i]]){
       err_args <- c(err_args, args[i])
     }
    }
  }

  if(length(err_args) > 0){
    alert <- generate_message(alert_message, "{col_magenta(err_args)}.")
    header <- generate_header(header,"The following {qty(err_args)} argument{?s} {?doesn't/don't} have the {col_red('expected class')}:")
    alert_generator("error", alert, n.evaluation_frame, quickalert, header = header, ...)
  }

  invisible(NULL)
}




#' Checks the numeric nature of selected arguments of a calling function.
#' @description
#' Allow to easily check for numeric arguments since is not easy to do that relying on classes or primitive types.
#' @inheritParams check_args_primitive_types
check_numeric_args <- function(args, null = FALSE, alert_message = NULL, header = "default", n.evaluation_frame = 0, quickalert = TRUE, ...){
  calling_env <- rlang::caller_env(n = 1)
  calling_args <- calling_env[args]

  if(null){
    err_args <- purrr::discard(calling_args, \(a) !is.numeric(a) && !is.null(a))
  } else {
    err_args <- purrr::discard(calling_args, \(a) !is.numeric(a))
  }

  if(length(err_args) > 0){
    header <- generate_header(header,"The following {qty(err_args)} argument{?s} {?is/are} {col_red('not numeric')}:")
    alert <- generate_message(alert_message, "{col_magenta(err_args)}.")
    alert_generator("error", alert, n.evaluation_frame, quickalert, header = header, ...)
  }
  invisible(NULL)
}






### HELPERS ===================================================================================================================

#' Helper of check_primitive_types to control the recycle of expected types and classes.
#' Vector2 is recycled according to the numeric correspondence vector.
#' @param vector1 first vector.
#' @param vector2 second vector.
#' @param numeric_correspondence number of times the elements of vector2 must be repeated in order.
control_recycle <- function(vector1, vector2, numeric_correspondence){
  if(!is.numeric(numeric_correspondence)){
   cli::cli_abort(c("x" = "numeric_correspondence must be a numeric vector"))
  }

  if(length(vector1) != sum(numeric_correspondence)){
    cli::cli_abort(c("x" = "The sum of numbers specified in numeric_correspondence must be equal to args length"))
  }

  vector2 <- rep(vector2, numeric_correspondence)
}
