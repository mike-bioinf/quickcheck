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
  return(NULL)
}




#' Checks the primitive types of the specified arguments of a calling function.
#' @param args character vector reporting the arguments of the outer function to check.
#' @param expected_types character vector with the expected types.
#' @param numeric_correspondence numeric vector that allows to recycle expected_types following
#'  its order and the numbers specified in this vector.
#' @param null logical, whether args can be NULL or not in addiction to the "normal" expected type (default FALSE).
#' @param alert_message String reporting the alert message. Its formatted by cli_bullets function.
#' @return NULL
#' @export
check_args_primitive_types <- function(args, expected_types, numeric_correspondence = NULL, null = FALSE, alert_message = NULL){
  if(!is.null(numeric_correspondence)){
    expected_types <- control_recycle(args, expected_types, numeric_correspondence)
  } else if(length(args) != length(expected_types)){
    cli::cli_abort(c("x" = "args and expected_types have {col_red('different lengths')}"))
  }

  type_args <- c()
  for(arg in args){
    type_args <- c(type_args, typeof(rlang::caller_env(n = 1)[[arg]]))
  }

  if(null){
    err_args <- args[type_args != expected_types & type_args != "NULL"]
  } else {
    err_args <- args[type_args != expected_types]
  }

  if(length(err_args) > 0){
    alert <- generate_message(
      alert_message = alert_message,
      default_message = c(
        "x" = "The following {qty(err_args)} argument{?s} {?is/are} of {col_red('wrong type')}: ",
        "{col_magenta(err_args)}"
      )
    )
    alert_generator("error", alert, 2)
  }

  invisible(NULL)
}





#' Checks the classes of selected arguments of a calling function.
#' @inheritParams check_args_primitive_types
#' @param expected_classes character vector with the expected classes (one for argument).
#' @return NULL
#' @export
check_args_classes <- function(args, expected_classes, numeric_correspondence = NULL, null = FALSE, alert_message = NULL){
  if(!is.null(numeric_correspondence)){
    expected_classes <- control_recycle(args, expected_classes, numeric_correspondence)
  } else if(length(args) != length(expected_classes)){
    cli::cli_abort(c("x" = "args and expected_classes have different lengths"))
  }

  class_args <- list()

  for(a in args){
    class_args[[a]] <- class(rlang::caller_env(n = 1)[[a]])
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
    alert <- generate_message(
      alert_message = alert_message,
      default_message = c(
        "x" = "The following {qty(err_args)} argument{?s} {?doesn't/don't} have the {col_red('expected class')}:",
        "{col_magenta(err_args)}"
      )
    )
    alert_generator("error", alert, 2)
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
