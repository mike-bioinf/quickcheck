#' @name check_args
#' @title function argument checks
#' @description
#' A set of functions useful to check the arguments provided in input by outer functions


#' Checks not provided arguments with no default value in the calling function.
#' @description
#' Checks the presence of all arguments that have no default values in the upper calling function
#' and stops if find some of them indicating their names. Ensemble version of rlang 'check_required'.
check_required_all <- function(){
  envlist <- as.list(rlang::caller_env(n = 1))
  envmiss <- purrr::keep(envlist, rlang::is_missing)

  if(length(envmiss) > 0){
    cli_abort(c("x" = "{col_red(names(envmiss))} {?is/are} absent but must be supplied."))
  }
  return(NULL)
}




#' Checks if the primitive types of the specified arguments are the ones expected.
#' @param args character vector reporting the arguments of the outer calling function to check.
#' @param arg_types character vector with the expected types for the specified elements.
#' @param numeric_corrispondence numeric vector that allows to recycle arg_types following
#'  the order of arg_types and the number specified by this vector.
check_primitive_types <- function(args, arg_types, numeric_corrispondence = NULL){
  if(!is.null(numeric_corrispondence)){
    arg_types <- control_recycle(args, arg_types, numeric_corrispondence)
  } else if(length(args) != length(args_types)){
    cli_abort(c("x" = "args and args_types have different lengths"))
  }

  type_outer <- c()
  for(a in args){
    type_outer <- c(type_outer, typeof(rlang::caller_env(n = 1)[[a]]))
  }

  err_args <- args[type_outer != arg_types]
  if(length(err_args) > 0){
    cli::cli_abort(c(
      "x" = "The following {qty(err_args)} argument{?s} {?is/are} of {col_red('wrong type')}: ",
      "{col_magenta(err_args)}"
    ))
  }
  return(NULL)
}





#' Helper of check_primitive_types to control the recycle of arg_types.
#' @param vector1 first vector.
#' @param vector2 second vector.
#' @param numeric_corrispondence number of times the elements of vector2 must be repeated in order.
control_recycle <- function(vector1, vector2, numeric_corrispondence){
  if(!is.numeric(numeric_corrispondence)){
    cli_abort(c("x" = "numeric_corrispondence is not a numeric vector"))
  }

  if(length(vector1) != sum(numeric_corrispondence)){
    cli_abort(c("x" = "The sum of numbers specified in numeric_correspondence is different from args length"))
  }

  vector2 <- rep(vector2, numeric_corrispondence)
}
