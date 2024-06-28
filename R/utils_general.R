#' Custom all to signal NULL values in input through a warning and to control
#' NA values in input and output.
#' @param ... passing arguments to all
#' @param na.rm logical indicating if the NA in input must be removed before evaluation
#' @param na_res logical indicating the result of all2 if all returns NA in output
all2 <- function(..., na.rm = FALSE, na_res = FALSE){
  dots_list <- list(...)
  null_list <- list()

  log_vec <- purrr::map_lgl(dots_list, is.null)

  if(any(log_vec)){
    warning("Some elements are NULL")
  }

  res <- all(..., na.rm = na.rm)

  if(is.na(res)){
    res <- na_res
  }

  return(res)
}




#' Checks the presence of null values in a vector in a broader sense.
#' @param vec vector to test.
#' @return A single boolean (FALSE even if one value is empty, TRUE otherwise).
#' @export
is_empty_vec <- function(vec){

  if("" %in% vec){
    return(TRUE)
  }

  if(is.null(vec)){
    return(TRUE)
  }

  if(length(vec) == 0){
    return(TRUE)
  }

  if(any(is.na(vec))){
    return(TRUE)
  }

  return(FALSE)
}




#' Flatten recursively a list
#' @description
#' The function allows to remove a desired number of layers or to flat out a list independently of its structure.
#' @param l list to flatten.
#' @param num number of layers to remove.
#' @param till_flat logical; flat out completely a list (default = TRUE).
#' @param nam_spec  If both inner and outer names are present, control how they are combined.
#'   It has to be a glue specification that uses variables inner and outer. Default = {outer}_{inner}.
#' @return Returns the flattened list.
rec_flatten_list <- function(l, num = NULL, till_flat = TRUE, nam_spec = "{outer}_{inner}"){

  if(!is.null(num) && till_flat){
    cli::cli_abort(c("x" = "Both num and till_flat are set: it's unclear how to operate."))
  }

  classes <- purrr::map_chr(l, function(x)(get_superclass(x)))

  if(is.numeric(num)){
    while(num > 0 && any(classes %in% "list")){
      l <- purrr::list_flatten(l, name_spec = nam_spec)
      classes <- purrr::map_chr(l, function(x)(get_superclass(x)))
      num <- num - 1
    }
  }

  if(till_flat){
    while(any(classes %in% "list")){
      l <- purrr::list_flatten(l, name_spec = nam_spec)
      classes <- purrr::map_chr(l, function(x)(get_superclass(x)))
    }
  }

  return(l)
}


#' Helper of rec_flatten_list
#' @param obj object passed in.
get_superclass <- function(obj){
  classes <- class(obj)
  superclass <- classes[length(classes)]
}

