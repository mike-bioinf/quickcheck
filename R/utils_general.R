#' Flatten recursively a list
#' @description
#' The function allows to remove a desired number of layers or to flat out a list independently of its structure.
#' @param l list to flatten.
#' @param num number of layers to remove.
#' @param till_flat logical; flat out completely a list (default = TRUE).
#' @param nam_spec  If both inner and outer names are present, control how they are combined.
#'   It has to be a glue specification that uses variables inner and outer. Default = {outer}_{inner}.
#' @return Returns the flattened list.
rec_flatten_list <- function(l, num = NULL, till_flat = TRUE, nam_spec = "{outer}${inner}"){

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

