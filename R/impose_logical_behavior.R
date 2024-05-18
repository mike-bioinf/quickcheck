### Impose Boolean returns for check functions


#' Alter the behavior of checking conditions to raising alerts to return boolean values.
#' @description
#' This function works as a wrapper of the checking conditions and allows to impose
#' to them a 'logical' behavior, in the sense they don't raise anymore the alerts but instead
#' boolean values. If the alert would be raised they will return TRUE otherwise FALSE.
#'
#' @param expr check function call.
#' @return Boolean value
#' @export
impose_logical_behaviour <- function(expr){
  alert <- FALSE
  logical_return <- FALSE

  tryCatch(
    expr = {expr},
    condition = function(cond){
      alert <<- TRUE
    }
  )

  if(alert){
    logical_return <- TRUE
  }

  return(logical_return)
}
