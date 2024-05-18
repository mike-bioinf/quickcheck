### Impose Boolean returns for check functions
# The tryCatch interrupts the execution of code, therefore if a 'quickalert' condition is
# hit before an other non 'quickalert' condition, this will not be seen by impose_logical_behavior.
# This means that alert_generator must be the last call in the check-functions.


#' Alter the behavior of checking conditions from raising alerts to return boolean values.
#' @description
#' This function works as a wrapper of the checking functions and impose them a
#' 'logical' behavior. This means that they will raise anymore the alerts condition
#' but instead they will return boolean values. In greater detail if a 'quickalert' class
#' condition is hit, then the function return TRUE otherwise FALSE.
#'
#' @param expr check function call.
#' @return A single logical value.
#' @export
impose_logical_behavior <- function(expr){
  alert <- FALSE
  logical_return <- FALSE

  tryCatch(
    expr = {expr},
    condition = function(cond){
      if(!"quickalert" %in% class(cond)){
        cli::cli_abort(c(
          "x" = "The supplied expression have raised an {col_red('unexpected')} alert:",
          "{cond$message}"
        ))
      }
      alert <<- TRUE
    }
  )

  if(alert){
    logical_return <- TRUE
  }

  return(logical_return)
}
