### Impose Boolean returns for check functions
# The tryCatch interrupts the execution of code, therefore if a 'quickalert' condition is
# hit before an other non 'quickalert' condition, this will not be seen by impose_logical_behavior.
# This means that alert_generator must be the last call in the check-functions.


#' Alter the behavior of checking conditions from raising alerts to return boolean values.
#' @description
#' This function works as a wrapper of the checking functions and impose them a
#' 'logical' behavior. This means that they will not raise anymore alerts but instead
#' they will return boolean values. In greater detail if a 'quickalert' class
#' condition is hit, then the function return TRUE otherwise FALSE.
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
          "{cond$message} {cond$body}"
        ))
      }
      alert <<- TRUE
    }
  )

  if(alert) {logical_return <- TRUE}
  return(logical_return)
}





#' Allow to accumulate in list non-error alert of checking functions
#' @description
#' Imposes an accumulation behavior for a checking function in a loop scenario, in which
#' the alert raised by the checking function are stored in a list and then displayed.
#' Works preferably with checking functions that raise accumulated message type for
#' a better final alert format. It does not work with error since they stop the loop execution.
#' @param expr check function call.
#' @param type type of the accumulated final alert if any.
#' @param header string to add as the header of the accumulated alert list.
#' @param n.evaluation_frame numeric, defines the number of calling frame to look up for the evaluation
#'  of the alert message in respect to where the function calling the alert is run. The default points
#'  to this function frame.
#' @param quickalert logical, whether the raised alert is of class "quickalert".
#' @return NULL
#' @export
impose_accumulation_behavior <- function(expr, type = "error", header = NULL, n.evaluation_frame = 2, quickalert = TRUE){
  accumulated_cond <- list()

  withCallingHandlers(
    expr = {expr},
    condition = function(cond){
      if(!"quickalert" %in% class(cond)){
        cli::cli_abort(c("x" = "An {col_red('unexpected')} alert is been raised:", "{cond$message} {cond$body}"))
      } else {
        accumulated_cond <<- c(accumulated_cond, list(cond$message))
        rlang::cnd_muffle(cond)
      }
    }
  )

  if(length(accumulated_cond) > 0){
    accumulated_cond <- add_header(header, accumulated_cond)
    alert_generator(type, accumulated_cond, n.evaluation_frame, quickalert)
  }

  invisible(NULL)
}
