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
          "{cond$message} {cond$body}"
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




#' Imposes an accumulation behavior of the checking function in a loop scenario, in which
#' the checking function can raise alerts, that are stored in a list and finally, at the end of
#' the loop, reported. Set to works exclusively with message type in order to distinguish
#' quickalert alert from normal alert.
#' @param expr check function call.
#' @param type type of the accumulated final alert if any.
#' @param header string to add as the head of the accumulated alert list.
#' @param n.evaluation_frame numeric, defines the number of calling frame to look up for the evaluation
#'  of the alert message in respect to where the function calling the alert is run. The default points
#'  to this function frame.
#' @return NULL
#' @export
impose_accumulation_behavior <- function(expr, type = "error", header = NULL, n.evaluation_frame = 2){
  accumulated_cond <- list()

  withCallingHandlers(
    expr = {expr},
    message = function(m){
      if(!"quickalert" %in% class(m)){
        cli::cli_abort(c("x" = "An {col_red('unexpected')} alert is been raised:", "{m$message} {m$body}"))
      } else {
        accumulated_cond <<- c(accumulated_cond, list(m$message))
        invokeRestart("muffleMessage")
      }
    },
    warning = function(w){
      cli::cli_abort(c("x" = "An {col_red('unexpected')} alert is been raised:", "{w$message} {w$body}"))
    },
    error = function(e){
      cli::cli_abort(c("x" = "An {col_red('unexpected')} alert is been raised:", "{e$message} {e$body}"))
    }
  )

  if(length(accumulated_cond) > 0){
    accumulated_cond <- add_header(header, accumulated_cond)
    alert_generator(type, accumulated_cond, n.evaluation_frame)
  }

  invisible(NULL)
}
