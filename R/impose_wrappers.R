### Set of wrappers of check functions that alter or add features to their behavior.
# IMPORTANT NOTE:
# The tryCatch interrupts the execution of code, therefore if a 'quickalert' condition is
# hit before an other non 'quickalert' condition, this will not be seen by impose_logical_behavior.
# This means that alert_generator must be the last call in the check-functions.



#' Alter the behavior of checking conditions from raising alerts to return boolean values.
#' @description
#' This function works as a wrapper of the checking functions and impose them a
#' 'logical' behavior. This means that they will not raise alerts (unless forced)
#' but instead they will return boolean values. In greater detail if a 'quickalert'
#' class condition is hit, then the function return TRUE otherwise FALSE.
#' @param expr check function call.
#' @param force_alert logical, whether to signal the quickalert once caught (default FALSE).
#'  Useful for message and warnings and in situation in which you want to raise the condition
#'  in addition to obtain the boolean value.
#' @return A single logical value.
#' @export
impose_logical_behavior <- function(expr, force_alert = FALSE){
  logical_return <- FALSE

  tryCatch(
    expr = {expr},
    condition = function(cond){
      if(!"quickalert" %in% class(cond)){
        cli::cli_abort(c("x" = "An {col_red('unexpected')} alert is been raised:", "{cond$message} {cond$body}"))
      }
      logical_return <<- TRUE
      if(force_alert) {
        type_cond <- detect_type_condition(cond)
        message <- break_condition_message(cond)
        alert_generator(type = type_cond, alert_message = message, sign = FALSE)
      }
    }
  )

  return(logical_return)
}






#' Allow to accumulate in list non-error alert of checking functions
#' @description
#' Imposes an accumulation behavior for a checking function in a loop scenario, in which
#' the alert raised by the checking function are stored in a list and then displayed.
#' Works with message or warning conditions. It does not work with error since they stop the loop execution.
#' @param expr check function call.
#' @param raise type of the accumulated final alert if any.
#' @param alert_message String or list reporting the alert message (by default the function build a list).
#' @param header string to add as the header of the accumulated alert list.
#' @param n.evaluation_frame numeric, defines the number of calling frame to look up for the evaluation
#'  of the alert message in respect to where the function calling the alert is run. The default points
#'  to this function frame.
#' @param quickalert logical, whether the raised alert is of class "quickalert".
#' @param ... To pass additional argument to alert_generator function.
#' @return invisible NULL
#' @export
impose_accumulation_behavior <- function(expr, raise = "error", alert_message = NULL, header = NULL, n.evaluation_frame = 0, quickalert = TRUE, ...){
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
    message <- generate_message(alert_message, accumulated_cond)
    alert_generator(raise, message, n.evaluation_frame, quickalert, header = header, ...)
  }

  invisible(NULL)
}






#' Adds a message at the start/end of raised quickalert messages.
#' @inheritParams impose_accumulation_behavior
#' @param message additional message to be added to the alert.
#' @param margin numeric equal to 1 or 2, indicating where to add the additional message (start or end respectively).
#' @return invisible NULL
#' @export
impose_additional_alert <- function(expr, message, margin = 1, raise = "error", n.evaluation_frame = 0, quickalert = TRUE){
  if(!margin %in% c(1, 2)) cli::cli_abort(c("x" = "margin must be equal to 1 or 2."))

  withCallingHandlers(
    expr = expr,
    condition = function(cond){
      if(!"quickalert" %in% class(cond)){
        cli::cli_abort(c("x" = "An {col_red('unexpected')} alert is been raised:", "{cond$message} {cond$body}"))
      } else {
        if(margin == 1){
          complete_alert <- c(message, break_condition_message(cond))
        } else {
          complete_alert <- c(break_condition_message(cond), message)
        }
        alert_generator(raise, complete_alert, n.evaluation_frame, quickalert, sign = FALSE)
        rlang::cnd_muffle(cond)
      }
    }
  )
}







### HELPERS ============================================================================================================================================


#' Inspect and return the "classic" nature of conditions. Helper of impose_logical_behavior for re signalling conditions.
#' Here classic means error, warning or message.
#' @param condition condition object.
detect_type_condition <- function(condition){
  if("error" %in% class(condition)){
    type_condition <- "error"
  } else if ("warning" %in% class(condition)){
     type_condition <- "warning"
  } else if("message" %in% class(condition)){
    type_condition <- "message"
  }
  return(type_condition)
}



#' Retrieve the condition message and breaks it in a character vector based on newline character.
#' This is helpful since quickalerts have been defined using only the message argument.
#' Therefore the body of the message is condensed with the message (we work with one single message).
#' @param condition condition object.
break_condition_message <- function(condition){
  full_message <- conditionMessage(condition)
  splitted_message <- strsplit(full_message, "\n")[[1]]
  return(splitted_message)
}

