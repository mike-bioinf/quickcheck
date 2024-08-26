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
#' @param expr check function call. It's important that the checking function raises warnings or messages
#'  since errors cannot be muffled (they will cause unexpected behavior).
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





#' Perform the check on all the elements of a list generating a single summarized alert
#' @description
#' Iterates the checking function on all the elements of the list generating informative alert messages,
#' that pinpoint the element names/positions for which the checking function failed.
#' @inheritParams check_columns_presence
#' @param x List which elements will be checked by the check_func.
#' @param check_func Checking function that will be used on every x element (this will be passed as first argument).
#' @param check_arg_list List of arguments to pass to check_func. Importantly the first checking function argument
#'  as well as 'alert-oriented' arguments (raise, header, quickalert and sign) are already set and therefore must NOT be specified.
#' @param element_nameroot String reporting the base name to use for the list elements without a name in case
#'  they must be pointed in the alert message (default "element").
#' @param n.evaluation_frame numeric, defines the number of calling frame to look up for the evaluation
#'  of the alert message in respect to where the function calling the alert is run. The default points
#'  to this function frame.
#' @return invisible NULL
#' @export
impose_loop_behavior <- function(x, check_func, check_arg_list = list(), element_nameroot = "element", raise = "error", alert_message = NULL, header = "default", n.evaluation_frame = 0, quickalert = TRUE, ...){
  check_args_primitive_types(c("x", "check_arg_list"), "list", 2, quickalert = FALSE)
  check_args_classes("check_func", "function", quickalert = FALSE)
  listnames <- names(x)
  failed_elements <- c()

  # x[[i]] passed as first element to match the first argument. Other alert oriented parameters are set.
  withCallingHandlers(
    {
      for(i in seq_along(x)){
        rlang::exec(.fn = check_func, x[[i]], raise = "message", header = NULL, quickalert = TRUE, sign = FALSE, !!!check_arg_list)
      }
    },
    condition = function(cond){
      failed_element <- get_name(name = listnames[[i]], nameroot = element_nameroot, iteration = i)
      if(!"quickalert" %in% class(cond)){
        cli::cli_abort(c("x" = "An {col_red('unexpected')} alert is been raised at element {failed_element}:", "{cond$message} {cond$body}"))
      } else {
        failed_elements <<- c(failed_elements, failed_element)
        rlang::cnd_muffle(cond)
      }
    }
  )

  if(length(failed_elements) > 0){
    header <- generate_header(header, "The checking action {cli::col_red('failed')} for the following elements:")
    alert_message <- generate_message(alert_message, "{cli::col_magenta(failed_elements)}.")
    alert_generator(raise, alert_message, n.evaluation_frame, quickalert, header = header, ...)
  }

  invisible(NULL)
}





#' Adds a message at the start/end of raised quickalert messages.
#' @inheritParams impose_accumulation_behavior
#' @param message additional message to be added to the alert.
#' @param margin numeric equal to 1 or 2, indicating where to add the additional message (start or end respectively).
#' @return invisible NULL
#' @details The alert sign of the resulting message (if any) is decided by the type of alert raised by the checking function in expr.
#'  This has been chosen to prevent that one must set sign argument to FALSE for the checking function all times.
#' @export
impose_additional_alert <- function(expr, message, margin = 1, raise = "error", n.evaluation_frame = 0, quickalert = TRUE){

  if(!margin %in% c(1, 2)){
    cli::cli_abort(c("x" = "margin must be equal to 1 or 2."))
  }

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

  invisible(NULL)
}








### HELPERS ============================================================================================================================================


#' Inspect and return the "classic" nature of conditions. Helper of impose_logical_behavior for re-signalling conditions.
#' Here classic means error, warning or message classes.
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
#' This is helpful since quickalerts are defined using only the message argument of the cli-signaling functions.
#' @param condition condition object.
break_condition_message <- function(condition){
  full_message <- conditionMessage(condition)
  splitted_message <- strsplit(full_message, "\n")[[1]]
  return(splitted_message)
}



#' Helper of impose_loop_behavior to check and get the name of the element of a list.
#' If not set in broad sense it get converted to "nameroot+iteration".
#' @param name name to check.
#' @param nameroot basename of the unnamed element.
#' @param iteration number added to the root to constitute the final name.
get_name <- function(name, nameroot, iteration){
  if(is_empty_vec(name)){
    name <- paste0(nameroot, iteration)
  }
  return(name)
}
