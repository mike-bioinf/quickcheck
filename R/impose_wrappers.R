### Set of wrappers of check functions that alter or add features to their behavior.


# IMPORTANT NOTE:
# The tryCatch interrupts the execution of code, therefore if a 'quickalert' condition is
# hit before an other non 'quickalert' condition, this will not be seen by impose_logical_behavior.
# This means that alert_generator must be the last call in the check-functions.

#' Alter the behavior of checking conditions from raising alerts to return boolean values.
#' @description
#' This function works as a wrapper of the checking functions and impose them a 'logical' behavior.
#' This means that they will not raise alerts (unless forced) but instead they will return boolean values.
#' In greater detail if a 'quickalert' class condition is hit, then the function return TRUE otherwise FALSE.
#' @param expr "check_*" function call.
#' @param force_alert Boolean, whether to signal the quickalert once caught (default FALSE).
#' Useful for messages and warnings and in all situations in which you want to raise the condition in addition to obtain the boolean value.
#' @param strip_quickalert Boolean, whether to strip the re-signaled quickalert of the "quickalert class" (default FALSE).
#' Works only if force_alert is set to TRUE.
#' @return A single logical value.
#' @export
impose_logical_behavior <- function(expr, force_alert = FALSE, strip_quickalert = FALSE){
  tryCatch(
    {
      expr
      FALSE
    },
    quickalert = function(cnd){
      if(force_alert) {
        if(strip_quickalert) class(cnd) <- setdiff(class(cnd), "quickalert")
        rlang::cnd_signal(cnd)
      }
      TRUE
    }
  )
}




#' Accumulate and then raise non-error quickalerts
#' @description
#' Imposes an accumulation behavior in which quickalerts are stored in a list, which is used to raise a condition.
#' Works only with messages and warnings (not errors).
#' @param expr check function call. It's important that the checking function raises warnings or messages and not errors.
#' @param raise type of the accumulated final alert if any.
#' @param alert_message String or list reporting the alert message (by default the function build a list).
#' @param header string to add as the header of the accumulated alert list.
#' @param n_evaluation_frame Intergerish, defines the number of stack frame to look down for the evaluation
#'  of the glue expressions of the alert message. The default (0) points to this function frame.
#' @param quickalert logical, whether the raised alert is of class "quickalert" (default TRUE).
#' @param ... To pass additional parameters to alert_generator function.
#' @return invisible NULL
#' @export
impose_accumulation_behavior <- function(expr, raise = "error", alert_message = NULL, header = NULL, n_evaluation_frame = 0, quickalert = TRUE, ...){
  accumulated_cond <- list()

  withCallingHandlers(
    expr = {expr},
    quickalert = function(cnd){
      accumulated_cond <<- c(accumulated_cond, list(cnd$message))
      rlang::cnd_muffle(cnd)
    }
  )

  if(length(accumulated_cond) > 0){
    message <- generate_message(alert_message, accumulated_cond)
    alert_generator(raise, message, n_evaluation_frame, quickalert, header = header, ...)
  }

  invisible(NULL)
}




#' Perform the check on all the elements of a list generating a single summarized alert
#' @description
#' Iterates the checking function on all the elements of the list generating alerts
#' that pinpoint the list names/positions for which the checking function failed.
#' In greater detail, the names are used if not-empty otherwise the positions.
#' @inheritParams impose_accumulation_behavior
#' @param x List which elements will be checked by the check_func.
#' @param check_func Checking function that will be used on every x element (this will be passed as first argument).
#' @param check_arg_list List of arguments to pass to check_func.
#' Note that this list must not contain 'quickalert-oriented' arguments (raise, header, quickalert and sign),
#' neither the objects on which the checks are done, since these are automatically set.
#' @param element_nameroot String reporting the base name to use for the list elements with an empty name (default "element").
#' @return invisible NULL
#' @export
impose_loop_behavior <- function(x, check_func, check_arg_list = list(), element_nameroot = "element",
                                  raise = "error", alert_message = NULL, header = "default", n_evaluation_frame = 0, quickalert = TRUE, ...){
  check_required_all()
  check_args_classes(c("x", "check_arg_list", "check_func"), c("list", "function"), c(2, 1), quickalert = FALSE)
  additional_params <- names(check_arg_list)

  if(!is.null(additional_params)){
    check_empty_vec(additional_params, na = TRUE, empty_string = TRUE, quickalert = FALSE)
    internal_check_absence_vec(
      vec = additional_params,
      values = c("raise", "header", "quickalert", "sign"),
      header = "Incompatible parameters passed in check_arg_list:",
      quickalert = FALSE
    )
  }

  listnames <- names(x)
  failed_elements <- c()

  # x[[i]] passed as first element to match the first argument. Other package typical parameters are set.
  withCallingHandlers(
    {
      for(i in seq_along(x)){
        rlang::exec(.fn = check_func, x[[i]], raise = "message", header = NULL, quickalert = TRUE, sign = FALSE, !!!check_arg_list)
      }
    },
    condition = function(cnd){
      failed_element <- get_name(name = listnames[i], nameroot = element_nameroot, iteration = i)
      is_quickalert <- inherits(cnd, "quickalert")
      if(!is_quickalert && inherits(cnd, "error")){
        cli::cli_abort(c("x" = "An {cli::col_red('unexpected')} error is raised at element {.strong {failed_element}}:", "{rlang::cnd_message(cnd)}"))
      } else if(is_quickalert){
        failed_elements <<- c(failed_elements, failed_element)
        rlang::cnd_muffle(cnd)
      }
    }
  )

  if(length(failed_elements) > 0){
    header <- generate_header(header, "The checking procedure {cli::col_red('failed')} for the following elements:")
    alert_message <- generate_message(alert_message, "{cli::col_magenta(failed_elements)}.")
    alert_generator(raise, alert_message, n_evaluation_frame, quickalert, header = header, ...)
  }

  invisible(NULL)
}




#' Adds alert messages at the start or end of raised quickalert messages
#' @description
#' This wrapper works only with messages and warnings and not errors.
#' Note that this is not enforced by the function and therefore the responsability fall on the user.
#' @inheritParams impose_accumulation_behavior
#' @param message String to add to the alert message.
#' @param margin Numeric equal to 1 or 2, indicating where to add the additional message (start or end respectively).
#' @return invisible NULL
#' @export
impose_additional_alert <- function(expr, message, margin = 1, raise = "error", n_evaluation_frame = 0, quickalert = TRUE, ...){
  rlang::check_required(message)
  check_args_classes("message", "character", quickalert = FALSE)

  if(!margin %in% c(1, 2) || length(margin) > 1){
    cli::cli_abort(c("x" = "margin must be a single integerish equal to 1 or 2"))
  }

  # new_message is set in this way in order to avoid problems with the evaluation frame mechanisms
  new_message <- NULL

  withCallingHandlers(
    expr = {expr},
    quickalert = function(cnd){
      if(isTRUE(all.equal(margin, 1))){
        new_message <<- c(message, break_condition_message(cnd))
      } else {
        new_message <<- c(break_condition_message(cnd), message)
      }
      rlang::cnd_muffle(cnd)
    }
  )

  if(!is.null(new_message)){
    alert_generator(raise, new_message, n_evaluation_frame, quickalert, ...)
  }
  
  invisible(NULL)
}





### HELPERS -----------------------------------------------------------------------------------------------------------------------------------

#' Inspect and return the "classic" nature of conditions. Helper of impose_logical_behavior for re-signalling conditions.
#' Here classic means error, warning or message classes.
#' @param condition condition object.
detect_type_condition <- function(condition){
  if(inherits(condition, "error")){
    type_condition <- "error"
  } else if (inherits(condition, "warning")){
     type_condition <- "warning"
  } else if(inherits(condition, "message")){
    type_condition <- "message"
  }
  return(type_condition)
}



#' Retrieve the condition message and breaks it in a character vector based on newline character.
#' This is helpful since quickalerts are defined using only the message argument of the cli-signaling function.
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
  if(is_empty_vec(name)) name <- paste0(nameroot, iteration)
  return(name)
}
