#' @name cli_features
#' @title Set of functions that manipulate cli functions and facilitate applying its syntax.


#' Allows to generate error, warning calls based on the specified type and using custom messages.
#' @param type string equal to one of: 'error', 'message' and 'warning'. Determines the type of alert.
#' @param alert_message character vector that follows the cli syntax and conventions.
alert_generator <- function(type, alert_message){
  alert_funcs <- generate_cli_alert_list()
  signs <- generate_cli_sign_list()
  selected_alert <- alert_funcs[[type]]
  names(alert_message)[1] <- signs[[type]]
  selected_alert(alert_message)
}



#' Generates a list of main cli alert functions with informative names.
generate_cli_alert_list <- function(){
  cli_main_list <- list(
    error = function(...) cli::cli_abort(message = ..., .envir = rlang::caller_env(n = 2)),
    warning = function(...) cli::cli_warn(message = ..., .envir = rlang::caller_env(n = 2)),
    message = function(...) cli::cli_inform(message = ..., .envir = rlang::caller_env(n = 2))
  )
}



# Generates a list of cli bullet signs with informative names.
generate_cli_sign_list <- function(){
  cli_sign_list <- list(
    error = "x",
    warning = "!",
    message = "i"
  )
}
