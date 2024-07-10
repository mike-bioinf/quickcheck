#' Function that allows to signal errors, warnings and messages.
#' @param type
#'  String equal to one of: 'error', 'warning', 'message', 'accumulate_message'. Determines the type of alert.
#' @param alert_message
#'  Single character vector that follow the cli format.
#' @param eval_env
#'  Integer indicating the calling frame in which evaluates the alert in respect to where the alert is generated.
#'  So it indicates the number of frames to look up in the calling stack.
#' @param quickalert
#'  Logical, whether to generate an alert with class "quickalert" or not (default TRUE).
#'  The generation of "plain" alerts is useful when the checking functions are used inside other ones.
#'  This allow the fact that only the main check raises quickalerts.
#' @param sign
#'  Logical, whether to add the sign to the first element of the alert (default TRUE).
#' @param header
#'  Character string to add at the beginning of the alert message (default NULL).
#' @param list_format
#'  Logical, whether to apply the list format style, which includes numbers of names
#'  of the alert message elements to be represented in violet in a bullet list.
alert_generator <- function(type, alert_message, eval_env, quickalert = TRUE, sign = TRUE, header = NULL, list_format = FALSE){
  rlang::arg_match(arg = type, values = c("error", "warning", "message", "accumulate_message"), multiple = F)
  alert_funcs <- generate_alertfunc_list()
  my_alert_func <- alert_funcs[[type]]

  if(list_format){
    if(is_empty_vec(names(alert_message))){
      bullet_names <- cli::col_cyan(paste0(1:length(alert_message), "."))
    } else {
      bullet_names <- cli::col_cyan(paste0(names(alert_message)), ".")
    }
    alert_message <- paste(bullet_names, alert_message, sep = " ")
  }

  alert_message <- c(header, alert_message)

  if(sign){
    signs <- generate_sign_list()
    names(alert_message)[1] <- signs[[type]]
  }

  if(quickalert){
    alertclass <- "quickalert"
  } else {
    alertclass <- NULL
  }

  my_alert_func(alert_message, eval_env, alertclass)
}





### HELPERS ===================================================================================================================================

#' Generates a list of aliases of main cli alert functions with intuitive names.
generate_alertfunc_list <- function(){
  cli_main_list <- list(
    error = function(alert, eval_env, alertclass) cli::cli_abort(alert, .envir = rlang::caller_env(n = eval_env), class = alertclass),
    warning = function(alert, eval_env, alertclass) cli::cli_warn(alert, .envir = rlang::caller_env(n = eval_env), class = alertclass),
    message = function(alert, eval_env, alertclass) cli::cli_inform(alert, .envir = rlang::caller_env(n = eval_env), class = alertclass),
    accumulate_message = function(alert, eval_env, alertclass) cli::cli_inform(alert, .envir = rlang::caller_env(n = eval_env), class = alertclass)
  )
}


#' Generates a list of cli bullet signs with informative names.
generate_sign_list <- function(){
  cli_sign_list <- list(
    error = "x",
    warning = "!",
    message = "i",
    accumulate_message = ""
  )
}


#' Helper that is used to construct a default alert message if not provided (if NULL).
#' @param alert_message takes in the alert_message argument of the checking function.
#' @param default_message character vector that is used as default message. Follow cli intax rules.
generate_message <- function(alert_message, default_message){
  if(is.null(alert_message)) {alert_message <- default_message}
  return(alert_message)
}
