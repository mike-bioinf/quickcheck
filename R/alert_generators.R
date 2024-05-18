### Set of functions that manipulate cli functions and facilitate applying its syntax.


#' Generic that allows to generate error, warning calls based on the specified type and using custom messages.
#' @param type string equal to one of: 'error', 'message' and 'warning'. Determines the type of alert.
#' @param alert_message list of character vector or a single character vector that follow the cli
#'  syntax. If list it is formatted following the scheme explained in format_cli_list.
#' @param eval_env numeric indicating the calling frame in which evaluates the alert in respect
#'  to which is called. So it indicates the number of frames to look up in the calling stack.
alert_generator <- function(type, alert_message, eval_env){
  UseMethod(generic = "alert_generator", object = alert_message)
}




#' S3 method of alert_generator for 'character' class
#' @inheritParams alert_generator
alert_generator.character <- function(type, alert_message, eval_env){
  rlang::arg_match(arg = type, values = c("error", "warning", "message"), multiple = F)
  alert_funcs <- generate_cli_alert_list()
  signs <- generate_cli_sign_list()
  selected_alert <- alert_funcs[[type]]
  names(alert_message)[1] <- signs[[type]]
  selected_alert(alert_message, eval_env)
}




#' S3 method of alert_generator for 'list' class
#' @inheritParams alert_generator
alert_generator.list <- function(type, alert_message, eval_env){
  rlang::arg_match(arg = type, values = c("error", "warning", "message"), multiple = F)
  alert_funcs <- generate_cli_alert_list()
  signs <- generate_cli_sign_list()
  selected_alert <- alert_funcs[[type]]
  header_sign <- signs[[type]]
  alert_message <- format_cli_list(alert_message, header_sign)
  selected_alert(alert_message, eval_env)
}





#' Helper of alert_generator.list. It formats the list message as header plus colored bullet list
#' with every name or position (in absence of the name) colored and a sequence of elements
#' (aka the collapsed character vector). The header is by default considered as the first element
#' in the list and it has to be a single string.
#' @param l list to format
#' @param header_sign sign associated with the header selected according to the raised alert type.
format_cli_list <- function(l, header_sign){
  list_wout_header <- l[2:length(l)]
  lnames <- names(list_wout_header)
  formatted_vector <- c()

  for(i in seq_along(list_wout_header)){
    if(is_empty_vec(lnames[i])){
      n <- i
    } else {
      n <- lnames[i]
    }
    formatted_vector <- c(
      formatted_vector,
      glue::glue("{cli::col_magenta(n)}: ", paste(list_wout_header[[i]], collapse = ", "), "\n")
    )
  }

  formatted_vector <- c(l[[1]], formatted_vector)
  names(formatted_vector)[1] <- header_sign
  return(formatted_vector)
}





#' Generates a list of main cli alert functions with informative names.
generate_cli_alert_list <- function(){
  cli_main_list <- list(
    error = function(alert, eval_env) cli::cli_abort(alert, .envir = rlang::caller_env(n = eval_env), class = "quickalert"),
    warning = function(alert, eval_env) cli::cli_warn(alert, .envir = rlang::caller_env(n = eval_env), class = "quickalert"),
    message = function(alert, eval_env) cli::cli_inform(alert, .envir = rlang::caller_env(n = eval_env), class = "quickalert")
  )
}




#' Generates a list of cli bullet signs with informative names.
generate_cli_sign_list <- function(){
  cli_sign_list <- list(
    error = "x",
    warning = "!",
    message = "i"
  )
}

