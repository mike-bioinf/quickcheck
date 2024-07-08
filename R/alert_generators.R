### Set of functions that manipulate cli alert functions.


#' Generic that allows to generate error, warning calls based on the specified type and using custom messages.
#' @param type string equal to one of: 'error', 'message' and 'warning'. Determines the type of alert.
#' @param alert_message list of character vector or a single character vector that follow the cli
#'  syntax. If list it is formatted following the scheme explained in format_cli_list.
#' @param eval_env numeric indicating the calling frame in which evaluates the alert in respect
#'  to which is called. So it indicates the number of frames to look up in the calling stack.
#' @param quickalert logical, whether to generate an alert with class "quickalert" or not (default TRUE).
#'  The generation of plain alerts is useful when using checking functions inside other ones,
#'  in order to have exclusively the main check to raise quickalert.
#' @param sign logical, whether to add the sign to the first element of the alert (default TRUE).
alert_generator <- function(type, alert_message, eval_env, quickalert = TRUE, sign = TRUE){
  UseMethod(generic = "alert_generator", object = alert_message)
}




#' S3 method of alert_generator for 'character' class
#' @inheritParams alert_generator
alert_generator.character <- function(type, alert_message, eval_env, quickalert = TRUE, sign = TRUE){
  rlang::arg_match(arg = type, values = c("error", "warning", "message", "accumulate_message"), multiple = F)
  alert_funcs <- generate_cli_alert_list(quickalert)
  selected_alert <- alert_funcs[[type]]

  if(sign){
    signs <- generate_cli_sign_list()
    names(alert_message)[1] <- signs[[type]]
  }

  selected_alert(alert_message, eval_env)
}




#' S3 method of alert_generator for 'list' class
#' @inheritParams alert_generator
alert_generator.list <- function(type, alert_message, eval_env, quickalert = TRUE, sign = TRUE){
  rlang::arg_match(arg = type, values = c("error", "warning", "message", "accumulate_message"), multiple = F)
  alert_funcs <- generate_cli_alert_list(quickalert)
  selected_alert <- alert_funcs[[type]]

  if(sign){
    signs <- generate_cli_sign_list()
    header_sign <- signs[[type]]
  } else {
    header_sign <- NULL
  }

  alert_message <- format_cli_list(alert_message, header_sign)
  selected_alert(alert_message, eval_env)
}






#' Helper of alert_generator.list. It formats the list message as header plus colored bullet list
#' with every name or position (in absence of the name) colored and a sequence of elements
#' (aka the collapsed character vector). The header is by default considered as the first element
#' in the list and it has to be a single string.
#' @param l list to format
#' @param header_sign sign associated with the header selected according to the raised alert type or NULL.
format_cli_list <- function(l, header_sign = NULL){
  list_wout_header <- l[2:length(l)]
  lnames <- names(list_wout_header)
  formatted_vector <- c()

  for(i in seq_along(list_wout_header)){
    if(is_empty_vec(lnames[i])){
      n <- i
    } else {
      n <- lnames[i]
    }
    formatted_vector <- c(formatted_vector, glue::glue("{cli::col_magenta(n)}: ", paste(list_wout_header[[i]], collapse = ", "), "\n"))
  }

  formatted_vector <- c(l[[1]], formatted_vector)

  if(!is.null(header_sign)){
    names(formatted_vector)[1] <- header_sign
  }

  return(formatted_vector)
}





#' Generates a list of aliases of main cli alert functions with intuitive names.
#' @inheritParams alert_generator
generate_cli_alert_list <- function(quickalert){
  if(quickalert){
    alertclass <- "quickalert"
  } else {
    alertclass <- NULL
  }

  cli_main_list <- list(
    error = function(alert, eval_env) cli::cli_abort(alert, .envir = rlang::caller_env(n = eval_env), class = alertclass),
    warning = function(alert, eval_env) cli::cli_warn(alert, .envir = rlang::caller_env(n = eval_env), class = alertclass),
    message = function(alert, eval_env) cli::cli_inform(alert, .envir = rlang::caller_env(n = eval_env), class = alertclass),
    accumulate_message = function(alert, eval_env) cli::cli_inform(alert, .envir = rlang::caller_env(n = eval_env), class = alertclass)
  )
}




#' Generates a list of cli bullet signs with informative names.
generate_cli_sign_list <- function(){
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
