#' Function to signal errors, warnings and messages.
#' @param type
#'  String equal to one of: 'error', 'warning' or 'message'. Determines the type of alert.
#' @param alert_message
#'  Single character vector that follow the cli format. It possible to pass a list of strings to have automatically
#'  a numbered or nominated list display (otherwise for character vectors use list_format = TRUE).
#' @param n_evaluation_frame
#'  Integer indicating the stack frame in which the alert glue expressions are evaluated in respect to where is generated.
#'  So it indicates the number of frames to look down in the stack. By default is zero plus two.
#'  The addition is used in order to start counting from zero from the frame of the function that uses this function.
#'  Be aware that if the glue variables are not found in the pointed frame then the resolution falls under the
#'  lexical scoping rules starting from the pointed frame.
#' @param quickalert
#'  Logical, whether to generate an alert with class "quickalert" or not (default TRUE).
#'  The generation of "plain" alerts is useful when the checking functions are used inside other ones.
#'  This allow that only the main check raises quickalerts.
#' @param sign
#'  Logical, whether to add the sign to the first element of the alert (default TRUE).
#'  Note that in case of errors the "!" sign is used as default by cli. This sign is changed with "X" if sign is TRUE,
#'  but with sign = FALSE it cannot be suppressed.
#' @param header
#'  Character string to add at the beginning of the alert message (default NULL). 
#'  Can be also a character vector, in this case cosecutive elementes will be displayed on consecutive lines.
#' @param list_format
#'  Logical, whether to apply the list format style, which includes numbers or names
#'  of the alert message elements to be represented in violet in a bullet list.
#' @return Raise a condition.
alert_generator <- function(type, alert_message, n_evaluation_frame = 0, quickalert = TRUE, header = NULL, sign = TRUE, list_format = FALSE){
  rlang::arg_match(arg = type, values = c("error", "warning", "message"), multiple = F)
  rlang::check_required(type)
  rlang::check_required(alert_message)
  check_alert_generator_args(n_evaluation_frame, quickalert, header, sign, list_format)

  alert_funcs <- generate_alertfunc_list()
  my_alert_func <- alert_funcs[[type]]

  if(is.list(alert_message)){
    list_format <- TRUE
    alert_message <- purrr::map_chr(alert_message, \(v) paste(v, collapse = " "))
  }

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

  if(quickalert) alertclass <- "quickalert" else alertclass <- NULL
  my_alert_func(alert_message, n_evaluation_frame + 2, alertclass)
}



#' Checks alert generator arguments
#' @inheritParams alert_generator
check_alert_generator_args <- function(n_evaluation_frame, quickalert, header, sign, list_format){
  if(!rlang::is_integerish(n_evaluation_frame) || length(n_evaluation_frame) > 1 || n_evaluation_frame < 0){
    cli::cli_abort(c("x" = "n_evalution frame must be a {cli::col_red('single positive integerish')} value."))
  }

  if(!is.logical(quickalert) || length(quickalert) > 1){
    cli::cli_abort(c("x" = "quickalert muste be a {cli::col_red('single logical')} value."))
  }

  if(!is.null(header) && !is.character(header)){
    cli::cli_abort(c("x" = "header must be a {cli::col_red('string or character vector')}."))
  }

  if(!is.logical(sign) || length(sign) > 1){
    cli::cli_abort(c("x" = "sign must be a {cli::col_red('single logical')} value."))
  }

  if(!is.logical(list_format) || length(list_format) > 1){
    cli::cli_abort(c("x" = "list_format must be a {cli::col_red('single logical')} value."))
  }
}


#' Generates a list of aliases of main cli alert functions with intuitive names.
generate_alertfunc_list <- function(){
  list(
    error = function(alert, n_evaluation_frame, alertclass) cli::cli_abort(alert, .envir = rlang::caller_env(n = n_evaluation_frame), class = alertclass),
    warning = function(alert, n_evaluation_frame, alertclass) cli::cli_warn(alert, .envir = rlang::caller_env(n = n_evaluation_frame), class = alertclass),
    message = function(alert, n_evaluation_frame, alertclass) cli::cli_inform(alert, .envir = rlang::caller_env(n = n_evaluation_frame), class = alertclass)
  )
}


#' Generates a list of cli bullet signs with informative names.
generate_sign_list <- function(){
  list(error = "x", warning = "!", message = "i")
}


#' Construct a default alert message if not provided (if NULL).
#' @param alert_message takes in the alert_message argument of the checking function.
#' @param default_message character vector that is used as default message. Follow cli intax rules.
generate_message <- function(alert_message, default_message){
  if(is.null(alert_message)) alert_message <- default_message
  return(alert_message)
}


#' Constructs a default header message if equal to "default".
#' @param header takes in the header argument of the checking function.
#' @param default_header string that is used as default header.
generate_header <- function(header, default_header){
  if(!is.null(header) && header == "default") header <- default_header
  return(header)
}


#' Raises the evaluation frame number by x if not 0. 
#' It's purpose is to allow to skip stack frames in a invisibly manner for the user.
#' @inheritParams alert_generator
#' @param up The number added to the frame number.
raise_custom_frame <- function(n_evaluation_frame, up){
  if(n_evaluation_frame != 0) n_evaluation_frame <- n_evaluation_frame + up
  return(n_evaluation_frame)
}
