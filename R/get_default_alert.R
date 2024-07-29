#' Get the default alert raised by a check function.
#' @description The alert compromises header + alert message when a defualt header is present.
#' @param check_func String reporting the name of the checking function.
#' @return Usually a character vector or rarely a list (in case of multiple defaults) of defaults alerts.
#' @export
get_default_alert <- function(check_func){
  rlang::check_required(check_func)
  check_args_primitive_types("check_func", "character")

  if(!check_func %in% names(quickcheck::default_alert_messages)) {
    cli::cli_abort("The {.val {check_func}} function does not exist or belong to quickcheck package.")
  }

  default_message <- quickcheck::default_alert_messages[[check_func]]
  return(default_message)
}


