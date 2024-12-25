#' Generic quickcheck constructor.
#' @description
#' Allows to test generic hypothesis and raising conditions while having all the additional features of quickcheck.
#' The condition is raised if the supplied expression evaluates to TRUE.
#' @inheritParams check_columns_presence
#' @param expr Expression to test. Must return a single boolean value.
#' @param n_evaluation_frame Numeric, defines the number of stack frame to look down for the evaluation of the glue expressions of the alert message.
#'  The default value (0) points to this function frame.
#' @return Invisible NULL
#' @export
generic_check <- function(expr, raise = "error", alert_message = NULL, n_evaluation_frame = 0, quickalert = TRUE, ...){
  rlang::check_required(expr)
  expr_res <- expr

  if(!is.logical(expr_res)){
    cli::cli_abort(c("x" = "expr {cli::col_red('does not evaluate')} to a boolean value."))
  }

  if(length(expr_res) > 1){
    cli::cli_abort(c("x" = "expr must evaluates to a {cli::col_red('single')} boolean value."))
  }

  alert_message <- generate_message(alert_message, "expr is not TRUE.")

  if(expr_res){
    alert_generator(raise, alert_message, n_evaluation_frame, quickalert, ...)
  }

  invisible(NULL)
}


