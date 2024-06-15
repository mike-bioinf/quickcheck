#' Generic quickcheck constructor.
#' @description
#' Allows to test generic hypothesis while having all the additional features of quickcheck.
#' The alert is raise if the provided expression evaluates to FALSE.
#' @inheritParams check_columns_presence
#' @param expr expression to test. Must return a single boolean value.
#' @return Invisible NULL
#' @export
generic_check <- function(expr, raise = "error", alert_message = NULL, n.evaluation_frame = 2, quickalert = TRUE){
  expr_res <- expr

  if(!is.logical(expr_res)){
    cli::cli_abort(c("x" = "expr {col_red('does not evaluate')} to a boolean value."))
  }

  if(length(expr_res) > 1){
    cli::cli_abort(c("x" = "expr must evaluates to a {col_red('single')} boolean value."))
  }

  alert_message <- generate_message(alert_message, "expr is not TRUE")
  if(!expr_res) {alert_generator(raise, alert_message, n.evaluation_frame, quickalert)}
  invisible(NULL)
}
