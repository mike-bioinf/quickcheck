% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_args.R
\name{check_args_incompatible}
\alias{check_args_incompatible}
\title{Check incompatible arguments}
\usage{
check_args_incompatible(
  args,
  alert_message = NULL,
  n_evaluation_frame = 0,
  quickalert = TRUE,
  ...
)
}
\arguments{
\item{args}{character vector reporting the arguments of the outer function to check.}

\item{alert_message}{Character vector reporting the alert message. Default NULL, in this case a standard message is used.
It's also possible to pass a list of strings that is displayed as a nominated or numbered list.}

\item{n_evaluation_frame}{Numeric, defines the number of stack frame to look down for the evaluation of the glue expressions of the alert message.
The default (0) points to this function frame.}

\item{quickalert}{logical, whether the raised alert has to be of class "quickalert".}

\item{...}{To pass the additional parameters sign, list_format and header (if not in the formals parameters).}
}
\value{
invisible NULL.
}
\description{
In rare occasion when one argument is set, others must remain NULL.
This function checks whether not compatible arguments are set in a caller function.
}
