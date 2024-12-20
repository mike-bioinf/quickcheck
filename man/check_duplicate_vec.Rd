% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_single_vec.R
\name{check_duplicate_vec}
\alias{check_duplicate_vec}
\title{Check the presence of duplicated values in a vector}
\usage{
check_duplicate_vec(
  vec,
  vec_arg = "vec",
  na_rm = TRUE,
  raise = "error",
  alert_message = NULL,
  header = "default",
  n_evaluation_frame = 0,
  quickalert = TRUE,
  ...
)
}
\arguments{
\item{vec}{Vector to check.}

\item{vec_arg}{String indicating how to address vec in the alert message (default 'vec').}

\item{na_rm}{Boolean indicating if NA must be excluded prior checking (default TRUE).}

\item{raise}{Character string equal to one of "error", "warning" or "message" (default error). Set the type of alert that is created.}

\item{alert_message}{Character vector reporting the alert message. Default NULL, in this case a standard message is used.
It's also possible to pass a list of strings that is displayed as a nominated or numbered list.}

\item{header}{Character string to add at the beginning of the alert message.}

\item{n_evaluation_frame}{Numeric, defines the number of stack frame to look down for the evaluation of the glue expressions of the alert message.
The default (0) points to this function frame.}

\item{quickalert}{logical, whether the raised alert has to be of class "quickalert".}

\item{...}{To pass additional argument to alert_generator function.}
}
\value{
invisible NULL
}
\description{
Check the presence of duplicated values in a vector
}
