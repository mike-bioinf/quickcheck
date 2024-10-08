% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_single_list.R
\name{check_uniform_list}
\alias{check_uniform_list}
\title{Checks if the list elements are of the same class.}
\usage{
check_uniform_list(
  x,
  flatten = TRUE,
  raise = "error",
  alert_message = NULL,
  n.evaluation_frame = 0,
  quickalert = TRUE,
  ...
)
}
\arguments{
\item{x}{list.}

\item{flatten}{logical, whether to flat out the list before doing the check (default TRUE).}

\item{raise}{Character string equal to one of "error", "warning" or "message" (default error). Set the type of alert that is created.}

\item{alert_message}{Character vector reporting the alert message. Default NULL, in this case a standard message is used.
It's also possible to pass a list of strings that is displayed as a nominated or numbered list.}

\item{n.evaluation_frame}{numeric, defines the number of stack frame to look down for the evaluation of the glue expressions of the alert message.
The default value (0) points to this function frame.}

\item{quickalert}{logical, whether the raised alert has to be of class "quickalert".}

\item{...}{To pass additional argument to alert_generator function.}
}
\value{
invisible NULL.
}
\description{
Checks if the list elements are of the same class.
}
