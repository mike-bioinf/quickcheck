% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_cross_vectors.R
\name{check_length_vecs}
\alias{check_length_vecs}
\title{Checks if two vectors have the same length (works with all object on which length can be called).}
\usage{
check_length_vecs(
  vec1,
  vec2,
  vec1_arg = "vec1",
  vec2_arg = "vec2",
  raise = "error",
  alert_message = NULL,
  quickalert = TRUE,
  n.evaluation_frame = 0,
  ...
)
}
\arguments{
\item{vec1}{First vector.}

\item{vec2}{Second vector.}

\item{vec1_arg}{string indicating how to address vec1 in the raised message (default 'vec1').}

\item{vec2_arg}{string indicating how to address vec2 in the raised message (default 'vec2)'.}

\item{raise}{Character string equal to one of "error", "warning" or "message" (default error). Set the type of alert that is created.}

\item{alert_message}{Character vector reporting the alert message. Default NULL, in this case a standard message is used.
It's also possible to pass a list of strings that is displayed as a nominated or numbered list.}

\item{quickalert}{logical, whether the raised alert has to be of class "quickalert".}

\item{n.evaluation_frame}{numeric, defines the number of stack frame to look down for the evaluation of the glue expressions of the alert message.
The default value (0) points to this function frame.}

\item{...}{not of direct use.}
}
\value{
invisible NULL
}
\description{
Checks if two vectors have the same length (works with all object on which length can be called).
}
