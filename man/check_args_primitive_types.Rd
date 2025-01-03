% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deprecated_check_args.R
\name{check_args_primitive_types}
\alias{check_args_primitive_types}
\title{Check the internal types of selected arguments of a calling function}
\usage{
check_args_primitive_types(
  args,
  expected_types,
  numeric_correspondence = NULL,
  null = FALSE,
  alert_message = NULL,
  header = "default",
  n_evaluation_frame = 0,
  quickalert = TRUE,
  ...
)
}
\arguments{
\item{args}{character vector reporting the arguments of the outer function to check.}

\item{expected_types}{character vector with the expected types.}

\item{numeric_correspondence}{numeric vector that allows to recycle expected_types following
its order and the numbers specified in this vector.}

\item{null}{Single logical, whether args can be NULL or not in addiction to the "normal" expected type (default FALSE).}

\item{alert_message}{Character vector reporting the alert message. Default NULL, in this case a standard message is used.
It's also possible to pass a list of strings that is displayed as a nominated or numbered list.}

\item{header}{String added at the beginning of the alert message. If "default" the default header is used, otherwise the string passed in.}

\item{n_evaluation_frame}{Numeric, defines the number of stack frame to look down for the evaluation of the glue expressions of the alert message.
The default (0) points to this function frame.}

\item{quickalert}{logical, whether the raised alert has to be of class "quickalert".}

\item{...}{To pass the additional parameters sign, list_format and header (if not in the formals parameters).}
}
\value{
invisible NULL.
}
\description{
Checks if the "primitive" or "internal" types obtained by applying the typeof function on the specified arguments are
in concordance with the expected ones.
}
