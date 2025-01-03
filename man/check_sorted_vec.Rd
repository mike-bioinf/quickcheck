% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_single_vec.R, R/make_test_function.R
\name{check_sorted_vec}
\alias{check_sorted_vec}
\alias{test_sorted_vec}
\title{Check whether a vector is sorted}
\usage{
check_sorted_vec(
  vec,
  decreasing = FALSE,
  strict = TRUE,
  vec_arg = "vec",
  raise = "error",
  alert_message = NULL,
  n_evaluation_frame = 0,
  quickalert = TRUE,
  ...
)

test_sorted_vec(
  vec,
  decreasing = FALSE,
  strict = TRUE,
  vec_arg = "vec",
  raise = "error",
  alert_message = NULL,
  n_evaluation_frame = 0,
  quickalert = TRUE,
  ...
)
}
\arguments{
\item{vec}{Vector to check.}

\item{decreasing}{Boolean indicating whether the expect sorted order is decreasing (default FALSE).}

\item{strict}{Boolean indicating whether to consider numeric instability in the check (default TRUE).
If FALSE the vector is compared to it's sorted version using "all_equal" instead of the "==" operator.
This helps to mitigate issues with floating-point precision, utilizing the default tolerance.}

\item{vec_arg}{String indicating how to address vec in the alert message (default 'vec').}

\item{raise}{Character string equal to one of "error", "warning" or "message" (default error). Set the type of alert that is created.}

\item{alert_message}{Character vector reporting the alert message. Default NULL, in this case a standard message is used.
It's also possible to pass a list of strings that is displayed as a nominated or numbered list.}

\item{n_evaluation_frame}{numeric, defines the number of stack frame to look down for the evaluation of the glue expressions of the alert message.
The default value (0) points to the frame above this function frame. So to point to the frame below this function frame you have to set 2.}

\item{quickalert}{logical, whether the raised alert has to be of class "quickalert".}

\item{...}{To pass the additional parameters sign, list_format and header (if not in the formals parameters).}
}
\value{
Depending on the function prefix: the "check" function returns the condition otherwise NULL invisibly,
the "test" function returns TRUE if the condition would be raised and FALSE otherwise.
}
\description{
Check whether a vector is sorted
}
\details{
NAs in the vector are ignored during the check. NULL and zero-length vector is treated as sorted.
}
