% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_cross_dfs.R, R/make_test_function.R
\name{check_presence_dfs}
\alias{check_presence_dfs}
\alias{test_presence_dfs}
\title{Perform an oriented check on the values of two columns of two dataframes}
\usage{
check_presence_dfs(
  df1,
  df2,
  columns,
  coerce = FALSE,
  direction = "first_in_second",
  df1_arg = "df1",
  df2_arg = "df2",
  raise = "error",
  alert_message = NULL,
  n_evaluation_frame = 0,
  quickalert = TRUE,
  ...
)

test_presence_dfs(
  df1,
  df2,
  columns,
  coerce = FALSE,
  direction = "first_in_second",
  df1_arg = "df1",
  df2_arg = "df2",
  raise = "error",
  alert_message = NULL,
  n_evaluation_frame = 0,
  quickalert = TRUE,
  ...
)
}
\arguments{
\item{df1}{first dataframe.}

\item{df2}{second dataframe.}

\item{columns}{Character vector with the names of the columns to consider. If of length one
the single name is assumed for both dataframes. If of length two the first name is assumed
for the first dataframe and the other for the second.}

\item{coerce}{Boolean indicating whether the 2 columns can be coerced during the check (default FALSE).}

\item{direction}{String equal to one of 'first_in_second', 'second_in_first' or 'bidirectional'. Set the direction of the comparison.}

\item{df1_arg}{String specifying how to address first df in the raised messages (default "df1").}

\item{df2_arg}{String specifying how to address second df in the raised messages (default "df2").}

\item{raise}{Character string equal to one of "error", "warning" or "message" (default error). Set the type of alert that is created.}

\item{alert_message}{Character vector reporting the alert message. Default NULL, in this case a standard message is used.
It's also possible to pass a list of strings that is displayed as a nominated or numbered list.}

\item{n_evaluation_frame}{Numeric, defines the number of stack frame to look down for the evaluation of the glue expressions of the alert message.
The default (0) points to this function frame.}

\item{quickalert}{logical, whether the raised alert has to be of class "quickalert".}

\item{...}{To pass the additional parameters sign, list_format and header (if not in the formals parameters).}
}
\value{
Depending on the function prefix: the "check" function returns the condition otherwise NULL invisibly,
the "test" function returns TRUE if the condition would be raised and FALSE otherwise.
}
\description{
The function allows to check the presence of all values of the selected column of one dataframe
in the other. If direction equal 'bidirectional' a bidirectional check is performed.
}
