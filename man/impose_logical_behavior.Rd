% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/impose_wrappers.R
\name{impose_logical_behavior}
\alias{impose_logical_behavior}
\title{Alter the behavior of checking conditions from raising alerts to return boolean values.}
\usage{
impose_logical_behavior(expr, force_alert = FALSE, strip_quickalert = FALSE)
}
\arguments{
\item{expr}{"check_*" function call.}

\item{force_alert}{Boolean, whether to signal the quickalert once caught (default FALSE).
Useful for messages and warnings and in all situations in which you want to raise the condition in addition to obtain the boolean value.}

\item{strip_quickalert}{Boolean, whether to strip the re-signaled quickalert of the "quickalert class" (default FALSE).
Works only if force_alert is set to TRUE.}
}
\value{
A single logical value.
}
\description{
This function works as a wrapper of the checking functions and impose them a 'logical' behavior.
This means that they will not raise alerts (unless forced) but instead they will return boolean values.
In greater detail if a 'quickalert' class condition is hit, then the function return TRUE otherwise FALSE.
}
