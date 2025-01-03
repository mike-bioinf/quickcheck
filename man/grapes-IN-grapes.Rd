% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trasversal_internals.R
\name{\%IN\%}
\alias{\%IN\%}
\title{Alternative version of the \%in\% operator that checks on identical set of classes for vec1 and vec2.
If the set of classes is different (not identical) then returns a vector of FALSE raising a warning.}
\usage{
vec1 \%IN\% vec2
}
\arguments{
\item{vec1}{First vector.}

\item{vec2}{Second vector.}
}
\value{
A logical vector.
}
\description{
Alternative version of the \%in\% operator that checks on identical set of classes for vec1 and vec2.
If the set of classes is different (not identical) then returns a vector of FALSE raising a warning.
}
