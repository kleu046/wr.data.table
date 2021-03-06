% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wr_helpers.R
\name{expand_colnames}
\alias{expand_colnames}
\title{Helper function to expand a range of columns given with ":" notation}
\usage{
expand_colnames(dt, range)
}
\arguments{
\item{dt}{a \code{data.table} or a \code{data.frame}}

\item{range}{a string defining range of columns using ":" notation}
}
\description{
Given a data.table or data.frame and a string of column range
using the ":", expand and find all columns within that range, including the
columns stated as the beginning and end of the range
}
\details{
This helper function is useful for extracting and finding the
columns between the bracket of column names provided.  This is useful when
evaluating the columns required when other wr_ functions use non-standard
evaluation.

This function is used internally in the package and not exported
}
\examples{
\dontrun{
mtcars
expand_colnames(mtcars, "mpg:hp")
}
}
