% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sel_cols.R
\name{sel_cols}
\alias{sel_cols}
\title{Function to subset data.table by selecting columns/variables}
\usage{
sel_cols(dt, ...)
}
\arguments{
\item{dt}{input data.table}

\item{...}{vector of characters of column names, column names separated by
commas or range given with ":" notation}
}
\description{
This is a wrapper function to select columns in a data.table
}
\details{
The function creates a copy as data.table would by default.  Columns
will appear in the output data.table in the order that they are given in
"...". Repeat column names are removed.
}
