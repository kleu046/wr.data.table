% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/condense.R
\name{condense}
\alias{condense}
\title{Summarize data}
\usage{
condense(dt, ...)
}
\arguments{
\item{dt}{input data.table}

\item{...}{some parameters}
}
\value{
returns a \code{data.table}
}
\description{
The condense function is similar to the summarize function in
the dplyr package. It is simply a wrapper function using data.table syntax
which performs a similar function as dplyr::summarize()

Variable names in characters need to be parsed into symbols first
}
\examples{
\dontrun{
 dt <- data.table(data1)

 # using condense function in various ways
 dt|>condense(mean_mpg=mean(mpg))
 dt|>condense(mean_mpg=mean("mpg"))
 dt|>condense(mean_mpg=mean(mpg), sd_mpg=sd(mpg),count=.N)
 dt|>condense(mean_mpg=mean("mpg"), sd_mpg=sd(mpg),count=.N)

 # grouping data by columns
 dt|>set_group("vs","am")|>condense(mean_mpg=mean(mpg))
 dt|>set_group(vs,am)|>condense(mean_mpg=mean("mpg"), sd_mpg=sd(mpg),count=.N)
 dt|>set_group(vs)|>rn_cols(mpg=`miles per gallon`)|>condense(mean_mpg=mean(`miles per gallon`))

 # using a variable to represent the character name of a column
 myvar <- "mpg"
 dt|>
   set_group(vs)|>
   condense(mean_mpg=mean(eval(char_to_symbol(myvar))),
            sd_mpg=sd(eval(char_to_symbol(myvar))),
            max_mpg = max(eval(char_to_symbol(myvar))),
            min_mpg = min(eval(char_to_symbol(myvar))),
            count=.N)
}
}
