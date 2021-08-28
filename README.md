
# wr.data.table

<!-- badges: start -->
<!-- badges: end -->

This project started as a training and learning example for build R packages following the teachings on
a very well structure and practical course "Build an R package" provided by John Hopkins University on Coursera.org

Motivations:

While funcitons / operations using data.table can be very fast (compared to similar operations in tidyverse),
data.table operations can be unintuitive and harder to read.The wr.data.table project is a set of functions 
for performing data manipulation routines using data.table.  These wrapper functions are written with the UNIX philosphy
in building functions that does one thing at one time and making it easy to read, avoiding having to understand the 
data.table syntax.

These wrapper functions are also written with piping in mind.  Operations using data.table syntax could sometimes
cause confusion when used with piping, where some operations changes the original data.table.  The functions in the package,
wherever appropriate, will make a copy of the input data.table and returns the output without modifying the original data.table
at in the global environment.

## Installation

You can install the released version of wr.data.table from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("wr.data.table")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(wr.data.table)
## basic example code
```
