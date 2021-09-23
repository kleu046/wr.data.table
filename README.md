
# wr.data.table

<!-- badges: start -->
[![Build Status](https://app.travis-ci.com/kleu046/assgnPkg.svg?branch=master)](https://app.travis-ci.com/kleu046/assgnPkg)
[![R-CMD-check](https://github.com/kleu046/wr.data.table/workflows/R-CMD-check/badge.svg)](https://github.com/kleu046/wr.data.table/actions)
<!-- badges: end -->

This project is started as a self-training and self-learning example for build R packages following the teachings on a very well structure and practical course "Build an R package" provided by John Hopkins University on Coursera.org.

Motivations:

Functions in data.table offers blazing fast speed and is especially suied for large datasets.  However, the author of these wrapper functions find data.table syntax being somewhat less intuitive, hard to remember and read.

The wr.data.table project is a set of functions for performing data manipulation using data.table.  These wrapper functions are written with the UNIX philosophy in building functions that does one thing at one time and making it easy to read, avoiding having to understand the data.table syntax.

The functions are written following tidy data philosophies and delibrately imitate the style and piping workflow as those in tidyverse.  With piping in mind, the functions in the package, wherever appropriate, will make a copy of the input data.table and returns the output without modifying the original data.table that is stored in the global environment.  Unlike some functions/operations in data.table, which does change the original data.table.

## Installation

You can install the released version of wr.data.table from [CRAN](https://CRAN.R-project.org) with:

You can also install using devtools::install_github("kleu046/wr.data.table")

The package can be downloaded as a zip file on https://github.com/kleu046/wr.data.table.git

Use install.packages(file.choose(), repos=NULL, type="source") to install with the zip file
``` r
install.packages("wr.data.table")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(wr.data.table)
## basic example code
```
