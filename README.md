
# wr.data.table
<!-- badges: start --> 
[![BuildStatus](https://app.travis-ci.com/kleu046/wr.data.table.svg?token=zXCbCkvh7MhXzkjbdY8t&branch=master)](https://app.travis-ci.com/kleu046/wr.data.table)
[![R-CMD-check](https://github.com/kleu046/wr.data.table/workflows/R-CMD-check/badge.svg)](https://github.com/kleu046/wr.data.table/actions)
<!-- badges: end -->

This project is started as a self-training and self-learning example for build R
packages following the teachings on a very well structure and practical course
"Build an R package" provided by John Hopkins University on Coursera.org.
Motivations:
Functions in data.table offers blazing fast speed and is especially suied for
large datasets.  However, the author of these wrapper functions find data.table
syntax being somewhat less intuitive, hard to remember and read.
The wr.data.table project is a set of functions for performing data manipulation
using data.table.  These wrapper functions are written with the UNIX philosophy
in building functions that does one thing at one time and making it easy to
read, avoiding having to understand the data.table syntax.
The functions are written following tidy data philosophies and delibrately
imitate the style and piping workflow as those in tidyverse.  With piping in
mind, the wrapper functions in the package will always producce an output copy
of the input data.table.  Unlike some functions/operations in data.table, which
does change the original data.table.

## License
This project is free, open-sourced and licensed under MPL 2.0 The wrapper
functions written in this package depends upon the data.table package which is
available at https://github.com/Rdatatable/data.table.git

## Installation
You can install the released version of wr.data.table from
[CRAN](https://CRAN.R-project.org) with: WIP
You can install using devtools::install_github("kleu046/wr.data.table")
Alternatively, the package can be downloaded as a zip file on
https://github.com/kleu046/wr.data.table.git
Use install.packages(file.choose(), repos=NULL, type="source") to install with
the zip file ``` r install.packages("wr.data.table") ```

## Example
This is a basic example which shows you how to solve a common problem:
``` r library(wr.data.table) ## basic example code ``` WIP
