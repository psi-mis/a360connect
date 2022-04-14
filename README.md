
<!-- README.md is generated from README.Rmd. Please edit that file -->

# a360connect

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/psi-mis/a360connect/branch/master/graph/badge.svg)](https://codecov.io/gh/psi-mis/a360connect?branch=master)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)
<!-- badges: end -->

The goal of a360connect is to integrate data from the A360 single events
program, Attendance and Service Log, into a DHIS2 tracker program,
Provider Call Log, to allow follow-up on clients on a method for method
continuation.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("psi-mis/a360connect")
```

## Getting started

The next step after installation is to add the configuration settings to
the `r_environ` file:

``` r
# open the r_env file
usethis::edit_r_environ()
#> • Edit '/Users/isaiahnyabuto/.Renviron'
#> • Restart R for changes to take effect
```

Add the following to the `r_environ` file:

``` r
CLONE = "https://play.dhis2.org/" # test server url
C_USER = "admin" # DHIS2 username
C_PASS = "district" # DHIS2 Password
PROGRAM = "QXuYEgGmTjX" # the ID of DHIS2 program to import the teis
BASEURL = "https://play.dhis2.org/" # production server
TRACKED_ENTITY_TYPE = "XV3kldsZq0H"# DHIS2 ID of the tracked entity type
```

## Get A360 events from the Attendance and Service Log program

This is a basic example which shows you how to get A360 events from the
Attendance and Service Log program:

``` r
library(a360connect)
#> Loading required package: data.table
#> Loading required package: dplyr
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:data.table':
#> 
#>     between, first, last
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
#> Loading required package: googlesheets4
#> Loading required package: httr
#> Loading required package: jsonlite
## basic example code
```
