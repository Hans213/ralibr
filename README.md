
<!-- README.md is generated from README.Rmd. Please edit that file -->
ralibr
======

Installation
------------

``` r
# To use the Excel integration first download and install BERT.

# If installing for R
install.packages("devtools")
devtools::install_github("ThVWh/ralibr")

# If installing for Excel (via BERT console)
install.packages("devtools")

# and Add this to functions.R
BERT$ClearUserButtons();
BERT$AddUserButton("Update ralibr",function(){
        library(devtools);
        install_github("ThVWh/ralibr")})
BERT$AddUserButton("Install ralibr",function(){BERT$UsePackage("ralibr","ralibr")})
```

Calendars
---------

You can determine whether dates are business days in a specific locale or specific locales:

``` r
library("lubridate", warn.conflicts = FALSE)
#> Warning: package 'lubridate' was built under R version 3.4.3
library("fmdates")
euta <- EUTACalendar()
aume <- AUMECalendar()
syme <- c(euta, aume) # handy JointCalendar construction approach
is_good(ymd(20140404), euta)
#> [1] TRUE
is_good(ymd(20141104), syme) # Melbourne Cup holiday
#> [1] FALSE
```

Adjusters and shifters
----------------------

You can adjust (or roll) and shift dates using predefined business day conventions:

``` r
# Adjust using the modified following convention
adjust(ymd(20140404), 'mf', euta)
#> [1] "2014-04-04"
# Shift dates
shift(ymd(20120229), months(1), 'u', euta, FALSE) # one month
#> [1] "2012-03-29"
shift(ymd(20120229), months(1), 'mf', euta, TRUE)  # one month with EOM rule
#> [1] "2012-03-30"
shift(ymd(20120229), years(1) + months(3), 'mf', euta, TRUE)  # 1y3m
#> [1] "2013-05-31"
```

Schedules
---------

The preceding methods are used to generate schedules of dates required to define common financial contracts events such as cash flow exchange dates:

``` r
generate_schedule(effective_date = ymd(20120103), termination_date = ymd(20130103), 
  tenor = months(3), calendar = euta, bdc = "mf", stub = "short_front", 
  eom_rule = FALSE)
#> [1] 2012-01-03 UTC--2012-04-03 UTC 2012-04-03 UTC--2012-07-03 UTC
#> [3] 2012-07-03 UTC--2012-10-03 UTC 2012-10-03 UTC--2013-01-03 UTC
```

Year fractions
--------------

Time lengths then usually need to be computed for each interval of such a schedule according to some day basis convention:

``` r
# 30/360us convention
year_frac(ymd("2010-03-31"), ymd("2012-03-31"), "30/360us")
#> [1] 2
# act/365 convention
year_frac(ymd("2010-02-28"), ymd("2012-03-31"), "act/365")
#> [1] 2.087671
```
