Processing M50 JTMS data
================
Dan Brennan
09 March 2017

This note outlines the process and code used to read the M50 JTMS data into a database and output journey time values to be used for the calculation of performance indicators.

``` r
knitr::opts_chunk$set(echo = TRUE)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
```

The path to where the journey time data is defined and a connection to the M50 sqlite database is opened. The JTMS links associated with the M50, in each direction are also defined.

``` r
direct <- "/var/datastore/2015/JTMS"
db <- src_sqlite("/var/datastore/SQLite/M50.sqlite", create = FALSE)

linksSB <- c(76, 61, 86, 17, 63, 58, 57, 91, 69, 53)
linksNB <- c(77, 66, 21, 19, 100, 52, 97, 70, 79, 94)
```

A function is created to iteratively read each JTMS csv file and load into a new table in the SQLite database.

``` r
csv2SQLite <- function(path) {
    setwd(path)
    files <- list.files(pattern = "*.csv")
    names <- c("linkID", "UTC", "n", "outliers", "sum", "sumSquared")

    lapply(files, function(i) {
        a <- read_csv(i, col_names = names, skip = 1) %>%
            db_insert_into(con = db$con, table = "journeyTimes", values = .)
        
        rm(a)
        gc()
        })
}
```

In order to filter by UNIX timestamps in SQLite, a function is defined to list the timestamps of the start and end of each year between 2013 and 2016.

``` r
createUnixCal <- function(year) {
    unixCal <- tibble(year = year, start = 0, end = 0) %>%
        mutate(start = as.POSIXct(paste(year, "-01-01 00:00:00", sep = "")),
               end = as.POSIXct(paste(year, "-12-31 23:59:59", sep = "")),
               startUnix = unclass(start),
               endUnix = unclass(end)) %>% 
        select(year, startUnix, endUnix)
}
```

Finally, a function is defined to split the journey times into seperate tables in the database for each year and for each direction. It also calculates the journey times in minutes

``` r
filterJT <- function(year, links, direction = c("SB", "NB")) {
    
    unix <- createUnixCal(year)
    
    tbl(db, "journeyTimes") %>%
        filter(linkID %in% links) %>% 
        mutate(UTC = UTC / 1000) %>% 
        filter(UTC >= unix[[2]], UTC <= unix[[3]]) %>%
        collect(n = Inf) %>% 
        mutate(journeyTime = ifelse(n == 0, 0, ((sum/1000)/n)/60)) %>% 
        db_insert_into(con = db$con, table = paste("journeyTimes" , unix[[1]], 
                                                   direction, sep = "_"), values = .)
        }
```

Running the csv2SQLite function loads all JTMS data into the database

``` r
csv2SQLite(direct)
```

``` r
tbl(db, "journeyTimes") %>% 
    glimpse(width = 100)
```

    ## Observations: NA
    ## Variables: 6
    ## $ linkID     <int> 3, 4, 6, 7, 8, 9, 10, 11, 12, 14, 15, 16, 17, 20, 21, 22, 23, 26, 27, 28, 29...
    ## $ UTC        <dbl> 1.32271e+12, 1.32271e+12, 1.32271e+12, 1.32271e+12, 1.32271e+12, 1.32271e+12...
    ## $ n          <int> 1, 2, 1, 3, 3, 3, 3, 3, 2, 3, 1, 4, 1, 2, 12, 3, 1, 3, 3, 2, 2, 4, 2, 0, 1, ...
    ## $ outliers   <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0...
    ## $ sum        <int> 253000, 990000, 975000, 1793000, 963000, 1943000, 1937000, 1409000, 1031000,...
    ## $ sumSquared <dbl> 6.400900e+10, 4.901220e+11, 9.506250e+11, 1.079170e+12, 3.133610e+11, 1.3015...

Running the filterJT function splits the data into seperate tables.

``` r
lapply((2013:2016), filterJT, linksSB, "SB")
lapply((2013:2016), filterJT, linksNB, "NB")
```

``` r
tbl(db, "journeyTimes_2015_NB") %>% 
    glimpse(width = 100)
```

    ## Observations: NA
    ## Variables: 7
    ## $ linkID      <int> 52, 66, 70, 77, 79, 94, 97, 100, 19, 52, 70, 77, 79, 94, 97, 100, 19, 52, 7...
    ## $ n           <int> 2, 2, 5, 3, 5, 4, 1, 3, 1, 6, 4, 3, 3, 4, 3, 5, 3, 6, 2, 2, 2, 3, 3, 3, 7, ...
    ## $ outliers    <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
    ## $ sum         <int> 139335, 331711, 475943, 314768, 371905, 595526, 331919, 399982, 162000, 420...
    ## $ sumSquared  <dbl> 9708544097, 55631432041, 45893545003, 33181134222, 28226249407, 89946290650...
    ## $ UTC         <dbl> 1420070400, 1420070400, 1420070400, 1420070400, 1420070400, 1420070400, 142...
    ## $ journeyTime <dbl> 1.161125, 2.764258, 1.586477, 1.748711, 1.239683, 2.481358, 5.531983, 2.222...
