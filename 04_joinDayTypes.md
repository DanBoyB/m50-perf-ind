Joining Day Type data
================
Dan Brennan
10 March 2017

``` r
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(lubridate)
```

Connect to the M50 SQLite database

``` r
db <- src_sqlite("/var/datastore/SQLite/M50.sqlite", create = FALSE)
```

Create a function to join day types to the C2 aggregated tables and JTMS tables

``` r
joinDayTypes <- function(table) {
    dayTypes15 <- tbl(db, "dayTypes1219") %>% 
        collect(n = Inf) %>% 
        mutate(date = as.Date(as.POSIXct(date, origin = "1970-01-01"))) 
    
    lapply(table, function(row) {
         x <- tbl(db, as.character(row)) %>%
           collect(n = Inf)
         
         # c2 tables have "time" column, JTMS tables have "UTC column
         if ("time" %in% colnames(x)) {
           x <- x %>%
             mutate(time = as.POSIXct(time, origin = "1970-01-01"),
                    date = as.Date(time)) %>%
             left_join(dayTypes15, by = "date")
         }

         else {
           x <- x %>%
             mutate(time = as.POSIXct(UTC, origin = "1970-01-01"),
                    date = as.Date(time)) %>%
             left_join(dayTypes15, by = "date")
         }

         # # backup table
         saveRDS(x, paste("output/", as.character(row), ".rds", sep = ""))
         
         # drop from SQLite database and replace with updated table
         db$con %>%
           db_drop_table(as.character(row))
         
         db$con %>%
           db_insert_into(as.character(row), x)

         })
}
```

A dataframe of all C2 aggregated table names and JTMS table names is created.

``` r
pattern <- "day_|hour_|min15_|min5_|journeyTimes_"

tables <- src_tbls(db) %>% 
  as_data_frame() %>% 
  filter(grepl(pattern, value)) %>% 
  as.list()

tables
```

    ## $value
    ##  [1] "day_50_2015"          "hour_50_2015"         "journeyTimes_2013_NB"
    ##  [4] "journeyTimes_2013_SB" "journeyTimes_2014_NB" "journeyTimes_2014_SB"
    ##  [7] "journeyTimes_2015_NB" "journeyTimes_2015_SB" "journeyTimes_2016_NB"
    ## [10] "journeyTimes_2016_SB" "min15_50_2015"        "min5_50_2015"

The joinDayTypes function is then applied to all C2 aggregated tables and JTMS tables

``` r
tables %>% 
  map(joinDayTypes)
```

Below is a sample of updated C2 hourly aggregated table

``` r
tbl(db, "hour_50_2015") %>% 
    select(-date, -day) %>% 
    head(10)
```

    ## Source:   query [?? x 7]
    ## Database: sqlite 3.11.1 [/var/datastore/SQLite/M50.sqlite]
    ## 
    ##    siteID       time Class  Code volume     speed dayType
    ##     <int>      <dbl> <int> <int>  <int>     <dbl>   <dbl>
    ## 1    1501 1420070400     2   622     24  97.91670      14
    ## 2    1501 1420070400     2   623    131  98.35882      14
    ## 3    1501 1420070400     2   624    145 101.17929      14
    ## 4    1501 1420070400     2   625     36 114.36090      14
    ## 5    1501 1420070400     2   626     27 110.66707      14
    ## 6    1501 1420070400     2   627    130 104.78462      14
    ## 7    1501 1420070400     2   628    147  93.42869      14
    ## 8    1501 1420070400     2   629     43 101.90696      14
    ## 9    1501 1420070400     3   623     13  97.00006      14
    ## 10   1501 1420070400     3   624      6 100.33380      14
    ## # ... with more rows

Below is a sample of updated JTMS table

``` r
tbl(db, "journeyTimes_2016_NB") %>% 
    select(-outliers, -time, -date, -day) %>% 
    head(10)
```

    ## Source:   query [?? x 7]
    ## Database: sqlite 3.11.1 [/var/datastore/SQLite/M50.sqlite]
    ## 
    ##    linkID     n    sum   sumSquared        UTC journeyTime dayType
    ##     <int> <int>  <int>        <dbl>      <dbl>       <dbl>   <dbl>
    ## 1      19     1 190000  36100000000 1451606400    3.166667      14
    ## 2      21     7 819000  97399000000 1451606400    1.950000      14
    ## 3      52     4 247640  15366448800 1451606400    1.031833      14
    ## 4      66     4 622464  96974972812 1451606400    2.593600      14
    ## 5      70     3 323179  35323899861 1451606400    1.795439      14
    ## 6      77     3 314412  32996645054 1451606400    1.746733      14
    ## 7      79     2 155995  12206496397 1451606400    1.299958      14
    ## 8      94     2 305497  46725469885 1451606400    2.545808      14
    ## 9      97     3 777819 202274987153 1451606400    4.321217      14
    ## 10    100     3 359154  43148834676 1451606400    1.995300      14
    ## # ... with more rows
