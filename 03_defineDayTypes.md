Defining Day Types
================
Dan Brennan
09 March 2017

This note outlines the process and code used to define day types for the years 2013 to 2017 and load them into the M50 SQLite database. Day types split all days in a year into 13 distinct day types as per the UK Highways England "On Time Reliability Measure" [(OTRM)](https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/527909/methodology-for-calculation-of-reliability-on-ha-network.pdf)

``` r
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(lubridate)
library(timeDate)
library(knitr)
```

A function is developed to classify all days in a specified year into day types. Initially public and bank holidays are defined which are common across all years. School holiday periods are then defined, depending on the year, these have been taken from the following links:

[Before 2014/15](http://www.tui.ie/news-events/standardised-school-year-dates-201112-201213-and-201314.1429.html)

[2014/15, 2015/16 and 2016/17](http://www.education.ie/en/Circulars-and-Forms/Active-Circulars/cl0016_2014.pdf)

[2017/18, 2018/19 and 2019/20](http://www.education.ie/en/Circulars-and-Forms/Active-Circulars/cl0009_2017.pdf)

The output of the function is a dataframe containing each day of the year classified by day type.

``` r
dayTypes <- function(year) {
    dates <- seq(ymd(paste(year, 01, 01, sep = "-")), ymd(paste(year, 12, 31, sep = "-")), by = "1 day")
    
    # Public Holidays / Bank Holidays
    newYears <- ymd(paste(year, 01, 01, sep = "-"))
    nextNewYears <- ymd(paste(year + 1, 01, 01, sep = "-"))
    stPatsBH <- as.Date(ifelse(wday(ymd(paste(year, 03, 17, sep = "-"))) == 7, 
                               ymd(paste(year, 03, 17, sep = "-")) + 2,
                        ifelse(wday(ymd(paste(year, 03, 17, sep = "-"))) == 1, 
                               ymd(paste(year, 03, 17, sep = "-")) + 1,
                        ymd(paste(year, 03, 17, sep = "-")))), 
                        origin = "1970-01-01")
    
    eastMon <- as.Date(EasterMonday(year))
    goodFri <- as.Date(GoodFriday(year))
    mayBH <- subset(dates, wday(dates, label = TRUE) == "Mon" & day(dates) <= 7 & month(dates) == 5)
    juneBH <- subset(dates, wday(dates, label = TRUE) == "Mon" & day(dates) <= 7 & month(dates) == 6)
    augBH <- subset(dates, wday(dates, label = TRUE) == "Mon" & day(dates) <= 7 & month(dates) == 8)
    octBH <- subset(dates, wday(dates, label = TRUE) == "Mon" & day(dates) >= (31 - 6) & month(dates) == 10)  
    xmas <- ymd(paste(year, 12, 25, sep = "-"))
    stephens <- ymd(paste(year, 12, 26, sep = "-"))
    
    pubHol <- c(stPatsBH, eastMon, goodFri, mayBH, juneBH, augBH, octBH)
    xmasPeriod <- seq(xmas + 1, nextNewYears - 1, by = "1 day")
    
    
    
    # School Holidays
    summerHols <- seq(ymd(paste(year, 07, 01, sep = "-")), ymd(paste(year, 08, 31, sep = "-")), by = "1 day")
    
    if (year == 2012) {
        
        xmasHolPrev <- 
        febMT <- seq(ymd(paste(year, 01, 02, sep = "-")), 
                     ymd(paste(year, 01, 05, sep = "-")), 
                     by = "1 day")
        eastHol <- seq(ymd(paste(year, 03, 31, sep = "-")), 
                       ymd(paste(year, 04, 15, sep = "-")), 
                       by = "1 day")
        octMT <- seq(ymd(paste(year, 10, 29, sep = "-")), 
                     ymd(paste(year, 11, 02, sep = "-")), 
                     by = "1 day")
        xmasHol <- seq(ymd(paste(year, 12, 21, sep = "-")), 
                       ymd(paste(year, 12, 31, sep = "-")), 
                       by = "1 day") 
        
    }
    
    if (year == 2013) {
        
        xmasHolPrev <- seq(ymd(paste(year, 01, 02, sep = "-")), 
                           ymd(paste(year, 01, 05, sep = "-")), 
                           by = "1 day") 
        febMT <- seq(ymd(paste(year, 02, 11, sep = "-")), 
                     ymd(paste(year, 02, 15, sep = "-")), 
                     by = "1 day")
        eastHol <- seq(ymd(paste(year, 03, 23, sep = "-")), 
                       ymd(paste(year, 04, 07, sep = "-")), 
                       by = "1 day")
        octMT <- seq(ymd(paste(year, 10, 28, sep = "-")), 
                     ymd(paste(year, 11, 01, sep = "-")), 
                     by = "1 day")
        xmasHol <- seq(ymd(paste(year, 12, 21, sep = "-")), 
                       ymd(paste(year, 12, 31, sep = "-")), 
                       by = "1 day")
        
    }
    
    if (year == 2014) {
        
        xmasHolPrev <- seq(ymd(paste(year, 01, 02, sep = "-")),
                           ymd(paste(year, 01, 05, sep = "-")),
                           by = "1 day") 
        febMT <- seq(ymd(paste(year, 02, 17, sep = "-")), 
                     ymd(paste(year, 02, 21, sep = "-")), 
                     by = "1 day")
        eastHol <- seq(ymd(paste(year, 04, 12, sep = "-")), 
                       ymd(paste(year, 04, 27, sep = "-")), 
                       by = "1 day")
        octMT <- seq(ymd(paste(year, 10, 27, sep = "-")), 
                     ymd(paste(year, 10, 31, sep = "-")), 
                     by = "1 day")
        xmasHol <- seq(ymd(paste(year, 12, 20, sep = "-")), 
                       ymd(paste(year, 12, 31, sep = "-")), 
                       by = "1 day")
        
    }
    
    if (year == 2015) {
        
        xmasHolPrev <- seq(ymd(paste(year, 01, 02, sep = "-")), 
                           ymd(paste(year, 01, 04, sep = "-")), 
                           by = "1 day") 
        febMT <- seq(ymd(paste(year, 02, 16, sep = "-")), 
                     ymd(paste(year, 02, 20, sep = "-")), 
                     by = "1 day")
        eastHol <- seq(ymd(paste(year, 03, 28, sep = "-")), 
                       ymd(paste(year, 04, 12, sep = "-")),
                       by = "1 day")
        octMT <- seq(ymd(paste(year, 10, 26, sep = "-")), 
                     ymd(paste(year, 10, 30, sep = "-")), 
                     by = "1 day")
        xmasHol <- seq(ymd(paste(year, 12, 23, sep = "-")),
                       ymd(paste(year + 1, 12, 31, sep = "-")), 
                       by = "1 day")
        
    }
    
    if (year == 2016) {
        
        xmasHolPrev <- seq(ymd(paste(year, 01, 02, sep = "-")),
                           ymd(paste(year, 01, 05, sep = "-")),
                           by = "1 day") 
        febMT <- seq(ymd(paste(year, 02, 15, sep = "-")),
                     ymd(paste(year, 02, 19, sep = "-")), 
                     by = "1 day")
        eastHol <- seq(ymd(paste(year, 03, 17, sep = "-")),
                       ymd(paste(year, 04, 03, sep = "-")), 
                       by = "1 day")
        octMT <- seq(ymd(paste(year, 10, 31, sep = "-")), 
                     ymd(paste(year, 11, 04, sep = "-")), 
                     by = "1 day")
        xmasHol <- seq(ymd(paste(year, 12, 23, sep = "-")), 
                       ymd(paste(year + 1, 01, 08, sep = "-")),
                       by = "1 day")
        
    }
    
    if (year == 2017) {
        
        xmasHolPrev <- seq(ymd(paste(year, 01, 02, sep = "-")), 
                           ymd(paste(year, 01, 06, sep = "-")),
                           by = "1 day") 
        febMT <- seq(ymd(paste(year, 02, 20, sep = "-")), 
                     ymd(paste(year, 02, 24, sep = "-")), 
                     by = "1 day")
        eastHol <- seq(ymd(paste(year, 04, 10, sep = "-")), 
                       ymd(paste(year, 04, 21, sep = "-")),
                       by = "1 day")
        octMT <- seq(ymd(paste(year, 10, 30, sep = "-")), 
                     ymd(paste(year, 11, 03, sep = "-")), 
                     by = "1 day")
        xmasHol <- seq(ymd(paste(year, 12, 25, sep = "-")), 
                       ymd(paste(year + 1, 01, 05, sep = "-")), 
                       by = "1 day")
        
    }

    if (year == 2018) {
        
        xmasHolPrev <- seq(ymd(paste(year, 01, 02, sep = "-")),
                           ymd(paste(year, 01, 05, sep = "-")), 
                           by = "1 day") 
        febMT <- seq(ymd(paste(year, 02, 12, sep = "-")), 
                     ymd(paste(year, 02, 16, sep = "-")), 
                     by = "1 day")
        eastHol <- seq(ymd(paste(year, 03, 26, sep = "-")), 
                       ymd(paste(year, 04, 06, sep = "-")), 
                       by = "1 day")
        octMT <- seq(ymd(paste(year, 10, 29, sep = "-")), 
                     ymd(paste(year, 11, 02, sep = "-")), 
                     by = "1 day")
        xmasHol <- seq(ymd(paste(year, 12, 24, sep = "-")),
                       ymd(paste(year + 1, 01, 04, sep = "-")),
                       by = "1 day")
        
    }

    if (year == 2019) {
        
        xmasHolPrev <- seq(ymd(paste(year, 01, 02, sep = "-")),
                           ymd(paste(year, 01, 04, sep = "-")),
                           by = "1 day") 
        febMT <- seq(ymd(paste(year, 02, 18, sep = "-")), 
                     ymd(paste(year, 02, 22, sep = "-")), 
                     by = "1 day")
        eastHol <- seq(ymd(paste(year, 04, 15, sep = "-")), 
                       ymd(paste(year, 04, 26, sep = "-")),
                       by = "1 day")
        octMT <- seq(ymd(paste(year, 10, 28, sep = "-")), 
                     ymd(paste(year, 11, 01, sep = "-")), 
                     by = "1 day")
        xmasHol <- seq(ymd(paste(year, 12, 23, sep = "-")), 
                       ymd(paste(year + 1, 01, 03, sep = "-")), 
                       by = "1 day")
        
    }
    
    schoolHols <- c(xmasHolPrev, febMT, eastHol, octMT, summerHols, xmasHol)    
    schoolHols <- schoolHols[!weekdays(schoolHols) %in% c('Saturday','Sunday')]
    schoolHols <- schoolHols[!schoolHols %in% c(xmas, newYears, xmasPeriod, pubHol, nextNewYears)]
    
    dayTypesDf <- data_frame(date = dates) %>% 
        mutate(day = wday(date, label = TRUE),
               dayType = ifelse(lead(date) %in% pubHol & wday(date) %in% c(3:5) & !date %in% schoolHols, 4,
                         ifelse(lag(date) %in% pubHol & wday(date) %in% c(3:5), 0,
                         ifelse(date %in% c(xmas, newYears), 14,
                         ifelse(date %in% xmasPeriod, 13,
                         ifelse(date %in% pubHol, 12,
                         ifelse(date %in% schoolHols & wday(date) == 6, 11,
                         ifelse(date %in% schoolHols & wday(date) %in% c(3:5), 9,
                         ifelse(date %in% schoolHols & wday(date) == 2, 7,
                         ifelse(wday(date) == 1, 6,
                         ifelse(wday(date) == 7, 5,
                         ifelse(!date %in% c(xmas, newYears, xmasPeriod, pubHol, schoolHols) 
                                & wday(date) == 6, 4,
                         ifelse(!date %in% c(xmas, newYears, xmasPeriod, pubHol, schoolHols) 
                                & wday(date) == 5, 3,
                         ifelse(!date %in% c(xmas, newYears, xmasPeriod, pubHol, schoolHols) 
                                & wday(date) == 4, 2,
                         ifelse(!date %in% c(xmas, newYears, xmasPeriod, pubHol, schoolHols) 
                                & wday(date) == 3, 1,0)
                         ))))))))))))))
    
    return(dayTypesDf)
    
}
```

A dataframe is created defining all day types between 2012 and 2019, which is then loaded into the M50 SQLite database

``` r
dayTypes1219 <- c(2012:2019) %>%
    map(dayTypes) %>% 
    bind_rows() %>% 
    mutate(date = as.numeric(as.POSIXct(date)))
    
dayTypes1219 %>% 
  head(10) %>% 
  kable()
```

|        date| day   |  dayType|
|-----------:|:------|--------:|
|  1325376000| Sun   |       14|
|  1325462400| Mon   |        7|
|  1325548800| Tues  |        9|
|  1325635200| Wed   |        9|
|  1325721600| Thurs |        9|
|  1325808000| Fri   |        4|
|  1325894400| Sat   |        5|
|  1325980800| Sun   |        6|
|  1326067200| Mon   |        0|
|  1326153600| Tues  |        1|

``` r
db <- src_sqlite("/var/datastore/SQLite/M50.sqlite", create = FALSE)
db_insert_into(con = db$con, table = "dayTypes1219", values = dayTypes1219)
```

Text descriptions for each day types are defined and loaded into the M50 SQLite database.

``` r
dtDesc <- data_frame(dayType = c(0:7, 9, 11, 12:14)) %>% 
    mutate(dtDesc = as.factor(c("First working day of normal week",
                                "Normal working Tuesday",
                                "Normal working Wednesday",
                                "Normal working Thursday",
                                "Last working day of normal week",
                                "Saturday",
                                "Sunday",
                                "First day of week - school holiday",
                                "Middle of week - school holiday",
                                "Last day of week - school holiday",
                                "Bank Holidays including Good Friday", 
                                "Christmas period holidays",
                                "Christmas Day / New Years Day")))

dtDesc %>% 
  kable()
```

|  dayType| dtDesc                              |
|--------:|:------------------------------------|
|        0| First working day of normal week    |
|        1| Normal working Tuesday              |
|        2| Normal working Wednesday            |
|        3| Normal working Thursday             |
|        4| Last working day of normal week     |
|        5| Saturday                            |
|        6| Sunday                              |
|        7| First day of week - school holiday  |
|        9| Middle of week - school holiday     |
|       11| Last day of week - school holiday   |
|       12| Bank Holidays including Good Friday |
|       13| Christmas period holidays           |
|       14| Christmas Day / New Years Day       |

``` r
db_insert_into(con = db$con, table = "dayTypesRef", values = dtDesc)
```
