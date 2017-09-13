#!/usr/bin/Rscript

library(tidyverse)
library(lubridate)
library(knitr)
library(aws.s3)

data_dir <- "~/datastore/"
analysis_dir <- "~/R/projects/m50-perf-ind/"
year <- 2014
route <- 50
site_no <- c(1501, 1502, 1503, 1504, 1505, 1506, 1507, 1508, 1509,
             15010, 15011, 15012)

names <- c("siteID", "RecTime", "SubSec", "Flownum", "Direction",
           "Speed", "Class", "Headway", "Gap", "Length", "GrossWeight", 
           "NumAxles", "ESAL", "Axles", "Weather", "VehicleId", 
           "ClassScheme", "Flag", "BSEQ", "Code", "LegalStatus", 
           "Validity", "ANPRConfidence", "ImagesPresent", "siteID2", "routeNo")

create_unix_cal <- function(y) {
    unix_cal <- tibble(year = rep(y, 12), month = formatC(1:12,
                                                          width = 2,
                                                          flag = "0"), 
                       days = 0, start = 0, end = 0) %>%
        mutate(days = as.character( days_in_month(as.POSIXct(paste(year,
                                                                   month,
                                                                   "01", 
                                                                   sep = "-"),
                                                             format = "%Y-%m-%d"))),
               start = as.POSIXct(paste(year, "-", month, "-", "01", " 00:00:00", sep = "")),		     
               end = as.POSIXct(paste(year, "-", month, "-", days, " 23:59:59", sep = "")),
               start_unix = unclass(start),
               end_unix = unclass(end))
    
    unix_cal <- unix_cal %>% 
        select(start_unix, end_unix, month)
    
}

unix_df <- create_unix_cal(year)

aggregate_pvr <- function(site_no, route, year, 
                          aggregation = c("day", "hour", "min15", "min5"), 
                          month) {
    
    
    secs <- ifelse(aggregation == "day", 86400,
                   ifelse(aggregation == "hour", 3600,
                          ifelse(aggregation == "min15", 900,
                                 ifelse(aggregation == "min5", 300, NA))))
    
    file_name <- paste(analysis_dir, 
                       paste0("output/pvr/", "pvr-", route, "-",
                              site_no, "-", month, "-", year),
                       ".csv", 
                       sep = "")
    
    if (!file.exists(file_name)) {
        return (NULL)
    }
    
    else {
        s <- read_csv(file_name, col_names = names) %>% 
            arrange(RecTime) %>% 
            select(siteID2, routeNo, RecTime, Code, Direction, 
                   Speed, Class, Length) %>%     
            rename(siteID = siteID2) %>% 
            mutate(time = trunc(as.numeric(RecTime) / secs) * secs,
                   Speed = (Speed / 1000) * 3.6) %>%
            group_by(siteID, time, Class, Code) %>%
            summarise(volume = n(), speed = mean(Speed)) %>% 
            ungroup() %>% 
            write_csv(paste0(analysis_dir, paste0("output/aggr/", 
                                                 aggregation, "-",
                                                 route,  "-",
                                                 year),
                            ".csv"),
                      append = TRUE)
    }
    
    rm(s)
    gc(verbose = FALSE)
    
}

lapply(site_no, function(x) {
    #daily
    lapply(unix_df$month, aggregate_pvr, 
           site_no = x, route = route, year = year, aggregation = "day")
    #hourly
    lapply(unix_df$month, aggregate_pvr, 
           site_no = x, route = route, year = year, aggregation = "hour")
    #15 min
    lapply(unix_df$month, aggregate_pvr, 
           site_no = x, route = route, year = year, aggregation = "min15")
    #5 min
    lapply(unix_df$month, aggregate_pvr, 
           site_no = x, route = route, year = year, aggregation = "min5")
})

aggregation <- c("day", "hour", "min15", "min5")

aggregation %>% 
    map(function(x) {
        
        file_name <- paste0(analysis_dir, paste0("output/aggr/", 
                                                 aggregation, "-",
                                                 route,  "-",
                                                 year),
                            ".csv")
        
        if (!file.exists(file_name)) {
            return (NULL)
        }
        
        else {
            read_csv(file_name, col_names = c("siteID", "time", "Class", "Code", "volume", "speed")) %>% 
                write_csv(file_name)    
        }
        
    })
