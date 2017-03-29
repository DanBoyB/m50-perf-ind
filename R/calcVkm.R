library(tidyverse)
library(lubridate)

calcVkm <- function(path, database, from, to) {
  
  db <- src_sqlite(paste(path, database, sep = ""), create = FALSE)
  from <- as.numeric(as.POSIXct(from))
  to <- as.numeric(as.POSIXct(to))
  
  dayTypes <- tbl(db, "dayTypes1219") %>% 
    filter(date >= from, date <= to) %>% 
    collect(n = Inf) %>% 
    mutate(date = as.Date(as.POSIXct(date, origin = "1970-01-01")))
  
  lengths <- tbl(db, "tmuLengths") %>% 
    collect()
  
  hourly <- tbl(db, "hour_50_2015")  %>% 
    filter(date >= from, date <= to)
 
  vkm <- hourly %>%
    group_by(time, siteID) %>% 
    summarise(volume = sum(volume)) %>%
    collect(n = Inf) %>% 
    ungroup() %>% 
    mutate(time =  as.POSIXct(time, origin = "1970-01-01"),
           date = as.Date(time),
           hour = hour(time)) %>% 
    left_join(dayTypes, by = "date") %>% 
    mutate(period = ifelse(dayType %in% c(5:6, 12:14), "Off Peak",
                    ifelse(dayType %in% c(0:4, 7:11) & hour %in% c(0:6, 19:23), "Off Peak",
                    ifelse(dayType %in% c(0:4, 7:11) & hour %in% c(7, 9), "AM Peak Shoulders",
                    ifelse(dayType %in% c(0:4, 7:11) & hour %in% c(16, 18), "PM Peak Shoulders",
                    ifelse(dayType %in% c(0:4, 7:11) & hour %in% c(8), "AM Peak Hour",
                    ifelse(dayType %in% c(0:4, 7:11) & hour %in% c(17), "PM Peak Hour",
                    ifelse(dayType %in% c(0:4, 7:11) & hour %in% c(10:16), "Inter Peak", NA)))))))) 
  
  vkm <- vkm %>% 
    left_join(lengths, by = "siteID", copy = TRUE) %>%
    ungroup() %>% 
    mutate(vkm = volume * lengthKm,
           mvkm = vkm / 1e06,
           siteID = as.factor(siteID)) %>% 
    select(-FID, -date)
  
  return(vkm)
}