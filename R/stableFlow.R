library(tidyverse)
library(lubridate)

path <- "C:/C2 PVR/"
db <- src_sqlite(paste(path, "/SQLite/M50.sqlite", sep = ""), create = FALSE)

dirLevels <- readRDS("data/nblevels.rds") %>% 
  append(readRDS("data/sblevels.rds"))

laneNum <- tbl(db, "laneNum") %>% 
  select(Code, direction, lane, sectionName) %>% 
  collect(n = Inf) 

dayTypes <- tbl(db, "dayTypes1219") %>% 
  filter(date >= 1420070400, date <= 1451606399) %>% 
  collect(n = Inf) %>% 
  mutate(date = as.Date(as.POSIXct(date, origin = "1970-01-01"))) %>% 
  select(-day)

hourly2015 <- tbl(db, "hour_50_2015") %>% 
  collect(n = Inf) %>% 
  mutate(date = as.Date(as.POSIXct(time, origin = "1970-01-01")))

los <- hourly2015 %>% 
  left_join(laneNum, by = "Code") %>% 
  group_by(siteID, sectionName, time, date, Code, lane, direction) %>% 
  summarise(speed = weighted.mean(speed, volume),
            volume = sum(volume)) %>%
  mutate(density = volume / speed,
         los = ifelse(density <= 7, "A",
               ifelse(density > 7 & density <= 11, "B",
               ifelse(density > 11 & density <= 16, "C",
               ifelse(density > 16 & density <= 22, "D",
               ifelse(density > 22 & density <= 28, "E", "F")))))) %>% 
  left_join(dayTypes, by = "date") %>% 
  ungroup() %>% 
  mutate(stable = ifelse(los %in% c("A", "B", "C", "D") & speed >= 60, 1, 0))

los %>% 
copy_to(db, ., "los2015", temporary = FALSE)

totHrs <- los %>% 
  mutate(time = as.POSIXct(time, origin = "1970-01-01")) %>% 
  group_by(dayType, siteID, direction, hour = hour(time)) %>% 
  summarise(annualHours = n()) %>% 
  mutate(ref = paste(dayType, siteID, direction, hour, sep = "_")) %>% 
  ungroup() %>% 
  select(ref, annualHours)

losPerf <- los %>% 
  mutate(time = as.POSIXct(time, origin = "1970-01-01")) %>% 
  group_by(los, sectionName, siteID, direction, dayType, hour = hour(time), stable) %>% 
  summarise(hours = n()) %>% 
  filter(stable == 1) %>% 
  mutate(ref = paste(dayType, siteID, direction, hour, sep = "_")) %>% 
  left_join(totHrs, by = "ref")

losPerfSum <- losPerf %>% 
  group_by(dayType, sectionName, direction, hour) %>% 
  summarise(hours = sum(hours),
            annualHours = first(annualHours)) %>% 
  ungroup() %>% 
  mutate(percLosAD = hours / annualHours,
         dayType = as.factor(dayType),
         hour = as.factor(hour))

losPerfSumPeak <- losPerf %>% 
  filter(dayType %in% c(0:4, 7, 9, 11)) %>% 
  mutate(stablePeak = ifelse(stable == 1 & hour %in% c(8, 17), hours, 0L),
         stablePeakSh = ifelse(stable == 1 & hour %in% c(7, 10, 16, 18), hours, 0L),
         stableIntPeak = ifelse(stable == 1 & hour %in% c(10:16), hours, 0L)) %>%
  group_by(dayType, sectionName, direction, hour) %>% 
  summarise(stablePeak = sum(stablePeak),
            stablePeakSh = sum(stablePeakSh),
            stableIntPeak = sum(stableIntPeak),
            annualHours = first(annualHours)) %>% 
  ungroup() %>% 
  mutate(percStablePeak = stablePeak / annualHours,
         percStablePeaksh = stablePeakSh / annualHours,
         percStableIntPeak = stableIntPeak / annualHours,
         dayType = as.factor(dayType),
         hour = as.factor(hour))

losPerfSumPeak %>% 
  group_by(direction) %>% 
  summarise(percStablePeak = mean(percStablePeak[percStablePeak > 0]),
            percStablePeaksh = mean(percStablePeaksh[percStablePeaksh > 0]),
            percStableIntPeak = mean(percStableIntPeak[percStableIntPeak > 0]))

losPerfSumPeak %>% 
  summarise(percStablePeak = mean(percStablePeak[percStablePeak > 0]),
            percStablePeaksh = mean(percStablePeaksh[percStablePeaksh > 0]),
            percStableIntPeak = mean(percStableIntPeak[percStableIntPeak > 0]))