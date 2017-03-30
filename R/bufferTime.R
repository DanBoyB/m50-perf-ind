library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(lubridate)

path <- "/var/datastore/"
db <- src_sqlite(paste(path, "/SQLite/M50.sqlite", sep = ""), create = FALSE)

laneNum <- tbl(db, "laneNum") %>% 
    mutate(siteID = ifelse(siteID %in% c(1504,1509), 15041509,
                           ifelse(siteID %in% c(1506,1507), 15061507, siteID)))

dayTypes <- tbl(db, "dayTypes1219") %>% 
    filter(date >= 1420070400, date <= 1451606399) %>% 
    mutate(date = sql(datetime(date, 'unixepoch'))) %>% 
    select(-day)

links <- read_csv("data/links.csv") %>% 
    rename(sectionName = Section)

times <- format(seq.POSIXt(ISOdatetime(2015, 1, 1, 0, 0, 0),
                           ISOdatetime(2015, 1, 1, 23, 55, 0), 
                           by = "5 min"), 
                "%H:%M") %>% 
    data_frame(timeSeq = seq_along(.), time = .)

dirLevels <- c("N11 - CHE", "CHE - CAR", "CAR - BAL", 
               "BAL - FIR", "FIR - N81", "N81 - N7", "N7 - N4",
               "N4 - N3", "N3 - N2", "N2 - BMN") %>% 
    append(c("M1 - BMN", "N2 - N3", "N3 - N4", "N4 - N7", 
             "N7 - N81", "N81 - FIR", "FIR - BAL", 
             "BAL - CAR", "CAR - CHE", "CHE - N11"))

traffic <- tbl(db, "min5_50_2015") %>% 
    rename(dateTime = time) %>% 
    filter(siteID != 1012) %>% 
    mutate(time = sql(strftime('%H:%M', datetime(dateTime, 'unixepoch')))) %>% 
    mutate(siteID = ifelse(siteID %in% c(1504,1509), 15041509,
                           ifelse(siteID %in% c(1506,1507), 15061507, siteID))) %>% 
    left_join(times, by = "time", copy = TRUE) %>% 
    left_join(laneNum, by = "Code", copy = TRUE) %>% 
    rename(siteID = siteID.x) %>% 
    left_join(links, by = "siteID", copy = TRUE) %>% 
    collect(n = Inf) %>% 
    mutate(dayType = as.integer(dayType),
           ref = paste(linkID, dayType, timeSeq, sep = "_"))

traffic <- traffic %>% 
    compute() %>% 
    group_by(linkID, ref) %>% 
    summarise(traffic = sum(volume)) 
    

jTNB <- tbl(db, "journeyTimes_2015_NB") %>%
    select(linkID, time, journeyTime, dayType) %>% 
    rename(dateTime = time) %>% 
    collect(n = Inf) %>% 
    mutate(time = format(as.POSIXct(dateTime, origin = "1970-01-01"), "%H:%M")) %>% 
    left_join(times, by = "time") %>% 
    left_join(links, by = "linkID") 

jTSB <- tbl(db, "journeyTimes_2015_SB") %>%
    select(linkID, time, journeyTime, dayType) %>% 
    rename(dateTime = time) %>% 
    collect(n = Inf) %>% 
    mutate(time = format(as.POSIXct(dateTime, origin = "1970-01-01"), "%H:%M")) %>% 
    left_join(times, by = "time") %>% 
    left_join(links, by = "linkID") 

jTime <- jTNB %>% 
    bind_rows(jTSB) %>% 
    filter(journeyTime > 0) %>% 
    mutate(ref = paste(linkID, dayType, timeSeq, sep = "_"),
           hour = hour(as.POSIXct(dateTime, origin = "1970-01-01"))) %>% 
    mutate(speed = lengthKm / (journeyTime / 60))

jTimeClean <- jTime %>% 
    filter(speed <= 150)

jTimeAgg <- jTimeClean %>%
    group_by(linkID, ref, direction, sectionName, dayType, hour) %>% 
    summarise(medianJt = median(journeyTime / lengthKm),
              meanJt = mean(journeyTime / lengthKm),
              stdevJt = ifelse(is.na(sd(journeyTime / lengthKm)), 0, sd(journeyTime / lengthKm)),
              planTime = quantile(journeyTime / lengthKm, 0.95),
              longTime = quantile(journeyTime / lengthKm, 0.975),
              lengthKm = first(lengthKm)) %>% 
    ungroup() %>% 
    mutate(jt100kph = ((lengthKm / 100) * 60) / lengthKm,
           percVar = (stdevJt / medianJt) * 100,
           buffTime = planTime - medianJt,
           buffTimeIndex = (buffTime / medianJt) * 100,
           planTimeIndex = (planTime / medianJt) * 100,
           miseryIndex = ((longTime) / medianJt) * 100)

jtC2join <- jTimeAgg %>% 
    left_join(traffic, by = "ref") %>% 
    mutate(vkt = traffic * lengthKm) %>% 
    mutate(period = ifelse(dayType %in% c(5:6, 12:14), "Off Peak",
                           ifelse(dayType %in% c(0:4, 7:11) & hour %in% c(0:6, 19:23), "Off Peak",
                           ifelse(dayType %in% c(0:4, 7:11) & hour %in% c(7, 9), "AM Peak Shoulders",
                           ifelse(dayType %in% c(0:4, 7:11) & hour %in% c(16, 18), "PM Peak Shoulders",
                           ifelse(dayType %in% c(0:4, 7:11) & hour %in% c(8), "AM Peak Hour",
                           ifelse(dayType %in% c(0:4, 7:11) & hour %in% c(17), "PM Peak Hour",
                           ifelse(dayType %in% c(0:4, 7:11) & hour %in% c(10:16), "Inter Peak", NA))))))))

saveRDS(jtC2join, "output/bufferMisery/bufferMisery.rds")

check <- jtC2join %>% 
    summarise(percVar = weighted.mean(percVar, vkt),
              planTimeIndex = weighted.mean(planTimeIndex, vkt),
              buffTimeIndex = weighted.mean(buffTimeIndex, vkt),
              miseryIndex = weighted.mean(miseryIndex, vkt))