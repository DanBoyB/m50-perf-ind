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
    mutate(month = month(as.POSIXct(dateTime, origin = "1970-01-01")),
           dayType = as.integer(dayType),
           ref = paste(linkID, dayType, timeSeq, sep = "_"))

janTraffic <- traffic %>% filter(month == 1)

traffic <- janTraffic %>% 
    group_by(linkID, ref) %>% 
    summarise(traffic = sum(volume))

janJT <- jTimeClean %>% 
    filter(month == 1)

jTimeAgg <- janJT %>%
    group_by(linkID, dayType, timeSeq, ref, direction, sectionName, hour) %>% 
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
    # na.omit() %>% 
    mutate(vkt = traffic * lengthKm) %>% 
   # rename(dayType = dayType.x) %>% 
    mutate(period = ifelse(dayType %in% c(5:6, 12:14), "Off Peak",
                    ifelse(dayType %in% c(0:4, 7:11) & hour %in% c(0:6, 19:23), "Off Peak", 
                    ifelse(dayType %in% c(0:4, 7:11) & hour %in% c(7, 9), "AM Peak Shoulders",
                    ifelse(dayType %in% c(0:4, 7:11) & hour %in% c(16, 18), "PM Peak Shoulders",
                    ifelse(dayType %in% c(0:4, 7:11) & hour %in% c(8), "AM Peak Hour",
                    ifelse(dayType %in% c(0:4, 7:11) & hour %in% c(17), "PM Peak Hour",
                    ifelse(dayType %in% c(0:4, 7:11) & hour %in% c(10:16), "Inter Peak", NA))))))))

total <- jtC2join %>% 
    summarise(buffTimeIndex = weighted.mean(buffTimeIndex, vkt),
              miseryIndex = weighted.mean(miseryIndex, vkt))

period <- jtC2join %>% 
    group_by(period) %>% 
    summarise(buffTimeIndex = weighted.mean(buffTimeIndex, vkt),
              miseryIndex = weighted.mean(miseryIndex, vkt))