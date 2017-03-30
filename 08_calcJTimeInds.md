Calculating Journey Time Indicators
================
Dan Brennan
30 March 2017

This note outlines the process to calculate the journey time indicators the JTMS data stored in the M50 SQLite database. The indicators selected to monitor journey times on the M50 are the buffer index and the misery index. These indices correspond to the difference between the 95th and 97.5th respective percentile travel times and average travel times on each section of the M50.

This metric is based on the 5 minute JTMS system journey times, the C2 hourly 5 minute aggregated traffic data and the day types defined.

A connection is made to the M50 SQLite databse and the required tables are loaded, i.e. lane numbers, day types and M50 JTMS links (i.e. the links between the permanent ANPR sites on the M50, as defined by the JTMS system)

``` r
path <- "/var/datastore/"
db <- src_sqlite(paste(path, "/SQLite/M50.sqlite", sep = ""), create = FALSE)
```

``` r
laneNum <- tbl(db, "laneNum") %>% 
    mutate(siteID = ifelse(siteID %in% c(1504,1509), 15041509,
                           ifelse(siteID %in% c(1506,1507), 15061507, siteID)))

dayTypes <- tbl(db, "dayTypes1219") %>% 
    filter(date >= 1420070400, date <= 1451606399) %>% 
    mutate(date = sql(datetime(date, 'unixepoch'))) %>% 
    select(-day)

links <- read_csv("data/links.csv") %>% 
    rename(sectionName = Section)
```

    ## Parsed with column specification:
    ## cols(
    ##   Section = col_character(),
    ##   Sec = col_character(),
    ##   siteID = col_integer(),
    ##   linkID = col_integer(),
    ##   lengthKm = col_double(),
    ##   tolerance = col_double(),
    ##   direction = col_character()
    ## )

A dataframe is created for a sequence of times across a day, by 5 minute intervals. This is used to...

``` r
times <- format(seq.POSIXt(ISOdatetime(2015, 1, 1, 0, 0, 0),
                           ISOdatetime(2015, 1, 1, 23, 55, 0), 
                           by = "5 min"), 
                "%H:%M") %>% 
    data_frame(timeSeq = seq_along(.), time = .)
```

The traffic data aggregated by 5 minute intervals is retrieved from the M50 SQLite database. TMU sites are located between each junction on the M50. ANPR sites are generally located between each junction also, with the exception of two sections - N7 to N81 (TMU 1504 & 1509) and Firhouse to Ballinteer (TMU 1506 & TMU 1507). As the journey time indices require weighted average traffic volumes as an input, the TMU sites on these sites are combined by concatenating their siteIDs before joining to the link types. The JTMS links csv file has this concatenation performed also.

The time sequence, lane number and JTMS links are joined and a reference column is created (for joining with JTMS data) consisting of the linkID, day type and time sequence.

``` r
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
```

The traffic data is then grouped by the link ID and refence column to group

``` r
traffic <- traffic %>% 
    group_by(linkID, ref) %>% 
    summarise(traffic = sum(volume))

traffic %>% 
    head(5) %>% 
    kable()
```

|  linkID| ref        |  traffic|
|-------:|:-----------|--------:|
|      17| 17\_0\_1   |     4187|
|      17| 17\_0\_10  |     2873|
|      17| 17\_0\_100 |    40127|
|      17| 17\_0\_101 |    40746|
|      17| 17\_0\_102 |    39613|

Journey Time data for each direction is retrived from the database and joined with the time sequence and JTMS link tables. Northbound & southbound tables are combined, references are created to match the traffic data references. Corresponding speeds are calculated and the data is cleaned by removing zero length journey times and excluding speeds greater than 150 kph.

``` r
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
    mutate(ref = paste(linkID, dayType, timeSeq, sep = "_"),
           hour = hour(as.POSIXct(dateTime, origin = "1970-01-01"))) %>% 
    mutate(speed = lengthKm / (journeyTime / 60))

jTimeClean <- jTime %>% 
    filter(journeyTime > 0,
           speed <= 150)
```

The cleaned journey time data is then aggregated by section, direction, day type and hour of the day and key stats are calculated including averages, quantiles etc. All stats are weighted by length.

``` r
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
```

The traffic data is joined to the journey time data and vehicle kilometres travelled are calculated. The peak hours, peak shoulders, inter peak and off peak periods are defined.

``` r
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
```

As a check, aggregated results for the full M50

``` r
total <- jtC2join %>% 
    summarise(buffTimeIndex = weighted.mean(buffTimeIndex, vkt),
              miseryIndex = weighted.mean(miseryIndex, vkt))

total %>%
    kable()
```

|  buffTimeIndex|  miseryIndex|
|--------------:|------------:|
|       33.88534|     149.2404|

``` r
period <- jtC2join %>% 
    group_by(period) %>% 
    summarise(buffTimeIndex = weighted.mean(buffTimeIndex, vkt),
              miseryIndex = weighted.mean(miseryIndex, vkt))

period %>%
    kable()
```

| period            |  buffTimeIndex|  miseryIndex|
|:------------------|--------------:|------------:|
| AM Peak Hour      |      95.940127|     235.5482|
| AM Peak Shoulders |      61.623232|     187.7608|
| Inter Peak        |       8.650294|     115.4879|
| Off Peak          |       8.545629|     112.6733|
| PM Peak Hour      |      87.606107|     229.3346|
| PM Peak Shoulders |      76.855101|     207.4263|
