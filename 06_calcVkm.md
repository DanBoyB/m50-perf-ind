Calculating Vkm Indicators
================
Dan Brennan
16 March 2017

This note outlines the process to calculate the vkm indicators from the C2 data stored in the M50 SQLite database

Connect to databse

``` r
path <- "/var/datastore/"
db <- src_sqlite(paste(path, "/SQLite/M50.sqlite", sep = ""), create = FALSE)
```

Load required tables from database

``` r
dayTypes <- tbl(db, "dayTypes1219") %>% 
  filter(date >= 1420070400, date <= 1451606399) %>% 
  collect(n = Inf) %>% 
  mutate(date = as.Date(as.POSIXct(date, origin = "1970-01-01")))

lengths <- tbl(db, "tmuLengths") %>% 
  collect()

links <- read_csv("data/links.csv")
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

``` r
hourly2015 <- tbl(db, "hour_50_2015") %>% 
    filter(siteID != 1012) %>% 
    mutate(siteID = ifelse(siteID %in% c(1504,1509), 15041509,
                    ifelse(siteID %in% c(1506,1507), 15061507, siteID)))
```

Join day type and length data to C2 hourly data, define time periods and calculate vehicle kilometres travelled. Save a backup rds file.

``` r
vkm2015 <- hourly2015 %>%
  group_by(time, siteID) %>% 
  summarise(volume = sum(volume)) %>%
  collect(n = Inf) %>% 
  ungroup() %>% 
  mutate(time =  as.POSIXct(time, origin = "1970-01-01"),
         date = as.Date(time),
         hour = hour(time),
         month = month(time)) %>% 
  left_join(dayTypes, by = "date") %>% 
  mutate(period = ifelse(dayType %in% c(5:6, 12:14), "Off Peak",
                 ifelse(dayType %in% c(0:4, 7:11) & hour %in% c(0:6, 19:23), "Off Peak",
                 ifelse(dayType %in% c(0:4, 7:11) & hour %in% c(7, 9), "AM Peak Shoulders",
                 ifelse(dayType %in% c(0:4, 7:11) & hour %in% c(16, 18), "PM Peak Shoulders",
                 ifelse(dayType %in% c(0:4, 7:11) & hour %in% c(8), "AM Peak Hour",
                 ifelse(dayType %in% c(0:4, 7:11) & hour %in% c(17), "PM Peak Hour",
                 ifelse(dayType %in% c(0:4, 7:11) & hour %in% c(10:16), "Inter Peak", NA)))))))) 

vkm2015 <- vkm2015 %>% 
  left_join(links, by = "siteID", copy = TRUE) %>%
  ungroup() %>% 
  mutate(vkm = volume * lengthKm,
         mvkm = vkm / 1e06,
         siteID = as.factor(siteID)) %>% 
  rename(sectionName = Section) %>% 
  select(siteID, sectionName, Sec, dayType, month, period, direction, vkm, mvkm)

saveRDS(vkm2015, "output/vkm/vkm2015.rds")
```

This table can output vkm data for 2015 at various levels of aggregation:

-   Total annual vkm on M50 in 2015
-   Annual vkm for each M50 section in 2015
-   Vkm disaggregated by month
-   Vkm disaggregated by time periods

The above process is defined in a function called [calcVkm.R](/R/calcVkm.R)
