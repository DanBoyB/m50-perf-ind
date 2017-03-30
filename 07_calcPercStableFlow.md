Calculating Percentage Stable Flow
================
Dan Brennan
21 March 2017

This note outlines the process to calculate the percentage stable flow indicators from the C2 data stored in the M50 SQLite database. Stable flow on the M50 is defined as an hourly period where average speeds 60 kph or greater and the speed flow relationship is such that there is a level of service of between A and D in that hour.

This metric is based on the C2 hourly aggregated traffic data and the day types defined.

A connection is made to the M50 SQLite databse and the required tables are loaded, i.e. lane numbers, day types and hourly aggregated traffic

``` r
path <- "/var/datastore/"
db <- src_sqlite(paste(path, "/SQLite/M50.sqlite", sep = ""), create = FALSE)
```

``` r
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
```

In order to calculate levels of service, the hourly data requires aggregation into total hourly lane flows over all vehicle classes. When aggregating over all classes, the hourly traffic volumes are summed and a weighted average speed is calculated.

Traffic density is calculated as *v**o**l**u**m**e*/*s**p**e**e**d*. Levels of service are then determined based on US HCM density ranges. Finally a column defining stable flow is added where the LOS is in the range A-D and the weighted average speed is 60 kph or greater.

``` r
los <- hourly2015 %>% 
  group_by(siteID, time, date, Code) %>% 
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
```

    ## Source:   query [?? x 10]
    ## Database: sqlite 3.11.1 [/var/datastore//SQLite/M50.sqlite]
    ## 
    ##    siteID       time  date  Code     speed volume   density   los dayType
    ##     <int>      <dbl> <dbl> <int>     <dbl>  <int>     <dbl> <chr>   <dbl>
    ## 1    1501 1420070400 16436   622  97.15389     26 0.2676167     A      14
    ## 2    1501 1420070400 16436   623  97.75681    148 1.5139611     A      14
    ## 3    1501 1420070400 16436   624 101.13158    152 1.5029925     A      14
    ## 4    1501 1420070400 16436   625 114.56737     37 0.3229541     A      14
    ## 5    1501 1420070400 16436   626 110.66707     27 0.2439750     A      14
    ## 6    1501 1420070400 16436   627 104.68119    138 1.3182884     A      14
    ## 7    1501 1420070400 16436   628  93.07108    169 1.8158165     A      14
    ## 8    1501 1420070400 16436   629 100.99996     46 0.4554457     A      14
    ## 9    1501 1420074000 16436   622  94.13567     59 0.6267550     A      14
    ## 10   1501 1420074000 16436   623  95.71857    206 2.1521426     A      14
    ## # ... with more rows, and 1 more variables: stable <dbl>

``` r
saveRDS(los, "output/los2015.rds")
```

In order to determine the percentage of hours in a year where stable flow exists, a sum of annual hours for each daytype is first calculated. A reference column for each day type, site, lane and hour is created to facilitate a table join with the LOS data.

``` r
monthlyHrs <- los %>% 
  mutate(time = as.POSIXct(time, origin = "1970-01-01")) %>% 
  group_by(dayType, siteID, Code, hour = hour(time), month = month(time)) %>% 
  summarise(monthlyHours = n()) %>% 
  mutate(refMon = paste(dayType, siteID, Code, month, hour, sep = "_")) %>% 
  ungroup() %>% 
  select(refMon, monthlyHours)
```

The LOS data is then aggregated by LOS, day type and hour and the number of hours over the year for each LOS is counted. The dataframe is filtered to include stable flow only and joined with the annualHrs dataframe.

``` r
losMonthly <- los %>% 
  mutate(time = as.POSIXct(time, origin = "1970-01-01")) %>%
  filter(stable == 1) %>% 
  group_by(los, Code, siteID, dayType, hour = hour(time), month = month(time), stable) %>% 
  summarise(stableHours = n()) %>% 
  mutate(refMon = paste(dayType, siteID, Code, month, hour, sep = "_")) %>% 
  left_join(monthlyHrs, by = "refMon")
```

Next the peak hours, peak shoulders, inter peak and off peak periods are defined.

A backup of this table is created.

``` r
stableFlowMonthly <- losMonthly %>% 
  left_join(laneNum, by = "Code") %>%
  mutate(period = ifelse(dayType %in% c(5:6, 12:14), "Off Peak",
                 ifelse(dayType %in% c(0:4, 7:11) & hour %in% c(0:6, 19:23), "Off Peak",
                 ifelse(dayType %in% c(0:4, 7:11) & hour %in% c(7, 9), "AM Peak Shoulders",
                 ifelse(dayType %in% c(0:4, 7:11) & hour %in% c(16, 18), "PM Peak Shoulders",
                 ifelse(dayType %in% c(0:4, 7:11) & hour %in% c(8), "AM Peak Hour",
                 ifelse(dayType %in% c(0:4, 7:11) & hour %in% c(17), "PM Peak Hour",
                 ifelse(dayType %in% c(0:4, 7:11) & hour %in% c(10:16), "Inter Peak", NA))))))))  %>%
  group_by(dayType, sectionName, month, period, direction) %>% 
  summarise(stableHours = sum(stableHours),
            monthlyHours = sum(monthlyHours)) %>% 
  ungroup() %>% 
  mutate(dayType = as.factor(dayType)) %>% 
  filter(!is.na(sectionName))

stableFlowMonthly %>% 
copy_to(db, ., "stableFlow", temporary = FALSE)
```

    ## Source:   query [?? x 7]
    ## Database: sqlite 3.11.1 [/var/datastore//SQLite/M50.sqlite]
    ## 
    ##    dayType                sectionName month            period  direction
    ##      <chr>                      <chr> <dbl>             <chr>      <chr>
    ## 1        0 Ballinteer to Carrickmines     1      AM Peak Hour Southbound
    ## 2        0 Ballinteer to Carrickmines     1 AM Peak Shoulders Southbound
    ## 3        0 Ballinteer to Carrickmines     1        Inter Peak Southbound
    ## 4        0 Ballinteer to Carrickmines     1          Off Peak Southbound
    ## 5        0 Ballinteer to Carrickmines     1      PM Peak Hour Southbound
    ## 6        0 Ballinteer to Carrickmines     1 PM Peak Shoulders Southbound
    ## 7        0 Ballinteer to Carrickmines     2      AM Peak Hour Southbound
    ## 8        0 Ballinteer to Carrickmines     2 AM Peak Shoulders Southbound
    ## 9        0 Ballinteer to Carrickmines     2        Inter Peak Southbound
    ## 10       0 Ballinteer to Carrickmines     2          Off Peak Southbound
    ## # ... with more rows, and 2 more variables: stableHours <int>,
    ## #   monthlyHours <int>

``` r
saveRDS(stableFlowMonthly, "output/stableFlow/stableFlow.rds")
```

This table can output percentage stable flow data for 2015 at various levels of aggregation:

-   For the full M50 in 2015
-   For each direction in 2015
-   For each M50 section
-   Vkm disaggregated by time periods

Sample output summary tables for aggregation by section, by direction and for the full M50 are produced below.

``` r
dirLevels <- readRDS("data/nblevels.rds") %>% 
  append(readRDS("data/sblevels.rds"))

stableFlowMonthly %>%
  mutate(sectionName = factor(sectionName, levels = dirLevels)) %>% 
  filter(direction == "Northbound", period %in% c("AM Peak Hour", "PM Peak Hour")) %>% 
  group_by(sectionName, direction, period) %>% 
  summarise(stableHours = sum(stableHours),
            monthlyHours = sum(monthlyHours)) %>% 
  mutate(percStable = stableHours / monthlyHours) %>% 
  na.omit() %>% 
  knitr::kable(digits = 2)
```

| sectionName                | direction  | period       |  stableHours|  monthlyHours|  percStable|
|:---------------------------|:-----------|:-------------|------------:|-------------:|-----------:|
| N11 to Cherrywood          | Northbound | AM Peak Hour |          476|           692|        0.69|
| N11 to Cherrywood          | Northbound | PM Peak Hour |          494|           621|        0.80|
| Cherrywood to Carrickmines | Northbound | AM Peak Hour |          323|           482|        0.67|
| Cherrywood to Carrickmines | Northbound | PM Peak Hour |          485|           638|        0.76|
| Carrickmines to Ballinteer | Northbound | AM Peak Hour |          291|           431|        0.68|
| Carrickmines to Ballinteer | Northbound | PM Peak Hour |          463|           679|        0.68|
| Ballinteer to Firhouse     | Northbound | AM Peak Hour |          563|           815|        0.69|
| Ballinteer to Firhouse     | Northbound | PM Peak Hour |          207|           447|        0.46|
| Firhouse to N81            | Northbound | AM Peak Hour |          683|           999|        0.68|
| Firhouse to N81            | Northbound | PM Peak Hour |          790|          1256|        0.63|
| N81 to N7                  | Northbound | AM Peak Hour |          717|          1005|        0.71|
| N81 to N7                  | Northbound | PM Peak Hour |          556|          1053|        0.53|
| N7 to N4                   | Northbound | AM Peak Hour |          940|          1204|        0.78|
| N7 to N4                   | Northbound | PM Peak Hour |          405|           809|        0.50|
| N4 to N3                   | Northbound | AM Peak Hour |          950|          1415|        0.67|
| N4 to N3                   | Northbound | PM Peak Hour |          959|          1321|        0.73|
| N3 to N2                   | Northbound | AM Peak Hour |          909|          1333|        0.68|
| N3 to N2                   | Northbound | PM Peak Hour |          957|          1171|        0.82|

``` r
stableFlowMonthly %>% 
  group_by(direction, period) %>% 
  summarise(stableHours = sum(stableHours),
            monthlyHours = sum(monthlyHours)) %>% 
  mutate(percStable = stableHours / monthlyHours) %>% 
  na.omit() %>% 
  knitr::kable(digits = 2)
```

| direction  | period            |  stableHours|  monthlyHours|  percStable|
|:-----------|:------------------|------------:|-------------:|-----------:|
| Northbound | AM Peak Hour      |         8586|         12227|        0.70|
| Northbound | AM Peak Shoulders |        18639|         24884|        0.75|
| Northbound | Inter Peak        |        60618|         76470|        0.79|
| Northbound | Off Peak          |       234965|        265418|        0.89|
| Northbound | PM Peak Hour      |         7422|         11115|        0.67|
| Northbound | PM Peak Shoulders |        16473|         24498|        0.67|
| Southbound | AM Peak Hour      |         6029|          9634|        0.63|
| Southbound | AM Peak Shoulders |        15755|         22645|        0.70|
| Southbound | Inter Peak        |        59091|         75945|        0.78|
| Southbound | Off Peak          |       228760|        258308|        0.89|
| Southbound | PM Peak Hour      |         8807|         12367|        0.71|
| Southbound | PM Peak Shoulders |        18074|         25437|        0.71|

``` r
stableFlowMonthly %>% 
  group_by(period) %>% 
  summarise(stableHours = sum(stableHours),
            monthlyHours = sum(monthlyHours)) %>% 
  mutate(percStable = stableHours / monthlyHours) %>% 
  na.omit() %>% 
  knitr::kable(digits = 2)
```

| period            |  stableHours|  monthlyHours|  percStable|
|:------------------|------------:|-------------:|-----------:|
| AM Peak Hour      |        14615|         21861|        0.67|
| AM Peak Shoulders |        34394|         47529|        0.72|
| Inter Peak        |       119709|        152415|        0.79|
| Off Peak          |       463725|        523726|        0.89|
| PM Peak Hour      |        16229|         23482|        0.69|
| PM Peak Shoulders |        34547|         49935|        0.69|
