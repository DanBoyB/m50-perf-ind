Load Supplementary Tables
================
Dan Brennan
13 March 2017

### M50 Section Names

A table with a description of each M50 section, in both directions, for each TMU site is loaded into the M50 SQLite database.

``` r
read_csv("data/sectionNames.csv") %>% 
  rename(siteID = TMU) %>% 
  copy_to(db, ., "sectionNames", temporary = FALSE)
```

### M50 Section Lengths

The length of each section of the M50 is measured as the distance between the centre point of each junction. Each of these sections have been measured and assigned to the appropriate TMU in a simple csv table. This is then loaded into the M50 SQLite database.

``` r
read_csv("data/m50TmuLengths.csv") %>% 
  rename(siteID = TMU) %>% 
  copy_to(db, ., "tmuLengths", temporary = FALSE)
```

### M50 Lane Numbers

The C2 PVR records define lane numbers via the "Code" column. Each value is a unique lane number for each TMU site. A reference table for the M50 lane numbers has been creates and is loaded into the M50 SQLite database. This also includes a reference to to JTMS link number.

``` r
read_csv("data/m50laneNum.csv") %>% 
  copy_to(db, ., "laneNum", temporary = FALSE)
```
