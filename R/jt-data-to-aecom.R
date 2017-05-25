library(tidyverse)

# Scipt to read all journey time data and output as csvs to send to AECOM

db <- src_sqlite("/var/datastore/SQLite/M50.sqlite", create = FALSE)

jt_to_csv <- function(year, dir) {
    tbl(db, paste("journeyTimes_", year, "_", dir, sep = "")) %>% 
        select(linkID, time, dayType, journeyTime) %>% 
        collect(n = Inf) %>% 
        write_csv(paste("output/to-aecom/", "journeyTimes_", year, "_", dir, ".csv", sep = ""))
}

lapply(c(2013:2016), jt_to_csv, "NB")
lapply(c(2013:2016), jt_to_csv, "SB")
