library(tidyverse)
library(lubridate)

jt_sum <- function(t, sec) {
    
    a <- data_frame()
    jt <- tbl(db, as.character(t)) %>% 
        filter(linkID %in% sec,
               dayType %in% c(1:3)) %>% 
        collect() %>% 
        mutate(time = as.POSIXct(time, origin = "1970-01-01"),
               hour = hour(time)) %>% 
        filter(hour %in% c(8,17),
               date(time) <= "2016-05-31") %>% 
        group_by(time) %>% 
        summarise(journeyTime = sum(journeyTime)) %>% 
        summarise(mean_jt = mean(journeyTime),
                  median_jt = median(journeyTime),
                  perc95_jt = quantile(journeyTime, 0.95))
    
    a <- a %>% 
        bind_rows(jt)
}

tables_nb <- c("journeyTimes_2013_NB", "journeyTimes_2014_NB", 
               "journeyTimes_2015_NB", "journeyTimes_2016_NB")

tables_sb <- c("journeyTimes_2013_SB", "journeyTimes_2014_SB", 
               "journeyTimes_2015_SB", "journeyTimes_2016_SB")

m11_stepaside_nb <- c(70, 79, 94)
stepaside_n7_nb <- c(97, 52, 100)
n7_n3_nb <- c(19, 21)

n3_n7_sb <- c(86, 17)
n7_step_sb <- c(63, 58, 57)
stepaside_m11_sb <- c(91, 69, 53)


m11_step_nb_sum <- map_df(tables_nb, jt_sum, m11_stepaside_nb)
step_n7_nb_sum <- map_df(tables_nb, jt_sum, stepaside_n7_nb)
n7_n3_nb_sum <- map_df(tables_nb, jt_sum, n7_n3_nb)

n3_n7_sb_sum <- map_df(tables_sb, jt_sum, n3_n7_sb)
n7_step_sb_sum <- map_df(tables_sb, jt_sum, n7_step_sb)
step_m11_sb_nb_sum <- map_df(tables_sb, jt_sum, stepaside_m11_sb)

