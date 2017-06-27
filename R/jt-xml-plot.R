library(tidyverse)

a <- read_csv("data/jt/2016-08-30.csv", col_names = c("datetime", 
                                                 "site_ref",
                                                 "period_secs",
                                                 "ff_speed",
                                                 "ff_trav_time",
                                                 "trav_time")) %>% 
    arrange(datetime)

a %>% 
    filter(site_ref == "ROI_ANPR_M50S.32-N11S.1270") %>% 
    mutate(trav_time1 = ifelse(is.na(trav_time), ff_trav_time / 60, trav_time / 60)) %>% 
    ggplot(aes(datetime, trav_time1)) +
    geom_line() +
    coord_cartesian(ylim = c(0, 150))
