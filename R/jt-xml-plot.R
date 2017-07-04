library(tidyverse)
library(lubridate)
library(scales)
library(plotly)

a <- read_csv("data/jt/all.csv", col_names = c("datetime", 
                                                 "site_ref",
                                                 "period_secs",
                                                 "ff_speed",
                                                 "ff_trav_time",
                                                 "trav_time")) %>% 
    arrange(datetime)

a %>% 
    mutate(hour = hour(datetime),
           date = date(datetime),
           time = format(datetime, "%H:%M")) %>%
    filter(site_ref == "ROI_ANPR_M50S.32-N11S.1270") %>% 
    ggplot(aes(x = time)) +
    geom_line(aes(y = trav_time / 60, group = date), alpha = 0.3) +           
    #geom_line(aes(y = ff_trav_time / 60), colour = "red", size = 2) +
    coord_cartesian(ylim = c(0, 150)) +
    scale_x_discrete() +
    theme_minimal()
    
a %>% 
    mutate(time = format(datetime, "%H:%M"),
           na_flag = ifelse(is.na(trav_time), hour(datetime), NA)) %>% 
    filter(site_ref == "ROI_ANPR_M50S.32-N11S.1270") %>% 
    ggplot(aes(na_flag)) + 
    geom_histogram()

ggplotly()