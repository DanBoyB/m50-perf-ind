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
    mutate(hour = (datetime),
           date = date(datetime),
           day = wday(datetime, label = TRUE),
           time = format(as.POSIXct(strptime(datetime, "%Y-%m-%d  %H:%M:%S",tz="")) ,
                         format = "%H:%M"),
           time = as.POSIXct(time, format = "%H:%M")) %>%
    filter(site_ref == "ROI_ANPR_M50S.32-N11S.1270") %>% 
    ggplot(aes(x = time)) +
    geom_point(aes(y = trav_time / 60, group = date, colour = day), alpha = 0.1) + 
    facet_wrap(~ day, ncol = 1) +
    #geom_line(aes(y = ff_trav_time / 60), colour = "red", size = 2) +
    coord_cartesian(ylim = c(0, 150)) +
    scale_x_datetime(labels = date_format("%H:%M"), breaks = date_breaks("2 hour")) +
    theme_minimal() +
    theme(legend.position = "none") +
    ggtitle("M50 Southbound Travel Times",
            subtitle = "Data taken from TII opendata xml files (Aug '16 - Jan '17)") +
    xlab("") +
    ylab("Travel Time (mins)")