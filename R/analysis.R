library(tidyverse)

# Load data
stats <- readRDS("output/combStats/combStats.rds") %>% 
    mutate(date = as.Date(strptime(paste(2015, month, 01, sep = "-"), 
                                   format = "%Y-%m-%d")))

links <- read_csv("data/links.csv") %>% 
    slice(2:11) %>% 
    select(siteID, Sec) %>% 
    mutate(siteID = as.factor(siteID))

lev <- c("N11 - CHE", "CHE - CAR", "CAR - BAL", "BAL - FIR", "FIR - N81", "N81 - N7", "N7 - N4",
         "N4 - N3", "N3 - N2")

tidy_stats <- stats %>% 
    mutate(percStable = stableHours / monthlyHours) %>% 
    gather(stat, value, 5:11) %>% 
    left_join(links, by = "siteID") %>% 
    mutate(Sec = factor(Sec, levels = lev)) %>% 
    ungroup() %>% 
    mutate(date = as.Date(strptime(paste(2015, month, 01, sep = "-"), 
                                   format = "%Y-%m-%d")))

# Plot vkm
vkm_2015 <- readRDS("output/vkm/vkm2015.rds") %>% 
    group_by(month) %>% 
    summarise(mvkm = sum(mvkm)) %>% 
    mutate(date = as.Date(strptime(paste(2015, month, 01, sep = "-"), 
                                   format = "%Y-%m-%d")))

vkm_plot <- vkm_2015 %>% 
    ggplot(aes(date, mvkm)) +
    geom_line(colour = "dark blue", alpha = 0.7) +
    theme_light() +
    scale_x_date(date_breaks = "months",
                 date_labels = "%b") +
    labs(x = "", y = "million vehicle km travelled in 2015") +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 90, hjust = 1))

# Plot Stable Flow
flow_stability <- stats %>% 
    group_by(period, direction) %>% 
    summarise(stableHours = sum(stableHours),
              monthlyHours = sum(monthlyHours)) %>% 
    mutate(percStable = round((stableHours / monthlyHours) * 100, 2)) 

stable_flow_plot <- flow_stability %>% 
    ggplot(aes(reorder(period, -percStable), percStable)) +
    geom_bar(stat = "identity", fill = "dark cyan", alpha = 0.7, width = 0.8) +
    coord_cartesian(ylim = c(0,100)) +
    facet_wrap(~ direction) +
    theme_light() +
    labs(x = "", y = "% stable flow in 2015") +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 90, hjust = 1))

# Plot buffer time index
buffer_plot <- tidy_stats %>% 
    filter(Sec == "N7 - N4", 
           period %in% c("AM Peak Hour", "PM Peak Hour"), 
           direction == "Southbound", 
           stat == "buffTimeIndex") %>% 
    ggplot(aes(x = date, group = 1)) +
    geom_point(aes(y = value), colour = "dark green", alpha = 0.7) +
    geom_segment(aes(xend = date, yend = 0, y = value), colour = "dark green", alpha = 0.7) +
    scale_x_date(date_breaks = "months",
                 date_labels = "%b") +
    theme_light() +
    facet_wrap( ~ period) +
    labs(x = "", y = "Buffer Time Index (%)") +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 90, hjust = 1))

# Plot misery index
misery_plot <- tidy_stats %>% 
    filter(Sec == "N7 - N4", 
           period %in% c("AM Peak Hour", "PM Peak Hour"), 
           direction == "Southbound", 
           stat == "miseryIndex") %>% 
    ggplot(aes(x = date, group = 1)) +
    geom_point(aes(y = value), colour = "dark red", alpha = 0.7) +
    geom_segment(aes(xend = date, yend = 0, y = value), colour = "dark red", alpha = 0.7) +
    scale_x_date(date_breaks = "months",
                 date_labels = "%b") +
    theme_light() +
    facet_wrap( ~ period) +
    labs(x = "", y = "Misery Index (%)") +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 90, hjust = 1))

# Save plots
ggsave("vkm-plot.jpeg", plot = vkm_plot, path = "output/plots/", width = 8, height = 5)
ggsave("stable-flow-plot.jpeg", plot = stable_flow_plot, path = "output/plots/", width = 8, height = 5)
ggsave("buffer-plot.jpeg", plot = buffer_plot, path = "output/plots/", width = 8, height = 5)
ggsave("misery-plot.jpeg", plot = misery_plot, path = "output/plots/", width = 8, height = 5)