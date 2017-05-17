library(tidyverse)

stats <- readRDS("output/combStats/combStats.rds")

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
    mutate(month = as.factor(month.abb[month]),
           month = factor(month,
                          levels = c("Jan", "Feb", "Mar", "Apr",
                                     "May", "Jun", "Jul", "Aug",
                                     "Sep", "Oct", "Nov", "Dec")))

vkm_2015 <- readRDS("output/vkm/vkm2015.rds") %>% 
    group_by(month) %>% 
    summarise(mvkm = sum(mvkm)) %>% 
    mutate(month = as.factor(month.abb[month]),
           month = factor(month,
                          levels = c("Jan", "Feb", "Mar", "Apr",
                                     "May", "Jun", "Jul", "Aug",
                                     "Sep", "Oct", "Nov", "Dec")))

vkm_plot <- vkm_2015 %>% 
    ggplot(aes(month, mvkm)) +
    geom_bar(stat = "identity", fill = "dark blue", alpha = 0.7, width = 0.8) +
    theme_minimal() +
    labs(x = "", y = "million vehicle km travelled in 2015") +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 90, hjust = 1))

flow_stability <- stats %>% 
    group_by(period, direction) %>% 
    summarise(stableHours = sum(stableHours),
              monthlyHours = sum(monthlyHours)) %>% 
    mutate(percStable = round((stableHours / monthlyHours) * 100, 2)) 
    

plot_stable_flow <- flow_stability %>% 
    ggplot(aes(reorder(period, -percStable), percStable)) +
    geom_bar(stat = "identity", fill = "dark blue", alpha = 0.7, width = 0.8) +
    coord_cartesian(ylim = c(0,100)) +
    facet_wrap(~ direction) +
    theme_minimal() +
    labs(x = "", y = "% stable flow in 2015") +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 90, hjust = 1))

ggsave("vkm-plot.jpeg", plot = vkm_plot, path = "output/plots/", width = 8, height = 5)
ggsave("stable-flow-plot.jpeg", plot = plot_stable_flow, path = "output/plots/", width = 8, height = 5)



tidy_stats %>% 
    filter(period == "PM Peak Hour",
           stat == "buffTimeIndex", 
           Sec == "N7 - N4") %>% 
    ggplot(aes(as.factor(month), value, group = siteID, fill = siteID)) +
    geom_bar(stat = "identity", fill = "dark blue", alpha = 0.7, width = 0.8) +
    facet_wrap(~ direction) +
    theme_light() +
    labs(x = "", y = "Buffer Time Index (%)") +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 90, hjust = 1))






