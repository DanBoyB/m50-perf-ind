library(tidyverse)
library(lubridate)

vkm <- readRDS("output/vkm/vkm2015.rds") %>% 
    filter(siteID != 1501) %>% 
    group_by(siteID, month, period, direction) %>% 
    summarise(vkm = sum(vkm),
              mvkm = sum(mvkm))

stableFlow <- readRDS("output/stableFlow/stableFlow.rds") %>% 
    filter(!sectionName %in% c("N2 to Ballymun", "M1 to Ballymun")) %>% 
    group_by(siteID, month, period, direction) %>% 
    summarise(stableHours = sum(stableHours),
              monthlyHours = sum(monthlyHours)) %>% 
    mutate(percStable = stableHours / monthlyHours)

bufferMisery <- readRDS("output/bufferMisery/bufferMisery.rds") %>% 
    group_by(siteID, month, period, direction) %>% 
    summarise(buffTimeIndex = weighted.mean(buffTimeIndex, vkt),
              miseryIndex = weighted.mean(miseryIndex, vkt))

combStats <- vkm %>% 
    bind_cols(stableFlow[5:6]) %>% 
    bind_cols(bufferMisery[5:6])

saveRDS(combStats, "output/combStats/combStats.rds")

combStats %>% 
    mutate(percStable = stableHours / monthlyHours) %>% 
    gather(stat, value, 5:11) %>% 
    filter(stat == "percStable", 
           period == "AM Peak Hour"
           ) %>% 
    ggplot(aes(as.factor(month), value, group = siteID, colour = siteID)) +
    geom_line() +
    facet_grid(direction ~ siteID) +
    theme(legend.position = "none")