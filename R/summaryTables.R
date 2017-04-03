library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(lubridate)

vkm <- readRDS("output/vkm/vkm2015.rds") %>% 
    group_by(sectionName, month, period, direction) %>% 
    summarise(vkm = sum(vkm),
              mvkm = sum(mvkm))

stableFlow <- readRDS("output/stableFlow/stableFlow.rds") %>% 
    group_by(sectionName, month, period, direction) %>% 
    summarise(stableHours = sum(stableHours),
              monthlyHours = sum(monthlyHours)) %>% 
    mutate(percStable = stableHours / monthlyHours)

bufferMisery <- readRDS("output/bufferMisery/bufferMisery1.rds") %>% 
    group_by(sectionName, month, period, direction) %>% 
    summarise(buffTimeIndex = weighted.mean(buffTimeIndex, vkt),
              miseryIndex = weighted.mean(miseryIndex, vkt))


combStats <- vkm %>% 
    bind_cols(stableFlow[7]) %>% 
    bind_cols(bufferMisery[5:6])