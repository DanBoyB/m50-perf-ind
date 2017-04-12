library(tidyverse)
library(stringr)
library(XML)
library(xml2)

ttDir <- "~/datastore/TravelTimes/"

x <- read_xml(paste(ttDir, "site_locations.xml", sep = ""))

ns <- xml_ns_rename(xml_ns(x), d1 = "d2LogicalModel")

y <- x %>% 
    #xml_find_all("//d2LogicalModel:measurementSiteTable", ns) %>% 
    xml_children() %>% 
    xml_children() %>% 
    xml_children()

y_df <- data_frame(nodeset = y[9:70]) %>% 
    mutate(id = nodeset %>% map(~ xml_attr(., "id")),
           contents = nodeset %>% map(~ xml_contents(.)),
           children = nodeset %>% map(~ xml_children(.)))
           
y_df$contents[[1]]
           
           ,
           text = children %>% map(~ xml_text(.))) %>% 
    filter(!is.na(name), !is.na(site)) %>% 
    select(name, site, text) %>% 
    mutate(name = as.character(name),
           site = as.character(site)) %>% 
    unnest()