library(tidyverse)
library(stringr)
library(xml2)

ttDir <- "~/datastore/TravelTimes/"

all_files <- list.files(pattern = "Content", path = ttDir, full.names = TRUE)

sites_xml <- read_xml(paste(ttDir, "site_locations.xml", sep = "")) %>% 
    xml_ns_strip()

sites <- sites_xml %>% 
    xml_find_all("//measurementSiteRecord")

# Parse sites xml to dataframe
sites_df <- data_frame(nodeset = sites) %>% 
    mutate(site_ref = nodeset %>% map(~ xml_attrs(.)),
           site_desc = nodeset %>% 
               xml_find_all("//measurementSiteName") %>% 
               xml_text(.),
           values = nodeset %>% 
               xml_find_all("//measurementSiteLocation"),
           from = values %>% 
               xml_find_all(".//from"),
           to = values %>% 
               xml_find_all(".//to"),
           from_lat = from %>% 
               xml_find_all(".//latitude") %>% 
               xml_double(.),
           from_long = from %>% 
               xml_find_all(".//longitude") %>% 
               xml_double(.),
           to_lat = to %>% 
               xml_find_all(".//latitude") %>% 
               xml_double(.),
           to_long = to %>% 
               xml_find_all(".//longitude") %>% 
               xml_double(.)) %>% 
    select(-nodeset, -values, -from, -to) %>% 
    unnest() %>% 
    select(site_ref, site_desc, from_lat, from_long, to_lat, to_long)

write_csv(sites_df, "data/jt/sites.csv")
