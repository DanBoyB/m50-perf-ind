library(tidyverse)
library(stringr)
#library(XML)
library(xml2)

ttDir <- "~/datastore/TravelTimes/"

times_xml <- read_xml(paste(ttDir, "2017-03-19/Content_2017-03-19T010359.xml", sep = ""))
sites_xml <- read_xml(paste(ttDir, "site_locations.xml", sep = ""))


ns <- xml_ns_rename(xml_ns(sites_xml), d1 = "d2LogicalModel")

sites <- sites_xml %>% 
    xml_find_all("//d2LogicalModel:measurementSiteRecord", ns)

sites_df <- data_frame(nodeset = sites) %>% 
    mutate(site_ref = nodeset %>% map(~ xml_attrs(.)),
           site_desc = nodeset %>% 
               xml_find_all("//d2LogicalModel:measurementSiteName", ns) %>% 
               xml_text(.),
           children = nodeset %>% map(~ xml_children(.)),
           from_lat = nodeset %>% 
               xml_find_all("//d2LogicalModel:from", ns))



          