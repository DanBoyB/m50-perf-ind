#!/usr/bin/Rscript

library(tidyverse)
library(xml2)
library(rvest)

opendata <- read_html("http://data.tii.ie/Datasets/Its/DatexII/TravelTimeData/")

table <- opendata %>% 
    html_node("table") %>% 
    html_table() %>% 
    as_data_frame() %>% 
    rename(from_time = X1,
           file_type = X2,
           folder_name = X3) %>% 
    mutate(site = paste0("http://data.tii.ie/Datasets/Its/DatexII/TravelTimeData/", folder_name, "/")) %>% 
    filter(!folder_name %in% c("Content.xml", "Metadata.xml"))

to_df <- function(site) {
    df <- read_html(site) %>% 
        html_node("table") %>% 
        html_table() %>% 
        as_data_frame() %>% 
        filter(!grepl("Metadata", X3))
    
    return(df)
}    

extract_times <- function(url) {
    times_xml <- read_xml(url) %>% 
        xml_ns_strip()
    
    times <- times_xml %>% 
        xml_find_all("//siteMeasurements")
    
    # Parse content xml to dataframe (sites with "faults" removed)
    times_df <- data_frame(nodeset = times) %>% 
        mutate(datetime = nodeset %>% 
                   xml_find_all("//measurementTimeDefault") %>% 
                   xml_text(.),
               site_desc = nodeset %>% 
                   xml_find_all("//measurementSiteReference") %>% 
                   xml_text(.),
               values = nodeset %>% 
                   xml_find_all("//measuredValue"),
               period_secs = values %>% 
                   xml_find_all(".//period") %>% 
                   xml_text(.),
               ff_speed = values %>% 
                   xml_find_all(".//freeFlowSpeed") %>% 
                   xml_text(.),
               ff_trav_time = values %>% 
                   xml_find_all(".//freeFlowTravelTime") %>% 
                   xml_text(.)) %>% 
        filter(!grepl("fault", values)) %>% # remove faults
        mutate(trav_time = values %>% 
                   xml_find_all(".//travelTime") %>% 
                   xml_text(.)) %>% 
        select(-nodeset, -values)
    
    # Parse content xml to dataframe (sites with "faults" included)
    faults <- data_frame(nodeset = times) %>% 
        mutate(datetime = nodeset %>% 
                   xml_find_all("//measurementTimeDefault") %>% 
                   xml_text(.),
               site_desc = nodeset %>% 
                   xml_find_all("//measurementSiteReference") %>% 
                   xml_text(.),
               values = nodeset %>% 
                   xml_find_all("//measuredValue"),
               period_secs = values %>% 
                   xml_find_all("//period") %>% 
                   xml_text(.),
               ff_speed = values %>% 
                   xml_find_all("//freeFlowSpeed") %>% 
                   xml_text(.),
               ff_trav_time = values %>% 
                   xml_find_all("//freeFlowTravelTime") %>% 
                   xml_text(.)) %>% 
        filter(grepl("fault", values)) %>% 
        mutate(trav_time = NA) %>% 
        select(-nodeset, -values)
    
    # Combine recorded travel times with faults (as NA travel time values)
    times_df <- times_df %>% 
        bind_rows(faults) %>% 
        write_csv(paste0("~/R/projects/m50-perf-ind/data/jt/", substr(url, 56, 65), ".csv"), append = TRUE)
    
    # Add a 3 second sleep to avoid timeout
    Sys.sleep(3)
}

sites <- table %>% 
    mutate(files = map(site, to_df)) %>% 
    unnest() %>% 
    mutate(url = paste0(site, X3)) %>% 
    select(url)

sites[41237:44068,]$url %>% 
    map(extract_times)
