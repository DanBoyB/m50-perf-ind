library(tidyverse)
library(stringr)
library(XML)
library(xml2)

ttDir <- "~/datastore/TravelTimes/2017-03-19/"

x <- read_xml(paste(ttDir, "Content_2017-03-19T010359.xml", sep = ""))

ns <- xml_ns_rename(xml_ns(x), d1 = "d2LogicalModel")

y <- x %>% 
    xml_find_all("//d2LogicalModel:siteMeasurements", ns) %>% 
    xml_children() 

z <- x %>% 
    xml_find_all("//d2LogicalModel:siteMeasurements", ns) %>% 
    xml_children() %>% 
    xml_children() %>% 
    xml_children()

y_df <- data_frame(nodeset = y) %>% 
    mutate(n = nodeset %>% map(~ xml_name(.)),
           text = nodeset %>% map(~ xml_text(.))) %>% 
    filter(n != "measuredValue") %>% 
    select(-nodeset) %>% 
    unnest() %>% 
    filter(str_detect(text, 'M50'))


z_df <- data_frame(row = seq_along(z),
                   nodeset = z) %>% 
    mutate(n = nodeset %>% map(~ xml_name(.)),
           text = nodeset %>% map(~ xml_text(.)),
           i = nodeset %>% map(~ seq_along(.)))

