library(tidyverse)
library(stringr)

sites <- read_csv("data/jt/sites.csv")
times <- read_csv("data/jt/jtime-xmlparsed.csv") # this is a csv export of the 
                                                 # TravelTimes database on 31-08-2017

#con <- DBI::dbConnect(odbc::odbc(), "Travel-Times") # set up ODBC connection to SQL

# filter to m50 and join sites data
m50_times <- times %>% 
    filter(str_detect(SiteReference, "M50")) %>% 
    rename(site_ref = SiteReference) %>% 
    left_join(sites, by = "site_ref")

# check site_desc NA values (i.e. site_ref values not joined)
missing_sites <- m50_times %>% 
    filter(is.na(site_desc)) %>% 
    distinct(site_ref)

# Add from & to site ref columns
m50_times <- m50_times %>% 
    mutate(site_ref2 = str_split_fixed(site_ref, pattern = "_", n = 3)[,3],
           from_site = str_split_fixed(site_ref2, pattern = "-", n = 2)[,1],
           to_site = str_split_fixed(site_ref2, pattern = "-", n = 2)[,2]) %>% 
    select(-site_ref2)

# Add direction column
m50_times <- m50_times %>% 
    mutate(direction = case_when(str_detect(site_desc, 'M50N') == TRUE ~ "northbound",
                                 str_detect(site_desc, 'M50S') == TRUE ~ "southbound"))

# Add from & to junction desc columns
m50_times <- m50_times %>% 
    mutate(site_desc2 = str_sub(m50_times$site_desc, 6),
           from_jn_desc = str_split_fixed(site_desc2, pattern = " to ", n = 2)[,1],
           to_jn_desc = str_split_fixed(site_desc2, pattern = " to ", n = 2)[,2]) %>% 
    select(-site_desc2) %>% 
    mutate(from_jn_desc = case_when(from_jn_desc == "Blanchardstown" ~ "J6 Blanchardstown",
                               TRUE ~ as.character(from_jn_desc)),
           to_jn_desc = case_when(to_jn_desc == "Blanchardstown" ~ "J6 Blanchardstown",
                             TRUE ~ as.character(to_jn_desc)))

# Add from & to junction no columns
m50_times <- m50_times %>%
    mutate(from_jn_no = parse_number(from_jn_desc),
           to_jn_no = parse_number(to_jn_desc))

# add link references

junctions <- data_frame(jn_no = c(3:7, 9:17),
                        jn_name = c("M1", "BMN", "N2", "N3", "N4", "N7", "BMT",
                                    "N81", "FIR", "BAL", "SAN", "CAR", "CHE",
                                    "N11"))

m50_times <- m50_times %>%
    left_join(junctions, by = c("from_jn_no" = "jn_no")) %>% 
    rename(from_jn_name = jn_name) %>% 
    left_join(junctions, by = c("to_jn_no" = "jn_no")) %>% 
    rename(to_jn_name = jn_name) %>% 
    mutate(Sec = paste0(from_jn_name, " - ", to_jn_name))

m50_new_secs <- m50_times %>% 
    select(Sec, from_jn_no, to_jn_no, site_ref) %>% 
    distinct

links <- read_csv("data/links.csv") %>% 
    left_join(m50_new_secs)



