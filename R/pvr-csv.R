#!/usr/bin/Rscript

library(tidyverse)
library(lubridate)
library(knitr)
library(aws.s3)

data_dir <- "~/datastore/"
analysis_dir <- "~/R/projects/m50-perf-ind/"
year <- 2016
route <- 50
site_no <- c(1501, 1502, 1503, 1504, 1505, 1506, 1507, 1508, 1509,
	     15010, 15011, 15012)

names <- c("siteID", "RecTime", "SubSec", "Flownum", "Direction",
	   "Speed", "Class", "Headway", "Gap", "Length", "GrossWeight", 
	   "NumAxles", "ESAL", "Axles", "Weather", "VehicleId", 
	   "ClassScheme", "Flag", "BSEQ", "Code", "LegalStatus", 
	   "Validity", "ANPRConfidence", "ImagesPresent", "siteID2", "routeNo")

create_unix_cal <- function(y) {
	  unix_cal <- tibble(year = rep(y, 12), month = formatC(1:12,
							       	width = 2,
							       	flag = "0"), 
			     days = 0, start = 0, end = 0) %>%
                      mutate(days = as.character( days_in_month(as.POSIXct(paste(year,
										 month,
										 "01", 
										 sep = "-"),
									   format = "%Y-%m-%d"))),
     			     start = as.POSIXct(paste(year, "-", month, "-", "01", " 00:00:00", sep = "")),		     
	   		     end = as.POSIXct(paste(year, "-", month, "-", days, " 23:59:59", sep = "")),
	   		     start_unix = unclass(start),
	   		     end_unix = unclass(end))

          unix_cal <- unix_cal %>% 
		  select(start_unix, end_unix, month)

}

unix_df <- create_unix_cal(year)

pvr_to_monthly <- function(year, route, site_no) {
       
	files <- c("0000_part_00", "0001_part_00", "0002_part_00", "0003_part_00")

	lapply(files, function(i) by(unix_df, 1:nrow(unix_df), function(row) {

            a <- s3read_using(FUN = read_tsv,
			      col_names = names, 
			      object = paste0("s3://ie-tii-data-internal/pvrs/", 
					      year,  "/route-", route, "/site-",
					      site_no, "/", i)) %>% 
	         filter(RecTime >= row[[1]] & RecTime <= row[[2]])
	    
	    if (nrow(a) == 0) {
		    message(paste("No data available at site",
				  site_no,
				  "for",
				  month(as.POSIXct(row[[1]], origin = "1970-01-01"),
					label = TRUE, abbr = FALSE), 
				  year, 
				  "\n", 
				  sep = " "))
		    
		    Sys.sleep(2)
		    return (NULL)
	    } 

	    else {
		    a %>% 
			write_csv(paste(analysis_dir, 
					paste0("output/pvr/", "pvr-", route, "-",
					       site_no, "-", row[3], "-", year),
					".csv", 
	  				sep = ""),
				  append = TRUE)
	    }

	    rm(a)
	    gc(verbose = FALSE) }))
}

site_no %>%
    map(pvr_to_monthly, year = year, route = 50)
