library(tidyverse)
library(rvest)
library(RSelenium)
library(XML)

url <- "https://www.nratrafficdata.ie/c2"

fp <- file.path(find.package("RSelenium"), "examples/serverUtils")

source(paste(fp, "/checkForServer.R", sep = ""))
source(paste(fp, "/startServer.R", sep = ""))

checkForServer()
startServer()

remDr <- remoteDriver(remoteServerAddr = "localhost",
                      port = 4444, 
                      browserName = "firefox")


remDr$open()
remDr$getStatus()

remDr$navigate(url)

