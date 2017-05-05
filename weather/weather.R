library(tidyverse)
library(weatherData) # need to use github version, not CRAN version

dubAirport <- getWeatherForDate("EIDW", "2015-01-01", "2015-12-31",opt_all_columns = TRUE) 

dubAirport$Date = as.POSIXct(dubAirport$Date, format = "%Y-%m-%d")

dubAirport <- dubAirport %>% 
    as_data_frame()

dubAirport %>% 
    ggplot(aes(Date, Precipitationmm)) +
    geom_line()
