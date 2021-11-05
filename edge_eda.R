library(tidyverse)
library(ggplot2)
library(lubridate)


bluebike <- read.csv("202109-bluebikes-tripdata.csv")
bluebike$start.station.id <- as.factor(bluebike$start.station.id)

# time zone is UTC -- we will convert to EDT
bluebike$starttime <- as.POSIXct(bluebike$starttime)
bluebike$starttime <- force_tz(bluebike$starttime, tzone = "Europe/London")
bluebike$starttime_edt <- with_tz(bluebike$starttime, tzone = "America/New_York")

bluebike$stoptime <- as.POSIXct(bluebike$stoptime)
bluebike$stoptime <- force_tz(bluebike$stoptime, tzone = "Europe/London")
bluebike$stoptime_edt <- with_tz(bluebike$stoptime, tzone = "America/New_York")


stations_sep2021 <- bluebike %>% 
  group_by(start.station.name) %>% 
  summarise(rides_started_here = n()) %>% 
  arrange(desc(rides_started_here))

stations_sep2021$rank <- 1:nrow(stations_sep2021)

top20_stations <- stations_sep2021 %>% 
  filter(rank <= 20) %>% 
  arrange(rides_started_here)

ggplot(top20_stations) + geom_col(aes(x=rides_started_here, y=reorder(start.station.name, rides_started_here))) + xlab("Total Rides") + ylab("Start Station") + ggtitle("Top Stations by Rides Started")


data <- bluebike %>% 
  mutate(hour_started = substr(starttime_edt, 12, 13))
data$hour_started <- as.numeric(data$hour_started)

station_hour_rides_started <- data %>% 
  group_by(start.station.name, start.station.id, hour_started) %>% 
  summarise(rides_started = n()) %>% 
  left_join(stations_sep2021, by="start.station.id") %>% 
  select(start.station.name, hour_started, rides_started, rank)

station_hour_rides_started_top10 <- station_hour_rides_started %>% 
  filter(rank <= 10)

ggplot(station_hour_rides_started_top10) + geom_line(aes(x=hour_started, y=rides_started, color=start.station.name)) + xlab("Hour Started") + ylab("Total Rides") + ggtitle("Rides Started by Hour for Top Stations")

top10stations <- stations_sep2021 %>% 
  filter(rank <= 10) %>% 
  left_join()

  
  
  
ggplot(top10stations)