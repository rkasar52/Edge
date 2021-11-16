library(tidyverse)
library(ggplot2)
library(lubridate)
library(zoo)


bluebike <- read.csv("202109-bluebikes-tripdata.csv")
stations <- read.csv("current_bluebikes_stations.csv")
bluebike$start.station.id <- as.factor(bluebike$start.station.id)

prejoin <- stations %>% 
  select(Name, Total.docks)

joinstation <- bluebike %>% 
  left_join(prejoin, by=c("start.station.name" = "Name")) %>% 
  rename(start.station.docks = Total.docks)
  


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

capacityDay_start <- function(data, station, day){

  station.capacity <- stations$Total.docks[which(stations$Name == station)]
  
  stationDay <- data %>% 
    filter(tripduration <= 86400 & (start.station.name == station | end.station.name == station) & (substr(starttime_edt, 1, 10) %in% day | substr(stoptime_edt, 1, 10) %in% day))
  
  start <- stationDay %>% 
    filter(start.station.name == station) %>% 
    select(starttime_edt) %>% 
    rename(time = starttime_edt)
  
  stop <- stationDay %>% 
    filter(end.station.name == station) %>% 
    select(stoptime_edt) %>% 
    rename(time = stoptime_edt)
  
  start$capacity <- -1
  stop$capacity <- 1
  
  together <- rbind(start, stop) %>% 
    arrange(time)
  together$trans <- 1:nrow(together)
  together$currentcapac <- cumsum(together$capacity)
  together$available <- station.capacity + together$currentcapac
  
  #currentCapacity <- together %>% 
  #  filter(trans <= transNum) %>% 
  #  summarise(avail = sum(capacity))
  
  #return(currentCapacity[1,1])
  return(together)
}

days <- c("2021-09-03", "2021-09-04")
AmesJan03_trans <- capacityDay_start(data, "1200 Beacon St", days) 

ggplot(AmesJan03_trans) + geom_line(aes(x=time, y=available))


stationDay <- data %>% 
  filter((start.station.name == "1200 Beacon St" | end.station.name == "1200 Beacon St") & (substr(starttime_edt, 1, 10) == "2021-09-04" | substr(stoptime_edt, 1, 10) == "2021-09-04"))
