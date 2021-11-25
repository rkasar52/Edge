library(tidyverse)
library(ggplot2)
library(lubridate)
library(zoo)
library(readxl)

## Semi's Data

all_data <- read.csv("all_data.csv") 

#Avg wait time by station, and day of week
station_byday.dow <- all_data %>% 
  group_by(station, month, day, day_of_week) %>% 
  summarise(avgwait = mean(wait_time),
            bikesin = sum(increment),
            bikesout = sum(decrement))

#Average wait time per station
station_avgwait <- all_data %>% 
  group_by(station) %>% 
  summarise(avgwait = mean(wait_time))

#Average of inflow and outflow of bikes by day
station_avgTraffic_byday <- station_byday.dow %>% 
  group_by(station) %>% 
  summarise(avgBikeIn = mean(bikesin),
            avgBikeOut = mean(bikesout))

#Average wait time for station by day of week 
station_avgwait_DOW <- all_data %>% 
  group_by(station, day_of_week) %>% 
  summarise(avgwait = mean(wait_time))

#Average inflow and outflow of bikes at station by day of week
station_avgTraffic_byDOW <- station_byday.dow %>% 
  group_by(station, day_of_week) %>% 
  summarise(avgBikeIn = mean(bikesin),
            avgBikeOut = mean(bikesout))

#Avg wait plus in and outflow day of week
stations.DOW <- station_avgwait_DOW %>% 
  left_join(station_avgTraffic_byDOW, b=c("station", "day_of_week"))

#Avg wait plus in and outflow of bikes
stations.day <- station_avgwait %>% 
  left_join(station_avgTraffic_byday, by="station")


## Original Data
fulldata <- read.csv("202109-bluebikes-tripdata.csv")

#Get subscribers at the starting point
getSubsStart <- fulldata %>% 
  mutate(startdate=substr(starttime, 1, 10),
         subflag = ifelse(usertype=="Subscriber", 1, 0)) %>% 
  group_by(start.station.name, startdate) %>% 
  summarise(sub_rides = sum(subflag),
            all_rides = n()) %>% 
  mutate(cust_rides = all_rides - sub_rides) %>% 
  select(-all_rides) %>% 
  mutate(DOW = wday(startdate))

#Get subscribers at the destination
getSubsStop <- fulldata %>% 
  mutate(enddate=substr(stoptime, 1, 10),
         subflag = ifelse(usertype=="Subscriber", 1, 0)) %>% 
  group_by(end.station.name, enddate) %>% 
  summarise(sub_rides = sum(subflag),
            all_rides = n()) %>% 
  mutate(cust_rides = all_rides - sub_rides) %>% 
  select(-all_rides) %>% 
  mutate(DOW = wday(enddate))

#Avg number of subscribers per station 
substart.day <- getSubsStart %>% 
  group_by(start.station.name) %>% 
  summarise(avg_Sub_start = mean(sub_rides),
            avg_Cust_start = mean(cust_rides))

#Avg number of subscribers and normal customers per station (destination)
substop.day <- getSubsStop %>% 
  group_by(end.station.name) %>% 
  summarise(avg_Sub_stop = mean(sub_rides),
            avg_Cust_stop = mean(cust_rides))

#Full info of subs and customers start and stop
station.subCount.day <- substart.day %>% 
  left_join(substop.day, by=c("start.station.name" = "end.station.name"))

# Get subscribers and customer by day of week
substart.DOW <- getSubsStart %>% 
  group_by(start.station.name, DOW) %>% 
  summarise(avg_Sub_start = mean(sub_rides),
            avg_Cust_start = mean(cust_rides))

# Get subscribers and customer by day of week, for destination
substop.DOW <- getSubsStop %>% 
  group_by(end.station.name, DOW) %>% 
  summarise(avg_Sub_stop = mean(sub_rides),
            avg_Cust_stop = mean(cust_rides))

#Full info for cust and sub day of week
station.subCount.DOW <- substart.DOW %>% 
  left_join(substop.DOW, by=c("start.station.name" = "end.station.name", "DOW")) %>% 
  mutate(day_of_week = ifelse(DOW == 1, "Sunday", ifelse(
                DOW == 2, "Monday", ifelse(
                DOW == 3, "Tuesday", ifelse(
                DOW == 4, "Wednesday", ifelse(
                DOW == 5, "Thursday", ifelse(
                DOW == 6, "Friday", "Saturday"))))))) %>% 
  select(-DOW)


## aggregated data - just need to add weather

#Full data by day
aggdata.day <- stations.day %>% 
  left_join(station.subCount.day, by = c("station" = "start.station.name"))

#Full data by day of week
aggdata.DOW <- stations.DOW %>% 
  left_join(station.subCount.DOW, by = c("station" = "start.station.name", "day_of_week"))

write.csv(aggdata.day, file = "aggdata_day.csv", row.names=FALSE)
write.csv(aggdata.DOW, file = "aggdata_day_DOW.csv", row.names=FALSE)
