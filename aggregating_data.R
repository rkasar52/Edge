library(tidyverse)
library(ggplot2)
library(lubridate)
library(zoo)

## Semi's Data

all_data <- read.csv("all_data.csv") 

station_byday.dow <- all_data %>% 
  group_by(station, month, day, day_of_week) %>% 
  summarise(avgwait = mean(wait_time),
            bikesin = sum(increment),
            bikesout = sum(decrement))

station_avgwait <- all_data %>% 
  group_by(station) %>% 
  summarise(avgwait = mean(wait_time))

station_avgTraffic_byday <- station_byday.dow %>% 
  group_by(station) %>% 
  summarise(avgBikeIn = mean(bikesin),
            avgBikeOut = mean(bikesout))

station_avgwait_DOW <- all_data %>% 
  group_by(station, day_of_week) %>% 
  summarise(avgwait = mean(wait_time))

station_avgTraffic_byDOW <- station_byday.dow %>% 
  group_by(station, day_of_week) %>% 
  summarise(avgBikeIn = mean(bikesin),
            avgBikeOut = mean(bikesout))

stations.DOW <- station_avgwait_DOW %>% 
  left_join(station_avgTraffic_byDOW, b=c("station", "day_of_week"))

stations.day <- station_avgwait %>% 
  left_join(station_avgTraffic_byday, by="station")


## Original Data
fulldata <- read.csv("202109-bluebikes-tripdata.csv")

getSubsStart <- fulldata %>% 
  mutate(startdate=substr(starttime, 1, 10),
         subflag = ifelse(usertype=="Subscriber", 1, 0)) %>% 
  group_by(start.station.name, startdate) %>% 
  summarise(sub_rides = sum(subflag),
            all_rides = n()) %>% 
  mutate(cust_rides = all_rides - sub_rides) %>% 
  select(-all_rides) %>% 
  mutate(DOW = wday(startdate))

getSubsStop <- fulldata %>% 
  mutate(enddate=substr(stoptime, 1, 10),
         subflag = ifelse(usertype=="Subscriber", 1, 0)) %>% 
  group_by(end.station.name, enddate) %>% 
  summarise(sub_rides = sum(subflag),
            all_rides = n()) %>% 
  mutate(cust_rides = all_rides - sub_rides) %>% 
  select(-all_rides) %>% 
  mutate(DOW = wday(enddate))

substart.day <- getSubsStart %>% 
  group_by(start.station.name) %>% 
  summarise(avg_Sub_start = mean(sub_rides),
            avg_Cust_start = mean(cust_rides))

substop.day <- getSubsStop %>% 
  group_by(end.station.name) %>% 
  summarise(avg_Sub_stop = mean(sub_rides),
            avg_Cust_stop = mean(cust_rides))

station.subCount.day <- substart.day %>% 
  left_join(substop.day, by=c("start.station.name" = "end.station.name"))

substart.DOW <- getSubsStart %>% 
  group_by(start.station.name, DOW) %>% 
  summarise(avg_Sub_start = mean(sub_rides),
            avg_Cust_start = mean(cust_rides))

substop.DOW <- getSubsStop %>% 
  group_by(end.station.name, DOW) %>% 
  summarise(avg_Sub_stop = mean(sub_rides),
            avg_Cust_stop = mean(cust_rides))

station.subCount.DOW <- substart.DOW %>% 
  left_join(substop.DOW, by=c("start.station.name" = "end.station.name")) %>% 
  select(-DOW.y) %>% 
  mutate(day_of_week = ifelse(DOW.x == 1, "Sunday", ifelse(
                DOW.x == 2, "Monday", ifelse(
                DOW.x == 3, "Tuesday", ifelse(
                DOW.x == 4, "Wednesday", ifelse(
                DOW.x == 5, "Thursday", ifelse(
                DOW.x == 6, "Friday", "Saturday"))))))) %>% 
  select(-DOW.x)


## aggregated data - just need to add weather

aggdata.day <- stations.day %>% 
  left_join(station.subCount.day, by = c("station" = "start.station.name"))

aggdata.DOW <- stations.DOW %>% 
  left_join(station.subCount.DOW, by = c("station" = "start.station.name", "day_of_week"))

