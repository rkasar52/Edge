library(tidyverse)
library(ggplot2)
library(lubridate)
library(zoo)


## Weather Data
all_data_cluster <- read.csv("all_data_cluster.csv")
all_data <- all_data_cluster %>% 
  select(-cluster1, -cluster2, -cluster3, -cluster4, -cluster5, -cluster6)

# Split train test (train: everything up through 9/25, test: everything after)
all_data_train_full <- all_data %>% 
  filter(month == 9 & day <= 25)

all_data_test_full_1 <- all_data %>% 
  filter((month == 9 & day > 25) | (month == 10))

# get rid of two stations not in train set
trainStations <- all_data_train_full %>% 
  distinct(station)
trainStations$intrain = 1

testStations <- all_data_test_full %>% 
  distinct(station)
testStations$intest = 1

stationsInBoth <- testStations %>% 
  left_join(trainStations, by="station")

all_data_test_full <- all_data_test_full_1 %>% 
  filter(station!="Valenti Way at Haverhill St" & station!="606 American Legion Hwy at Canterbury St")



# get measures from semi data

station_trafficbyday <- all_data_train_full %>% 
  group_by(station, month, day)  %>%
  summarise(bikesin = sum(increment),
  bikesout = sum(decrement))

station_traffic_avgs <- station_trafficbyday %>% 
  group_by(station) %>% 
  summarise(avgBikeIn = mean(bikesin),
            avgBikeOut = mean(bikesout))

station_avgwait <- all_data_train_full %>% 
  filter(wait_time>0) %>% 
  group_by(station) %>% 
  summarise(avgwait = mean(wait_time))
                          
                        
## Original Data
fulldata <- read.csv("202109-bluebikes-tripdata.csv")
                          
fulldataWNumDates <- fulldata %>% 
  mutate(stopmonth = as.numeric(substr(stoptime, 6,7)),
         stopday = as.numeric(substr(stoptime,9,10)))
                           
# split train test
fulldata_train <- fulldataWNumDates %>% 
  filter(stopmonth == 9 & stopday <= 25)


#Get subscribers at the starting point
getSubsStart <- fulldata_train %>% 
  mutate(startdate=substr(starttime, 1, 10),
         subflag = ifelse(usertype=="Subscriber", 1, 0)) %>% 
  group_by(start.station.name, startdate) %>% 
  summarise(sub_rides = sum(subflag),
            all_rides = n()) %>%
  mutate(cust_rides = all_rides - sub_rides) %>%
  select(-all_rides) %>%
  mutate(DOW = wday(startdate))

#Get subscribers at the destination
getSubsStop <- fulldata_train %>% 
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

# get durations
startDuration <- fulldata_train %>% 
  group_by(start.station.name) %>% 
  summarise(avgStartDuration = mean(tripduration))

stopDuration <- fulldata_train %>% 
  group_by(end.station.name) %>% 
  summarise(avgStopDuration = mean(tripduration))


# pull station dock data
stationinfo_full <- read.csv("current_bluebikes_stations.csv") 

stationinfo <- stationinfo_full %>% 
  select(Name, Total.docks)

all_data_train <- all_data_train_full %>% 
 left_join(station_traffic_avgs, by="station") %>% 
 left_join(station_avgwait, by="station") %>% 
 left_join(station.subCount.day, by=c("station" = "start.station.name")) %>% 
 left_join(startDuration,by=c("station" = "start.station.name")) %>% 
 left_join(stopDuration,by=c("station" = "end.station.name")) %>% 
 left_join(stationinfo, by=c("station" = "Name"))

# Join metrics to test set
all_data_test <- all_data_test_full %>% 
 left_join(station_traffic_avgs, by="station") %>% 
 left_join(station_avgwait, by="station") %>% 
 left_join(station.subCount.day, by=c("station" = "start.station.name")) %>% 
 left_join(startDuration,by=c("station" = "start.station.name")) %>% 
 left_join(stopDuration,by=c("station" = "end.station.name")) %>% 
 left_join(stationinfo, by=c("station" = "Name"))



# get cluster info
cluster_data <- all_data_train %>% 
 select(station, avgBikeIn:Total.docks) %>% 
 distinct()

# write to csv
write.csv(all_data_train, file = "all_data_train.csv", row.names=FALSE)
write.csv(all_data_test, file = "all_data_test.csv", row.names=FALSE)
write.csv(cluster_data, file = "cluster_data.csv", row.names=FALSE)