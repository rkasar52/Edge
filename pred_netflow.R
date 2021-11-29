library(tidyverse)
library(ggplot2)
library(lubridate)
library(zoo)
library(readxl)
library(gbm) #this is the generic package for gradient boosting methods
library(xgboost) 

source("functions.R")
## Semi's Data

all_data <- read.csv("all_data.csv") 
sept_weather_boston <- read_excel("sept_weather_boston.xlsx")

cap <- all_data %>% 
  filter(station == "The Dimock Center")

#Avg wait time by station, and time. 
station_byday.dow <- all_data %>% 
  group_by(station, month, day, day_of_week) %>% 
  summarise(avgwait = mean(wait_time),
            bikesin = sum(increment),
            bikesout = sum(decrement))

#### Subscriber Calculations ####

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
  select(-DOW) %>% 
  filter( start.station.name != "18 Dorrance Warehouse") %>% 
  filter(start.station.name != "Graham and Parks School ??? Linnaean St at Walker St") %>% 
  filter(start.station.name != "Union St at Herrick Rd ??? Newton Centre Green Line") #%>% 
  #rename(station = start.station.name)
  print("Weird encoding for graham and union st staions")



#Daily bike flow at each station
station_byday_sub <- station_byday.dow %>% 
  right_join(station.subCount.DOW, by = c("station" = "start.station.name", "day_of_week")) %>% 
  mutate(net_flow = bikesin - bikesout)



#Get total capacity
current_bluebikes_stations <- read_csv("current_bluebikes_stations.csv")
current_bluebikes_stations <- current_bluebikes_stations %>% 
  select(Name, `Total docks`)

station_byday_sub <- station_byday_sub %>% 
  left_join(current_bluebikes_stations, by = c("station" = "Name")) %>% 
  mutate(exceed_cap = ifelse(net_flow <= (-1)*`Total docks`, 1, 0)) %>% 
  left_join(sept_weather_boston, by = c("day", "month"))

station_byday_sub$day_of_week <-  factor(station_byday_sub$day_of_week)
station_byday_sub$station <- factor(station_byday_sub$station)


model_df  = station_byday_sub %>% 
  ungroup() %>% 
  select(net_flow, station, month, day_of_week, avgwait, avg_Sub_start, avg_Cust_start, avg_Cust_stop, `Total docks`, Temp, Rain)

smp_size <- floor(0.75 * nrow(model_df))
set.seed(123)
train_ind <- sample(seq_len(nrow(model_df)), size = smp_size)

train <- model_df[train_ind, ]
test <- model_df[-train_ind, ]

qq <- train[2139,]

model_fit <- gbm(net_flow~ .,
                      data = train,
                      distribution = "gaussian",
                      n.trees = 100,
                      interaction.depth = 1,
                      shrinkage = 0.1,
                      n.minobsinnode = 10)

xgboost_train <- predict(model_fit, newdata=train, n.trees=100)
xgboost_test <- predict(model_fit, newdata=test, n.trees=100)

rsquared(xgboost_train, train$net_flow, mean(train$net_flow))
rsquared(xgboost_test, test$net_flow, mean(train$net_flow))

MAE(xgboost_test, test$net_flow)














qq <- station_byday_sub %>% 
  filter(station == "MIT Hayward St at Amherst St")




