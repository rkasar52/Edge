library(caret)
library(tidyverse)
library(readxl)

aggdata.day <- read_csv("aggdata_day.csv")
head(aggdata.day)

Stations <- aggdata.day$station
Stations

aggdata.day <-  aggdata.day %>% 
  select(-c(station)) %>% 
  mutate(sub_cust_in = avg_Sub_start-avg_Cust_start ,
         sub_cust_out = avg_Sub_stop-avg_Cust_stop  ) %>% 
  select(avgwait, avgBikeIn, avgBikeOut, sub_cust_in, sub_cust_out)

pp <- preProcess(aggdata.day, method =  c("center", "scale"))
aggdata.scaled <- predict(pp, aggdata.day)

colMeans(aggdata.scaled)
apply(aggdata.scaled, 2, sd)








dat <- data.frame(k = 1:30)

# We will apply the k-means function for each value of k, as follows
dat$SS1 <- sapply(dat$k, function(k) {
  set.seed(144)
  kmeans(aggdata.scaled, iter.max=100, k)$tot.withinss
})

# Let us repeat this several times.
# WHY?
# Because k means is a randomized algorithm (with randomized start) so we want to check the stability of the results
dat$SS2 <- sapply(dat$k, function(k) {
  set.seed(123)
  kmeans(aggdata.scaled, iter.max=100, k)$tot.withinss
})
dat$SS3 <- sapply(dat$k, function(k) {
  set.seed(1)
  kmeans(aggdata.scaled, iter.max=100, k)$tot.withinss
})
dat$SS4 <- sapply(dat$k, function(k) {
  set.seed(15071123)
  kmeans(aggdata.scaled, iter.max=100, k)$tot.withinss
})


dat %>%
  pivot_longer(c(SS1, SS2, SS3, SS4),
               names_to = "Trial",
               values_to = "SS") %>%
  ggplot() +
  geom_line(aes(x=k, y=SS, color=Trial), lwd=1) +
  theme_bw() +
  labs(x="Number of Clusters (k)",
       y="Within-Cluster Sum of Squares") +
  ylim(0, 2500) +
  theme(axis.title=element_text(size=18), 
        axis.text=element_text(size=18)) +
  scale_color_brewer(palette="Set1")


######################################################################
##### KMEANS Cluster ####
set.seed(69)

#Run the algorithim
km <- kmeans(aggdata.scaled, centers = 6, iter.max = 100)
names(km)

#Get the clusters
centers <- km$centers
View(centers)

#Get the number in each cluster
km.size <- km$size
km.size

#Get the within cluster standard error
km$tot.withinss  

#Get the stations in each cluster. 
cluster1 <- Stations[km$cluster ==1]
cluster2 <- Stations[km$cluster ==2]
cluster3 <- Stations[km$cluster ==3]
cluster4 <- Stations[km$cluster ==4]
cluster5 <- Stations[km$cluster ==5]
cluster6 <- Stations[km$cluster ==6]



######################################################################
### agg day of week ####
aggdata.DOW <- read_csv("aggdata_day_DOW.csv")



Station <- aggdata.DOW$station
aggdata.DOW[is.na(aggdata.DOW)] <- 0
aggdata.DOW <- aggdata.DOW %>% 
  mutate(day_of_week = ifelse(day_of_week == "Monday",1, ifelse(
                              day_of_week == "Tuesday", 2, ifelse(
                              day_of_week == "Wednesday", 3, ifelse(
                              day_of_week == "Thursday", 4, ifelse(
                              day_of_week == "Friday", 5, ifelse(
                             day_of_week == "Saturday", 6, 7  ) ) ) ) )  )) %>% 
  select(-c(station)) %>% 
  mutate(sub_cust_in = avg_Sub_start-avg_Cust_start ,
         sub_cust_out = avg_Sub_stop-avg_Cust_stop  ) %>% 
  select(avgwait, day_of_week, avgBikeIn, avgBikeOut, sub_cust_in, sub_cust_out)

pp <- preProcess(aggdata.DOW, method =  c("center", "scale"))
aggdata.DOW.scaled <- predict(pp, aggdata.DOW)

colMeans(aggdata.DOW.scaled)
apply(aggdata.DOW.scaled, 2, sd)


### Skree Plot ###
dat <- data.frame(k = 1:60)

# We will apply the k-means function for each value of k, as follows
dat$SS1 <- sapply(dat$k, function(k) {
  set.seed(144)
  kmeans(aggdata.DOW.scaled, iter.max=100, k)$tot.withinss
})

# Let us repeat this several times.
# WHY?
# Because k means is a randomized algorithm (with randomized start) so we want to check the stability of the results
dat$SS2 <- sapply(dat$k, function(k) {
  set.seed(123)
  kmeans(aggdata.DOW.scaled, iter.max=100, k)$tot.withinss
})
dat$SS3 <- sapply(dat$k, function(k) {
  set.seed(1)
  kmeans(aggdata.DOW.scaled, iter.max=100, k)$tot.withinss
})
dat$SS4 <- sapply(dat$k, function(k) {
  set.seed(15071123)
  kmeans(aggdata.DOW.scaled, iter.max=100, k)$tot.withinss
})


dat %>%
  pivot_longer(c(SS1, SS2, SS3, SS4),
               names_to = "Trial",
               values_to = "SS") %>%
  ggplot() +
  geom_line(aes(x=k, y=SS, color=Trial), lwd=1) +
  theme_bw() +
  labs(x="Number of Clusters (k)",
       y="Within-Cluster Sum of Squares") +
  ylim(0, 10000) +
  theme(axis.title=element_text(size=18), 
        axis.text=element_text(size=18)) +
  scale_color_brewer(palette="Set1") +
  geom_vline(xintercept = 11)

######################





km_DOW <- kmeans(aggdata.DOW.scaled, centers = 11, iter.max = 100)

dow_centers <- km_DOW$centers

km_DOW$size


aggdata_day_DOW$Cluster = km_DOW$cluster

########################################################################################
#### Get Day of Week ####
current_stations <- read_excel("current_stations.xlsx")
Monday <- aggdata_day_DOW %>% 
  filter(day_of_week == "Monday") %>% 
  select(station, Cluster) %>% 
  left_join(current_stations, by = c("station" = "Name"))
write_csv(Monday, "DOW_Cluster/Monday.csv")

Tuesday <- aggdata_day_DOW %>% 
  filter(day_of_week == "Tuesday") %>% 
  select(station, Cluster) %>% 
  left_join(current_stations, by = c("station" = "Name"))
write_csv(Tuesday, "DOW_Cluster/Tuesday")

Wednesday <- aggdata_day_DOW %>% 
  filter(day_of_week == "Wednesday") %>% 
  select(station, Cluster) %>% 
  left_join(current_stations, by = c("station" = "Name"))
write_csv(Wednesday, "DOW_Cluster/Wednesday")

Thursday <- aggdata_day_DOW %>% 
  filter(day_of_week == "Thursday") %>% 
  select(station, Cluster) %>% 
  left_join(current_stations, by = c("station" = "Name"))
write_csv(Thursday, "DOW_Cluster/Thursday")

Friday <- aggdata_day_DOW %>% 
  filter(day_of_week == "Friday") %>% 
  select(station, Cluster) %>% 
  left_join(current_stations, by = c("station" = "Name"))
write_csv(Friday, "DOW_Cluster/Friday")

Saturday <- aggdata_day_DOW %>% 
  filter(day_of_week == "Saturday") %>% 
  select(station, Cluster) %>% 
  left_join(current_stations, by = c("station" = "Name"))
write_csv(Saturday, "DOW_Cluster/Saturday")

Sunday <- aggdata_day_DOW %>% 
  filter(day_of_week == "Sunday") %>% 
  select(station, Cluster) %>% 
  left_join(current_stations, by = c("station" = "Name"))
write_csv(Sunday, "DOW_Cluster/Sunday")


#7 different maps by day of week. 
########################################################################################




################################################
## Adding weather data and clusters to the full data. ##


all_data <- read_csv("all_data.csv")
sept_weather_boston <- read_excel("sept_weather_boston.xlsx")

#Join weather data. 
all_data_cluster <- all_data %>% 
  left_join(sept_weather_boston, by = c("day", "month")) %>% 
  mutate(cluster1 = ifelse(station %in% cluster1, 1, 0), 
         cluster2 = ifelse(station %in% cluster2, 1, 0),
         cluster3 = ifelse(station %in% cluster3, 1, 0),
         cluster4 = ifelse(station %in% cluster4, 1, 0),
         cluster5 = ifelse(station %in% cluster5, 1, 0),
         cluster6 = ifelse(station %in% cluster6, 1, 0))

write_csv(all_data_cluster, "all_data_cluster.csv")



#DOW cluster....
all_data_DOW_cluster <- all_data %>% 
  left_join(sept_weather_boston, by = c("day", "month")) 


#################################################
## Geographic Plot ##

current_stations <- read_excel("current_stations.xlsx")

current_stations_cluster <- current_stations %>% 
  mutate(cluster1 = ifelse(Name %in% cluster1, 1, 0), 
         cluster2 = ifelse(Name %in% cluster2, 1, 0),
         cluster3 = ifelse(Name %in% cluster3, 1, 0),
         cluster4 = ifelse(Name %in% cluster4, 1, 0),
         cluster5 = ifelse(Name %in% cluster5, 1, 0),
         cluster6 = ifelse(Name %in% cluster6, 1, 0)) %>% 
  mutate(Cluster = ifelse(cluster1 == 1, 1, 
                          ifelse(cluster2 == 1, 2, 
                                 ifelse(cluster3 == 1, 3, 
                                        ifelse(cluster4 == 1, 4, 
                                               ifelse(cluster5 == 1, 5, 
                                                      ifelse(cluster6 == 1, 6, 0))))))) %>% 
  filter(Cluster != 0 )   #Remove three stations not in the data
  
  
write_csv(current_stations_cluster, "station_cluster.csv")

