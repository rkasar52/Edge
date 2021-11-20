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
km <- kmeans(aggdata.scaled, centers = 7, iter.max = 100)
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
cluster7 <- Stations[km$cluster ==7]


######################################################################
### agg day of week ####
aggdata.DOW <- read_csv("aggdata_day_DOW.csv")



Station <- aggdata.DOW$station
aggdata.DOW[is.na(aggdata.DOW)] <- 0
aggdata.DOW <- aggdata.DOW %>% 
  mutate(day_of_week = ifelse(day_of_week == "Sunday",1, ifelse(
                              day_of_week == "Monday", 2, ifelse(
                              day_of_week == "Tuesday", 3, ifelse(
                              day_of_week == "Wednesday", 4, ifelse(
                              day_of_week == "Thursday", 5, ifelse(
                             day_of_week == "Friday", 6, 7  ) ) ) ) )  )) %>% 
  select(-c(station)) %>% 
  mutate(sub_cust_in = avg_Sub_start-avg_Cust_start ,
         sub_cust_out = avg_Sub_stop-avg_Cust_stop  ) %>% 
  select(avgwait, day_of_week, avgBikeIn, avgBikeOut, sub_cust_in, sub_cust_out)

pp <- preProcess(aggdata.DOW, method =  c("center", "scale"))
aggdata.DOW.scaled <- predict(pp, aggdata.DOW)

colMeans(aggdata.scaled)
apply(aggdata.scaled, 2, sd)


