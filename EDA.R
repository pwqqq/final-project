library(data.table)
library(ggplot2)
library(leaflet)
library(dplyr)
library(Rmisc)
library(plotly)
library(tidyverse)
library(lubridate)


#train = fread('train_taxi.csv')
train=fread("train.csv")

train <- train %>%
  mutate(pickup_datetime = ymd_hms(pickup_datetime),
         dropoff_datetime = ymd_hms(dropoff_datetime),
         vendor_id = factor(vendor_id),
         passenger_count = factor(passenger_count))

summary(train)

head(train)

###### select a sample of 8000 to see the pick up location
set.seed(1234)
foo <- sample_n(train, 8e3)

leaflet(data = foo) %>% 
  addProviderTiles("Esri.NatGeoWorldMap") %>%
  addCircleMarkers(~ pickup_longitude, ~ pickup_latitude, radius = 1,
                   color = "blue", fillOpacity = 0.3)  

###### the distribution of log10(trip_duration)
## note that the y-axis is sqrt of count
train %>%
  ggplot(aes(trip_duration)) +
  geom_histogram(bins = 150) +
  scale_x_log10() +
  scale_y_sqrt()

###### plot all pickup locations
min_lat <- 40.5774
max_lat <- 40.9176
min_long <- -74.15
max_long <- -73.7004

map1 <- ggplot(train, aes(x=pickup_longitude, y=pickup_latitude)) +
  geom_point(size=0.06) +
  scale_x_continuous(limits=c(min_long, max_long)) +
  scale_y_continuous(limits=c(min_lat, max_lat))
#png("map1.png", w=600, h=600)
map1
#dev.off()

###### plot all dropoff locations
map2 <- ggplot(train, aes(x=dropoff_longitude, y=dropoff_latitude)) +
  geom_point(size=0.06) +
  scale_x_continuous(limits=c(min_long, max_long)) +
  scale_y_continuous(limits=c(min_lat, max_lat))
#png("map2.png", w=600, h=600)
map2
#dev.off()


###### split date and time 
pickup = stringr::str_split(train$pickup_datetime, ' ')
pick_up = data.frame(matrix(unlist(pickup), ncol = 2, byrow=T))
names(pick_up) = c('pickup_date','pickup_time')
pick_up$pickup_date = as.Date(pick_up$pickup_date,'%Y-%m-%d')
pick_up$pday = format(pick_up$pickup_date,'%m-%d')

dropoff = stringr::str_split(train$dropoff_datetime, ' ')
drop_off = data.frame(matrix(unlist(dropoff), ncol = 2, byrow=T))
names(drop_off) = c('dropoff_date','dropoff_time')
drop_off$dropoff_date = as.Date(drop_off$dropoff_date, '%Y-%m-%d')
drop_off$dday = format(drop_off$dropoff_date, '%m-%d')

train_s = data.frame(cbind(train,pick_up,drop_off)) %>%
  select(-pickup_datetime, -dropoff_datetime)


p1 = plot_ly(train_s, x = ~ pickup_date)  %>% add_histogram(name = "pick_up date")  
p2 = plot_ly(train_s, x = ~ dropoff_date)  %>% add_histogram(name = "drop_off date")  

subplot(p1, p2,nrows = 2, shareX = T,titleX = F )

