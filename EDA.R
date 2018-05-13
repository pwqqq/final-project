library(data.table)
library(ggplot2)
library(leaflet)
library(dplyr)
library(Rmisc)
library(plotly)
library(tidyverse)
library(geosphere)

train = fread('train_taxi.csv')

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

####
pick_coord <- train %>%
  select(pickup_longitude, pickup_latitude)
drop_coord <- train %>%
  select(dropoff_longitude, dropoff_latitude)

# compute the direct distiance between pickup and dropoff
train$dist <- distCosine(pick_coord, drop_coord)

train <- train %>%
  mutate(speed = dist/trip_duration*3.6)

## plot of direct distance vs trip duration
p_dis_duration = train %>%
  sample_n(5e4) %>%
  ggplot(aes(dist, trip_duration)) +
  geom_point(shape=1, alpha = 0.1) +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "Direct distance [m]", y = "Trip duration [s]") +
  ggtitle('plot of direct distance vs trip duration') +
  theme_bw()

ggplotly(p_dis_duration)

## the distribution of average speed
p_speed = train %>%
  filter(speed > 2 & speed < 1e2) %>%
  ggplot(aes(speed)) +
  geom_histogram(bins = 50) +
  labs(x = "Average speed [km/h] (direct distance)") +
  theme_bw()

ggplotly(p_speed)

## 
set.seed(1234)
foo <- sample_n(train, 8e3)

leaflet(data = foo) %>% 
  addProviderTiles("Esri.NatGeoWorldMap") %>%
  addCircleMarkers(~ pickup_longitude, ~ pickup_latitude, radius = 1,
                   color = speed, fillOpacity = 0.3)  

