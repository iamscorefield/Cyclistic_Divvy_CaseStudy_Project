## Install R packages

available.packages()
install.packages("tidyverse")
library(tidyverse)
update.packages()
install.packages("dplyr")
library(dplyr)

@@ Import and Read DATA

datatrip_2204 <- read.csv("202204-divvy-tripdata.csv")
datatrip_2205 <- read.csv("202205-divvy-tripdata.csv")
datatrip_2206 <- read.csv("202206-divvy-tripdata.csv")
datatrip_2207 <- read.csv("202207-divvy-tripdata.csv")
datatrip_2208 <- read.csv("202208-divvy-tripdata.csv")
datatrip_2209 <- read.csv("202209-divvy-tripdata.csv")
datatrip_2210 <- read.csv("202210-divvy-tripdata.csv")
datatrip_2211 <- read.csv("202211-divvy-tripdata.csv")
datatrip_2212 <- read.csv("202212-divvy-tripdata.csv")
datatrip_2301 <- read.csv("202301-divvy-tripdata.csv")
datatrip_2302 <- read.csv("202302-divvy-tripdata.csv")
datatrip_2303 <- read.csv("202303-divvy-tripdata.csv")

## Inspect data frame

colnames(datatrip_2204)
colnames(datatrip_2205)
colnames(datatrip_2206)
colnames(datatrip_2207)
colnames(datatrip_2208)
colnames(datatrip_2209)
colnames(datatrip_2210)
colnames(datatrip_2211)
colnames(datatrip_2212)
colnames(datatrip_2301)
colnames(datatrip_2302)
colnames(datatrip_2303)

str(datatrip_2204)
str(datatrip_2205)
str(datatrip_2206)
str(datatrip_2207)
str(datatrip_2208)
str(datatrip_2209)
str(datatrip_2210)
str(datatrip_2211)
str(datatrip_2212)
str(datatrip_2301)
str(datatrip_2302)
str(datatrip_2303)

View(datatrip_2204)
View(datatrip_2205)
View(datatrip_2206)
View(datatrip_2207)
View(datatrip_2208)
View(datatrip_2209)
View(datatrip_2210)
View(datatrip_2211)
View(datatrip_2212)
View(datatrip_2301)
View(datatrip_2302)
View(datatrip_2303)


### Combine the whole 12 months data

divvy_all_datatrips <- bind_rows(datatrip_2204, datatrip_2205, datatrip_2206, datatrip_2207, datatrip_2208, datatrip_2209, datatrip_2210, datatrip_2211, datatrip_2212, datatrip_2301, datatrip_2302, datatrip_2303)


## Inspect new data frame

str(divvy_all_datatrips)
View(divvy_all_datatrips)
colnames(divvy_all_datatrips)
summary(divvy_all_datatrips)

table(divvy_all_datatrips$member_casual)


## Data Cleaning: remove error or unwanted columns

divvy_all_datatrips <- subset(divvy_all_datatrips, select = -c(start_station_name, start_station_id, end_station_name, end_station_id, start_lat, start_lng, end_lat, end_lng))


## Add date column in data format month, day, and year for analysis 

divvy_all_datatrips$date <- as.Date(divvy_all_datatrips$started_at)
divvy_all_datatrips$month <- format(as.Date(divvy_all_datatrips$date), "%m")
divvy_all_datatrips$day <- format(as.Date(divvy_all_datatrips$date), "%d")
divvy_all_datatrips$year <- format(as.Date(divvy_all_datatrips$date), "%Y")
divvy_all_datatrips$day_of_week <- format(as.Date(divvy_all_datatrips$date), "%A")

## Add new column, ride_length and calculate the ride_length
divvy_all_datatrips$ride_length <- difftime(divvy_all_datatrips$ended_at,divvy_all_datatrips$started_at)

## convert ride_length to numeric data str
is.factor(divvy_all_datatrips$ride_length)
divvy_all_datatrips$ride_length <- as.numeric(as.character(divvy_all_datatrips$ride_length))
is.numeric(divvy_all_datatrips$ride_length)


## Assign the correct order to each day of the week
divvy_all_datatrips$day_of_week <- 
ordered(divvy_all_datatrips$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

divvy_all_datatrips %>%
    group_by(member_casual, day_of_week) %>%
    summarise(number_of_ride = n(), .groups = 'drop') %>%
    arrange(day_of_week)
    
## Assign the correct order to each month of the year
divvy_all_datatrips$month <-
    ordered(divvy_all_datatrips$month, levels = c('04', '05', '06', '07', '08', '09', '10', '11', '12', '01', '02', '03'))    
    
 divvy_all_datatrips %>%
    group_by(member_casual, month) %>%
    summarise(number_of_ride = n(), .groups = 'drop') %>%
    arrange(month)
    
## Aggregate data for analysis
aggregate(divvy_all_datatrips$ride_length ~ divvy_all_datatrips$member_casual, FUN = mean)
aggregate(divvy_all_datatrips$ride_length ~ divvy_all_datatrips$member_casual, FUN = median)
aggregate(divvy_all_datatrips$ride_length ~ divvy_all_datatrips$member_casual, FUN = max)
aggregate(divvy_all_datatrips$ride_length ~ divvy_all_datatrips$member_casual, FUN = min)
mode <- aggregate(day_of_week ~ member_casual, data = divvy_all_datatrips, FUN = mode)
aggregate(ride_length ~ member_casual + month, data = divvy_all_datatrips, FUN = length)

### Ccalculate mean, median, mode, max of ride_lenght to showcase trend
summary(divvy_all_datatrips$ride_length)
mean(divvy_all_datatrips$ride_length)
median(divvy_all_datatrips$ride_length)
max(divvy_all_datatrips$ride_length)
min(divvy_all_datatrips$ride_length)

## read the calculations analysis report tables
mean <- aggregate(divvy_all_datatrips$ride_length ~ divvy_all_datatrips$member_casual + divvy_all_datatrips$day_of_week, FUN = mean)
median <- aggregate(divvy_all_datatrips$ride_length ~ divvy_all_datatrips$member_casual + divvy_all_datatrips$day_of_week, FUN = median)
max <- aggregate(divvy_all_datatrips$ride_length ~ divvy_all_datatrips$member_casual + divvy_all_datatrips$day_of_week, FUN = max)
min <- aggregate(divvy_all_datatrips$ride_length ~ divvy_all_datatrips$member_casual + divvy_all_datatrips$day_of_week, FUN = min)
total <- table(divvy_all_datatrips$member_casual)



aggregate(divvy_all_datatrips$ride_length ~ divvy_all_datatrips$member_casual + divvy_all_datatrips$day_of_week, FUN = mean)
aggregate(divvy_all_datatrips$ride_length ~ divvy_all_datatrips$member_casual + divvy_all_datatrips$day_of_week, FUN = mean)


## calculate and read ride length mode 
mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

mode_agg <- aggregate(divvy_all_datatrips$ride_length ~ divvy_all_datatrips$member_casual + divvy_all_datatrips$day_of_week, FUN = mode)



library(lubridate)

Day of week format

divvy_all_datatrips %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n() 
            ,average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday)
  

## Visualization: some visualization insight before i expoert analysis file report


library(ggplot2)

divvy_all_datatrips %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Average duration of Divvy rides by day of the week and user type",
       x = "Day of the week",
       y = "Average ride duration",
       fill = "User type")


divvy_all_datatrips %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") +
labs(title = "Number of Divvy rides by day of the week and user type",
     x = "Day of the week",
     y = "Number of rides",
     fill = "User type")


## Export file in csv for visualization

write.csv(mean, file = "mean.csv")
write.csv(median, file = "median.csv")
write.csv(max, file = "max.csv")
write.csv(min, file = "min.csv")
write.csv(total, file = "total.csv")
write.csv(lenght, file = "lenght.csv")
write.csv(mode_agg, file = "mode_agg.csv")

Data wiill be export as csv file for visualization... My Visualization for this business task was done with Excel chart and Pivot....

