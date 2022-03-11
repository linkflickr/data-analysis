install.packages("readxl")
install.packages("tidyverse")
install.packages("ggplot2")

library(readxl)
library(tidyverse)
library(lubridate)
library(ggplot2)

path <- "~/Downloads/Project data analyst"

getwd()

setwd(path)

sheet = excel_sheets("Cyclist_rides_2021.xlsx")

all_trips = lapply(setNames(sheet, sheet), 
                    function(x) read_excel ("Cyclist_rides_2021.xlsx", sheet=x))

all_trips = bind_rows(all_trips, .id = "Sheet")

print(all_trips)

colnames(all_trips)

all_trips$Sheet <- NULL

table(all_trips['member_casual']) 

all_trips <- all_trips %>% 
  select(-c(start_lat, start_lng, end_lat, end_lng))

all_trips$date <- as.Date(all_trips$started_at)

all_trips$month <- format(as.Date(all_trips$date), "%m")

all_trips$day <- format(as.Date(all_trips$date), "%d")

all_trips$year <- format(as.Date(all_trips$date), "%Y")

all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

all_trips$ride_length <- difftime(all_trips$ended_at, all_trips$started_at)

str(all_trips)

all_trips_v2 <- subset(all_trips, ride_length >0)

mean(all_trips_v2$ride_length)
median(all_trips_v2$ride_length)
max(all_trips_v2$ride_length)
min(all_trips_v2$ride_length)

summary(all_trips_v2$ride_length)

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual 
          + all_trips_v2$day_of_week, FUN = mean)

all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week,levels=c("Sunday",
"Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual 
          + all_trips_v2$day_of_week, FUN = mean)

all_trips_v2 %>% mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n() 
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)

all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n() 
            ,average_duration =mean(ride_length)) %>%
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")
  
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n() 
            ,average_duration =mean(ride_length)) %>%
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")

counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual +
                      all_trips_v2$day_of_week, FUN = mean)

stats <- aggregate(all_trips_v2$number_of_ride)

write.csv(counts, file = '~/Documents/avg_ride_length.csv')