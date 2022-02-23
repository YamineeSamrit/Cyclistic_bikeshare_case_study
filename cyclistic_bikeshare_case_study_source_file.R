# Download and store the data into working directory of the project.

# Loading Data.

## Loading packages for operations.

library(tidyverse)
library(lubridate)
library(janitor)
library(hms)
library(DescTools)
library(scales)

## Loading .csv files into system.

tripdata_202101 <- read_csv("202101-divvy-tripdata.csv")
tripdata_202102 <- read_csv("202102-divvy-tripdata.csv")
tripdata_202103 <- read_csv("202103-divvy-tripdata.csv")
tripdata_202104 <- read_csv("202104-divvy-tripdata.csv")
tripdata_202105 <- read_csv("202105-divvy-tripdata.csv")
tripdata_202106 <- read_csv("202106-divvy-tripdata.csv")
tripdata_202107 <- read_csv("202107-divvy-tripdata.csv")
tripdata_202108 <- read_csv("202108-divvy-tripdata.csv")
tripdata_202109 <- read_csv("202109-divvy-tripdata.csv")
tripdata_202110 <- read_csv("202110-divvy-tripdata.csv")
tripdata_202111 <- read_csv("202111-divvy-tripdata.csv")
tripdata_202112 <- read_csv("202112-divvy-tripdata.csv")


# Preparing Data for Processing.

## Combining them into one.

trip_data <- rbind(tripdata_202101,
                   tripdata_202102,
                   tripdata_202103,
                   tripdata_202104,
                   tripdata_202105,
                   tripdata_202106,
                   tripdata_202107,
                   tripdata_202108,
                   tripdata_202109,
                   tripdata_202110,
                   tripdata_202111,
                   tripdata_202112)

glimpse(trip_data)


# Processing Data for Analysis.

## Removing na values.

trip_data_cleaned <- trip_data[complete.cases(trip_data),]

glimpse(trip_data_cleaned)

## Dropping the trips with start time is greater than end time (add for length =0).

trip_data_cleaned <- trip_data_cleaned %>% 
  filter(trip_data_cleaned$started_at < trip_data_cleaned$ended_at & (trip_data_cleaned$started_at - trip_data_cleaned$ended_at) = 0)

## Adding ride duration with column name trip_length then converting it to hhmmss format.

trip_data_cleaned$trip_length <- trip_data_cleaned$ended_at - trip_data_cleaned$started_at

trip_data_cleaned$trip_length <- hms::hms(seconds_to_period(trip_data_cleaned$trip_length))

## Adding a column as day_of_week to note the day of the week for ride starting day.

trip_data_cleaned$day_of_week <- wday(trip_data_cleaned$started_at, label = TRUE)

## Added new column for month when trip started with name month

trip_data_cleaned$month <- month(trip_data_cleaned$started_at, label = TRUE)


# Analyzing Data.

## To get mean of trip_length.

trip_length_avg <- trip_data_cleaned %>% 
  summarise(mean(trip_length))

## To get maximum of trip_length.

trip_length_max <- trip_data_cleaned %>% 
  summarise(max(trip_length))

## To get the mode of day of week.

mode_day_of_week <- trip_data_cleaned %>% 
  group_by(day_of_week) %>% 
  summarise(Mode(trip_length))

#1 all member casual

## To calculate and visualize number of rides for user type

trip_data_cleaned%>% 
  group_by(member_casual) %>% 
  summarise(number_of_rides_by_user_type = n()) %>% 
  ggplot(aes(x = member_casual, y = number_of_rides_by_user_type, fill = member_casual)) + 
  geom_col(width = 0.2) +
  labs(title = "Number of rides by user type") +
  ylab("Number of rides") +
  xlab("User type") +
  scale_y_continuous(labels = comma)

## To calculate and visualize average trip length for user type

trip_data_cleaned%>% 
  group_by(member_casual) %>% 
  summarise(average_trip_length = mean(trip_length)) %>% 
  ggplot(aes(x = member_casual, y = average_trip_length,fill = member_casual)) + 
  geom_col(width = 0.2) +
  labs(title = "Average Ride Duration by user type") +
  ylab("Average Ride Duration in secs") +
  xlab("User type") +
  scale_y_continuous(labels = comma)

#2 all day of week

## To calculate number of riders for users by day of week 

trip_data_cleaned %>% 
  group_by(day_of_week) %>% 
  summarise(number_of_rides=n()) %>% 
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = "#CC6633")) + 
  geom_col(width = 0.2) +
  labs(title = "Number of Rides by day of week") +
  ylab("Number of rides") +
  xlab("Day of the week") +
  scale_y_continuous(labels = comma) 

## To calculate and visualize average trip length for user by day of the week

trip_data_cleaned%>% 
  group_by(day_of_week) %>% 
  summarise(average_trip_length = mean(trip_length)) %>% 
  ggplot(aes(x = day_of_week, y = average_trip_length, fill = "#CC6633")) + 
  geom_col(width = 0.2) +
  labs(title = "Average Ride Duration by day of week") +
  ylab("Average Ride Duration in secs") +
  xlab("Day of the week") +
  scale_y_continuous(labels = comma)

#3 day of week member casual

## To calculate and visualize number of rides for rider type by day of week

trip_data_cleaned %>% 
  group_by(member_casual, day_of_week) %>% 
  summarize(number_of_rides = n()) %>% 
  arrange(member_casual, day_of_week) %>% 
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("#CC6633","#6699CC")) +
  labs(title = "Number of Rides for User Type by days of week") +
  ylab("Number of Rides") +
  xlab("Day of Week") +
  scale_y_continuous(labels = comma)

## To calculate and visualize average trip length for rider type by day of week

trip_data_cleaned %>% 
  group_by(member_casual, day_of_week) %>% 
  summarize(average_duration = mean(trip_length)) %>% 
  arrange(member_casual, day_of_week) %>% 
  ggplot(aes(x = day_of_week, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("#CC6633","#6699CC")) +
  labs(title = "Average Ride duration for User Type by days of week") +
  ylab("Average Ride Duration in secs") +
  xlab("Day of Week")+
  scale_y_continuous(labels = comma )

#4 month member casual

## To calculate and visualize number of rides for rider type by month

trip_data_cleaned %>% 
  group_by(member_casual, month) %>% 
  summarize(number_of_rides = n()) %>% 
  arrange(member_casual, month) %>% 
  ggplot(aes(x = month, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("#CC6633","#6699CC")) +
  labs(title = "Number of Rides for User Type by month") +
  ylab("Number of Rides") +
  xlab("List of months") +
  scale_y_continuous(labels = comma)

## To calculate and visualize average trip length for rider type by month

trip_data_cleaned %>% 
  group_by(member_casual, month) %>% 
  summarize(average_duration = mean(trip_length)) %>% 
  arrange(member_casual, month) %>% 
  ggplot(aes(x = month, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("#CC6633","#6699CC")) +
  labs(title = "Average Ride Duration for User Type by month") +
  ylab("Average Ride Duration in secs") +
  xlab("List of months") +
  scale_y_continuous(labels = comma )


## Files for Tableau visualization

write.csv(trip_data_cleaned, file = "all_trips.csv", row.names = FALSE)

