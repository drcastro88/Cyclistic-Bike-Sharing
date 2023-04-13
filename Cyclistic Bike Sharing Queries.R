##Installing packages
install.packages("tidyverse")
install.packages("lubridate")
install.packages("ggplot2")

##Loading libraries
library("tidyverse")
library("lubridate")
library("ggplot2")

##Getting work directory
getwd()

##Uploading csv files
q2_2019 <- read_csv("Divvy_Trips_2019_Q2.csv")
q3_2019 <- read_csv("Divvy_Trips_2019_Q3.csv")
q4_2019 <- read_csv("Divvy_Trips_2019_Q4.csv")
q1_2020 <- read_csv("Divvy_Trips_2020_Q1.csv")

##Reading column names
colnames(q3_2019)
colnames(q4_2019)
colnames(q2_2019)
colnames(q1_2020)

##Renaming column names for consistency
(q4_2019 <- rename(q4_2019
                   ,ride_id = trip_id
                   ,rideable_type = bikeid 
                   ,started_at = start_time  
                   ,ended_at = end_time  
                   ,start_station_name = from_station_name 
                   ,start_station_id = from_station_id 
                   ,end_station_name = to_station_name 
                   ,end_station_id = to_station_id 
                   ,member_casual = usertype))

(q3_2019 <- rename(q3_2019
                   ,ride_id = trip_id
                   ,rideable_type = bikeid 
                   ,started_at = start_time  
                   ,ended_at = end_time  
                   ,start_station_name = from_station_name 
                   ,start_station_id = from_station_id 
                   ,end_station_name = to_station_name 
                   ,end_station_id = to_station_id 
                   ,member_casual = usertype))

(q2_2019 <- rename(q2_2019
                   ,ride_id = "01 - Rental Details Rental ID"
                   ,rideable_type = "01 - Rental Details Bike ID" 
                   ,started_at = "01 - Rental Details Local Start Time"  
                   ,ended_at = "01 - Rental Details Local End Time"  
                   ,start_station_name = "03 - Rental Start Station Name" 
                   ,start_station_id = "03 - Rental Start Station ID"
                   ,end_station_name = "02 - Rental End Station Name" 
                   ,end_station_id = "02 - Rental End Station ID"
                   ,member_casual = "User Type"))

##Inspecting dataframes
str(q1_2020)
str(q4_2019)
str(q3_2019)
str(q2_2019)

##Converting ride_id and rideables_type to character for stacking
q4_2019 <-  mutate(q4_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
q3_2019 <-  mutate(q3_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
q2_2019 <-  mutate(q2_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type))

##Combining all data into one big data frame
all_trips <- bind_rows(q2_2019, q3_2019, q4_2019, q1_2020)

##Removing lat, long, birthyear, and gender fields
all_trips <- all_trips %>%  
  select(-c(start_lat, start_lng, end_lat, end_lng, birthyear, gender, "01 - Rental Details Duration In Seconds Uncapped", "05 - Member Details Member Birthday Year", "Member Gender", "tripduration"))

##Inspecting new table
colnames(all_trips)
nrow(all_trips)
dim(all_trips)
head(all_trips)
str(all_trips)
summary(all_trips)

##Seeing how many observations fall under each usertype
table(all_trips$member_casual)

##Reassigning Subscriber and Customer to member and casual for consistency
all_trips <-  all_trips %>% 
  mutate(member_casual = recode(member_casual
                                ,"Subscriber" = "member"
                                ,"Customer" = "casual"))

##Checking to make sure the correct number of observations were reassigned
table(all_trips$member_casual)

##Adding columns for date, month, day, and year of each ride
all_trips$date <- as.Date(all_trips$started_at) 
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

##Adding ride_length calculation to all_trips (in seconds)
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)

##Inspecting structure of columns
str(all_trips)

##Converting ride_length to numeric to run calculations on the data
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)

##Removing bad data
##Creating a new dataframe by removing entries where bikes were checked for quality or ride_length was negative
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]

##Calculations of new dataframe
mean(all_trips_v2$ride_length)
median(all_trips_v2$ride_length) 
max(all_trips_v2$ride_length) 
min(all_trips_v2$ride_length)

##Summary of the four lines above
summary(all_trips_v2$ride_length)

##Comparing members and casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

##Average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

##Putting days of week in correct order
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

##Running average ride times by day of week for members vs casual users again
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

##Analyzing ridership data by type and weekday
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, weekday)	

##Calculating most popular start and end stations by member and casual

##Most popular start stations for members
total_start_stations_member <- all_trips_v2 %>% 
  filter(member_casual == 'member') %>% 
  group_by(start_station_name) %>% 
  summarise(number_of_starts = n()) %>% 
  filter(start_station_name != "") %>% 
  arrange(- number_of_starts)

##Most popular end stations for members
total_end_stations_member <- all_trips_v2 %>% 
  filter(member_casual == 'member') %>% 
  group_by(end_station_name) %>% 
  summarise(number_of_ends = n()) %>% 
  filter(end_station_name != "") %>% 
  arrange(- number_of_ends)

##Most popular start stations for casuals
total_start_stations_casual <- all_trips_v2 %>% 
  filter(member_casual == 'casual') %>% 
  group_by(start_station_name) %>% 
  summarise(number_of_starts = n()) %>% 
  filter(start_station_name != "") %>% 
  arrange(- number_of_starts)

##Most popular end stations for casuals
total_end_stations_casual <- all_trips_v2 %>% 
  filter(member_casual == 'casual') %>% 
  group_by(end_station_name) %>% 
  summarise(number_of_ends = n()) %>% 
  filter(end_station_name != "") %>% 
  arrange(- number_of_ends)

##Visualization for the number of rides by rider type
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

##Visualization for average duration
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")

##Making a table for average ride lenth by members vs casual and day of week
counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

##Creating dataframe for total trips per weekday
day_of_week_trips <- all_trips_v2 %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(total_trips = n())

##Creating dataframe for total trips per month - members vs casual
monthly_usage <- all_trips_v2 %>% 
  group_by(member_casual, month) %>% 
  summarise(total_trips = n())

##Saving code as csv file
write.csv(counts, file = "avg_ride_length.csv")
write.csv(all_trips_v2, file = "all_trips_v2.csv")
write.csv(total_start_stations_member, file = "total_start_stations_member.csv")
write.csv(total_end_stations_member, file = "total_end_stations_member.csv")
write.csv(total_start_stations_casual, file = "total_start_stations_casual.csv")
write.csv(total_end_stations_casual, file = "total_end_stations_casual.csv")
write.csv(total_trips_members_vs_casual_day_of_week, file = "total_trips_members_vs_casual_day_of_week.csv")
write.csv(monthly_usage, file = "monthly_usage.csv")
write.csv(day_of_week_trips, file = "day_of_week_trips.csv")
write.csv(time_of_trips, file = "time_of_trips.csv")









