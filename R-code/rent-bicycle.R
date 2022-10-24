#collect the data
q2_2019 <- read_csv("Divvy_Trips_2019_Q2.csv")
q3_2019 <- read_csv("Divvy_Trips_2019_Q3.csv")
q4_2019 <- read_csv("Divvy_Trips_2019_Q4.csv")
q1_2020 <- read_csv("Divvy_Trips_2020_Q1.csv")

#change the variable names
(q4_2019 <- rename(q4_2019, "ride_id"="trip_id", "rideable_type" = "bikeid",
                   "started_at" = "start_time",
                   "ended_at" = "end_time",
                   "start_station_name" = "from_station_name",
                   "start_station_id" = "from_station_id",
                   "end_station_name" = "to_station_name",
                   "end_station_id" = "to_station_id",
                   "member_casual" = "usertype"
))

(q3_2019 <- rename(q3_2019,
                   "ride_id" = "trip_id",
                   "rideable_type" = "bikeid",
                   "started_at" = "start_time",
                   "ended_at" = "end_time",
                   "start_station_name" = "from_station_name",
                   "start_station_id" = "from_station_id",
                   "end_station_name" = "to_station_name",
                   "end_station_id" = "to_station_id",
                   "member_casual" = "usertype"))

(q2_2019 <- rename(q2_2019,
                   "ride_id" = "01 - Rental Details Rental ID",
                   "rideable_type" = "01 - Rental Details Bike ID",
                   "started_at" = "01 - Rental Details Local Start Time",
                   "ended_at" = "01 - Rental Details Local End Time",
                   "start_station_name" = "03 - Rental Start Station Name",
                   "start_station_id" = "03 - Rental Start Station ID",
                   "end_station_name" = "02 - Rental End Station Name",
                   "end_station_id" = "02 - Rental End Station ID",
                   "member_casual" = "User Type"))

#change ride_id dan rideable_type to character
q4_2019 <-  mutate(q4_2019, ride_id = as.character(ride_id),
                   rideable_type = as.character(rideable_type)) 

q3_2019 <-  mutate(q3_2019, ride_id = as.character(ride_id),
                   rideable_type = as.character(rideable_type))

q2_2019 <-  mutate(q2_2019, ride_id = as.character(ride_id),
                   rideable_type = as.character(rideable_type)) 

#bind all to a df
all_trip <-bind_rows(q2_2019,q3_2019,q4_2019,q1_2020)

#delete the private data of customer
all_trip <- all_trip %>%
  select(-c("start_lat", "start_lng", "end_lat", "end_lng", "birthyear", "gender", 
            "01 - Rental Details Duration In Seconds Uncapped", 
            "05 - Member Details Member Birthday Year", 
            "Member Gender", 
            "tripduration"))

#clean up the inconsitency of data in member_casual
all_trip <- all_trip%>%
  mutate(member_casual=recode(member_casual,
                              "Subscriber" = "member",
                              "Customer" = "casual"))

#add col date, month, day, and year in every data
all_trip$date <- as.Date(all_trip$started_at)
all_trip$month <- format(as.Date(all_trip$date), "%m")
all_trip$day <- format(as.Date(all_trip$date), "%d")
all_trip$year <- format(as.Date(all_trip$date), "%Y")
all_trip$day_of_week <- format(as.Date(all_trip$date), "%A")

all_trip$ride_length <- difftime(all_trip$ended_at, all_trip$started_at)

#change ride_length to numeric 
is.factor(all_trip$ride_length)
all_trip$ride_length <- as.numeric(as.character(all_trip$ride_length))
is.numeric(all_trip$ride_length)

#delete bad data
all_trip_v2 <- all_trip[!(all_trip$start_station_name == "HQ QR" | all_trip$ride_length<0),]

#summary
summary(all_trip_v2$ride_length)

#comparing the ride_length of members
aggregate(all_trip_v2$ride_length ~ all_trip_v2$member_casual, FUN = mean)
aggregate(all_trip_v2$ride_length ~ all_trip_v2$member_casual, FUN = median)
aggregate(all_trip_v2$ride_length ~ all_trip_v2$member_casual, FUN = max)
aggregate(all_trip_v2$ride_length ~ all_trip_v2$member_casual, FUN = min)

#average ride length
aggregate(all_trip_v2$ride_length ~ all_trip_v2$member_casual + 
            all_trip_v2$day_of_week, FUN = mean)

#the weekday aren't ordered. let's order them
all_trip_v2$day_of_week <- ordered(all_trip_v2$day_of_week, 
                                   levels=c("Minggu", "Senin", "Selasa", "Rabu", "Kamis", "Jumat", "Sabtu"))


#average ride length with weekdays ordered
aggregate(all_trip_v2$ride_length ~ all_trip_v2$member_casual + 
            all_trip_v2$day_of_week, FUN = mean)

#analize the data of ride_length group by member and weekday
all_trip_v2 %>% 
  mutate(weekday = wday(started_at, label=TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarize(number_of_rides = n(), average_duration = mean(ride_length)) %>% arrange(member_casual, weekday)

#data viz
all_trip_v2 %>% 
  mutate(weekday = wday(started_at, label=TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarize(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)%>%
  ggplot(aes(x=weekday, y=number_of_rides, fill=member_casual))+
  geom_col(position="dodge")

all_trip_v2 %>% 
  mutate(weekday = wday(started_at, label=TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarize(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)%>%
  ggplot(aes(x=weekday, y=average_duration, fill=member_casual))+
  geom_col(position="dodge")

#simpan
counts <- aggregate(all_trip_v2$ride_length ~ all_trip_v2$member_casual + all_trip_v2$day_of_week, FUN = mean)
write_csv(counts, file = "./avg_ride_length.csv")