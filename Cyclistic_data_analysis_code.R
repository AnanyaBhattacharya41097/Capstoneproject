#Loading necessary libraries

library(tidyverse)
library(ggplot2)
library(lubridate)
library(dplyr)
library(readr)
library(janitor)
library(data.table)
library(tidyr)
library(anytime)
getwd() #to check the path
setwd("C:\\Users\\anabh\\Documents\\Google data analytics\\Capstone project") #to set the path

#importing datas ets

apr20 <- read_csv("C:\\Users\\anabh\\Documents\\Google data analytics\\Capstone project\\tripdata_april_2020.csv")
may20 <- read_csv("C:\\Users\\anabh\\Documents\\Google data analytics\\Capstone project\\tripdata_may_2020.csv")
jun20 <- read_csv("C:\\Users\\anabh\\Documents\\Google data analytics\\Capstone project\\tripdata_june_2020.csv")
jul20 <- read_csv("C:\\Users\\anabh\\Documents\\Google data analytics\\Capstone project\\tripdata_july_2020.csv")
aug20 <- read_csv("C:\\Users\\anabh\\Documents\\Google data analytics\\Capstone project\\tripdata_august_2020.csv")
sept20 <- read_csv("C:\\Users\\anabh\\Documents\\Google data analytics\\Capstone project\\tripdata_september_2020.csv")
oct20 <- read_csv("C:\\Users\\anabh\\Documents\\Google data analytics\\Capstone project\\tripdata_october_2020.csv")
nov20 <- read_csv("C:\\Users\\anabh\\Documents\\Google data analytics\\Capstone project\\tripdata_november_2020.csv")
dec20 <- read_csv("C:\\Users\\anabh\\Documents\\Google data analytics\\Capstone project\\tripdata_december_2020.csv")
jan21 <- read_csv("C:\\Users\\anabh\\Documents\\Google data analytics\\Capstone project\\tripdata_january_2021.csv")
feb21 <- read_csv("C:\\Users\\anabh\\Documents\\Google data analytics\\Capstone project\\tripdata_february_2021.csv")
mar21 <- read_csv("C:\\Users\\anabh\\Documents\\Google data analytics\\Capstone project\\tripdata_march__2021.csv")
apr21 <- read_csv("C:\\Users\\anabh\\Documents\\Google data analytics\\Capstone project\\tripdata_april_2021.csv")
may21 <- read_csv("C:\\Users\\anabh\\Documents\\Google data analytics\\Capstone project\\tripdata_may_2021.csv")
jun21 <- read_csv("C:\\Users\\anabh\\Documents\\Google data analytics\\Capstone project\\tripdata_june_2021.csv")
jul21 <- read_csv("C:\\Users\\anabh\\Documents\\Google data analytics\\Capstone project\\tripdata_july_2021.csv")
aug21 <- read_csv("C:\\Users\\anabh\\Documents\\Google data analytics\\Capstone project\\tripdata_august_2021.csv")


#To check the consistency of data sets, columns names of each data sets are checked ans matched

colnames(apr20)
colnames(may20)
colnames(jun20)
colnames(jul20)
colnames(aug20)
colnames(sept20)
colnames(oct20)
colnames(nov20)
colnames(dec20)
colnames(jan21)
colnames(feb21)
colnames(mar21)
colnames(apr21)
colnames(may21)
colnames(jun21)
colnames(jul21)
colnames(aug21)


#Summary of each data sets are checked for datatype consistencies in each columns

summary(apr20)
summary(may20)
summary(jun20)
summary(jul20)
summary(aug20)
summary(sept20)
summary(oct20)
summary(nov20)
summary(dec20)
summary(jan21)
summary(feb21)
summary(mar21)
summary(apr21)
summary(may21)
summary(jun21)
summary(jul21)
summary(aug21)


#Note: The data sets of all the months were filtered and sorted during the prepare phase, and irrelevant columns were removed in Excel.
#It was observed that 2 extra columns still existed in some of the data sets, and were not consistent. These extra columns are removed from the data sets using the following function.

may20_tripdata = subset(may20, select = -c(start_station_id,end_station_id))
summary(may20_tripdata)

jun20_tripdata = subset(jun20, select = -c(start_station_id,end_station_id))
summary(jun20_tripdata)

jul20_tripdata = subset(jul20, select = -c(start_station_id,end_station_id))
summary(jul20_tripdata)

sept20_tripdata = subset(sept20, select = -c(start_station_id,end_station_id))
summary(sept20_tripdata)

oct20_tripdata = subset(oct20, select = -c(start_station_id,end_station_id))
summary(oct20_tripdata)

nov20_tripdata = subset(nov20, select = -c(start_station_id,end_station_id))
summary(nov20_tripdata)

#combining all data sets into one data frame
trips <- bind_rows(apr20, may20_tripdata, jun20_tripdata, jul20_tripdata, aug20, sept20_tripdata, oct20_tripdata, nov20_tripdata, dec20, jan21, feb21, mar21, apr21, may21, jun21, jul21, aug21)

#check structure of the new data frame
str(trips)

#Viewing the data frame
View(trips)

#Combine membership_status and member_casual into 1 columns named customer_type
trips <- trips %>% mutate(customer_type = coalesce(membership_status, member_casual))

#Remove the 2 columns and store it as a new data frame
trips_new <- subset(trips, select = -c(membership_status,member_casual) )


#Rename column names for convenience
trips_new <- trips_new %>%
  rename(ride_type = rideable_type, 
         start_time = started_at,
         end_time = ended_at)

#View first few rows of the data set
glimpse(trips_new)

#View the new data set
View(trips_new)

#Set the start time to mm-dd-yyyy format to standardize the start_time column
trips_new$formatted <- as.Date(trips_new$start_time, format = "%m/%d/%y")


#Save the start_time column including hours and minutes in a new column named formattedstarttime. For this the mdy_hm() function is used
trips_new$formattedstarttime <- mdy_hm(trips_new$start_time)

#Save the end_time column including hours and minutes in a new column named formattedendtime
trips_new$formattedendtime <- mdy_hm(trips_new$end_time)

#calculate the trip duration and store it in a new column
trips_new$trip_duration <- (as.double(difftime(trips_new$formattedendtime, trips_new$formattedstarttime)))/60

glimpse(trips_new)

#create a new column which shows the days of the week according to the dates, using format() and as.Date()
trips_new$day_of_the_week <- format(as.Date(trips_new$formattedstarttime),'%a')

#Create a column for storing abbreviations of month and year from the formatted start time.
#Note: The elements of formatted column were extracted first for standardization and then it was set to formattedstarttime. This was done to prevent NA values in the month column 
trips_new$month <- format(trips_new$formatted,'%b_%y')
trips_new$month <- format(trips_new$formattedstarttime,'%b_%y')

#create a column for showing time 
#First the time is converted to a character vector, removing all the dates. 
#The time is then converted back to POSIXct, including only the hours-minutes-seconds.
trips_new$time <- format(trips_new$formattedstarttime, format = "%H:%M")
trips_new$time <- as.POSIXct(trips_new$time, format = "%H:%M")
View(trips_new)

# check the dataframe
glimpse(trips_new)


#checking for trip lengths less than 0
nrow(subset(trips_new,trip_duration < 0))

#checking for test rides that were made by company for quality checks
nrow(subset(trips_new, start_station_name %like% "TEST"))
nrow(subset(trips_new, start_station_name %like% "test"))
nrow(subset(trips_new, start_station_name %like% "Test"))

#remove test rides
trips_new[!((trips_new$start_station_name %like% "TEST" | trips_new$start_station_name %like% "test")),]

#check dataframe
glimpse(trips_new)

View(trips_new)


# checking count of distinct values
table(trips_new$customer_type)
#aggregating total trip duration by customer type
setNames(aggregate(trip_duration ~ customer_type, trips_new, sum), c("customer_type", "total_trip_duration(mins)"))

# statistical summary of trip_duration for all trips
summary(trips_new$trip_duration)



#statistical summary of trip_duration by customer_type
trips_new %>%
  group_by(customer_type) %>%
  summarise(min_trip_duration = min(trip_duration),max_trip_duration = max(trip_duration),
            median_trip_duration = median(trip_duration), mean_trip_duration = mean(trip_duration))


# fix the order for the day_of_the_week and month variable so that they show up in the same sequence in output tables and visualizations
trips_new$day_of_the_week <- ordered(trips_new$day_of_the_week, levels=c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
trips_new$month <- ordered(trips_new$month, levels=c("Apr_20", "May_20", "Jun_20", "Jul_20", "Aug_20", "Sep_20", "Oct_20",
                                                           "Nov_20", "Dec_20", "Jan_21", "Feb_21", "Mar_21", "Apr_21", "May_21", "Jun_21", "Jul_21", "Aug_21"))


#Set the negative trip durations to positive and store it in a new column
trips_new$positive_trip_duration = abs(trips_new$trip_duration)

#remove the trip_duration column with negative values to simplify the data set
trips_new <- subset(trips_new, select = -c(trip_duration))

View(trips_new)

glimpse(trips_new)

#statistical summary of trip duration
summary(trips_new$positive_trip_duration)

#statistical summary of trip duration by customer type and day of week
trips_new %>% 
  group_by(customer_type, day_of_the_week) %>%  
  summarise(number_of_rides = n(),average_duration_mins = mean(positive_trip_duration)) %>% 
  arrange(customer_type, desc(number_of_rides))

#Visualize to compare number of rides on each day of week to check total trip duration. Bar charts are used here to get better insights
trips_new %>%  
  group_by(customer_type, day_of_the_week) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(customer_type, day_of_the_week)  %>% 
  ggplot(aes(x = day_of_the_week, y = number_of_rides, fill = customer_type)) +
  labs(title ="Total trips by customer type Vs. Day of the week") +
  geom_col(width=0.5, position = position_dodge(width=0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))


#check statistical summary of trip duration by customer type and month
trips_new %>% 
  group_by(customer_type, month) %>%  
  summarise(number_of_rides = n(),`average_duration_(mins)` = mean(positive_trip_duration)) %>% 
  arrange(customer_type,desc(number_of_rides))

#Visualize to compare number of rides in each months of 2020 and 2021 and check total trip duration
trips_new %>%  
  group_by(customer_type, month) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(customer_type, month)  %>% 
  ggplot(aes(x = month, y = number_of_rides, fill = customer_type)) +
  labs(title ="Total trips by customer type Vs. Month") +
  theme(axis.text.x = element_text(angle = 30)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

#Visualizaton of average trip duration by customer type on each day of the week
trips_new %>%  
  group_by(customer_type, day_of_the_week) %>% 
  summarise(average_trip_duration = mean(positive_trip_duration)) %>%
  ggplot(aes(x = day_of_the_week, y = average_trip_duration, fill = customer_type)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) + 
  labs(title ="Average trip duration by customer type Vs. Day of the week")

#Visualizaton of average trip duration by customer type Vs. month
trips_new %>%  
  group_by(customer_type, month) %>% 
  summarise(average_trip_duration = mean(positive_trip_duration)) %>%
  ggplot(aes(x = month, y = average_trip_duration, fill = customer_type)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) + 
  labs(title ="Average trip duration by customer type Vs. Month") +
  theme(axis.text.x = element_text(angle = 30))


View(trips_new)




#Visualizaton of bike demand over 24 hr period (a day)
trips_new %>%  
  group_by(customer_type, time) %>% 
  summarise(number_of_trips = n()) %>%
  ggplot(aes(x = time, y = number_of_trips, color = customer_type, group = customer_type)) +
  geom_line() +
  scale_x_datetime(date_breaks = "1 hour", minor_breaks = NULL,
                   date_labels = "%H:%M", expand = c(0,0)) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title ="Demand over 24 hours of a day", x = "Time of the day")

#Visualizaton of ride type Vs. number of trips by customer type
trips_new %>%
  group_by(ride_type, customer_type) %>%
  summarise(number_of_trips = n()) %>%  
  ggplot(aes(x= ride_type, y=number_of_trips, fill= customer_type))+
  geom_bar(stat='identity') +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  labs(title ="Ride type Vs. Number of trips")

#Creating a csv file of the clean data for futher analysis or visualizations in other tools like SQL, Tableau, Power BI, etc.
write.csv(trips_new, file = 'C:\\Users\\anabh\\Documents\\Google data analytics\\Capstone project\\trips_new.csv')

clean_data_new <- aggregate(trips_new$positive_trip_duration ~ trips_new$customer_type + trips_new$day_of_the_week + trips_new$month + trips_new$time + trips_new$ride_type + trips_new$start_station_name, FUN = mean)
shuffled_data = clean_data_new[sample(1:nrow(clean_data_new)), ]
glimpse(shuffled_data)
write.csv(shuffled_data, "New Clean data.csv", row.names = F)























