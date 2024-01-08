library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
#importing data from 11-2022 to 10-2023
tripdata_2022_11 <- read.csv("C:/Users/ejspe/OneDrive/Desktop/Capstone_Cycles/202211-divvy-tripdata.csv")
tripdata_2022_12 <- read.csv("C:/Users/ejspe/OneDrive/Desktop/Capstone_Cycles/202212-divvy-tripdata.csv")
tripdata_2023_01 <- read.csv("C:/Users/ejspe/OneDrive/Desktop/Capstone_Cycles/202301-divvy-tripdata.csv")
tripdata_2023_02 <- read.csv("C:/Users/ejspe/OneDrive/Desktop/Capstone_Cycles/202302-divvy-tripdata.csv")
tripdata_2023_03 <- read.csv("C:/Users/ejspe/OneDrive/Desktop/Capstone_Cycles/202303-divvy-tripdata.csv")
tripdata_2023_04 <- read.csv("C:/Users/ejspe/OneDrive/Desktop/Capstone_Cycles/202304-divvy-tripdata.csv")
tripdata_2023_05 <- read.csv("C:/Users/ejspe/OneDrive/Desktop/Capstone_Cycles/202305-divvy-tripdata.csv")
tripdata_2023_06 <- read.csv("C:/Users/ejspe/OneDrive/Desktop/Capstone_Cycles/202306-divvy-tripdata.csv")
tripdata_2023_07 <- read.csv("C:/Users/ejspe/OneDrive/Desktop/Capstone_Cycles/202307-divvy-tripdata.csv")
tripdata_2023_08 <- read.csv("C:/Users/ejspe/OneDrive/Desktop/Capstone_Cycles/202308-divvy-tripdata.csv")
tripdata_2023_09 <- read.csv("C:/Users/ejspe/OneDrive/Desktop/Capstone_Cycles/202309-divvy-tripdata.csv")
tripdata_2023_10 <- read.csv("C:/Users/ejspe/OneDrive/Desktop/Capstone_Cycles/202310-divvy-tripdata.csv")

#checking column names are all matching
colnames(tripdata_2022_11)
colnames(tripdata_2022_12)
colnames(tripdata_2023_01)
colnames(tripdata_2023_02)
colnames(tripdata_2023_03)
colnames(tripdata_2023_04)
colnames(tripdata_2023_05)
colnames(tripdata_2023_06)
colnames(tripdata_2023_07)
colnames(tripdata_2023_08)
colnames(tripdata_2023_09)
colnames(tripdata_2023_10)

#check the data types in each file
str(tripdata_2022_11)
str(tripdata_2022_12)
str(tripdata_2023_01)
str(tripdata_2023_02)
str(tripdata_2023_03)
str(tripdata_2023_04)
str(tripdata_2023_05)
str(tripdata_2023_06)
str(tripdata_2023_07)
str(tripdata_2023_08)
str(tripdata_2023_09)
str(tripdata_2023_10)

#Combine all the data sets
trips_whole_yr <-bind_rows(tripdata_2022_11, tripdata_2022_12,tripdata_2023_01,
                           tripdata_2023_02, tripdata_2023_03,tripdata_2023_04, tripdata_2023_05, tripdata_2023_06,
                           tripdata_2023_07,tripdata_2023_08,tripdata_2023_09, tripdata_2023_10)
#check if combined files show same column names
colnames(trips_whole_yr) 
#checking data for general info
nrow(trips_whole_yr) # number rows
dim(trips_whole_yr) 
head(trips_whole_yr)#shows 1st 6 rows
str(trips_whole_yr) #column data type here
summary(trips_whole_yr) #ride_length row: 5652827

colSums(is.na(trips_whole_yr))
min(trips_whole_yr$ride_length)
max(trips_whole_yr$ride_length)#[1] "9:59:58"


#clean data
# deleting rows where ride is more than 1 day and less than 0 secs
trips_whole_yr_rev <- trips_whole_yr[!(trips_whole_yr$ride_length <= 0 | trips_whole_yr$ride_length > 1440),] 
trips_whole_yr_rev$month <- format(as.Date(trips_whole_yr_rev$date), "%m")
trips_whole_yr_rev$day_of_week <- format(as.Date(trips_whole_yr_rev$day_of_week), "%A")
min(trips_whole_yr_rev$ride_length)
dim(trips_whole_yr_rev) #dimension of data [1] 5619463      19
View(trips_whole_yr_rev) 
summary(trips_whole_yr_rev)


View(trips_whole_yr_rev)
#Analyze 
#Assigning the day to each day in week
trips_whole_yr_rev$day_of_week <- 
  ordered(trips_whole_yr_rev$day_of_week, levels = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))

trips_whole_yr_rev %>%
  group_by(member_casual, day_of_week) %>%
  summarise(number_of_ride = n()) %>%
  arrange(day_of_week)


trips_whole_yr_rev$month <- 
  ordered(trips_whole_yr_rev$month, levels = c('11', '12', '01', '02', '03', '04', '05','06','07','08','09','10'))

trips_whole_yr_rev %>%
  group_by(member_casual, month) %>%
  summarise(number_of_ride = n(),) %>%
  arrange(month) %>%print(n=27)

# seeing the difference between how many casual and members ride at different months of the year
trips_whole_yr_rev %>%
  group_by(member_casual, month) %>%
  summarise(number_of_ride = n()) %>%
  ggplot(mapping=aes(x= month,y=number_of_ride,fill= member_casual,label = number_of_ride)) +geom_bar(position = "dodge", stat = "identity")+  geom_text(size = 3, position = position_stack(vjust = 0.25))

# seeing the difference between how many casual and members ride at different times of the week
trips_whole_yr_rev %>%
  group_by(member_casual, day_of_week) %>%
  summarise(number_of_ride = n()) %>%
  ggplot(mapping=aes(x= day_of_week,y=number_of_ride,fill= member_casual,label = number_of_ride)) +geom_bar(position = "dodge", stat = "identity") +geom_text(size = 3, position = position_stack(vjust = 0.25))

 

#seeing how long members ride in comparison to casual in week
trips_whole_yr_rev %>%
  group_by(member_casual, day_of_week) %>%
  summarise(average_ride_length = n()) %>%
  ggplot(mapping=aes(x= day_of_week,y=average_ride_length,fill= member_casual,label = average_ride_length)) 
+geom_bar(position = "dodge", stat = "identity") +geom_text(size = 3, position = position_stack(vjust = 0.25))


#seeing how long members ride in comparison to casual each month
trips_whole_yr_rev %>%
  group_by(member_casual, month) %>%
  summarise(average_ride_length = n()) %>%
  ggplot(mapping=aes(x= month,y=average_ride_length,fill= member_casual, label = average_ride_length)) 
+geom_bar(position = "dodge", stat = "identity") +geom_text(size = 3, position = position_stack(vjust = 0.25))


                                                                                                                                

