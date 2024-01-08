# Case Study 1: How does a bike-share navigate speedy success?
## Table of Contents
 - [Project Overview](#project-overview)
 - [Section 1: Cleaning and Preparing Data](#section-1-cleaning-and-preparing-data)
 - [Section 2: Processing Data](#section-2-processing-data)
 - [Section 3: Analyzing Data](#section-3-analyzing-data)
 - [Section 4: Visualizing Data](#section-4-visualizing-data)
 - [Section 5: Conclusion](#section-5-conclusion)

## Project Overview
This project studies a company called Cyclist, which provides a bike sharing services with 5,800 bicycles and 600
docking stations.   
The goal of this project is to analyze the data and use our insights to create a marketing strategy for converting casual riders into 
annual members.  
## Section One: Preparing and Cleaning Data
In this section, I started by importing data from 11-2022 to 10-2023 that I was provided with to get a year's worth of data to analyze

```{r Import data}
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


```
Next, I used the colnames() function for each file to see if the column names are all matching before combining.
```{r Check Columns}
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
```
After assuring all of the files matched, I combined them into one variable for easier analysis.

```{r Combine Excel Sheets}
trips_whole_yr <-bind_rows(tripdata_2022_11, tripdata_2022_12,tripdata_2023_01,
                           tripdata_2023_02, tripdata_2023_03,tripdata_2023_04, tripdata_2023_05, tripdata_2023_06,
                           tripdata_2023_07,tripdata_2023_08,tripdata_2023_09, tripdata_2023_10)
```
I then start the cleaning process by deleting rows where the ride length is more than 1 day and less than 0 seconds to illustrate more accurate results.
```{r Clean data}
trips_whole_yr_rev <- trips_whole_yr[!(trips_whole_yr$ride_length <= 0 | trips_whole_yr$ride_length > 1440),] 
````
To conclude this section, I added a couple more variables for more efficient analysis; Days of week and month. 
```{r Process data}
trips_whole_yr_rev$month <- format(as.Date(trips_whole_yr_rev$date), "%m")
trips_whole_yr_rev$day_of_week <- format(as.Date(trips_whole_yr_rev$day_of_week), "%A")
```
## Section 2: Processing Data
After the cleaning of the data, we can process the data to see if it is ready for analysis. I start by checking the minimun and maximun ride_length to show that the data 
will only shows times above 0 seconds and less than 24 hours. Hastags in the the following code are the outputs execited
```{r checking data}
min(trips_whole_yr_rev$ride_length) #[1] "0:00:00"
max(trips_whole_yr_rev$ride_length#[1] "14:59:56"
```
I also went to see the how many columns and rows are in the table by the following:
```{r dimensions}
dim(trips_whole_yr_rev) #dimension of data [1] 5619463      19
```
Before ending this section, I made sure to check the summary of the data to see if the data types of each column are what they need to be for analysis. 
```{r sum}
summary(trips_whole_yr_rev)
```
This function also provided additional information about each category such as variable type, average, etc. 
```{r output sum}
 ride_id          rideable_type       started_at          ended_at         start_station_name start_station_id   end_station_name  
 Length:5619463     Length:5619463     Length:5619463     Length:5619463     Length:5619463     Length:5619463     Length:5619463    
 Class :character   Class :character   Class :character   Class :character   Class :character   Class :character   Class :character  
 Mode  :character   Mode  :character   Mode  :character   Mode  :character   Mode  :character   Mode  :character   Mode  :character  
                                                                                                                                     
                                                                                                                                     
                                                                                                                                     
                                                                                                                                     
 end_station_id       start_lat       start_lng         end_lat         end_lng       member_casual      ride_length           day_of_week    
 Length:5619463     Min.   :41.63   Min.   :-87.94   Min.   : 0.00   Min.   :-88.16   Length:5619463     Length:5619463     Monday   :826893  
 Class :character   1st Qu.:41.88   1st Qu.:-87.66   1st Qu.:41.88   1st Qu.:-87.66   Class :character   Class :character   Tuesday  :843649  
 Mode  :character   Median :41.90   Median :-87.64   Median :41.90   Median :-87.64   Mode  :character   Mode  :character   Wednesday:824979  
                    Mean   :41.90   Mean   :-87.65   Mean   :41.90   Mean   :-87.65                                         Thursday :851324  
                    3rd Qu.:41.93   3rd Qu.:-87.63   3rd Qu.:41.93   3rd Qu.:-87.63                                         Friday   :724046  
                    Max.   :42.07   Max.   :-87.46   Max.   :42.18   Max.   :  0.00                                         Saturday :717348  
                                                     NA's   :5613    NA's   :5613                                           Sunday   :831224  
     month              day             year          date          
 08     : 766322   Min.   : 1.00   Min.   :2022   Length:5619463    
 07     : 761722   1st Qu.: 8.00   1st Qu.:2023   Class :character  
 06     : 714645   Median :15.00   Median :2023   Mode  :character  
 09     : 662194   Mean   :15.48   Mean   :2023                     
 05     : 600974   3rd Qu.:23.00   3rd Qu.:2023                     
 10     : 534447   Max.   :31.00   Max.   :2023   
 ```
## Section 3: Analyzing Data
I begin my analysis by converting all of the day_of_week variables from a number label to the actual day of the week with the ordered function. this is so I'm able
to analyze different concepts based on the day of the week

```{r Categorize data by day of week}
trips_whole_yr_rev$day_of_week <- 
  ordered(trips_whole_yr_rev$day_of_week, levels = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))
trips_whole_yr_rev %>%
  group_by(member_casual, day_of_week) %>%
  summarise(number_of_ride = n()) %>%
  arrange(day_of_week)
```
The output shows how many casual and full-time members ride the bikes from Cyclist on each day of the week. 
output
```{r output of data per week}
  member_casual day_of_week number_of_ride
   <chr>         <ord>                <int>
 1 casual        Monday              249496
 2 member        Monday              577397
 3 casual        Tuesday             268395
 4 member        Tuesday             575254
 5 casual        Wednesday           307686
 6 member        Wednesday           517293
 7 casual        Thursday            396053
 8 member        Thursday            455271
 9 casual        Friday              326889
10 member        Friday              397157
11 casual        Saturday            231038
12 member        Saturday            486310
13 casual        Sunday              249363
14 member        Sunday              581861
```
```{r Categorize data by month}

trips_whole_yr_rev$month <- 
  ordered(trips_whole_yr_rev$month, levels = c('11', '12', '01', '02', '03', '04', '05','06','07','08','09','10'))
```
The output shows how many casual and full-time members ride the bikes from Cyclist per month.

Output:
```{r output for data by month}
 member_casual month number_of_ride
   <chr>         <ord>          <int>
 1 casual        11            100058
 2 member        11            236513
 3 casual        12             44588
 4 member        12            136559
 5 casual        01             39710
 6 member        01            149947
 7 casual        02             42613
 8 member        02            147090
 9 casual        03             61681
10 member        03            196096
11 casual        04            145491
12 member        04            278813
13 casual        05            231096
14 member        05            369878
15 casual        06            297101
16 member        06            417544
17 casual        07            326388
18 member        07            435334
19 casual        08            306844
20 member        08            459478
21 casual        09            258268
22 member        09            403926
23 casual        10            175082
24 member        10            359365
> 
```
## Section 4: Visualizing Data
The visualizations created in this section will be at the top of this repository in the files section. 


In this section, I will be describing my findings from the visualizations I created.

### Table 1: Number of rides per week for both casual and annual members.



Code used to make table: 
```{r}
# seeing the difference between how many casual and members ride at different times of the week
trips_whole_yr_rev %>%
  group_by(member_casual, day_of_week) %>%
  summarise(number_of_ride = n()) %>%
  ggplot(mapping=aes(x= day_of_week,y=number_of_ride,fill= member_casual,label = number_of_ride)) +
  geom_bar(position = "dodge", stat = "identity") +geom_text(colour = "white", size = 3,
                                                             vjust = 1.5, position = position_dodge(.9))
```
*Please refer to the weekly num_of_rides.pdf file in files section*


From the num of rides.pdf  table, we can find that the 
 - the peak amount of casual riders appears on Saturday at 396053 rides
 - the peak amount of full time members appears on Sunday at 581861 rides 
 - the least amount of casual riders appears on Sunday at 231038 rides
 - the least amount of full time members appears on Sunday at 396053 rides

### Table 2: Number of rides per month for both casual and annual members.


### Table 3: Table 4: Average ride length in comparison to casual and annual members each day of the week
```{r}
# seeing how long members ride in comparison to casual and members each day of the week 
trips_whole_yr_rev %>%
  group_by(member_casual, day_of_week) %>%
  summarise(average_ride_length = n()) %>%
  ggplot(mapping=aes(x= day_of_week,y=average_ride_length,fill= member_casual,label = average_ride_length)) 
+geom_bar(position = "dodge", stat = "identity") +geom_text(size = 3, position = position_stack(vjust = 0.25))
```


### Table 4: Average ride length in comparison to casual and annual members each month
```{r}
# seeing how long members ride in comparison to casual and members each month
trips_whole_yr_rev %>%
  group_by(member_casual, month) %>%
  summarise(average_ride_length = n()) %>%
  ggplot(mapping=aes(x= month,y=average_ride_length,fill= member_casual, label = average_ride_length)) +
  geom_bar(position = "dodge", stat = "identity") +geom_text(colour = "white", size = 3,
                                                             vjust = 1.5, position = position_dodge(.9))
```

## Section 5: Conclusion
