# Case Study: How Does a Bike-Share Navigate Speedy Success?

## Google Data Analytics Capstone Project
## Geoffrey Cho

**Scenario**: You are a junior data analyst working in the marketing analyst team at Cyclistic, a bike-share company in Chicago. The director of marketing believes the company’s future success depends on maximizing the number of annual memberships. Therefore, your team wants to understand how casual riders and annual members use Cyclistic bikes differently. From these insights, your team will design a new marketing strategy to convert casual riders into annual members. But first, Cyclistic executives must approve your recommendations, so they must be backed up with compelling data insights and professional data visualizations.

**About the company**: In 2016, Cyclistic launched a successful bike-share offering. Since then, the program has grown to a fleet of 5,824 bicycles that are geotracked and locked into a network of 692 stations across Chicago. The bikes can be unlocked from one station and returned to any other station in the system anytime. Until now, Cyclistic’s marketing strategy relied on building general awareness and appealing to broad consumer segments. One approach that helped make these things possible was the flexibility of its pricing plans: single-ride passes, full-day passes, and annual memberships. Customers who purchase single-ride or full-day passes are referred to as casual riders. Customers who purchase annual memberships are Cyclistic members. Cyclistic’s finance analysts have concluded that annual members are much more profitable than casual riders. Although the pricing flexibility helps Cyclistic attract more customers, Moreno believes that maximizing the number of annual members will be key to future growth. Rather than creating a marketing campaign that targets all-new customers, Moreno believes there is a very good chance to convert casual riders into members. She notes that casual riders are already aware of the Cyclistic program and have chosen Cyclistic for their mobility needs. Moreno has set a clear goal: Design marketing strategies aimed at converting casual riders into annual members. In order to do that, however, the marketing analyst team needs to better understand how annual members and casual riders differ, why casual riders would buy a membership, and how digital media could affect their marketing tactics. Moreno and her team are interested in analyzing the Cyclistic historical bike trip data to identify trends.

## **ASK** 
Business task: 
To understand how annual members and casual riders differ, in order to design marketing strategies aimed at converting casual riders into annual members.

## **PREPARE**
Data made available by Motivate International Inc. (under a specific license), which is Cyclistic’s public, historical trip data that can be used to explore how different customer types are using the bikes. Due to data-privacy issues, however, riders’ personally identifiable information is off-limits; in other words, pass purchases cannot be connected to credit card numbers to see if casual riders live in the Cyclistic service area or if they purchased multiple single passes. 

Although there were slight variations in column names throughout all of the tables, the key information/column names remained relatively the same: start and end station, the times that the ride started and ended, whether the rider was a member or casual, in addition to the starting and ending longitude and latitude. 

## **PROCESS**
Downloaded the previous 12 months of Cyclistic trip data from: https://divvy-tripdata.s3.amazonaws.com/index.html
Turned the unzipped .csv files into Excel Workbook files, which ran from February 2023 to March 2022.

(Using the data from February 2023, in the file “Feb2023-divvy-tripdata.xslx”)
Created a new column/metric called “ride_length” that calculated the length of each ride by subtracting the column “started_at” from the column “ended_at” (formula =D2-C2), with the time being formatted as HH:MM:SS.

Created a new column/metric called “day_of_week” that calculated the day of the week, noting that 1 = Sunday and 7 = Saturday, using the “WEEKDAY” command.


## **ANALYZE**
Cleaned data to prepare for analysis by filtering for and deleting rows with blank cells and cells with invalid values or values that don’t make sense, such as ride_length values over 24 hours long.

Ran a couple of calculations in one file (“Feb2023-divvy-tripdata.xslx”) in order to get a better sense of the data layout, and created new metrics such as:
Average Ride Length: the mean of the ride_lengths column. 0:11:59 or 11 minutes and 59 seconds, calculated by using the AVERAGE function in Excel.
Longest Ride Length: the maximum value of the ride_lengths column. 23:43:47, or 23 hours 43 minutes and 47 seconds, calculated by using the MAX function.
Mode of the Day of the Week: the mode of the day_of_week column. 3, or Tuesday, calculated by using the MODE function.

Created a pivot table to quickly calculate and visualize the data, and calculated for metrics such as:
The average ride length time for the 2 different categories of riders (annual members and casual riders). For casual riders, the average ride_length value was 0:17:38 (17 minutes, 38 seconds) and for annual members, the average ride_length value was 0:10:24 (10 minutes, 24 seconds). 
The average ride length time for the 2 different categories of riders, based on the days of the week. A copy of the pivot table is provided below:

![Pivot Table] (https://user-images.githubusercontent.com/121268398/231094017-a853ce2b-871c-41ea-8eb5-802f548e4780.png)





RStudio was then used to analyze data from 2020 Quarter 1, 2019 Quarter 4, 2019 Quarter 3, 2019 Quarter 2, using a R script inspired from "'Sophisticated, Clear, and Polished’: Divvy and Data Visualization" written by Kevin Hartman (found here: https://artscience.blog/home/divvy-dataviz-case-study).

First, required packages were installed: tidyverse for data import and wrangling, lubridate for date functions, and ggplot for visualization.

library(tidyverse)  #helps wrangle data
library(lubridate)  #helps wrangle date attributes
library(ggplot2)  #helps visualize data
getwd() #displays working directory
setwd("/Users/geoffreycho/Downloads") #sets working directory to simplify calls to data

```
#=====================
COLLECT DATA
#=====================
# Upload Divvy datasets (csv files) here
q2_2019 <- read_csv("Divvy_Trips_2019_Q2.csv")
q3_2019 <- read_csv("Divvy_Trips_2019_Q3.csv")
q4_2019 <- read_csv("Divvy_Trips_2019_Q4.csv")
q1_2020 <- read_csv("Divvy_Trips_2020_Q1.csv")

#====================================================
# STEP 2: WRANGLE DATA AND COMBINE INTO A SINGLE FILE
#====================================================
# Compare column names each of the files
# While the names don't have to be in the same order, they DO need to match perfectly before we can use a command to join them into one file
colnames(q3_2019)
colnames(q4_2019)
colnames(q2_2019)
colnames(q1_2020)

# Rename columns  to make them consistent with q1_2020 (as this will be the supposed going-forward table design for Divvy)
(q4_2019 <- rename(q4_2019
                   ,trip_id = ride_id
                   ,bikeid = rideable_type 
                   ,start_time = started_at  
                   ,end_time = ended_at  
                   ,from_station_name = start_station_name 
                   ,from_station_id = start_station_id 
                   ,to_station_name = end_station_name 
                   ,to_station_id = end_station_id 
                   ,usertype = member_casual))

(q3_2019 <- rename(q3_2019
                   ,trip_id = ride_id
                   ,bikeid = rideable_type 
                   ,start_time = started_at  
                   ,end_time = ended_at  
                   ,from_station_name = start_station_name 
                   ,from_station_id = start_station_id
                   ,to_station_name = end_station_name 
                   ,to_station_id = end_station_id 
                   ,usertype = member_casual))

(q2_2019 <- rename(q2_2019
                   ,"01 - Rental Details Rental ID" = ride_id
                   ,"01 - Rental Details Bike ID" = rideable_type
                   ,"01 - Rental Details Local Start Time" = started_at  
                   ,"01 - Rental Details Local End Time" = ended_at 
                   ,"03 - Rental Start Station Name" = start_station_name
                   ,"03 - Rental Start Station ID" = start_station_id
                   ,"02 - Rental End Station Name" = end_station_name
                   ,"02 - Rental End Station ID" = end_station_id
                   ,"User Type" = member_casual))

# Inspect the dataframes and look for incongruencies
str(q1_2020)
str(q4_2019)
str(q3_2019)
str(q2_2019)

# Convert ride_id and rideable_type to character so that they can stack correctly
q4_2019 <-  mutate(q4_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)
                   ,started_at = as.character(started_at)
                   ,ended_at = as.character(ended_at)) 
q3_2019 <-  mutate(q3_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)
                   ,started_at = as.character(started_at)
                   ,ended_at = as.character(ended_at)) 
q2_2019 <-  mutate(q2_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)
                   ,started_at = as.character(started_at)
                   ,ended_at = as.character(ended_at))


# Stack individual quarter's data frames into one big data frame
all_trips <- bind_rows(q2_2019, q3_2019, q4_2019, q1_2020)

# Remove lat, long, birthyear, and gender fields as this data was dropped beginning in 2020
all_trips <- all_trips %>%  
  select(-c(start_lat, start_lng, end_lat, end_lng, birthyear, gender, "01 - Rental Details Duration In Seconds Uncapped", "05 - Member Details Member Birthday Year", "Member Gender", "tripduration"))

#======================================================
# STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
#======================================================
# Inspect the new table that has been created
colnames(all_trips)  #List of column names
nrow(all_trips)  #How many rows are in data frame?
dim(all_trips)  #Dimensions of the data frame?
head(all_trips)  #See the first 6 rows of data frame.  Also tail(all_trips)
str(all_trips)  #See list of columns and data types (numeric, character, etc)
summary(all_trips)  #Statistical summary of data. Mainly for numerics

# There are a few problems we will need to fix:
# (1) In the "member_casual" column, there are two names for members ("member" and "Subscriber") and two names for casual riders ("Customer" and "casual"). We will need to consolidate that from four to two labels.
# (2) The data can only be aggregated at the ride-level, which is too granular. We will want to add some additional columns of data -- such as day, month, year -- that provide additional opportunities to aggregate the data.
# (3) We will want to add a calculated field for length of ride since the 2020Q1 data did not have the "tripduration" column. We will add "ride_length" to the entire dataframe for consistency.
# (4) There are some rides where tripduration shows up as negative, including several hundred rides where Divvy took bikes out of circulation for Quality Control reasons. We will want to delete these rides.

# In the "member_casual" column, replace "Subscriber" with "member" and "Customer" with "casual"
# Before 2020, Divvy used different labels for these two types of riders ... we will want to make our dataframe consistent with their current nomenclature
# N.B.: "Level" is a special property of a column that is retained even if a subset does not contain any values from a specific level
# Begin by seeing how many observations fall under each usertype
table(all_trips$member_casual)

# Reassign to the desired values (we will go with the current 2020 labels)
all_trips <-  all_trips %>% 
  mutate(member_casual = recode(member_casual
                                ,"Subscriber" = "member"
                                ,"Customer" = "casual"))

# Check to make sure the proper number of observations were reassigned
table(all_trips$member_casual)

# Add columns that list the date, month, day, and year of each ride
# This will allow us to aggregate ride data for each month, day, or year ... before completing these operations we could only aggregate at the ride level
# https://www.statmethods.net/input/dates.html more on date formats in R found at that link
all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

# Add a "ride_length" calculation to all_trips (in seconds)
# https://stat.ethz.ch/R-manual/R-devel/library/base/html/difftime.html
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)

# Inspect the structure of the columns
str(all_trips)

# Convert "ride_length" from Factor to numeric so we can run calculations on the data
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)

# Remove "bad" data
# The dataframe includes a few hundred entries when bikes were taken out of docks and checked for quality by Divvy or ride_length was negative
# We will create a new version of the dataframe (v2) since data is being removed
# https://www.datasciencemadesimple.com/delete-or-drop-rows-in-r-with-conditions-2/
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]

#=====================================
# STEP 4: CONDUCT DESCRIPTIVE ANALYSIS
#=====================================
# Descriptive analysis on ride_length (all figures in seconds)
mean(all_trips_v2$ride_length) #straight average (total ride length / rides)
median(all_trips_v2$ride_length) #midpoint number in the ascending array of ride lengths
max(all_trips_v2$ride_length) #longest ride
min(all_trips_v2$ride_length) #shortest ride

# You can condense the four lines above to one line using summary() on the specific attribute
summary(all_trips_v2$ride_length)

# Compare members and casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

# See the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

# Notice that the days of the week are out of order. Let's fix that.
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Now, let's run the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

# analyze ridership data by type and weekday
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, weekday)								# sorts

# Let's visualize the number of rides by rider type
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

# Let's create a visualization for average duration
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")

#=================================================
# STEP 5: EXPORT SUMMARY FILE FOR FURTHER ANALYSIS
#=================================================
# Create a csv file that we will visualize in Excel, Tableau, or my presentation software

counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
write.csv(counts, file = '~/Downloads/Divvy_Exercise/avg_ride_length.csv')
```




## **SHARE**
Provided below are the two data visualization charts created by the R script:
### **Table 1: Annual Member and Casual Rider Average Number of Rides, Based on Days of Week**

![Table 1: Annual Member and Casual Rider Average Number of Rides, Based on Days of Week] (https://user-images.githubusercontent.com/121268398/231095024-7483be05-2c66-40ee-acff-d0b066e7b1ba.png)



### **Table 2: Annual Member and Casual Rider Average Trip Duration, Based on Days of Week**

![Table 2: Annual Member and Casual Rider Average Trip Duration, Based on Days of Week] (https://user-images.githubusercontent.com/121268398/231095063-a67db612-d54b-4d96-bd9c-a1c26bfae58d.png)




## **ACT**
**Key Findings**:
Annual members account for a higher share of ride counts compared to casual riders, especially so during the weekdays. This likely means that annual members live within the vicinity of the city/bike docking stations and utilize the bikes as a means of transportation to work.
For casual riders, ride counts increase during the weekends compared to the weekdays; for annual members, ride counts increase significantly more during the weekdays and decrease during weekends. Again, this likely points to annual members viewing the bikes as more of a means of transportation, whereas casual riders probably view the bikes as more of a leisure activity/recreational rental for the weekends.
When renting the bikes, casual riders use them for much longer than annual members, across all days of the week. In fact, on average, casual riders’ ride lengths are three times longer than annual members. 

**Deliverables for Moreno and the executive team (keeping the business task of converting casual riders to annual members in mind)**:
The findings point towards casual riders likely viewing the bikes as more of a leisure activity than a mode of transportation; in addition, casual riders on average ride the bikes for longer than annual members-who view the bikes as more of a mode of transportation. Therefore, the marketing/pricing strategy could be more catered towards incentivizing longer bike rides, which could mean offering discounts on either ride prices or annual membership costs after a certain amount of distance traveled.
Casual riders use the bikes for much longer than annual members, per bike ride. Therefore, Cyclistic could offer discounts based on the trip duration time, or some other marketing strategy aimed at incentivizing riders who go on longer bike rides to convert to being an annual member.
Cyclistic’s flexibility in terms of its pricing plans have served the company well thus far, and a little more flexibility could also benefit them even further. Because casual riders use the bikes more on the weekends than weekdays, Cyclistic could offer an additional membership/subscription plan for the weekends, which would give riders a solid middle option between full day passes and annual memberships.
