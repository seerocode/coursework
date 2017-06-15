library(tidyverse)
library(lubridate)

########################################
# READ AND TRANSFORM THE DATA
###############################V#########

# read one month of data
trips <- read_csv('~/coursework/week1/201402-citibike-tripdata.csv')

# replace spaces in column names with underscores
names(trips) <- gsub(' ', '_', names(trips))

# convert dates strings to dates
# trips <- mutate(trips, starttime = mdy_hms(starttime), stoptime = mdy_hms(stoptime))

# recode gender as a factor 0->"Unknown", 1->"Male", 2->"Female"
trips <- mutate(trips, gender = factor(gender, levels=c(0,1,2), labels = c("Unknown","Male","Female")))


########################################
# YOUR SOLUTIONS BELOW
########################################

# count the number of trips (= rows in the data frame)
#summarise(trips, count = n())
NROW(trips) %>% View

# find the earliest and latest birth years (see help for max and min to deal with NAs)
summarise(trips, min_birth_year=min(trips$birth_year, na.rm=TRUE) , max_birth_year=max(trips$birth_year, na.rm=TRUE))

# use filter and grepl to find all trips that either start or end on broadway
filter(trips, grepl('Broadway', start_station_name) | grepl('Broadway', end_station_name)) %>% View

# do the same, but find all trips that both start and end on broadway
filter(trips, grepl('Broadway', start_station_name) & grepl('Broadway', end_station_name)) %>% View

# find all unique station names
uniq_stations <- trips %>% group_by(start_station_name) %>% summarise(count=n())
#trips %>% distinct(start_station_name)
#trips %>% count(start_station_name)
#unique(trips$start_station_name)
#unique(combine(trips[,"start_station_name"],trips[,"end_station_name"]))

# count the number of trips by gender
trips_by_sex <- trips %>% group_by(gender) %>% summarise(count=n())
#trips_by_sex <- trips %>% group_by(gender) %>% count()

# compute the average trip time by gender
trips %>% 
  group_by(gender) %>% 
  summarise(avg_mins=mean(tripduration/60)) %>%
  View

# comment on whether there's a (statistically) significant difference
trips %>% 
  group_by(gender) %>% 
  summarise(avg_mins=mean(tripduration/60), std=sd(tripduration/60)) %>%
  View

# find the 10 most frequent station-to-station trips
trips %>% 
  group_by(start_station_name, end_station_name) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  head(10) %>% 
  View

# find the top 3 end stations for trips starting from each start station
trips %>% group_by(start_station_name, end_station_name) %>% #start with both statioms
  count() %>% # count them up
  arrange(start_station_name, desc(n)) %>% #arrange by station name in descending order
  head(3) %>%
  View

# find the top 3 most common station-to-station trips by gender
trips %>% 
  group_by(start_station_name, end_station_name, gender) %>% 
  count() %>% 
  group_by(gender) %>% 
  arrange(gender, desc(n)) %>%
  top_n(3) %>% 
  View

# find the day with the most trips
# tip: first add a column for year/month/day without time of day (use as.Date or floor_date from the lubridate package)
trips %>% 
  mutate(trip_date = as.Date(trips$starttime)) %>% 
  group_by(trip_date) %>% count() %>% 
  arrange(desc(n)) %>% 
  View

# compute the average number of trips taken during each of the 24 hours of the day across the entire month
# what time(s) of day tend to be peak hour(s)?
trips %>% 
  mutate(trip_date = as.Date(trips$starttime)) %>% 
  mutate(trip_start_hr = hour(trips$starttime)) %>% 
  group_by(trip_start_hr) %>% 
  summarise(cumm_trips_per_hour = n(), 
            avg_trips_perhour_perday = mean(cumm_trips_per_hour/28)) %>% 
  View

#visualizing the peak
trips %>% 
  mutate(trip_date = as.Date(trips$starttime)) %>% 
  mutate(trip_start_hr = hour(trips$starttime)) %>% 
  group_by(trip_start_hr) %>% 
  summarise(avg_trips_perhour_perday = mean(n()/28)) %>% 
  ggplot() + 
  geom_line(aes(trip_start_hr,avg_trips_perhour_perday)) + 
  labs(caption = "(based on Citibike data from Feb 2014)") + # caption of graph
  labs(x = "Hour trip started (24 hours)", y="Avg # of Trips for Hour") # x and y labels

#what are the destinations for midnight? by gender?
trips %>% 
  mutate(trip_date = as.Date(trips$starttime)) %>% 
  mutate(trip_start_hr = hour(trips$starttime)) %>% 
  group_by(trip_start_hr[1], start_station_name, end_station_name, gender) %>% 
  summarise(count = n()) %>% arrange(desc(count)) %>% View
