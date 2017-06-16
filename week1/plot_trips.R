########################################
# load libraries
########################################

# load some packages that we'll need
library(tidyverse)
library(scales)

# be picky about white backgrounds on our plots
theme_set(theme_bw())

# load RData file output by load_trips.R
load('trips.RData')


########################################
# plot trip data
########################################

# plot the distribution of trip times across all rides
ggplot(trips, aes(x=tripduration/60)) + 
  geom_density()+ xlim(c(0,60)) #scale_x_log10() 
#my isue was I was choosing the wrong graph

# plot the distribution of trip times by rider type
ggplot(trips, aes(x=tripduration/60, color=usertype, fill=usertype)) + 
  geom_density(alpha=0.2)+xlim(c(0,60))
#my issue: UNDERSTAND HOW DENSITY AND XLIM WORKS

# plot the number of trips over each day
trips %>% 
  group_by(ymd) %>% count() %>% ggplot() + geom_line(aes(ymd,n))

# plot the total number of trips by age, split by gender
trips %>% filter(gender != "Unknown") %>% #REMOVE NA's'
  group_by(gender, birth_year) %>% 
  summarise(count = n()) %>% ggplot(aes(x=(2017-birth_year), y=count, color=gender)) + geom_point()
#COLOR = IS WHAT THE GRAPH WILL USE AS THE LEGEND

# plot the ratio of male to female trips by age
trips %>% group_by(gender, birth_year) %>% 
  filter(gender== !"Unknown") %>%
  summarize(count = n()) %>% 
  spread(gender,count) %>% 
  mutate(ratio = Male/Female) %>%
  ggplot(birth_year, ratio, size=Male+Female)+ #SIZE shows you how many riders
  xlim(15, 70) +
  ylim(0, 7.5) +
  geom_point()
# hint: use the spread() function to reshape things to make it easier to compute this ratio


########################################
# plot weather data
########################################
# plot the minimum temperature over each day
ggplot(weather) + geom_line(aes(x=ymd, y=tmin))

# plot the minimum temperature and maximum temperature over each day
# hint: try using the gather() function for this to reshape things before plotting
  weather %>%
  select(tmin, tmax, ymd) %>%
  gather("range","temp", 1.2) %>%
  ggplot(aes(x=ymd, y=temp, color=range)) +
  geom_point()


########################################
# plot trip and weather data
########################################

# join trips and weather
trips_with_weather <- inner_join(trips, weather, by="ymd")

# plot the number of trips as a function of the minimum temperature, where each point represents a day
# you'll need to summarize the trips and join to the weather data to do this
  trips_with_weather %>% 
    group_by(tmin)

  
# repeat this, splitting results by whether there was substantial precipitation or not
# you'll need to decide what constitutes "substantial precipitation" and create a new T/F column to indicate this

  
# add a smoothed fit on top of the previous plot, using geom_smooth

# compute the average number of trips and standard deviation in number of trips by hour of the day
# hint: use the hour() function from the lubridate package

# plot the above

# repeat this, but now split the results by day of the week (Monday, Tuesday, ...) or weekday vs. weekend days
# hint: use the wday() function from the lubridate package
