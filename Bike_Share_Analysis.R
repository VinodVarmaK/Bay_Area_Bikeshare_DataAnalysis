library(readr)
Bike_Data1 <- read.csv("E:/R/Bayarea_bikeshare_data/2017-bikeshare-tripdata.csv", header = T)
Bike_Data2 <- read.csv("E:/R/Bayarea_bikeshare_data/201801_bikeshare_tripdata.csv",  header = T)
Bike_Data3 <- read.csv("E:/R/Bayarea_bikeshare_data/201802_bikeshare_tripdata.csv", header = T)
Bike_Data4 <- read.csv("E:/R/Bayarea_bikeshare_data/201803_bikeshare_tripdata.csv", header = T)
Bike_Data.extended <- rbind(Bike_Data1, Bike_Data2 )
Bike_Data.extended2 <- rbind(Bike_Data.extended, Bike_Data3)
Bike_Data.ext <- rbind(Bike_Data.extended2, Bike_Data4)

str(Bike_Data.ext)
library(ggplot2)
library(lubridate)
library(plyr)
library(dplyr)
library(forecast)
tripdata <- Bike_Data.ext
head(Bike_Data.ext)

tripdata$start_station_name <-  as.character(tripdata$start_station_name)
tripdata$end_station_name <-  as.character(tripdata$end_station_name)


tripdata$start_time = as.POSIXct(Bike_Data.ext$start_time, format = "%m/%d/%Y %H:%M");
tripdata$end_time = as.POSIXct(Bike_Data.ext$end_time, format = "%m/%d/%Y %H:%M");
StartDate <- strptime(tripdata$start_time, "%m/%d/%Y %H:%M")
tripdata$start_time <- as.POSIXct(StartDate) 
EndDate <- strptime(tripdata$end_time, "%m/%d/%Y %H:%M")
tripdata$end_time <- as.POSIXct(EndDate) 

#Riders by membership/subscription
bar <- ggplot(tripdata, aes(x = factor(1),  fill = factor(user_type))) + geom_bar(width = 1) 
pie <- bar + coord_polar(theta = "y") + theme_void() + labs(title = "Riders by Membership") +
  theme(plot.title = element_text(hjust=0.5))
pie


original <- tripdata

# Creatinf several useful columns with dplyr (breakdown start/end date by Month, Day, Day of Week for analysis
tripdata <- mutate(tripdata, sdate = date(start_time), smonth = month(start_time,label = TRUE), 
                   sday = day(start_time), swday = wday(start_time,label = TRUE), shr = hour(start_time), 
                   edate = date(start_time), emonth = month(end_time,label = TRUE), eday = day(end_time), 
                   ewday = wday(end_time,label = TRUE), ehr = hour(end_time))

# To turn the start_time and end_time into character format to avoid conflicting issues in tallying
tripdata$start_time <- as.character(tripdata$start_time)
tripdata$end.time <- as.character(tripdata$end_time)

# Rides in days of the week
week.rider <- ddply(tripdata, .(swday), tally)
cwd$wkday <- ifelse(week.usertype$swday %in% c("Mon","Tues","Wed","Thurs","Fri"), "Weekday","Weekend")
ggplot(week.rider, aes(x = swday, y = n)) + geom_bar(stat='identity', fill = "#2b8cbe") +
  labs(title = "Rides over days of week", x = "Days of Week", y = "Count of Rides") + 
  theme(plot.title = element_text(hjust=0.5))

# Rides in days of the week with user type split
week.usertype <- ddply(tripdata, .(swday,user_type), tally)
ggplot(week.usertype, aes(x = swday, y = n, fill = user_type)) + geom_bar(stat='identity') + 
  labs(title = "Ridership over days of week by Subscriber Type", x = "Days of Week", y = "Count") +
  theme(plot.title = element_text(hjust=0.5))


remove(week.usertype,week.rider)

#Hourly Distribution of Rides
hourly.rider <- ddply(tripdata, .(shr,user_type), tally)
ggplot(hourly.rider, aes(x = shr, y = n, fill = user_type)) + geom_bar(stat='identity') + 
  labs(title = "Ridership over time of day by user Type", x = "Time of day (hr)", y = "Count of Rides") +
  theme(plot.title = element_text(hjust=0.5))

#Weekly & Hourly Distribution of Rides timing
week.hourly.rider <- ddply(tripdata, .(shr,swday,user_type), tally)
ggplot(week.hourly.rider, aes(x = shr, y = n, fill = user_type)) + facet_grid(. ~ swday) + 
  geom_bar(stat='identity') + labs(title = "Ridership over Weekday over Time by Subscriber Type", 
                                   x = "Time", y = "Count of Rides")



#duration of trips distribution plot
tripdata <- mutate(tripdata, total_min = ((duration_sec)/60), label = TRUE )

duration.min <- ddply(tripdata, .(total_min, user_type), tally) 
ggplot(duration.min, aes(x = total_min, y = n, fill = factor(user_type))) + geom_bar(stat='identity') + 
  coord_cartesian(xlim = c(1, 70)) +
  labs(title = "Usage of bikes in Minutes", x = "Rides by Duration(Min)", y = "Count of Rides") +
  theme(plot.title = element_text(hjust=0.5)) 

# Popular Ride Start Staions
start.station <- ddply(tripdata, .(start_station_name), tally) %>% arrange(desc(n))
head(start.station)

#Popular Ride End Stations
end.station <- ddply(tripdata, .(end_station_name), tally) %>% arrange(desc(n))
head(end.station)



#Distribution of trips starting from specific stations
start.station <- ddply(tripdata, .(start_station_name), tally) %>% arrange(desc(n))
head(start.station)
end.station <- ddply(tripdata, .(end_station_name), tally) %>% arrange(desc(n))
head(start.station)
tripdata <- mutate(tripdata, wkend = (swday %in% c("Sat","Sun")))
startdata <- tripdata[tripdata$start_station_name %in% start.station$start_station_name[1:6],] %>%
  ddply(.(start_station_name, shr, wkend), tally) 
startdata$wkend <- ifelse(startdata$wkend == TRUE, 'Weekend', 'Weekday')
ggplot(startdata, aes(x = shr, y = n, colour = wkend)) + facet_wrap( ~ start_station_name, ncol = 2) + 
  geom_line(aes(group = wkend)) + geom_point(aes(group = wkend)) + 
  labs(title = "Distribution of trips starting from each station across time by weekday/weekend", 
       x = "Time (hr)", y = "Rides Count") +   theme(plot.title = element_text(hjust=0.5))