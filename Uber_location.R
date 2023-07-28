
# Importing the Libraries
library(ggplot2)
library(ggthemes)
library(lubridate) 
library(dplyr)
library(tidyr) 

# Reading the Dataset
apr_data <- read.csv("E:\\Paras Degree\\4th sem\\DS (Data Science)\\CP\\Uber-dataset\\uber-raw-data-apr14.csv")
may_data <- read.csv("E:\\Paras Degree\\4th sem\\DS (Data Science)\\CP\\Uber-dataset\\uber-raw-data-may14.csv")
jun_data <- read.csv("E:\\Paras Degree\\4th sem\\DS (Data Science)\\CP\\Uber-dataset\\uber-raw-data-jun14.csv")
jul_data <- read.csv("E:\\Paras Degree\\4th sem\\DS (Data Science)\\CP\\Uber-dataset\\uber-raw-data-jul14.csv")
aug_data <- read.csv("E:\\Paras Degree\\4th sem\\DS (Data Science)\\CP\\Uber-dataset\\uber-raw-data-aug14.csv")
sep_data <- read.csv("E:\\Paras Degree\\4th sem\\DS (Data Science)\\CP\\Uber-dataset\\uber-raw-data-sep14.csv")


cmb_data <- rbind(apr_data, may_data, jun_data, jul_data, aug_data, sep_data)

rm(apr_data)
rm(may_data)
rm(jun_data)
rm(jul_data)
rm(aug_data)
rm(sep_data)
gc()


cmb_data$Date.Time <- as.POSIXct(cmb_data$Date.Time, format = "%m/%d/%Y %H:%M:%S")

cmb_data$Time <- format(as.POSIXct(cmb_data$Date.Time, format = "%m/%d/%Y %H:%M:%S"), format="%H:%M:%S")


cmb_data$day <- factor(day(cmb_data$Date.Time))
cmb_data$month <- factor(month(cmb_data$Date.Time, label = TRUE))
cmb_data$year <- factor(year(cmb_data$Date.Time))
cmb_data$dayofweek <- factor(wday(cmb_data$Date.Time, label = TRUE))

cmb_data$hour <- factor(hour(hms(cmb_data$Time)))
cmb_data$minute <- factor(minute(hms(cmb_data$Time)))
cmb_data$second <- factor(second(hms(cmb_data$Time)))


hour_data <- cmb_data %>% group_by(hour) %>% dplyr::summarize(Total = n())

ggplot(hour_data, aes(hour, Total)) + 
  geom_bar(stat = "identity", fill = rainbow(1)) +
  ggtitle("Uber Trips Every Hour") + 
  theme_bw()

# Bar Graph  month_hour
month_hour <- cmb_data %>% group_by(month, hour) %>% dplyr::summarize(Total = n())

ggplot(month_hour, aes(hour, Total, fill = month)) + 
  geom_bar( stat = "identity") +
  ggtitle("Trips are done every Hour and Month") +
  theme_bw()

# Bar Graph of day_group
day_group <- cmb_data %>% group_by(day) %>% dplyr::summarize(Total = n())

ggplot(day_group, aes(day, Total)) + 
  geom_bar( stat = "identity", fill = 'Orange') +
  ggtitle("Trips done Every Day") +
  theme(legend.position = "none") +
  theme_bw()

# Bar Graph of day_month_group
day_month_group <- cmb_data %>% group_by(month, day) %>% dplyr::summarize(Total = n())

ggplot(day_month_group, aes(day, Total, fill = month)) + 
  geom_bar(stat = "identity") +
  ggtitle("Uber Rides Done Each Day and Month") +
  labs(fill = "Month", y = "Rides", x = "Days") +
  theme_bw()

# Bar Graph of month_group...
month_group <- cmb_data %>% group_by(month) %>% dplyr::summarize(Total = n()) 

ggplot(month_group, aes(month, Total)) + 
  geom_bar(stat = "identity", fill = "orange") + 
  ggtitle("Trips Done Each Month") +
  labs(y = "Rides", x = "Month") +
  theme_bw()

# Bar Graph month_weekday...
month_weekday <- cmb_data %>% group_by(month, dayofweek) %>% dplyr::summarize(Total = n())

ggplot(month_weekday, aes(month, Total, fill = dayofweek)) + 
  geom_bar( stat = "identity", position = "dodge") +
  ggtitle("Trips Each Day and Month") +
  labs(fill = "Days of Week", x = "Months", y = "Total Number of Rides")

# Bar Graph of Trips by Bases...
ggplot(cmb_data, aes(Base)) + 
  geom_bar(fill = "Orange") +
  ggtitle("Number of Rides Done by Bases") +
  labs(y = "Total Number Of Rides") +
  theme_bw()

# Bar Graph Trips by Bases and Month...
ggplot(cmb_data, aes(Base, fill = month)) + 
  geom_bar(position = "dodge") +
  ggtitle("Rides Done By Each Bases On Particular Month") +
  labs(fill = "Months", y = "Total Number of Rides") +
  theme_bw()

# Bar Graph Trips by Bases and Day_of_Week...
ggplot(cmb_data, aes(Base, fill = dayofweek)) + 
  geom_bar(position = "dodge") +
  ggtitle("Rides Done By Each Bases On Particular DayofWeek") +
  labs(fill = "Days", y = "Total Number of Rides") +
  theme_bw()

# Heat Map by Hour and Day...
day_and_hour <- cmb_data %>% group_by(day, hour) %>% dplyr::summarize(Total = n())

ggplot(day_and_hour, aes(day, hour, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map on Trips Each Hour and Day") +
  labs(fill = "Rides", y = "Hours", x = "Days in Numbers") +
  scale_fill_gradient(low = "#353436",
                      high = "#f6f805") +
  theme_bw()

# Heat Map by Month and Day
ggplot(day_month_group, aes(day, month, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map on Trips Each Month and Day") +
  labs(fill = "Rides", y = "Months", x = "Days in Numbers") +
  scale_fill_gradient(low = "#353436",
                      high = "#f6f805") +
  theme_bw()

# Heat Map by Month and Day of Week
ggplot(month_weekday, aes(dayofweek, month, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Month and Day of Week") +
  labs(fill = "Rides", y = "Months", x = "Days") +
  scale_fill_gradient(low = "#353436",
                      high = "#f6f805") +
  theme_bw()

month_base <-  cmb_data %>% group_by(Base, month) %>% dplyr::summarize(Total = n()) 

day0fweek_bases <-  cmb_data %>% group_by(Base, dayofweek) %>%  dplyr::summarize(Total = n()) 

# Heat Map by Month and Bases...
ggplot(month_base, aes(Base, month, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map on Rides Each Month and Bases") +
  labs(fill = "Rides", y = "Months", x = "Bases") +
  scale_fill_gradient(low = "#353436",
                      high = "#f6f805") +
  theme_bw()

# Heat Map by Bases and Day of Week...
ggplot(day0fweek_bases, aes(Base, dayofweek, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map on Rides Done Each Bases and Day of Week") +
  labs(fill = "Rides", y = "Days", x = "Bases") +
  scale_fill_gradient(low = "#353436",
                      high = "#f6f805") +
  theme_bw()

# Cleaning Unrequired Variables...
rm(cmb_data)
rm(day_and_hour)
rm(day_group)
rm(day_month_group)
rm(day0fweek_bases)
rm(hour_data)
rm(month_base)
rm(month_group)
rm(month_hour)
rm(month_weekday)
gc()

# Another Dataset... (My Uber Drives.csv)

df <- read.csv("E:\\Paras Degree\\4th sem\\DS (Data Science)\\CP\\Uber-dataset\\My Uber Drives.csv")

df[df==""] = "Not Given"

# Bar Graph..
ggplot(data = df, aes(x = CATEGORY., fill = PURPOSE.)) +
  geom_bar(position = "dodge") +
  labs(title = "Purpose of the Ride", y = "Number of Rides", x = "Category of Ride")

# Density Plot..
ggplot(df, aes(y = 1:nrow(df))) + 
  geom_density(aes(fill=factor(CATEGORY.)), alpha=0.5) + 
  coord_flip() +
  labs(title = "Density Plot of Category", 
       y = "Number of Rides", 
       x = "Density")

# Scatter Plot for Distance Covered
ggplot(df, aes(x = PURPOSE., y = MILES.)) + 
  geom_point(aes(col = CATEGORY.)) +
  labs(title = "Scatter Plot of Distance Covered by Each Category",
       x = "Purpose Of Ride") +
  theme(axis.text.x = element_text(angle = 90, vjust=0.5, size = 8))

rm(df)
gc()

