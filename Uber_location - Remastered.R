
# Importing the Libraries
library(ggplot2)
library(ggthemes)
library(lubridate) 
library(dplyr)
library(tidyr) 
library(ggalt)

theme_set(theme_classic())

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

# Another Dataset... (My Uber Drives.csv)

df <- read.csv("E:\\Paras Degree\\4th sem\\DS (Data Science)\\CP\\Uber-dataset\\My Uber Drives.csv")

df[df == ""] = "Not Given"


# Lollipop Plot...
day_data = cmb_data %>% group_by(dayofweek) %>% dplyr::summarize(Total = n())

ggplot(day_data, aes(x = dayofweek, y = Total)) +
  geom_lollipop(size = 1.5, color = "orange") +
  labs(title = "Lollipop plot",
       x = "Days",
       y = "Total Number of Rides") +
  theme_light()

rm(day_data)


# Heat Map by Month and Day of Week
month_weekday = cmb_data %>% group_by(dayofweek, month) %>% summarize(Total = n())

ggplot(month_weekday, aes(dayofweek, month, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Month and Day of Week") +
  labs(fill = "Rides", y = "Months", x = "Days") +
  scale_fill_gradient(low = "#353436",
                      high = "#f6f805") +
  theme_bw()

rm(month_weekday)


# Bar Graph by Month and Bases...
month_base <-  cmb_data %>% group_by(Base, month) %>% dplyr::summarize(Total = n()) 

ggplot(month_base, aes(Base, Total, fill = month)) +
  geom_bar(stat = "identity") +
  ggtitle("Heat Map on Rides Each Month and Bases") +
  labs(y = "Number of Rides", fill = "Months", x = "Bases")

rm(month_base)

# Density Plot..
ggplot(df, aes(y = 1:nrow(df))) + 
  geom_density(aes(fill=factor(CATEGORY.)), alpha=0.5) + 
  coord_flip() +
  labs(title = "Density Plot of Category", 
       y = "Number of Rides", 
       x = "Density")

# Advance scatter plot....
mix_data = cmb_data %>% group_by(Base, month, dayofweek) %>% summarize(Total = n())

ggplot(mix_data, aes(x = Base, y = Total)) +
  geom_point(aes(col = month, size = dayofweek)) +
  labs(title = "Scatter Plot of Rides, Bases, Days and Month",
       x = "Bases / Location of Pickup",
       y = "Total Number of Rides",
       col = "Month", size = "Day") +
  theme_bw()

rm(mix_data)

# Box plot....

box_data = cmb_data %>% group_by(Base, dayofweek, hour) %>% summarize(Total = n())

ggplot(box_data, aes(Base, Total)) + 
  geom_boxplot(aes(fill=dayofweek)) +
  labs(title = "Box Plot",
       x = "Base / Location of Pickup",
       y = "Total number of Rides") +
  theme_bw()

rm(box_data)

# Violin Plot

# Shows the density, max, min and quartiles...

ggplot(df, aes(CATEGORY., MILES.)) +
  ylim(0.5,20.5) +
  geom_violin(fill = "yellowgreen") +
  geom_boxplot(width=0.1, fill = "lightgreen") +
  labs(title = "Violin Plot",
       x = "Category of Ride",
       y = "Miles Travelled") +
  theme_bw()

# clustering.....

cluster = df %>% group_by(CATEGORY., MILES.) %>% summarize(Total = n())

ggplot(cluster, aes(y = Total, x = MILES., col = CATEGORY.)) + 
  geom_point(aes(shape=CATEGORY.), size=2) +
  geom_encircle(aes(y = Total, x = MILES.), size = 2) +
  labs(tile = "Scatter Plot With Clustering",
       x = "Miles Travelled",
       y = "Total number of Rides") +
  theme_bw()

rm(cluster)

# Line Plot

line_data = cmb_data %>% group_by(month, dayofweek, hour) %>% summarize(Total = n())

ggplot(line_data, aes(x = hour)) + 
  geom_line(aes(y = Total, col = dayofweek), size = 2) +
  scale_color_manual(values = rainbow(7)) +
  labs(title = "Line Chart",
       x = "Hour of the Day",
       y = "Total number of Rides")

rm(line_data)

####################################################