# setwd("../datasets")
# getwd()
# list.files()

# install.packages('tidyverse')
library(tidyverse)
# install.packages('shiny')
# install.packages('lubridate')
# library(lubridate)
install.packages("RColorBrewer")

# PREPARE

# Get list of all files for the project
all_files <- list.files(pattern = "*.csv")


# Read all files into a single data frame
all_df <- map_df(all_files, read_csv)

# Familiarize with the data
## View column specification
spec(all_df)

## View structure 
str(all_df) 
# 1. Data contains 8,825,826 rows and 13 columns
colnames(all_df)
# 2. "ride_id", "rideable_type", "started_at", "ended_at", "start_station_name", 
# "start_station_id", "end_station_name", "end_station_id", "start_lat", "start_lng",
# "end_lat", "end_lng", "member_casual"  

## Preview the data
glimpse(all_df)

## First 6 rows
head(all_df)

## Last 6 rows
tail(all_df)

# PROCESS 
# Clean data by:
# 1. Select necessary columns  
# 2. Remove NAs 
# 3. Add columns for "trip_duration" and "day_of_week"
# 4. Select final data columns
# 5. Filter data to remove bad date i.e negative "trip_duration" and is greater than 1 day 

clean_df <- all_df %>%
  select("ride_id", "rideable_type", "started_at", "ended_at", "start_station_name", 
         "start_station_id", "end_station_name", "end_station_id", "member_casual") %>%
  rename(trip_id = ride_id,
         bike_type = rideable_type, 
         start_time = started_at,
         end_time = ended_at,
         from_station_name = start_station_name,
         to_station_name = end_station_name,
         from_station_id = start_station_id,
         to_station_id = end_station_id,
         usertype = member_casual) %>%
  na.omit() %>%
  mutate(trip_date = as.Date(start_time),
         trip_time = format(as.POSIXct(start_time), format = "H%:M%:S%"),
         trip_duration = as.numeric(difftime(end_time, start_time, units = "mins")), 
         day_of_week = wday(start_time, label = TRUE, abbr = FALSE),) %>%
  filter(trip_duration >= 1, trip_duration <= (24*60))

min(clean_df$trip_duration) # -168.7
max(clean_df$trip_duration) # 34354.07 approx. 23 days

glimpse(clean_df)

head(clean_df)

View(clean_df)

# 6. Verify that NAs have all been removed
anyNA(clean_df) #FALSE
colSums(is.na(clean_df))
colSums(is.null(clean_df, start_time))

# 7. Verify the data has no duplicates 
sum(duplicated(clean_df$trip_id)) # 0


# ANALYZE
# Descriptive Analysis 
# Calculate the mean and max of trip_duration 
# Calculate the mode of day_of_week

# Function to calculate mode of a vector
getMode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Create summary tables

# 0.1 
# Get general summary of the trip duration
summary(clean_df$trip_duration) 

# 0.2
# See the average ride time by each day for members vs casual users
aggregate(clean_df$trip_duration ~ clean_df$usertype + clean_df$day_of_week, FUN = mean)

data_summary <- clean_df %>%
  group_by(usertype) %>%
  summarise(
    row_count = n(),
    avg_trip_duration = mean(trip_duration),
    max_trip_duration = max(trip_duration),
    min_trip_duration = min(trip_duration),
    mode_bike_type = getMode(bike_type),
    mode_day_of_week = getMode(day_of_week)
  )

View(data_summary)

write.csv(data_summary, 
          file = '~/Documents/epudom/DA/data_analytics_portfolio_project/GDAC/Case_Study_1/analysis/data_summary.csv')


# 1. Summarize by day_of_week
summary_by_weekday <- clean_df %>%
  group_by(usertype, day_of_week) %>%
    summarise(
      row_count = n(),
      avg_trip_duration = mean(trip_duration),
      max_trip_duration = max(trip_duration),
      min_trip_duration = min(trip_duration),
      mode_bike_type = getMode(bike_type)
    ) %>%
  arrange(usertype, day_of_week)

View(summary_by_weekday)

write.csv(summary_by_weekday, 
          file = '~/Documents/epudom/DA/data_analytics_portfolio_project/GDAC/Case_Study_1/analysis/summary_by_weekday.csv')


# 1. Summarize by bike type
summary_by_bike_type <- clean_df %>%
  group_by(usertype, bike_type) %>%
  summarise(
    row_count = n(),
    avg_trip_duration = mean(trip_duration),
    max_trip_duration = max(trip_duration),
    min_trip_duration = min(trip_duration),
    mode_day_of_week = getMode(day_of_week)
  ) %>%
  arrange(usertype)

View(summary_by_bike_type)

write.csv(summary_by_bike_type, 
          file = '~/Documents/epudom/DA/data_analytics_portfolio_project/GDAC/Case_Study_1/analysis/summary_by_bike_type.csv')

# 3. Summarize by station
summary_by_station <- union(filter(clean_df, usertype == 'casual') %>%
  group_by(usertype, from_station_name) %>%
  summarise(
    row_count = n()
  ) %>%
    arrange(-row_count) %>%
    head(5),
  filter(clean_df, usertype == 'member') %>%
  group_by(usertype, from_station_name) %>%
  summarise(
    row_count = n()
  ) %>%
    arrange(-row_count) %>%
    head(5))

View(summary_by_station)

write.csv(summary_by_station, 
          file = '~/Documents/epudom/DA/data_analytics_portfolio_project/GDAC/Case_Study_1/analysis/summary_by_station_2.csv')

# 2. Summarize by year (this is because the data contains records from both 2022 and 2023(Q1, Q2 and Q3))

total_year_count <- clean_df %>%
  group_by(year = year(start_time), 
           mnth = months(start_time), 
           qtr = quarters(start_time), 
           usertype) %>%
  summarise(
    row_count = n(),
    avg_trip_duration = mean(trip_duration),
    mode_day_of_week = getMode(day_of_week),
    mode_bike_type = getMode(bike_type)
  )

View(total_year_count)

write.csv(total_year_count, 
          file = '~/Documents/epudom/DA/data_analytics_portfolio_project/GDAC/Case_Study_1/analysis/total_year_count.csv')


# PLOTS
# 1. Show proportion of members to casual users
clean_df %>%
  group_by(usertype) %>%
  summarize(
    row_count = n()
  ) %>%
  ggplot(aes(x = "", y = row_count, fill = usertype)) +
  geom_bar(stat = "identity", width = 1, color = "white") + 
  coord_polar("y", start = 0) + 
  theme_void() +
  theme(legend.position = "none") +
  geom_text(aes(label = usertype), 
            color = "white", 
            size = 6, 
            position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Set1")

# 2. Average trip duration by user type
ggplot(data = summary_df, aes(x = day_of_week, y = avg_trip_duration, fill = usertype)) +
  geom_col(position = "dodge")+
  scale_fill_brewer(palette = "Set1")


ggplot(data = yr_2022, aes(x = qtr, y = row_count, fill = member_casual)) +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_y_continuous(labels = scales::comma) +
  scale_fill_brewer(palette = "Set1")

ggplot(data = yr_2023, aes(x = qtr, y = row_count, fill = member_casual)) +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_y_continuous(labels = scales::comma) +
  scale_fill_brewer(palette = "Set1")


ggplot(data = yr_2022, aes(x = mnth, y = row_count, fill = member_casual)) +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_y_continuous(labels = scales::comma) +
  scale_fill_brewer(palette = "Set1") + 
  theme(axis.text.x = element_text(angle = 45))


ggplot(data = yr_2022, aes(x = qtr, y = avg_trip_duration, group = member_casual, color = member_casual)) +
  geom_smooth() + 
  theme(axis.text.x = element_text(angle = 45))



  

  







  
  
  
