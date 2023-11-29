# install.packages('tidyverse')
library(tidyverse)
# install.packages('shiny')
# install.packages('lubridate')
# library(lubridate)

# Read data file
raw_trips_q1 <- read.csv('datasets/2022_Q1.csv') 

# Clean it up a bit: changed the column 'rideable_type' to 'ride_type'
trips_q1 <- raw_trips_q1 %>%
  rename(ride_type = rideable_type)

View(trips_q1)

str(trips_q1)

# trips_q1$trip_duration <- as.POSIXct(strptime(trips_q1$trip_duration, format="%M:%S"))
# trips_q1$trip_duration <- hms(trips_q1$trip_duration)
# trips_q1$trip_duration <- strptime(trips_q1$trip_duration, "%H:%M:%S")
# str(trips_q1)

# Function to get the mode of a vector
getMode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

formatMeanTime <- function(t) {
  format(mean(strptime(t, "%H:%M:%S")), "%H:%M:%S")
}
# Create a summary table of main dataset 
# Get the mean, max, and min trip duration, the most day of the week and the 
# most ride type by member type
summary_df <- trips_q1 %>%
  group_by(member_casual) %>%
  summarise(
    average_duration = formatMeanTime(trip_duration), 
    maximum_duration = max(trip_duration),
    minimum_duration = min(trip_duration),
    mode_day_of_week = getMode(start_day_of_week),
    mode_ride_type = getMode(ride_type)
  ) 

View(summary_df)

# Retrieve the count by member type 
count_df <- count(trips_q1, member_casual)

# This is main summary table
main_summary_df <- left_join(summary_df, count_df, by = 'member_casual') %>%
  rename(count = n)

View(main_summary_df)

str(main_summary_df)

# Function to filter on member type
filtered_df <- function(df, filter_col){
  filter(df, member_casual == filter_col) 
}

# Split dataset by member type
casual_df <- filtered_df(trips_q1, filter_col = 'casual')
member_df <- filtered_df(trips_q1, filter_col = 'member')


ggplot(data = casual_df) + 
  geom_bar(mapping = aes(x = start_day_of_week, fill = ride_type))

ggplot(data = casual_df) + 
  geom_bar(mapping = aes(x = start_day_of_week)) + 
  facet_grid(~ride_type)+ 
  theme(axis.text.x = element_text(angle = 45))

ggplot(data = member_df) + 
  geom_bar(mapping = aes(x = start_day_of_week, fill = ride_type))

ggplot(data = member_df) + 
  geom_bar(mapping = aes(x = start_day_of_week)) + 
  facet_grid(~ride_type)+ 
  theme(axis.text.x = element_text(angle = 45))

# Based on the summary, casual riders mostly ride on Sundays 
# while members tend to mostly ride on  Tuesdays

# To further drill down, we will be getting the number of rides on each day of the week
summarize_df_func <- function(df){
  sub_df <- df %>%
    group_by(start_day_of_week) %>%
    summarise(
      average_trip_duration = mean(trip_duration),
      max_trip_duration = max(trip_duration),
    ) %>%
    rename(day_of_week = start_day_of_week)
  
  sub_count_df <- count(df, start_day_of_week) %>%
    arrange(-n) %>%
    rename(total_num = n, day_of_week = start_day_of_week)
  
  sub_summary_df <- left_join(sub_df, sub_count_df, by = 'day_of_week') %>%
    arrange(-total_num)
  
  return(sub_summary_df)
}

casual_summary_df <- summarize_df_func(casual_df)
member_summary_df <- summarize_df_func(member_df)
map

test_df <- trips_q1 %>%
  hms(trips_q1$trip_duration)

View(test_df)

ggplot(data = trips_q1, aes(x = start_day_of_week, y = mean(trip_duration), group = member_casual)) +
  geom_line(aes(linetype = member_casual))+
  geom_point()


# # rm(week_summary_df)
# rm(casual_df,casual_summary_df,count_df, main_summary_df, raw_trips_q1, member_df, 
#    summary_df, trips_q1, test_df, filtered_df, formatMeanTime, formatTime, getCount, 
#    getMode)








