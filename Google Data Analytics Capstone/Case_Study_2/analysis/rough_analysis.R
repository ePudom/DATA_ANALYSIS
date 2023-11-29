# install.packages("tidyverse")
# library(tidyverse)

#Read the data files
daily_activity <- read.csv("./datasets/main_data/daily_activity.csv")
hourly_calories <- read.csv("./datasets/main_data/hourly_calories.csv")
hourly_intensities <- read.csv("./datasets/main_data/hourly_intensities.csv")
hourly_steps <- read.csv("./datasets/main_data/hourly_steps.csv")
sleep <- read.csv("./datasets/main_data/sleep_day.csv")
heartrate <- read.csv("./datasets/main_data/heartrate_seconds.csv")
weight <- read.csv("./datasets/main_data/weight_log.csv")

# Number of participants in each data file
n_distinct(daily_activity$Id)
n_distinct(hourly_calories$Id)
n_distinct(hourly_intensities$Id)
n_distinct(hourly_steps$Id)
n_distinct(sleep$Id)
n_distinct(heartrate$Id)

# Number of records in each data file
nrow(hourly_calories)
nrow(hourly_intensities)
nrow(hourly_steps)

# 
glimpse(daily_activity)
glimpse(hourly_calories)
glimpse(hourly_intensities)
glimpse(hourly_steps)
glimpse(sleep)
glimpse(heartrate)

head(daily_activity)
head(hourly_intensities)

# Check for NAs
anyNA(daily_activity)
anyNA(hourly_calories)
anyNA(hourly_intensities)
anyNA(hourly_steps)
anyNA(sleep)
anyNA(heartrate)

# Check for duplicates
anyDuplicated(daily_activity)
anyDuplicated(hourly_calories)
anyDuplicated(hourly_intensities)
anyDuplicated(hourly_steps)
anyDuplicated(sleep)
anyDuplicated(heartrate)

# Verify duplicates
duplicated(sleep)
filter(sleep, duplicated(sleep) == TRUE)

# Remove duplicates 
sleep <- unique(sleep)

View(head(daily_activity))
View(head(hourly_calories))
View(sleep)


# Format and split datetime values to date and time columns
daily_activity$ActivityDate = as.POSIXct(daily_activity$ActivityDate, format = "%m/%d/%Y", tz = Sys.timezone())
daily_activity$ActivityDate <- as.Date(format(daily_activity$ActivityDate, format = "%Y/%m/%d"))
daily_activity$ActivityWeekDay <- format(as.Date(daily_activity$ActivityDate), "%A")

hourly_calories$ActivityHour =  as.POSIXct(hourly_calories$ActivityHour, format = "%m/%d/%Y %I:%M:%S %p", tz = Sys.timezone())
hourly_calories$ActivityDate <- as.Date(format(hourly_calories$ActivityHour, format = "%Y/%m/%d"))
hourly_calories$ActivityTime <- format(hourly_calories$ActivityHour, format = "%H:%M:%S")
hourly_calories$ActivityWeekDay <- format(as.Date(hourly_calories$ActivityDate), "%A")

hourly_intensities$ActivityHour =  as.POSIXct(hourly_intensities$ActivityHour, format = "%m/%d/%Y %I:%M:%S %p", tz = Sys.timezone())
hourly_intensities$ActivityDate <- as.Date(format(hourly_intensities$ActivityHour, format = "%Y/%m/%d"))
hourly_intensities$ActivityTime <- format(hourly_intensities$ActivityHour, format = "%H:%M:%S")
hourly_intensities$ActivityWeekDay <- format(as.Date(hourly_intensities$ActivityDate), "%A")

hourly_steps$ActivityHour =  as.POSIXct(hourly_steps$ActivityHour, format = "%m/%d/%Y %I:%M:%S %p", tz = Sys.timezone())
hourly_steps$ActivityDate <- as.Date(format(hourly_steps$ActivityHour, format = "%Y/%m/%d"))
hourly_steps$ActivityTime <- format(hourly_steps$ActivityHour, format = "%H:%M:%S")
hourly_steps$ActivityWeekDay <- format(as.Date(hourly_steps$ActivityDate), "%A")

sleep$SleepDay =  as.POSIXct(sleep$SleepDay, format = "%m/%d/%Y %I:%M:%S %p", tz = Sys.timezone())
sleep$SleepDate <- as.Date(format(sleep$SleepDay, format = "%Y/%m/%d"))
sleep$SleepTime <- format(sleep$SleepDay, format = "%H:%M:%S")
sleep$SleepWeekDay <- format(as.Date(sleep$SleepDate), "%A")

heartrate <- heartrate %>%
  rename(RecordDay = Time)

heartrate$RecordDay =  as.POSIXct(heartrate$RecordDay, format = "%m/%d/%Y %I:%M:%S %p", tz = Sys.timezone())
heartrate$Date <- as.Date(format(heartrate$RecordDay, format = "%Y/%m/%d"))
heartrate$Time <- format(heartrate$RecordDay, format = "%H:%M:%S")


# Review the column names of the cleaned data 
colnames(daily_activity)
colnames(hourly_calories)
colnames(hourly_intensities)
colnames(hourly_steps)
colnames(sleep)
colnames(heartrate)
colnames(weight)


# We will be calculating the average daily heart rate for each participant
average_daily_heartrate <- heartrate %>%
  group_by(Id, Date) %>%
  summarise(AverageValue = mean(Value))

View(average_daily_heartrate)

# ----------------------------------------------------------------------------- #

# Since the the number of participants who recorded their sleep pattern and heart rates
# are less than the total number of participants, we will be creating a new data frame
# to join the daily activity with the sleep records and heart rate so we can analyse those groups 

# - First we check that the number of days is the same on both records
n_distinct(daily_activity$ActivityDate)
n_distinct(sleep$SleepDate)
n_distinct(heartrate$Date)

# - Since this is the same, we then go on to join the two data frames
sleep_daily_activity <- sleep %>%
  left_join(daily_activity, join_by(Id == Id, SleepDate == ActivityDate))

sleep_daily_activity_2 <- sleep %>%
  full_join(daily_activity, join_by(Id == Id, SleepDate == ActivityDate)) %>%
  full_join(average_daily_heartrate, join_by(Id == Id, SleepDate == Date))


head(sleep_daily_activity)

# - Verify the newly created data frame
nrow(sleep_daily_activity)
n_distinct(sleep_daily_activity$Id)
n_distinct(sleep_daily_activity$SleepDay)
any(is.na(sleep_daily_activity))
anyDuplicated(sleep_daily_activity)

# - Clean the data frame further by selecting relevant columns and also renaming some of the columns
sleep_daily_activity <- sleep_daily_activity %>%
  select("Id", "SleepDate", "SleepWeekDay", "TotalSteps", "TotalDistance", "VeryActiveDistance",
         "ModeratelyActiveDistance", "LightActiveDistance", "SedentaryActiveDistance",
         "VeryActiveMinutes", "FairlyActiveMinutes", "LightlyActiveMinutes", "SedentaryMinutes",
         "Calories", "TotalSleepRecords", "TotalMinutesAsleep", "TotalTimeInBed") %>%
  rename(FairlyActiveDistance = ModeratelyActiveDistance, 
         Date = SleepDate,
         WeekDay = SleepWeekDay)

sleep_daily_activity_2 <- sleep_daily_activity_2 %>%
  select("Id", "SleepDate", "SleepWeekDay", "TotalSteps", "TotalDistance", "VeryActiveDistance",
         "ModeratelyActiveDistance", "LightActiveDistance", "SedentaryActiveDistance",
         "VeryActiveMinutes", "FairlyActiveMinutes", "LightlyActiveMinutes", "SedentaryMinutes",
         "Calories", "TotalSleepRecords", "TotalMinutesAsleep", "TotalTimeInBed", "AverageValue") %>%
  rename(FairlyActiveDistance = ModeratelyActiveDistance, 
         Date = SleepDate,
         WeekDay = SleepWeekDay,
         AverageHeartRate = AverageValue)

View(sleep_daily_activity_2)

# ----------------------------------------------------------------------------- #

# Next we will be creating average table to group each participant
participant_summary <- sleep_daily_activity_2 %>%
  group_by(Id) %>%
  summarise(avg_steps = mean(TotalSteps),
            avg_distance = mean(TotalDistance),
            avg_very_active_min = mean(VeryActiveMinutes),
            avg_mod_active_min = mean(FairlyActiveMinutes),
            avg_lightly_active_min = mean(LightlyActiveMinutes),
            avg_sedentary_min = mean(SedentaryMinutes),
            avg_calories = mean(Calories),
            avg_min_asleep = mean(TotalMinutesAsleep),
            avg_time_in_bed = mean(TotalTimeInBed),
            avg_min_not_asleep = mean(TotalTimeInBed - TotalMinutesAsleep),
            avg_heartrate = mean(AverageHeartRate),
            # no_activity_days = n(!is.na(TotalSteps)),
            # no_sleep_days = n(!is.na(TotalMinutesAsleep)),
            # no_heartrate_days = n(!is.na(AverageHeartRate)))
            
distinct(sleep$Id)

rm(participant_summary)
View(participant_summary)

# General Summary of each table1
daily_activity %>% 
  select("TotalSteps", "TotalDistance", "TrackerDistance", "LoggedActivitiesDistance", 
         "VeryActiveDistance", "ModeratelyActiveDistance", "LightActiveDistance", 
         "SedentaryActiveDistance", "VeryActiveMinutes", "FairlyActiveMinutes", 
         "LightlyActiveMinutes", "SedentaryMinutes", "Calories") %>%
  
  summary()

sleep %>% 
  select("TotalMinutesAsleep", "TotalTimeInBed") %>%
  summary()


# Physical Activity 
# Comparing the active minutes
# - Average very active minutes of 4 and high sendatary minutes of 991 shows that 
# the activity level of partcipants seem to be more sendantry, hence partcipants consists of mostly office workers.
# Comparing Active distance
# - Very active distance of 1.503 shows participants rarely partake in physical activitoes

# Steps
# Daily average steps of 7638 falls within the recommended 7000 - 10000 steps required
# whooping 10727 shows that more steps are covered during the later part of the day

# Calories
# - Daily average calories of 2304 falls within the recommended 2,000 to 2,500 calories required
# - 2793 shows that more calories are burned during the later part of the day

# Sleep 
# - Daily average of 419 minutes of sleep (approx 7hrs) falls an hour short os the daily recommended
# sleep time 

# ----------------------------------------------------------------------------- #

# 1. Daily Activity Data:

# a. Total Steps and Distance: Analyze the total steps and distance covered by users 
# on a daily basis to understand their overall activity levels.
  
ggplot(data = daily_activity, aes(x = ActivityDate, y = TotalSteps)) +
  geom_bar(stat = "identity", fill =  "pink2") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous(
    "ActivityDate", 
    labels = as.character(daily_activity$ActivityDate), 
    breaks = daily_activity$ActivityDate) + 
  scale_y_continuous(labels = scales::comma)

ggplot(data = daily_activity, aes(x = ActivityDate, y = TotalDistance)) +
  geom_bar(stat = "identity", fill =  "pink2") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous(
    "ActivityDate", 
    labels = as.character(daily_activity$ActivityDate), 
    breaks = daily_activity$ActivityDate)

ggplot(data = daily_activity, aes(x = TotalDistance, y = TotalSteps)) +
  geom_point() + 
  geom_smooth(method = "loess", formula = "y ~ x") 

# There is a positive relationship between steps and distance

# strike Active Minutes: Explore very active, moderately active, and lightly active minutes to identify peak activity periods during the day.

# Sedentary Behavior: Investigate sedentary minutes to understand periods of inactivity.

average_daily_activity <- daily_activity %>%
  group_by(ActivityDate, ActivityWeekDay) %>%
  summarise(avg_very_active_dist = mean(VeryActiveDistance),
            avg_mod_active_dist = mean(ModeratelyActiveDistance),
            avg_lightly_active_dist = mean(LightActiveDistance),
            avg_sedentary_dist = mean(SedentaryActiveDistance),
            avg_very_active_min = mean(VeryActiveMinutes),
            avg_mod_active_min = mean(FairlyActiveMinutes),
            avg_lightly_active_min = mean(LightlyActiveMinutes),
            avg_sedentary_min = mean(SedentaryMinutes))

View(average_daily_activity)

# -- Analyzing the sedentary distance and minutes 
ggplot(data = average_daily_activity, aes(x = avg_sedentary_min, y = avg_sedentary_dist)) +
  geom_point() +
  geom_smooth(method = "loess", formula = "y ~ x")
  # geom_bar(stat = "identity")

ggplot(data = average_daily_activity, aes(x = ActivityDate, y = avg_sedentary_min)) +
  geom_point() +
  geom_smooth(method = "loess", formula = "y ~ x") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous(
    "ActivityDate", 
    labels = as.character(daily_activity$ActivityDate), 
    breaks = daily_activity$ActivityDate)

# -- Analyzing the lightly active distance and minutes 
ggplot(data = average_daily_activity, aes(x = avg_lightly_active_min, y = avg_lightly_active_dist)) +
  geom_point() +
  geom_smooth(method = "loess", formula = "y ~ x")

ggplot(data = average_daily_activity, aes(x = ActivityDate, y = avg_lightly_active_min)) +
  geom_point() +
  # geom_bar(stat = "identity", fill = "pink2") +
  geom_smooth(method = "loess", formula = "y ~ x") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous(
    "ActivityDate", 
    labels = as.character(daily_activity$ActivityDate), 
    breaks = daily_activity$ActivityDate)

# -- Analyzing the moderately active distance and minutes 
ggplot(data = average_daily_activity, aes(x = avg_mod_active_min, y = avg_mod_active_dist)) +
  geom_point() +
  geom_smooth(method = "loess", formula = "y ~ x")

ggplot(data = average_daily_activity, aes(x = ActivityDate, y = avg_mod_active_min)) +
  geom_point() +
  # geom_bar(stat = "identity", fill = "pink2") +
  geom_smooth(method = "loess", formula = "y ~ x") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous(
    "ActivityDate", 
    labels = as.character(daily_activity$ActivityDate), 
    breaks = daily_activity$ActivityDate)

# -- Analyzing the very active distance and minutes 
ggplot(data = average_daily_activity, aes(x = avg_very_active_min, y = avg_very_active_dist)) +
  geom_point() +
  geom_smooth(method = "loess", formula = "y ~ x")

ggplot(data = average_daily_activity, aes(x = ActivityDate, y = avg_very_active_min)) +
  geom_point() +
  # geom_bar(stat = "identity", fill = "pink2") +
  geom_smooth(method = "loess", formula = "y ~ x") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous(
    "ActivityDate", 
    labels = as.character(daily_activity$ActivityDate), 
    breaks = daily_activity$ActivityDate)





ggplot(data = average_daily_activity, aes(x = ActivityDate, y = avg_lightly_active_min)) +
  # geom_point() +
  geom_bar(stat = "identity", fill = "pink2") +
  # geom_smooth(method = "loess", formula = "y ~ x") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous(
    "ActivityDate", 
    labels = as.character(daily_activity$ActivityDate), 
    breaks = daily_activity$ActivityDate)

ggplot(data = average_daily_activity) +
  # geom_bar(stat = "identity", fill = "pink2", aes(x = ActivityDate, y = avg_sedentary_min)) +
  # geom_point() +
  # geom_smooth(method = "loess", formula = "y ~ x") +
  geom_line(
    color = "blue3",
    size = 1,
    aes(x = ActivityDate, y = avg_lightly_active_min)) +
  geom_line(
    color = "green3", 
    size = 1, 
    aes(x = ActivityDate, y = avg_mod_active_min)) +
  geom_line(
    color = "red3", 
    size = 1, 
    aes(x = ActivityDate, y = avg_very_active_min)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous(
    "ActivityDate",
    labels = as.character(average_daily_activity$ActivityDate),
    breaks = average_daily_activity$ActivityDate) +
  labs(y = "Active minutes")



ggplot(data = daily_activity, aes(x = VeryActiveMinutes, y = TotalSteps)) +
  geom_point() + 
  geom_smooth(method = "loess", formula = "y ~ x")  

ggplot(data = daily_activity, aes(x = FairlyActiveMinutes, y = TotalSteps)) +
  geom_point() + 
  geom_smooth(method = "loess", formula = "y ~ x")  

ggplot(data = daily_activity, aes(x = SedentaryMinutes, y = TotalSteps)) +
  geom_point() + 
  geom_smooth(method = "loess", formula = "y ~ x")  

ggplot(data = daily_activity, aes(x = VeryActiveDistance, y = TotalSteps)) +
  geom_point() + 
  geom_smooth(method = "loess", formula = "y ~ x")  

ggplot(data = daily_activity, aes(x = ModeratelyActiveDistance, y = TotalSteps)) +
  geom_point() + 
  geom_smooth(method = "loess", formula = "y ~ x")  


ggplot(data = average_daily_activity, aes(x = avg_sedentary_min, y = avg_sedentary_dist)) +
  geom_point() + 
  geom_smooth(method = "loess", formula = "y ~ x")  


ggplot(data = daily_activity, aes(x = ActivityDate, y = SedentaryMinutes)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous(
    "ActivityDate", 
    labels = as.character(daily_activity$ActivityDate), 
    breaks = daily_activity$ActivityDate)


# Calories Burned: Analyze daily calorie expenditure to gauge the intensity of physical activities.


ggplot() +
  geom_bar(
    data = hourly_calories, 
    aes(x = ActivityTime, y = Calories), 
    stat = "identity",
    fill = "blue") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "Activity Time", y = "Calories")

# - The rise and fall per hour in calories per hour is similar to the intesity
# -- To verify this, we plot average calories burened per hour to average intensity


# - There is a positive corelation betwen the intensity and the number of calories burned

# Activity Weekday Patterns: Examine how activity levels vary based on different days of the week.

ggplot(data = daily_activity, aes(x = ActivityWeekDay, y = ))

# ----------------------------------------------------------------------------- #

# 2. Hourly Calorie Data:
#   
#   Calories Burned per Hour: Analyze hourly calorie expenditure to identify peak activity hours during the day.
# Activity Time Patterns: Investigate how calorie burn varies across different times of the day.
# Activity Weekday Patterns: Examine how calorie expenditure differs based on weekdays.

# ----------------------------------------------------------------------------- #

# 3. Hourly Intensity Data:
#   
glimpse(hourly_calories)

summary(hourly_intensities$TotalIntensity)
# - There is more intensity in the later part of the day

ggplot() +
  geom_bar(
    data = hourly_intensities, 
    aes(x = ActivityTime, y = AverageIntensity), 
    stat = "identity",
    fill = "blue") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "Activity Time", y = "Total Intensity")

# - There is a decline from 12AM to 3AM
# - A steady rise from 4AM to 10AM
# - Peak intensity is from 5PM to 7PM
#   Total and Average Intensity: Analyze the total and average intensity of activities on an hourly basis.
# Intensity Patterns: Identify patterns in high and low-intensity activities during different hours and weekdays.

# ----------------------------------------------------------------------------- #

# 4. Hourly Steps Data:
#   
# Steps per Hour: Analyze the number of steps taken by users on an hourly basis to identify peak activity hours.

ggplot() +
  geom_bar(
    data = hourly_steps, 
    aes(x = ActivityTime, y = StepTotal), 
    stat = "identity",
    fill = "blue") + 
  facet_wrap(~ActivityWeekDay,) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "Activity Time", y = "Total Steps") 


# Step Time Patterns: Investigate how step count varies across different times of the day.

ggplot(data = hou, aes(x = SleepWeekDay, y = TotalMinutesAsleep)) +
  geom_bar(
    stat = "identity", 
    aes(x = factor(sleep$SleepWeekDay, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")))) 


# Activity Weekday Patterns: Examine how step count differs based on weekdays.

# ----------------------------------------------------------------------------- #

# 5. Sleep Data:
#   
# Total Sleep Duration: Analyze the total minutes asleep and time spent in bed to understand users' sleep patterns.
ggplot(data = sleep, aes(x = TotalTimeInBed, y = TotalMinutesAsleep)) +
  geom_point() + 
  geom_smooth(method = "loess", formula = "y ~ x") 

ggplot(data = sleep, aes(x = SleepDate, y = TotalMinutesAsleep)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous(
    "SleepDate", 
    labels = paste(as.character(sleep$SleepDate), " - ", sleep$SleepWeekDay), 
    breaks = sleep$SleepDate)

ggplot(data = sleep, aes(x = SleepDate, y = TotalTimeInBed)) +
  geom_point() + 
  geom_smooth(method = "loess", formula = "y ~ x") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous(
    "SleepDate", 
    labels = paste(as.character(sleep$SleepDate), " - ", sleep$SleepWeekDay), 
    breaks = sleep$SleepDate)


# Sleep Quality: Investigate variations in sleep duration and quality across different days of the week.

ggplot(data = sleep, aes(x = SleepWeekDay, y = TotalMinutesAsleep)) +
  geom_bar(
    stat = "identity", 
    aes(x = factor(sleep$SleepWeekDay, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")))) 

ggplot(data = sleep, aes(x = SleepWeekDay, y = TotalTimeInBed)) +
  geom_bar(
    stat = "identity", 
    aes(x = factor(sleep$SleepWeekDay, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")))) 

# Sleep Time Patterns: Identify regular sleep times and variances in sleep onset and wake times.

# ----------------------------------------------------------------------------- #

# Additionally, it's important to perform comparative analyses such as:
#   
# Comparing Activity Levels with Sleep Patterns: Understand how physical activity during the day correlates with sleep duration and quality at night.
# Correlating Calorie Expenditure with Activity Intensity: Analyze whether higher intensity activities result in a proportionally higher calorie burn.
# DF to plot average calories and average intensity
calorie_intensity <- left_join(
  hourly_calories %>%
    group_by(ActivityTime) %>%
    summarize(average_calories = mean(Calories)),
  hourly_intensities %>%
    group_by(ActivityTime) %>%
    summarize(average_intensity = mean(TotalIntensity)),
  join_by(ActivityTime == ActivityTime)
)

View(calorie_intensity)

ggplot(data = calorie_intensity, aes(x = average_calories, y = average_intensity)) +
  geom_point() +
  geom_smooth(method = "loess", formula = "y ~ x")

# Identifying Trends Over Time: Explore trends in activity levels, calorie burn, and sleep patterns over days, weeks, or months to identify long-term behavioral patterns.









