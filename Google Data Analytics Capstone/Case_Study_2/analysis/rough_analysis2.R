# install.packages("tidyverse")
# install.packages("skimr")
library(tidyverse)
# library(skimr)
# rm(list = ls())

#Read the data files
daily_activity <- read.csv("./datasets/main_data/daily_activity.csv")
sleep <- read.csv("./datasets/main_data/sleep_day.csv")
heartrate <- read.csv("./datasets/main_data/heartrate_seconds.csv")
weight <- read.csv("./datasets/main_data/weight_log.csv")


# Number of participants in each data file
n_distinct(daily_activity$Id)
n_distinct(sleep$Id)
n_distinct(heartrate$Id)
n_distinct(weight$Id)

# Number of records in each data file
nrow(daily_activity)
nrow(sleep)
nrow(heartrate)
nrow(weight)

skim_without_charts(daily_activity)
skim_without_charts(sleep)
skim_without_charts(heartrate)
skim_without_charts(weight)

# 
glimpse(daily_activity)
glimpse(sleep)
glimpse(heartrate)
glimpse(weight)

# Check for NAs
anyNA(daily_activity)
anyNA(sleep)
anyNA(heartrate)
anyNA(weight)

# The weight data has a mostly of NAs in the 'Fat' column. However we would be excluding this 
# column in our analysis
weight <- subset(weight, select = -Fat)

# Check for duplicates
anyDuplicated(daily_activity)
anyDuplicated(sleep)
anyDuplicated(heartrate)
anyDuplicated(weight)

# The sleep data has a couple of duplicates 

# Verify duplicates
duplicated(sleep)
filter(sleep, duplicated(sleep) == TRUE)

# Remove duplicates 
sleep <- unique(sleep)

View(head(daily_activity))
View(head(hear))
View(sleep)
View(weight)

# Formating columns 
# 1. Id - from numeric to factor
daily_activity$Id <- as.factor(daily_activity$Id)
sleep$Id <- as.factor(sleep$Id)
heartrate$Id <- as.factor(heartrate$Id)
weight$Id <- as.factor(weight$Id)
weight$LogId <- as.factor(weight$LogId)

# 2. Dates - from character to dates format(YYYY-MM-DD)
daily_activity$ActivityDate = as.POSIXct(daily_activity$ActivityDate, format = "%m/%d/%Y", tz = Sys.timezone())
sleep$SleepDay = as.POSIXct(sleep$SleepDay, format = "%m/%d/%Y", tz = Sys.timezone())
heartrate$Time = as.POSIXct(heartrate$Time, format = "%m/%d/%Y %I:%M:%S %p", tz = Sys.timezone())
weight$Date = as.POSIXct(weight$Date, format = "%m/%d/%Y %I:%M:%S %p", tz = Sys.timezone())

# 3. Format the weight isManualReport to boolean 
weight$IsManualReport <- as.logical(weight$IsManualReport)

# 4. Split datetime columns to date and time columns 
daily_activity$ActivityDate <- as.Date(format(daily_activity$ActivityDate, format = "%Y/%m/%d"))
sleep$Date <- as.Date(format(sleep$SleepDay, format = "%Y/%m/%d"))
heartrate$Date <- as.Date(format(heartrate$Time, format = "%Y/%m/%d")) 
heartrate$Time_s <- format(heartrate$Time, format = "%H:%M:%S")
weight$Date_s <- as.Date(format(weight$Date, format = "%Y/%m/%d"))

# 5. Add additional weekday column to the daily activity table as this is to be used as the main table
daily_activity$Weekday <- format(daily_activity$ActivityDate, "%A")
sleep$Weekday <- format(sleep$Date, "%A")

# 6. Add additional column for BMI ranges in the weight table
# below 18.5 – underweight range. 
# between 18.5 and 24.9 – healthy weight range. 
# between 25 and 29.9 – overweight range. 
# 30 or over – obese range.
weight <- weight %>%
  mutate(BMIRange = case_when(
    BMI < 18.5 ~ "Underweight",
    BMI >= 18.5 & BMI < 25 ~ "Healthy",
    BMI >= 25 & BMI < 30 ~ "Overweight", 
    BMI >= 30 ~ "Obese",
    TRUE ~ "No BMI"
  ))

# 6. Classify users based on activity 
daily_activity <- daily_activity %>%
  mutate(ActivityType = case_when(
    SedentaryMinutes >= LightlyActiveMinutes & 
      SedentaryMinutes >= FairlyActiveMinutes & 
      SedentaryMinutes >= VeryActiveMinutes 
    ~ "Sedentary",
    LightlyActiveMinutes >= SedentaryMinutes & 
      LightlyActiveMinutes >= FairlyActiveMinutes & 
      LightlyActiveMinutes >= VeryActiveMinutes 
    ~ "Lightly Active",
    FairlyActiveMinutes >= LightlyActiveMinutes & 
      FairlyActiveMinutes >= SedentaryMinutes & 
      FairlyActiveMinutes >= VeryActiveMinutes 
    ~ "Moderately Active",
    VeryActiveMinutes >= LightlyActiveMinutes & 
      VeryActiveMinutes >= FairlyActiveMinutes & 
      VeryActiveMinutes >= SedentaryMinutes 
    ~ "Very Active",
    TRUE ~ "No Activity"
  ))

# 7. Calculate total minutes not asleep and classify based on the sleep length
sleep <- sleep %>%
  mutate(TotalMinutesNotAsleep = TotalTimeInBed - TotalMinutesAsleep,
         SleepRange = case_when(
           TotalMinutesAsleep < (7 * 60) ~ "Under Sleep",
           TotalMinutesAsleep >= (7 * 60) & 
             TotalMinutesAsleep <= (8 * 60) ~ "Good Sleep",
           TotalMinutesAsleep > (8 * 60) ~ "Over Sleep",
           TRUE ~ "No Sleep"
  ))


# 8. Rename columns and select necessary columns 

# a. daily_activity
daily_activity <- daily_activity %>%
  select("Id", "ActivityDate", "TotalSteps", "TotalDistance", "VeryActiveDistance", "ModeratelyActiveDistance",
         "LightActiveDistance", "SedentaryActiveDistance", "VeryActiveMinutes", "FairlyActiveMinutes", 
         "LightlyActiveMinutes", "SedentaryMinutes", "Calories", "Weekday", "ActivityType") %>%
  rename(ModeratelyActiveMinutes = FairlyActiveMinutes,
         Date = ActivityDate)

# b.heartrate
heartrate <- heartrate %>%
  select("Id", "Date", "Time_s", "Value") %>%
  rename(Time = Time_s)

# c. sleep 
sleep <- sleep %>%
  select("Id", "Date", "Weekday", "TotalTimeInBed", "TotalMinutesAsleep", "TotalMinutesNotAsleep", "SleepRange")

# d. weight
weight <- weight %>%
  select("Id", "Date_s", "BMI", "IsManualReport", "BMIRange") %>%
  rename(Date = Date_s)


# ------------------------------------------------------------------------------------------------ #


# 1. Daily Activity Dataset:
# a. Physical Activity Patterns: Analyze "TotalSteps," "VeryActiveMinutes," "ModeratelyActiveMinutes," 
# and "LightlyActiveMinutes" to identify trends in users' physical activities. 

daily_activity %>% 
  select("TotalSteps", "VeryActiveMinutes", "ModeratelyActiveMinutes", "LightlyActiveMinutes") %>%
  summary()

# TotalSteps    VeryActiveMinutes ModeratelyActiveMinutes LightlyActiveMinutes
# Min.   :    0   Min.   :  0.00    Min.   :  0.00          Min.   :  0.0       
# 1st Qu.: 3790   1st Qu.:  0.00    1st Qu.:  0.00          1st Qu.:127.0       
# Median : 7406   Median :  4.00    Median :  6.00          Median :199.0       
# Mean   : 7638   Mean   : 21.16    Mean   : 13.56          Mean   :192.8       
# 3rd Qu.:10727   3rd Qu.: 32.00    3rd Qu.: 19.00          3rd Qu.:264.0       
# Max.   :36019   Max.   :210.00    Max.   :143.00          Max.   :518.0  


# OBSERVATIONS
# i. Average daily steps of 7638 falls within the recommended daily steps of 7,000 - 10,000
# ii. More time was spent on light activities
ggplot(daily_activity, aes(TotalSteps, VeryActiveMinutes)) +
  geom_point(shape = 16, color = "purple3") + 
  geom_smooth(span = 0.3) +
  theme_bw() +
  labs(title = "Total Steps vs. Very Active Minutes",
       x = "Very Active Minutes",                   
       y = "Total Steps") +                   
  theme(plot.title = element_text(hjust = 0.5))

# iii. Though there is a positive relationship between no of steps and the VeryActiveMinutes,
# most people have less active minutes with less no of steps

ggplot(daily_activity, aes(TotalSteps, ModeratelyActiveMinutes)) +
  geom_point(shape = 16, color = "yellow3") + 
  geom_smooth(span = 0.3) +
  theme_bw() +
  labs(title = "Total Steps vs. Moderately Active Minutes",
       x = "Moderately Active Minutes",                   
       y = "Total Steps") +                   
  theme(plot.title = element_text(hjust = 0.5))

# iv. This relationship is slightly similar that of very active

ggplot(daily_activity, aes(TotalSteps, LightlyActiveMinutes)) +
  geom_point(shape = 16, color = "coral3") + 
  geom_smooth(span = 0.3) +
  theme_bw() +
  labs(title = "Total Steps vs. Lightly Active Minutes",
       x = "Lightly Active Minutes",                   
       y = "Total Steps") +                   
  theme(plot.title = element_text(hjust = 0.5))

# iv. This shows hat for lesser active minutes for light activities there was a straight line but
# as more time is spent doing light activites there was a lesser no of steps.

# Are there specific days or times when users are more active?

average_per_weekday <- daily_activity %>%
  group_by(Weekday) %>%
  summarise(avg_steps = round(mean(TotalSteps), digits = 0),
            avg_vam = round(mean(VeryActiveMinutes), digits = 2),
            avg_mam = round(mean(ModeratelyActiveMinutes), digits = 2),
            avg_lam = round(mean(LightlyActiveMinutes), digits = 2),
            avg_sed = round(mean(SedentaryMinutes), digits = 2))

ggplot(average_per_weekday, 
       aes(x = factor(Weekday, 
                      levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")),
           y = avg_steps)) +
geom_bar(stat = "identity", position = "dodge", fill="blue3") +
geom_text(aes(label = avg_steps), vjust = 2, color = "white") +
theme_bw() +
labs(title = "Steps vs. Day of the Week", 
     x = "Weekday", 
     y = "Steps") +
theme(plot.title = element_text(hjust = 0.5)) 

# i. they tend to take more steps on Saturdays and least number of steps on Sunday
# ii. On an average, they complete the recommended number of steps everyday except on Sunday 
# iii. Highest average step is on Saturday

ggplot(average_per_weekday, 
       aes(x = factor(Weekday, 
                      levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")),
           y = avg_vam)) +
geom_bar(stat = "identity", position = "dodge", fill="red3") +
geom_text(aes(label = avg_vam), vjust = 2, color = "white") +
theme_bw() +
labs(title = "Average very active minutes vs. Day of the Week", 
     x = "Weekday", 
     y = "Steps") +
theme(plot.title = element_text(hjust = 0.5))

# iv. However, they do more physical activity on Monday

ggplot(average_per_weekday, 
       aes(x = factor(Weekday, 
                      levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")),
           y = avg_mam)) +
geom_bar(stat = "identity", position = "dodge", fill="yellow3") +
geom_text(aes(label = avg_mam), vjust = 2, color = "black") +
theme_bw() +
labs(title = "Average moderately active minutes vs. Day of the Week", 
     x = "Weekday", 
     y = "Average moderately active minutes") +
theme(plot.title = element_text(hjust = 0.5)) 

# v. They do more moderate activities on Saturday

ggplot(average_per_weekday, 
       aes(x = factor(Weekday, 
                      levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")),
           y = avg_lam)) +
geom_bar(stat = "identity", position = "dodge", fill="coral3") +
geom_text(aes(label = avg_lam), vjust = 2, color = "black") +
theme_bw() +
labs(title = "Average lightly active minutes vs. Day of the Week", 
     x = "Weekday", 
     y = "Average lightly active minutes") +
theme(plot.title = element_text(hjust = 0.5)) 

# vi. This shows that more time is spent doing light activities and these are mostly on Fridays and Saturdays
  
# CONCLUSION
# From these observations, we can say that the users tend to be more active on Saturdays 


# b. Sedentary Behavior: Explore "SedentaryMinutes" to understand periods of inactivity. 

summary(daily_activity$SedentaryMinutes)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0   729.8  1057.5   991.2  1229.5  1440.0 

# Periods of inactivity are way higher than periods of in activity

ggplot(daily_activity, aes(TotalSteps, SedentaryMinutes)) +
  geom_point(shape = 16, color = "orange2") + 
  geom_smooth() +
  theme_bw() +
  labs(title = "Total Steps vs. Sedentary Minutes",
       x = "Sedentary Minutes",                   
       y = "Total Steps") +                   
  theme(plot.title = element_text(hjust = 0.5))

cor(TotalDistance, SedentaryMinutes)

# There is little or no correlation between steps and inactivity periods

ggplot(average_per_weekday, 
       aes(x = factor(Weekday, 
                      levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")),
           y = avg_sed)) +
  geom_bar(stat = "identity", position = "dodge", fill="orange2") +
  geom_text(aes(label = avg_sed), vjust = 2, color = "black") +
  theme_bw() +
  labs(title = "Average sedentary minutes vs. Day of the Week", 
       x = "Weekday", 
       y = "Average sedentary minutes") +
  theme(plot.title = element_text(hjust = 0.5)) 

# There is high inactivity days as low as 16 hours (962) which is roughly 2/3 of the whole day.
# Coupled with the mean 

# Identify patterns and potential health implications associated with prolonged sitting.

# # Potential health implications include
# * Increased Health Risks: Prolonged sedentary behavior is associated with an 
# ** increased risk of obesity, heart disease, type 2 diabetes, and certain cancers. 
# ** It can also lead to high blood pressure and high cholesterol levels.
#
# * Muscle Weakness: Sitting for extended periods can cause your muscles to weaken and atrophy, 
# ** leading to poor posture and musculoskeletal problems.
# 
# * Mental Health Issues: Sedentary behavior is linked to higher stress levels, 
# ** anxiety, and depression. Physical activity helps release endorphins, which are natural mood lifters.
# 
# * Digestive Problems: Sitting too much can slow down your digestion, 
# ** leading to gastrointestinal issues and discomfort.
# 
# * Sleep Disruptions: Prolonged sedentary behavior, especially close to bedtime, 
# ** can disrupt your sleep patterns and cause insomnia.
# 
# * Reduced Lifespan: Studies have shown that spending long hours sitting each day can shorten your lifespan.


average_per_participant <- daily_activity %>%
  group_by(Id) %>%
  summarise(avg_steps = round(mean(TotalSteps), digits = 0),
            avg_vam = round(mean(VeryActiveMinutes), digits = 2),
            avg_mam = round(mean(ModeratelyActiveMinutes), digits = 2),
            avg_lam = round(mean(LightlyActiveMinutes), digits = 2),
            avg_sed = round(mean(SedentaryMinutes), digits = 2))

  ggplot(average_per_participant,
    aes(x = reorder(Id, -avg_sed),
        y = avg_sed)) +
  geom_bar(stat = "identity", position = "dodge", fill="green3") +
  geom_text(aes(label = avg_sed), hjust = 2, angle = 90, color = "black") +
  theme_bw() +
  labs(title = "Average sedentary minutes per participant", 
       x = "Participant ID", 
       y = "Average sedentary minutes") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))


average_per_participant %>%
  filter(average_per_participant$avg_sed > mean(daily_activity$SedentaryMinutes)) %>% 
  ggplot(
       aes(x = Id,
           y = avg_sed)) +
  geom_bar(stat = "identity", position = "dodge", fill="green3") +
  geom_text(aes(label = avg_sed), hjust = 2, angle = 90, color = "black") +
  theme_bw() +
  labs(title = "Average sedentary minutes per participant", 
       subtitle = "filtered participants that have sendentary minutes greater than the overall average",
       x = "Participant ID", 
       y = "Average sedentary minutes") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

# Participants spend an unhealthy amount of time on sedentary activities

# c. Caloric Expenditure: Study "Calories" to gauge users' daily energy expenditure.

summary(daily_activity$Calories)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0    1828    2134    2304    2793    4900 

# Relationship between steps and calories burned

ggplot(daily_activity, aes(TotalSteps, Calories)) +
  geom_point(shape = 16, color = "pink2") + 
  geom_smooth() +
  theme_bw() +
  labs(title = "Total Steps vs. Calories",
       x = "Total Steps",                   
       y = "Calories") +                   
  theme(plot.title = element_text(hjust = 0.5))
cor(TotalSteps, Calories)

# There is a positive correlation showing the more steps the steps the more calories that were burned
# Relationship between sedentary and calories burned

ggplot(daily_activity, aes(SedentaryMinutes, Calories)) +
  geom_point(shape = 16, color = "pink2") + 
  geom_smooth() +
  theme_bw() +
  labs(title = "Sedentary Minutes vs. Calories",
       x = "Sedentary Minutes",                   
       y = "Calories") +                   
  theme(plot.title = element_text(hjust = 0.5))

attach(daily_activity)
cor(SedentaryMinutes, Calories) 

# There is no correlation between these two values

# Relationship between very active minutes and calories burned

ggplot(daily_activity, aes(VeryActiveMinutes, Calories)) +
  geom_point(shape = 16, color = "pink2") + 
  geom_smooth() +
  theme_bw() +
  labs(title = "Very Active Minutes vs. Calories",
       x = "Very Active Minutes",                   
       y = "Calories") +                   
  theme(plot.title = element_text(hjust = 0.5))
cor(VeryActiveMinutes, Calories)

# There is a strong correlation here. The More the time spent doing more physical activities, 
# the calories that were burned

ggplot(daily_activity, aes(ModeratelyActiveMinutes, Calories)) +
  geom_point(shape = 16, color = "pink2") + 
  geom_smooth() +
  theme_bw() +
  labs(title = "Moderately Active Minutes vs. Calories",
       x = "Moderately Active Minutes",                   
       y = "Calories") +                   
  theme(plot.title = element_text(hjust = 0.5))

cor(ModeratelyActiveMinutes, Calories)

ggplot(daily_activity, aes(LightlyActiveMinutes, Calories)) +
  geom_point(shape = 16, color = "pink2") + 
  geom_smooth() +
  theme_bw() +
  labs(title = "Lightly Active Minutes vs. Calories",
       x = "Lightly Active Minutes",                   
       y = "Calories") +                   
  theme(plot.title = element_text(hjust = 0.5))

cor(LightlyActiveMinutes, Calories) # Weak correlation 

# We can see that there is a relationship between high activity minutes 
# We will further analyze how calories are burned per week day

daily_activity %>%
  group_by(Weekday) %>%
  summarise(total_cal = sum(Calories)) %>%
  ggplot(aes(x = factor(Weekday, 
                        levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")),
             y = total_cal)) +
  geom_bar(stat = "identity", position = "dodge", fill="pink2") +
  geom_text(aes(label = total_cal), vjust = 2, color = "black") +
  theme_bw() +
  labs(title = "Calories vs. Day of the Week", 
       x = "Weekday", 
       y = "Calories") +
  theme(plot.title = element_text(hjust = 0.5)) 

daily_activity %>%
  group_by(Weekday) %>%
  summarise(avg_cal = round(mean(Calories), digits = 2)) %>%
  ggplot(aes(x = factor(Weekday, 
                        levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")),
             y = avg_cal)) +
  geom_bar(stat = "identity", position = "dodge", fill="pink2") +
  geom_text(aes(label = avg_cal), vjust = 2, color = "black") +
  theme_bw() +
  labs(title = "Average Calories burned per Weekday", 
       x = "Weekday", 
       y = "Average Calories") +
  theme(plot.title = element_text(hjust = 0.5)) 

# 

# Identify factors influencing higher or lower calorie burn.

# d. Activity Types: Utilize "ActivityType" to categorize different types of activities and understand user preferences.

daily_activity %>%
  group_by(ActivityType) %>%
  summarize(
    row_count = n(),
    percentage = round((n()/nrow(daily_activity)) * 100, digits = 2),
  ) %>%
  ggplot(aes(x = "", y = row_count, fill = ActivityType)) +
  geom_bar(stat = "identity", width = 1, color = "white") + 
  coord_polar("y", start = 0) + 
  theme_void() +
  theme(legend.position = "right") +
  geom_text(aes(label = paste(percentage, "%")), 
            color = "white", 
            size = 6, 
            position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Set2")

# This goes further to emphasize that the participants are sedentary

# 2. Sleep Dataset:
# a. Sleep Patterns: Analyze "TotalTimeInBed," "TotalMinutesAsleep," and "TotalMinutesNotAsleep" 
# to understand users' sleep duration, quality, and disturbances. 

sleep %>%
  select("TotalTimeInBed", "TotalMinutesAsleep", "TotalMinutesNotAsleep") %>%
  summary()

# TotalTimeInBed  TotalMinutesAsleep TotalMinutesNotAsleep
# Min.   : 61.0   Min.   : 58.0      Min.   :  0.00       
# 1st Qu.:403.8   1st Qu.:361.0      1st Qu.: 17.00       
# Median :463.0   Median :432.5      Median : 25.50       
# Mean   :458.5   Mean   :419.2      Mean   : 39.31       
# 3rd Qu.:526.0   3rd Qu.:490.0      3rd Qu.: 40.00       
# Max.   :961.0   Max.   :796.0      Max.   :371.00

# Participants spend 
# * a minimum of 61 mins in bed and 58 mins asleep
# * average of 419 mins(about 7 hrs) asleep which falls with the recomended range
# * 

View(sleep  %>%
       arrange(-TotalMinutesNotAsleep))

# Identify any correlations between sleep patterns and physical activity levels.

sleep %>%
  group_by(Id) %>%
  summarise(row_c = n())

sleep_activity <- sleep%>%
  left_join(daily_activity, join_by(Id, Date)) %>%
  subset(select = -Weekday.y) %>%
  rename(Weekday = Weekday.x)
  

View(sleep_activity)

# Relationship between sedentary minutes and sleep

ggplot(sleep_activity, aes(TotalMinutesAsleep, SedentaryMinutes, color = SleepRange)) +
  geom_point(shape = 16, ) + 
  geom_smooth() +
  theme_bw() +
  labs(title = "Sleep vs.Sedentary Minutes",
       x = "Sleep",                   
       y = "Sedentary Minutes") +                   
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c("#3772FF", "#DF2935", "#FDCA40"))

# This is a negative correlation between the minutes as sleep and  time spent on sedentary activities.
# This shows that spending time on sedentary activities does not leave time for enough sleep

# Relationship between very active minutes and sleep

ggplot(sleep_activity, aes(TotalMinutesAsleep, VeryActiveMinutes)) +
  geom_point(shape = 16, color = "#DF2935") + 
  geom_smooth() +
  theme_bw() +
  labs(title = "Sleep vs. Very Active Minutes",
       x = "Sleep",                   
       y = "Very Active Minutes") +                   
  theme(plot.title = element_text(hjust = 0.5))

# There is a strong correlation here. The More the time spent doing more physical activities, 
# the calories that were burned

ggplot(sleep_activity, aes(TotalMinutesAsleep, ModeratelyActiveMinutes)) +
  geom_point(shape = 16, color = "turquoise3") + 
  geom_smooth() +
  theme_bw() +
  labs(title = "Sleep vs. Moderately Active Minutes",
       x = "Sleep",                   
       y = "Moderately Active Minutes") +                   
  theme(plot.title = element_text(hjust = 0.5))

ggplot(sleep_activity, aes(TotalMinutesAsleep, LightlyActiveMinutes)) +
  geom_point(shape = 16, color = "turquoise3") + 
  geom_smooth() +
  theme_bw() +
  labs(title = "Sleep vs. Lightly Active Minutes",
       x = "Sleep",                   
       y = "Lightly Active Minutes") +                   
  theme(plot.title = element_text(hjust = 0.5))

# There is little or no correlation between other physical activities and minutes asleep

# b.Sleep Range: Investigate "SleepRange" to identify regular sleep times and variations in users’ sleep schedules.

sleep_activity %>%
  group_by(SleepRange) %>%
  summarize(
    row_count = n(),
    percentage = round((n()/nrow(sleep_activity)) * 100, digits = 2),
  ) %>%
  ggplot(aes(x = "", y = row_count, fill = SleepRange)) +
  geom_bar(stat = "identity", width = 1, color = "white") + 
  coord_polar("y", start = 0) + 
  theme_void() +
  theme(legend.position = "right") +
  geom_text(aes(label = paste(percentage, "%")), 
            color = "white", 
            size = 6, 
            position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c())

  
ggplot(sleep_activity, 
       aes(x = factor(Weekday, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")),
           y = TotalMinutesAsleep, fill = SleepRange)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  theme(legend.position = "right") +
  labs(title = "Total Sleep per Weekday", 
       x = "Weekday", 
       y = "Total Sleep") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c("skyblue2", "pink2", "turquoise3"))



ggplot(sleep_activity, aes(TotalMinutesAsleep, Calories)) +
  geom_point(shape = 16, color = "turquoise3") + 
  geom_smooth() +
  theme_bw() +
  labs(title = "Total sleep vs. Calories",
       x = "Total sleep",                   
       y = "Calories") +                   
  theme(plot.title = element_text(hjust = 0.5))
cor(TotalMinutesAsleep, Calories)

# There is little or no correlation beetween sleep and calories

  
attach(heartrate)
# 3. Heart Rate Dataset:
# a. Heart Rate Patterns: Explore "Value" to analyze users' heart rate data over time. 

heartrate %>%
  group_by(Date) %>%
  summarise(avg_hr = mean(Value)) %>%
  ggplot(heartrate, mapping = aes(Date, avg_hr)) +
  geom_line()

getwd()

# Identify variations in resting heart rate and potential correlations with physical activity and sleep patterns.

# 4. Weight Dataset:
# a. BMI Trends: Analyze "BMI" to track changes in users' Body Mass Index (BMI) over time. 
View(weight)
# Identify any patterns related to weight changes and user behaviors.
# b. Manual Reporting: Investigate "IsManualReport" to understand if weight data is manually reported by users. 
# Identify potential discrepancies between self-reported and automated data.

# Download file for further visualization and analysis in Tableau
write.csv(sleep_activity_data, file = './analysis/sleep_activity.csv')

sleep_activity_data <- daily_activity %>%
  full_join(sleep, join_by(Id, Date)) %>%
  subset(select = -Weekday.y) %>%
  rename(Weekday = Weekday.x)
