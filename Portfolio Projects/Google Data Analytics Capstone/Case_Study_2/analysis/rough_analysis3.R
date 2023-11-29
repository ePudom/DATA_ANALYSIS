# install.packages("tidyverse")
# install.packages("skimr")
# install.packages("ggpubr")
# install.packages("here")
# install.packages("janitor")
# install.packages("lubridate")
# install.packages("ggrepel")
# library(ggpubr)
# library(tidyverse)
# library(here)
# library(skimr)
# library(janitor)
# library(lubridate)
# library(ggrepel)
# rm(list = ls())


# PHASE 3: PROCESS

#Read the data files
daily_activity <- read.csv("./datasets/main_data/daily_activity.csv")
daily_sleep <- read.csv("./datasets/main_data/sleep_day.csv")
hourly_intensities <- read.csv("./datasets/main_data/hourly_intensities.csv")
hourly_calories <- read.csv("./datasets/main_data/hourly_calories.csv")
hourly_steps <- read.csv("./datasets/main_data/hourly_steps.csv")


# Number of participants in each data file
n_distinct(daily_activity$Id)
n_distinct(daily_sleep$Id)
n_distinct(hourly_intensities$Id)
n_distinct(hourly_calories$Id)
n_distinct(hourly_steps$Id)

# Number of records in each data file
skim_without_charts(daily_activity)
skim_without_charts(daily_sleep)
skim_without_charts(hourly_calories)
skim_without_charts(hourly_intensities)
skim_without_charts(hourly_steps)


# Check for NAs
anyNA(daily_activity)
anyNA(daily_sleep)
anyNA(hourly_calories)
anyNA(hourly_intensities)
anyNA(hourly_steps)

# Check for duplicates
sum(duplicated(daily_activity))
sum(duplicated(daily_sleep))
sum(duplicated(hourly_calories))
sum(duplicated(hourly_intensities))
sum(duplicated(hourly_steps))

# Remove duplicates, NAs and make column names more readable and easy to work with
daily_activity <- daily_activity %>%
  unique() %>%
  drop_na() %>%
  clean_names()

daily_sleep <- daily_sleep %>%
  unique() %>%
  drop_na() %>%
  clean_names()

hourly_calories <- hourly_calories %>%
  unique() %>%
  drop_na() %>%
  clean_names()

hourly_intensities <- hourly_intensities %>%
  unique() %>%
  drop_na() %>%
  clean_names()

hourly_steps <- hourly_steps %>%
  unique() %>%
  drop_na() %>%
  clean_names()


colnames(daily_activity)
colnames(daily_sleep)
colnames(hourly_calories)
colnames(hourly_intensities)
colnames(hourly_steps)
# > colnames(daily_sleep)
# [1] "id"                   "sleep_day"            "total_sleep_records"  "total_minutes_asleep" "total_time_in_bed"   
# > colnames(hourly_calories)
# [1] "id"            "activity_hour" "calories"     
# > colnames(hourly_intensities)
# [1] "id"                "activity_hour"     "total_intensity"   "average_intensity"
# > colnames(hourly_steps)
# [1] "id"            "activity_hour" "step_total"   


# Formating columns 
# 1. Id - from numeric to factor
daily_activity$id <- as.factor(daily_activity$id)
daily_sleep$id <- as.factor(daily_sleep$id)
hourly_calories$id <- as.factor(hourly_calories$id)
hourly_intensities$id <- as.factor(hourly_intensities$id)
hourly_steps$id <- as.factor(hourly_steps$id)

# 2. Dates - 
# ## i. Rename columns 
# ## ii. Change data type from character to date/date-time format(YYYY-MM-DD HH:MM:SS)
# ## iii. Split date-time columns to date and time
daily_activity <- daily_activity %>%
  rename(date = activity_date) %>%
  mutate(date = as.Date(date, format = "%m/%d/%Y"))

daily_sleep <- daily_sleep %>%
  rename(date = sleep_day) %>%
  mutate(date = as.Date(date, format = "%m/%d/%Y"))

hourly_calories <- hourly_calories %>%
  mutate(activity_hour = as.POSIXct(activity_hour, format = "%m/%d/%Y %I:%M:%S %p"),
         date = as.Date(activity_hour),
         time = format(activity_hour, format = "%H:%M:%S")) %>%
  subset(select = -activity_hour)

hourly_intensities <- hourly_intensities %>%
  mutate(activity_hour = as.POSIXct(activity_hour, format = "%m/%d/%Y %I:%M:%S %p"),
         date = as.Date(activity_hour),
         time = format(activity_hour, format = "%H:%M:%S")) %>%
  subset(select = -activity_hour)

hourly_steps <- hourly_steps %>%
  mutate(activity_hour = as.POSIXct(activity_hour, format = "%m/%d/%Y %I:%M:%S %p"),
         date = as.Date(activity_hour),
         time = format(activity_hour, format = "%H:%M:%S")) %>%
  subset(select = -activity_hour)

# 3. Merging 'daily_activity' and 'daily_sleep' and add column for day of week for further analysis
daily_activity_sleep <- merge(daily_activity, daily_sleep, by = c("id", "date")) %>%
  mutate(day_of_week = format(date, "%A"),
         ) %>%
  subset(select = -c(tracker_distance, logged_activities_distance))

glimpse(daily_activity_sleep)

# PHASE 3: ANALYZE

# Calculate the average step, calories and sleep for each user
# The activity level of users based on the number of steps
daily_user_averages <- daily_activity_sleep %>%
  group_by(id) %>%
  summarise(avg_steps = mean(total_steps),
            avg_calories = mean(calories),
            avg_sleep = mean(total_minutes_asleep))


# Based on the daily average steps for each user we go ahead add a column to clasify the users based 
# on the guideline provided by https://10000steps.org.au/articles/healthy-lifestyles/counting-steps/

# Sedentary is less than 5,000 steps per day 
# Low active is 5,000 to 7,499 steps per day
# Somewhat active is 7,500 to 9,999 steps per day
# Active is more than 10,000 steps per day

daily_user_averages<- daily_user_averages %>%
  mutate(step_activity_level = case_when(
    avg_steps < 5000 ~ 'Sedentary',
    avg_steps >= 5000 & avg_steps < 7500 ~ 'Lightly Active',
    avg_steps >= 7500 & avg_steps < 10000 ~ 'Moderately Active',
    avg_steps >= 10000 ~ 'Very Active'),
    sleep_category = case_when(
      avg_sleep < (7 * 60) ~ "Under Sleep",
      avg_sleep >= (7 * 60) & 
        avg_sleep <= (8 * 60) ~ "Good Sleep",
      avg_sleep > (8 * 60) ~ "Over Sleep",
      TRUE ~ "No Sleep")
  ) 
  
daily_user_averages

step_activity_group <- daily_user_averages %>%
  group_by(step_activity_level) %>%
  summarise(total_per_level = n()) %>%
  mutate(sum_total = sum(total_per_level)) %>%
  mutate(percentage_per_level = round((total_per_level/sum_total)*100, digits = 2))

step_activity_group$step_activity_level = factor(
  step_activity_group$step_activity_level,
  levels = c("Very Active", "Moderately Active", "Lightly Active", "Sedentary"))

step_activity_group

ggplot(step_activity_group, aes(x = "", y = percentage_per_level, fill = step_activity_level)) +
  geom_bar(stat = "identity", width = 1, color = "white") + 
  coord_polar("y", start = 0) + 
  theme_void() +
  theme(legend.position = "right") +
  geom_text(aes(label = paste(percentage_per_level, "%")), 
            color = "white", 
            size = 4, 
            position = position_stack(vjust = 0.5)) +
  labs(fill='Activitylevel', title = "Distribution of Users per Activity Level") +
  scale_fill_manual(values = c("#2EBAE1", "#FE9077", "#C0C65D", "#9A275A"))
  
# Users are mostly lightly active with an almost equal distribution across the activity level based on the no of steps

# Analyze steps, calaories and sleep based on day
daily_averages <- daily_activity_sleep %>%
  group_by(date) %>%
  summarise(avg_steps = mean(total_steps),
            avg_calories = mean(calories),
            avg_sleep = mean(total_minutes_asleep),
            avg_light_active_min = mean(lightly_active_minutes),
            avg_mod_active_min = mean(fairly_active_minutes),
            avg_very_active_min = mean(very_active_minutes),
            avg_sedentary_min = mean(sedentary_minutes))


# Analyze steps, calaories and sleep based on week day
weekly_averages <- daily_activity_sleep %>%
  group_by(day_of_week) %>%
  summarise(avg_steps = mean(total_steps),
            avg_calories = mean(calories),
            avg_sleep = mean(total_minutes_asleep),
            avg_sed = mean(sedentary_minutes),
            avg_vam = mean(very_active_minutes),
            avg_lam = mean(lightly_active_minutes),
            avg_fam = mean(fairly_active_minutes)) %>%
  mutate(day_of_week = 
           factor(day_of_week,
                  levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")))

options(repr.plot.width = 10, repr.plot.height = 8)

ggarrange(
  ggplot(daily_averages, aes(x = date, y = avg_steps)) +
    geom_bar(stat = "identity", fill = "#2EBAE1") +
    geom_hline(yintercept = 7500) +
    labs(title = "Steps per Day",
         x = "Steps",
         y = "Weekday") +
    theme_minimal() + 
    theme(axis.text.x = element_text(angle = 45), plot.title = element_text(hjust = 0.4)),
  ggplot(daily_averages, aes(x = date, y = avg_sleep)) +
    geom_bar(stat = "identity", fill = "#FE9077") +
    geom_hline(yintercept = 480) +
    labs(title = "Minutes Asleep per Day",
         x = "Minutes Asleep",
         y = "Day") +
    theme_minimal() + 
    theme(axis.text.x = element_text(angle = 45), plot.title = element_text(hjust = 0.4)),
  ggplot(weekly_averages, aes(x = day_of_week, y = avg_steps)) +
    geom_bar(stat = "identity", fill = "#2EBAE1") +
    geom_hline(yintercept = 7500) +
    labs(title = "Steps per Weekday",
         x = "Steps",
         y = "Weekday") +
    theme_minimal() + 
    theme(axis.text.x = element_text(angle = 45), plot.title = element_text(hjust = 0.4)),
  ggplot(weekly_averages, aes(x = day_of_week, y = avg_sleep)) +
    geom_bar(stat = "identity", fill = "#FE9077") +
    geom_hline(yintercept = 480) +
    labs(title = "Minutes Asleep per Weekday",
         x = "Minutes Asleep",
         y = "Weekday") +
    theme_minimal() + 
    theme(axis.text.x = element_text(angle = 45), plot.title = element_text(hjust = 0.4))
)

# Users tend to:
# i. For the most part, complete the recommended no of steps per day 
# ii. take more steps on Saturday 
# iii. surpass the average daily steps per day i.e 7500 every day except Sundays
# iv. Sleep more on Sundays
# v. However don't take the recommended no of hours asleep


ggplot(daily_averages, aes(x = date)) +
  # geom_bar(aes(y = avg_steps), stat = "identity", fill = "#2EBAE1") +
  geom_line(aes(y = avg_steps), size = 1, color = "#2EBAE1") +
  geom_line(aes(y = avg_calories/coeff), size = 1, color = "#9A275A") +
  scale_y_continuous(
    name = "Average Steps", 
    sec.axis = sec_axis(~.*coeff, name = "Average Calorie Expenditure")
  ) +
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 90),
    plot.title = element_text(hjust = 0.4),
    axis.title = ,
  ) + 
  ggtitle("Average Steps vs Average Calorie Expenditure per day")

ggplot(daily_averages, aes(x = avg_steps, y = avg_calories)) +
  geom_point(color = "#FE9077") +
  geom_smooth() +
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 90),
    plot.title = element_text(hjust = 0.4),
    axis.title = ,
  ) + 
  ggtitle("Average Steps vs Average Calorie Expenditure")

cor(avg_steps, avg_calories) # 0.8195922
# The daily steps and calories have roughly the same pattern and this shows a positive relationship


ggplot(daily_averages, aes(x = avg_sleep, y = avg_calories)) +
  geom_point(color = "#9A275A") +
  geom_smooth() +
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 90),
    plot.title = element_text(hjust = 0.4),
    axis.title = ,
  ) + 
  ggtitle("Daily Minutes Asleep vs Daily Calorie Expenditure")

cor(avg_sleep, avg_calories) # -0.2936243

# There is little or no relationship between the daily minutes asleep and calorie expenditure

ggplot(daily_averages, aes(x = avg_sleep, y = avg_steps)) +
  geom_point(color = "#2EBAE1") +
  geom_smooth() +
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 90),
    plot.title = element_text(hjust = 0.4),
    axis.title = ,
  ) + 
  ggtitle("Daily Minutes Asleep vs Daily Steps")

cor(avg_sleep, avg_steps) # -0.4234998

# Though a negative relationship, there is little or no relationship between 
# the daily minutes asleep and daily steps

hourly_intensities %>%
  head( 100) %>% 
    arrange(id, date, time)

hourly_intensities%>%
  group_by(id) %>%
   count()

hourly_intensities %>%
  group_by(time) %>%
  summarise(avg_intensity = mean(total_intensity)) %>%
  ggplot(aes(x = time, y = avg_intensity, fill = avg_intensity)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Hourly Intensity",
       x = "Hour",
       y = "Total Intensity") +
  scale_fill_gradient(low = "#F3E4EB", high = "#9A275A") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90), plot.title = element_text(hjust = 0.4))

hourly_steps %>%
  group_by(time) %>%
  summarise(avg_steps = mean(step_total)) %>%
  ggplot(aes(x = time, y = avg_steps, fill = avg_steps)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Hourly Steps",
       x = "Hour",
       y = "Total Steps") +
  scale_fill_gradient(low = "#FFF2EE", high = "#FE9077") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90), plot.title = element_text(hjust = 0.4))

hourly_calories %>%
  group_by(time) %>%
  summarise(avg_calories = mean(calories)) %>%
  ggplot(aes(x = time, y = avg_calories, fill = avg_calories)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Hourly Calories",
       x = "Hour",
       y = "Calorie Expenditure") +
  scale_fill_gradient(low = "#CBEEF8", high = "#2EBAE1") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90), plot.title = element_text(hjust = 0.4))

# Peak Hours for intensities, calorie expenditure and steps falls between 5pm and 7pm 

daily_sleep_averages <- daily_activity_sleep %>%
  select("id", "date", "total_minutes_asleep", "total_time_in_bed", "total_steps", "calories") %>%
  mutate(total_minutes_not_asleep = total_time_in_bed - total_minutes_asleep) %>%
  group_by(id) %>%
  summarise(avg_minutes_asleep = mean(total_minutes_asleep),
            avg_time_in_bed = mean(total_time_in_bed),
            avg_minutes_not_asleep = mean(total_minutes_not_asleep),
            avg_steps = mean(total_steps),
            avg_calories = mean(calories))


# 7. Classify based on the sleep length
sleep_group <- daily_user_averages %>%
  group_by(sleep_category) %>%
  summarise(total_per_group = n()) %>%
  mutate(sum_total = sum(total_per_group)) %>%
  mutate(percentage_per_group = round((total_per_group/sum_total) * 100, digits = 2))

ggplot(sleep_group, aes(x = "", y = percentage_per_group, fill = sleep_category)) +
  geom_bar(stat = "identity", width = 1, color = "white") + 
  coord_polar("y", start = 0) + 
  theme_void() +
  theme(legend.position = "right", plot.title = element_text(hjust = 0.7)) +
  geom_text(aes(label = paste(percentage_per_group, "%")), 
            color = "white", 
            size = 4, 
            position = position_stack(vjust = 0.5)) +
  labs(fill='Sleep Group', title = "Distribution of Users based on Sleep Length") +
  scale_fill_manual(values = c("#FE9077", "#C0C65D", "#9A275A"))

# 54.17% fall in the 'under sleep' category.
# 37.5% fall in the 'good sleep' category.
# 8.33% fall in the 'over sleep' category.














  

  
  



