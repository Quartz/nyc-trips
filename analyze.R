library(dplyr)
library(lubridate)
library(readr)
library(ggplot2)
library(tidyr)
library(zoo)

gett <- read_csv("Gett_Trips_NYC_2016.csv") %>%
  mutate(
    week_start = mdy(`Pickup Start Date`),
    month = floor_date(week_start, "month")
  )

gett_weekly <- gett %>%
  group_by(
    week_start
  ) %>%
  summarise(
    gett = sum(`Total Dispatched Trips`)
  )
  
gett_monthly <- gett %>%
  group_by(
    month
  ) %>%
  summarise(
    gett = sum(`Total Dispatched Trips`)
  )

lyft <- read_csv("Lyft_Trips_NYC_2016.csv") %>%
  mutate(
    week_start = mdy(`Pickup Start Date`),
    month = floor_date(week_start, "month")
  )

lyft_weekly <- lyft %>%
  group_by(
    week_start
  ) %>%
  summarise(
    lyft = sum(`Total Dispatched Trips`)
  )
   
lyft_monthly <- lyft %>%
  group_by(
    month
  ) %>%
  summarise(
    lyft = sum(`Total Dispatched Trips`)
  )

uber <- read_csv("Uber_Trips_NYC_2016.csv") %>%
  mutate(
    week_start = mdy(`Pickup Start Date`),
    month = floor_date(week_start, "month")
  )

uber_weekly <- uber %>%
  group_by(
    week_start
  ) %>%
  summarise(
    uber = sum(`Total Dispatched Trips`)
  )

uber_monthly <- uber %>%
  group_by(
    month
  ) %>%
  summarise(
    uber = sum(`Total Dispatched Trips`)
  )

yellow_monthly <- read_csv("data_reports_monthly_indicators_yellow (3).csv") %>%
  mutate(
    month = ymd(paste0(`Month/Year`, "-01")),
    yellow =`Trips Per Day` * days_in_month(month)
  ) %>%
  select(
    month,
    yellow
  )

merged_weekly <- uber_weekly %>%
  full_join(lyft_weekly, by = c("week_start" = "week_start")) %>%
  full_join(gett_weekly, by = c("week_start" = "week_start")) %>%
  arrange(week_start)

merged_monthly <- yellow_monthly %>%
  full_join(uber_monthly, by = c("month" = "month")) %>%
  full_join(lyft_monthly, by = c("month" = "month")) %>%
  full_join(gett_monthly, by = c("month" = "month")) %>%
  arrange(month)

merged_weekly %>%
  filter(week_start >= ymd("2015-01-01")) %>%
  mutate(
    uber = uber / 1000000,
    lyft = lyft / 1000000,
    gett = gett / 1000000
  ) %>%
  write_csv('merged_weekly.csv', na = "null")

merged_monthly %>%
  filter(month >= ymd("2013-01-01")) %>%
  mutate(
    yellow = yellow / 1000000,
    uber = uber / 1000000,
    lyft = lyft / 1000000,
    gett = gett / 1000000
  ) %>%
  write_csv('merged_monthly.csv', na = "null")

merged_weekly_flat <- merged_weekly %>%
  gather(provider, total_trips, uber:gett)

merged_monthly_flat <- merged_monthly %>%
  gather(provider, total_trips, yellow:gett)

ggplot(merged_weekly_flat) +
  geom_line(aes(x = week_start, y = total_trips, color = provider)) +
  scale_y_continuous(labels = scales::comma)

ggplot(merged_monthly_flat) +
  geom_line(aes(x = month, y = total_trips, color = provider)) +
  scale_y_continuous(labels = scales::comma)
  