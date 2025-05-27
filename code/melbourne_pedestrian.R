## Packages
library(sugrrants)
library(tidyverse)
library(tsibble)

## Data
data(hourly_peds)
hourly_peds

## Plot 1 version 1: using tibble
hourly_peds |>
  filter(Date < as.Date("2016-05-01")) %>% 
  ggplot(aes(x = Time, y = Hourly_Counts, colour = Sensor_Name)) +
  geom_line() +
  facet_calendar(~ Date) + # a variable contains dates
  theme_bw() +
  theme(legend.position = "bottom")

#-----------------------------------------------

## Plot 1 version 2: tsibble
# Convert hourly_peds to a tsibble 
pedestrian_processed <- hourly_peds |> 
  as_tsibble(key = Sensor_Name, index = Date_Time) |> 
  group_by(Sensor_Name) |>
  fill_gaps(.full = TRUE) |>
  ungroup() |>
  mutate(
    Year = year(Date_Time),
    Day = wday(Date_Time, label = TRUE, week_start = 1),
    Time = hour(Date_Time),
    Date = as_date(Date_Time),
    Workday = if_else(Day %in% c("Sat", "Sun"),
      "Work day", "Non-Work day")
  )

pedestrian_processed |>
  filter(Date_Time < as.Date("2016-05-01")) |>
  ggplot(aes(x = Time, y = Hourly_Counts, colour = Sensor_Name)) +
  geom_line() +
  facet_calendar(~ Date) + # a variable contains dates
  theme_bw() +
  theme(legend.position = "bottom")

# -----------------------------------------------
## Plot 2: with tsibble

pt1 <- pedestrian_processed |>
  filter(Sensor_ID == 9, Year == 2016) |>
  mutate(Weekend = if_else(Day %in% c("Sat", "Sun"), 
                           "Weekend", "Weekday")) |>
  frame_calendar(x = Time,
                 y = Hourly_Counts, date = Date) |>
  ggplot(aes(x = .Time, y = .Hourly_Counts, 
             group = Date, colour = Weekend)) +
  geom_line() +
  theme(legend.position = "bottom")
prettify(pt1)

## Plot 2: With tibble
p2 <- hourly_peds |>
  filter(Sensor_ID == 9, Year == 2016) |>
  mutate(Weekend = if_else(Day %in% c("Saturday", "Sunday"), 
                           "Weekend", "Weekday")) |>
  frame_calendar(x = Time,
                 y = Hourly_Counts, date = Date) |>
  ggplot(aes(x = .Time, y = .Hourly_Counts, 
             group = Date, colour = Weekend)) +
  geom_line() +
  theme(legend.position = "bottom")
prettify(p2)

##-------------------------------
#https://github.com/earowang/paper-calendar-vis/blob/master/scripts/main.R
# selected sensors
sensors <- c("Melbourne Central", 
             "Flagstaff Station", 
             "Southern Cross Station")

sensor_cols <- c(
  "Melbourne Central" = "#5e3c99", 
  "Flagstaff Station" = "#1b9e77", 
  "Southern Cross Station" = "#e66101"
) 

subdat <- pedestrian_processed |>
  filter(Sensor_Name %in% sensors) 
# conventional time series plot
subdat |>
  ggplot(aes(x = Date_Time, y = Hourly_Counts, colour = Sensor_Name)) +
  geom_line(size = 0.3) +
  facet_grid(
    Sensor_Name ~ ., 
    labeller = labeller(Sensor_Name = label_wrap_gen(20))
  ) +
  scale_colour_manual(name = "Sensor", values = sensor_cols, guide = "legend") +
  scale_x_datetime(date_labels = "%d %b %Y",
                   date_minor_breaks = "1 month") +
  theme(legend.position = "bottom") +
  xlab("Date Time") +
  ylab("Hourly Counts")


# A relatively persistent pattern
# repeats from one week to another 
# at Flagstaff Station. 
## ----------------------
subdat |>
  ggplot(aes(x = Time, y = Hourly_Counts, group = Date, colour = Sensor_Name)) +
  geom_line(size = 0.3) +
  facet_grid(
    Sensor_Name ~ Day, 
    labeller = labeller(Sensor_Name = label_wrap_gen(20))
  ) +
  scale_x_continuous(breaks = seq(0, 24, by = 6)) +
  scale_colour_manual(name = "Sensor", values = sensor_cols, guide = "legend") +
  theme(legend.position = "bottom") +
  xlab("Time") +
  ylab("Hourly Counts")

# The focus is on time of day and day of 
# week across the sensors.
## ----------------------
## FS:2016
rdbu <- c("Work day" = "#d7191c", "Non-work day" = "#2c7bb6")
# calendar plot for Flagstaff station
fs <- subdat |>
  filter(Sensor_Name == "Flagstaff Station")

fs_cal <- fs |>
  frame_calendar(x = Time, 
                 y = Hourly_Counts,
                 date = Date)

p_fs <- fs_cal |>
  ggplot(aes(x = .Time,
             y = .Hourly_Counts,
             group = Date, 
             colour = Workday)) +
  geom_line() +
  scale_color_manual(values = rdbu) +
  theme(legend.position = "bottom")
prettify(p_fs)

## Sources
#https://thesis.earo.me/2-1-introduction
