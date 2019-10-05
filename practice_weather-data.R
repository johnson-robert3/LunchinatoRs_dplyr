# Practice script for Ames weather data


library(tidyverse)
library(lubridate)


# data
data_raw = read_csv("ames_weather_data.csv")


# rename variables
data_raw = data_raw %>%
   rename(date_time = valid,
          temp = tmpc,   # air temperature, degrees C
          humidity = relh,   # relative humidity, %
          wind_dir = drct,   # wind direction 
          wind_sp = sknt,   # wind speed, knots
          pressure = mslp,   # sea level pressure, millibars
          precip = p01m)   # hourly precipitation, mm


# change missing values from "M" to "NA"
data = data_raw %>%
   mutate_at(.vars = vars(temp:precip),
             .funs = ~na_if(., "M")) %>%
   # change data from character to numeric
   mutate_at(.vars = vars(temp:precip),
             .funs = ~as.numeric(.))


# make separate columns for data and month
data = data %>%
   mutate(date = date(date_time),
          month = month(date_time))


# calculate daily values of variables
data_day = data %>%
   group_by(month, date) %>%
   summarize(temp = mean(temp, na.rm=T),
             humidity = mean(humidity, na.rm=T),
             wind_dir = mean(wind_dir, na.rm=T),
             wind_sp = mean(wind_sp, na.rm=T),
             pressure = mean(pressure, na.rm=T),
             precip = sum(precip, na.rm=T)) %>%
   ungroup()


# look at only September
data_day %>%
   filter(month==9)


# calculate monthly values
data_month = data %>%
   group_by(month) %>%
   summarize_at(.vars = vars(temp, precip),
                .funs = list(~mean(., na.rm=T), ~sd(., na.rm=T), ~min(., na.rm=T), ~max(., na.rm=T))) %>%
   ungroup()


# which day had the most precipitation each month?
data_day %>%
   group_by(month) %>%
   slice(which.max(precip))

   # OR
   data_day %>%
      group_by(month) %>%
      filter(precip == max(precip))


# what was the coldest day each month?
data_day %>%
   group_by(month) %>%
   slice(which.min(temp))

   # OR
   data_day %>%
      group_by(month) %>%
      filter(temp == min(temp))


# which was the wettest month?
data %>%
   group_by(month) %>%
   summarize_at(vars(precip),
                .fun = ~sum(., na.rm=T)) %>%
   arrange(precip)



