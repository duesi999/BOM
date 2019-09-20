#Challenge: Putting it all together

# Getting the data
# 
# Create a new RStudio project for this analysis
# Download two data files from the Bureau of Meterology, one containing meterological information, and one containing metadata about weather stations
# Take some time to explore the data files and understand what they contain
# Write a script that answers the following questions:
#   

library(tidyverse)

#import data
data <- read_csv("data/BOM_data.csv")
stations <- read_csv("data/BOM_stations.csv")

#Question 1
#For each station, how many days have a minimum temperature, a maximum temperature and a rainfall measurement recorded?

data %>%
  separate(Temp_min_max, into = c("Temp_min", "Temp_max"), sep = "/") %>%
  mutate(
    Temp_min = as.numeric(Temp_min),
    Temp_max = as.numeric(Temp_max),
    Rainfall = as.numeric(Rainfall),
    Solar_exposure = as.numeric(Solar_exposure)
  ) %>%
  filter(
    !is.na(Temp_min),
    !is.na(Temp_max),
    !is.na(Rainfall)
  ) %>%
  group_by(Station_number) %>%
  summarise(count = n())

#Question 2
#Which month saw the lowest average daily temperature difference?
data %>%
  separate(Temp_min_max, into = c("Temp_min", "Temp_max"), sep = "/") %>%
  mutate(
    Temp_min = as.numeric(Temp_min),
    Temp_max = as.numeric(Temp_max),
    Temp_diff = (Temp_max - Temp_min)
  ) %>%
  drop_na(Temp_diff) %>%
  group_by(Year, Month) %>%
  summarise(Temp_diff_avg = mean(Temp_diff)) %>%
#  arrange(Temp_diff_avg, Year, Month) %>%
  ungroup() %>%
  filter(Temp_diff_avg == min(Temp_diff_avg))

#Question 3
#Which state saw the lowest average daily temperature difference?
stations_1 <- stations %>% 
  gather(Station_number, values, -info) %>%
  filter(info == "state") %>%
  spread(info, values) %>%
  mutate(Station_number = as.numeric(Station_number))

data %>%
  left_join(stations_1, by = "Station_number") %>%
  separate(Temp_min_max, into = c("Temp_min", "Temp_max"), sep = "/") %>%
  mutate(
    Temp_min = as.numeric(Temp_min),
    Temp_max = as.numeric(Temp_max),
    Temp_diff = (Temp_max - Temp_min)
  ) %>%
  drop_na(Temp_diff) %>%
  group_by(state) %>%
  summarise(Temp_diff_avg = mean(Temp_diff)) %>%
#  arrange(Temp_diff_avg) %>%
  filter(Temp_diff_avg == min(Temp_diff_avg))


# Question 4
# Does the westmost (lowest longitude) or eastmost (highest longitude) weather station in our dataset have a higher average solar exposure?
stations_2 <- stations %>%
  gather(Station_number, values, -info) %>%
  filter(info == "lon" | info == "lat") %>%
  spread(info, values) %>%
  mutate(Station_number = as.numeric(Station_number))

data %>%
  left_join(stations_2, by = "Station_number") %>%
  filter(lon == min(lon) | lon == max(lon)) %>%
  mutate(Solar_exposure = as.numeric(Solar_exposure)) %>%
  drop_na(Solar_exposure) %>%
  group_by(Station_number, lon) %>%
  summarise(Solar_exposure_avg = mean(Solar_exposure))

