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

q1_answ <- data %>%
  #separate Temp_min_max into two and convert temps and rainfall to numeric
  separate(Temp_min_max, into = c("Temp_min", "Temp_max"), sep = "/") %>%
  mutate(
    Temp_min = as.numeric(Temp_min),
    Temp_max = as.numeric(Temp_max),
    Rainfall = as.numeric(Rainfall),
    Solar_exposure = as.numeric(Solar_exposure)
  ) %>%
  #remove NAs
  filter(
    !is.na(Temp_min),
    !is.na(Temp_max),
    !is.na(Rainfall)
  ) %>%
  group_by(Station_number) %>%
  summarise(count = n())

#answer to Q1
q1_answ

#Question 2
#Which month saw the lowest average daily temperature difference?
q2_answ <- data %>%
  #separate Temp_min_max into two and convert temps to numeric; calculate temperature difference
  separate(Temp_min_max, into = c("Temp_min", "Temp_max"), sep = "/") %>%
  mutate(
    Temp_min = as.numeric(Temp_min),
    Temp_max = as.numeric(Temp_max),
    Temp_diff = (Temp_max - Temp_min)
  ) %>%
  #remove NAs from newly created variable
  drop_na(Temp_diff) %>%
  #group by month and get average temp fifference
  group_by(Month) %>%
  summarise(Temp_diff_avg = mean(Temp_diff)) %>%
  #get lowest temperatire difference accross months
  filter(Temp_diff_avg == min(Temp_diff_avg))

#answer q2; month 6 (June) has the lowest average temperature difference
q2_answ

#Question 3
#Which state saw the lowest average daily temperature difference?

#prepare station data; data is in the wrong format so has to be restructured; 
#first gather and put station numbers and other info into column; 
#then filter for rows where info is state (this is what we are interested in), 
#get rid of info column, rename and convert station to numeric

stations_1 <- stations %>% 
  #gather(Station_number, values, -info) %>%
  pivot_longer(cols = c(2:21), names_to = "Station_name", values_to = "values")
  filter(info == "state") %>%
  select(-info)%>%
  rename(state = values) %>%
  mutate(Station_number = as.numeric(Station_number))

q3_answ <- data %>%
  #join datatsets
  left_join(stations_1, by = "Station_number") %>%
  #same as above, separate temp column and convert to numeric
  separate(Temp_min_max, into = c("Temp_min", "Temp_max"), sep = "/") %>%
  mutate(
    Temp_min = as.numeric(Temp_min),
    Temp_max = as.numeric(Temp_max),
    Temp_diff = (Temp_max - Temp_min)
  ) %>%
  #remove NAs
  drop_na(Temp_diff) %>%
  #group by state and calculate mean temp diff
  group_by(state) %>%
  summarise(Temp_diff_avg = mean(Temp_diff)) %>%
  #get minimum temp diff
  filter(Temp_diff_avg == min(Temp_diff_avg))

#answer q3: QLD has the lowest temperature difference
q3_answ

# Question 4
# Does the westmost (lowest longitude) or eastmost (highest longitude) weather station in our dataset have a higher average solar exposure?

#prepare and restructure station data; gather and extract longitude and latitude columns; spread out lon and lat
#convert station number to numeric

stations_2 <- stations %>%
  #gather(Station_number, values, -info) %>%
  pivot_longer(-info, names_to = "Station_number", values_to = "values") %>%
  filter(info == "lon" | info == "lat") %>%
  #spread(info, values) %>%
  pivot_wider(id_cols = Station_number, names_from = info, values_from = values) %>%
  mutate(Station_number = as.numeric(Station_number))

q4_answ <- data %>%
  #join datasets
  left_join(stations_2, by = "Station_number") %>%
  #filter for minimum and maximun longitude
  filter(lon == min(lon) | lon == max(lon)) %>%
  #convert solar exposure to numeric, remove NAs 
  mutate(Solar_exposure = as.numeric(Solar_exposure),
  lon = as.numeric(lon)) %>%
  drop_na(Solar_exposure) %>%
  #group by station number and calculate mean solar exposure
  group_by(Station_number, lon) %>%
  summarise(Solar_exposure_avg = mean(Solar_exposure)) %>%
  ungroup() %>%
  #create new variable to with station location info based on longitude
  mutate("Station_location" = case_when(lon == min(lon) ~ "westmost", lon == max(lon) ~ "eastmost"))

#Answer q4:
q4_answ


