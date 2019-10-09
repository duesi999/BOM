library(tidyverse)


#import data
data <- read_csv("data/BOM_data.csv")
stations <- read_csv("data/BOM_stations.csv")

data_1 <- data %>%
  separate(Temp_min_max, into = c("Temp_min", "Temp_max"), sep = "/") %>%
  mutate(
    Temp_min = as.numeric(Temp_min),
    Temp_max = as.numeric(Temp_max),
    Rainfall = as.numeric(Rainfall),
    Solar_exposure = as.numeric(Solar_exposure)
  )

#turn station data into long dataset
stations_long <- stations %>%
  #use the new pivot_longer function instead of gather
  pivot_longer(-info, names_to = "Station_number", values_to = "values") %>%
  #use pivot wider instead of spread
  pivot_wider(id_cols = Station_number, names_from = info, values_from = values) %>%
  mutate(Station_number = as.numeric(Station_number))

#merge datafiles
data_all <- data_1 %>%
  left_join(stations_long, by = "Station_number")

# Question 1
# 
# For the Perth station (ID 9225), produce three scatter plots showing the relationship between 
# the maximum temperature and each other measurement recorded (minimum temperature, rainfall and solar exposure).

#filter to get only Perth station
data_perth <- data_all %>%
    filter(Station_number == 9225)

fig1 <- ggplot(data_perth, aes(x = Temp_max, y = Temp_min)) +
  geom_point(alpha = 0.5) +
  ggtitle("max and minimum temperature")
fig1

fig2 <- ggplot(data_perth, aes(x = Temp_max, y = Rainfall)) +
  geom_point(alpha = 0.5) +
  ggtitle("max temp and rainfall")
fig2

fig3 <- ggplot(data_perth, aes(x = Temp_max, y = Solar_exposure)) +
  geom_point(alpha = 0.5) +
  ggtitle("maximum temp and solar exposure")
fig3

# Question 2
# 
# Display these four measurements for the Perth station in a single scatter plot by using additional aesthetic mappings.
# You may need to try a few different data/aesthetic mappings to find one you like.

fig4 <- ggplot(data_perth, aes(x = Temp_max, y = Temp_min, size = Rainfall, colour = Solar_exposure)) +
  geom_point(alpha = 0.3, shape = 1) +
  ggtitle("max and min temp, rainfall and solar exposure")
fig4

# Question 3
# 
# Take the four plots you have produced in Q1 and Q2 and save them as a multi-panel figure.
library(cowplot)

fig5 <- plot_grid(fig1, fig2, fig3, fig4)
fig5
ggsave("results/max_vs_min_temp_rainfall_solar-exposure.pdf", plot = fig5, width = 12, height = 12)


# Question 4
# 
# Using the entire BOM dataset, calculate the average monthly rainfall for each station. Produce a lineplot to visualise this data and the state each station is in.

data_q4 <- data_all %>%
  group_by(state, Station_number, Month) %>%
  drop_na() %>%
  summarise(rain_avg = mean(Rainfall)) %>%
  unite("Station", Station_number, state)

#plot with facets
ggplot(data_q4, aes(x= factor(Month), y = rain_avg, group = 1)) +
  geom_line() +
  xlab("Month") +
  ylab("Average Rainfall") +
  ggtitle("Average rainfall for each station") +
  facet_wrap(~Station, ncol = 4)

