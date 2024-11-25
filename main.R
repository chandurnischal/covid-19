library(dplyr)
library(ggplot2)

# f2c93f70 -> Maryland

data<- read.csv("data\\raw.csv", header=TRUE)
data$date<- as.Date(data$date)

total <- data %>%
  group_by(administrative_area_level_2, latitude, longitude) %>%
  summarise(total_confirmed = sum(confirmed, na.rm = TRUE), .groups = "drop")