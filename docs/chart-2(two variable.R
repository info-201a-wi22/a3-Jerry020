#load the csv file.
incarceration_trends <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv",stringsAsFactors = FALSE)

#load the package.
library(ggplot2)
library(dplyr)
library(tidyverse)

#calculate the top 5 highest rate of white incarceration in 2018.
top5_white_highest_rate <- incarceration_trends %>%
  filter(year == 2018) %>%
  replace_na(list(total_pop_15to64 = 0,
                  white_pop_15to64 = 0,
                  total_jail_pop = 0,
                  white_jail_pop = 0
                  )) %>%
  group_by(state)%>%
  summarise(across(c(total_pop_15to64,white_pop_15to64,total_jail_pop,white_jail_pop),sum)) %>%
  mutate(
    total = total_jail_pop / total_pop_15to64 * 100,
    white = white_jail_pop / white_pop_15to64 * 100
  )%>%
  slice_max(order_by =white , n = 5)%>%
  pivot_longer(c(white,total), names_to = "race", values_to = "rate")

#create the plot
top5_white_highest_rate_plot <- ggplot(
  data = top5_white_highest_rate,
  mapping = aes(x = state, y = rate, group = race, fill = race)
) +
  geom_col(position = "dodge")+
  labs(x="state name", y="percentage(%)",title = "Five states with the highest white people in jail rates")
  

 