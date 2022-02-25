#load the csv file.
incarceration_trends <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv",stringsAsFactors = FALSE)

#load the package.
library(ggplot2)
library(dplyr)
library(tidyverse)
library(sf)
#install.packages("mapview")
library(mapview)
library("maps")


#the ratio between black and white in jail in 2018.
ratio_black_white <- incarceration_trends %>%
  filter(year == "2018", state == "WA") %>%
  replace_na(list(
    black_jail_pop = 0, black_prison_pop = 0, white_jail_pop = 0,
    white_prison_pop = 0, black_pop_15to64 = 0, white_pop_15to64 = 0
  )) %>%
  mutate(
    total_black_jail = black_jail_pop + black_prison_pop,
    total_white_jail = white_jail_pop + white_prison_pop,
    total_balck = black_pop_15to64,
    total_white = white_pop_15to64
  ) %>%
  group_by(county_name) %>%
  summarise(across(total_black_jail:total_white, sum)) %>%
  mutate(
    black_ratio = total_black_jail / total_balck,
    white_ratio = total_white_jail/  total_white
  ) %>%
  mutate(ratio = black_ratio / white_ratio)
#create the chart
ratio_black_white_new <- map_data("county") %>% mutate(polyname = paste(region, subregion, sep = ","))
ratio_black_white_new_join <- left_join(ratio_black_white_new, county.fips, by = "polyname") %>% filter(region == "washington")
# View(WA_joined_shape)
ratio_black_white_new_join$fips[ratio_black_white_new_join$subregion == "pierce"] <- "53053"
ratio_black_white_new_join$fips[ratio_black_white_new_join$subregion == "san juan"] <- "53055"
# View(WA_joined_shape)

# create the map
ratio_black_white$county_name <- ratio_black_white$county_name %>%
  tolower() %>%
  str_remove(" county")
ratio_black_white <- ratio_black_white %>%
  mutate(county_name = paste("washington", county_name, sep = ","))
# View(black_white_rate_county_df)
Washington_map <- left_join(ratio_black_white_new_join, ratio_black_white, by = c("polyname" = "county_name"))
# View(map_with_data)
US_map <- ggplot(Washington_map) +
  geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = ratio)) +
  scale_fill_distiller(palette = "RdYIBu", direction = 1) +
  coord_quickmap() +
  theme_void() +
  labs(
    title = "Geographical distribution of the proportion of black incarceration to white incarceration",
    fill = "ratio"
  )
US_map








