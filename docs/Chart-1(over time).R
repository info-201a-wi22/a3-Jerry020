#load the csv file.
incarceration_trends <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv",stringsAsFactors = FALSE)

#load the package.
library(ggplot2)
library(dplyr)
library(tidyverse)

#calculate the population in jails each race each year.
incarceration_trends_rename <- incarceration_trends%>%
  rename(black=black_jail_pop,white=white_jail_pop,latinx=latinx_jail_pop,aapi=aapi_jail_pop,native=native_jail_pop,other=other_race_jail_pop)
each_race_pop_each_year <- incarceration_trends_rename%>%
  replace_na(list(
    black = 0, 
    white = 0, 
    latinx = 0, 
    aapi = 0, 
    native = 0,
    other = 0
  ))%>%
  group_by(year)%>%
  summarise(across(aapi:other,na.rm = TRUE,sum))%>%
  pivot_longer(!year, names_to = "race", values_to = "population")

#create plot
each_race_pop_each_year_plot <-ggplot(data = each_race_pop_each_year)+
  geom_line(mapping = aes(x=year,y=population,group=race,color=race)) +
  geom_point(mapping = aes(x=year,y=population,group=race,color=race)) +
  labs(title = "Peoples of different races in jail over time")

  
