#load the csv file.
incarceration_trends <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv",stringsAsFactors = FALSE)

#load the package.
library(dplyr)
library(tidyverse)

#What the highest total populations in jail？
highest_total_populations_jails<- incarceration_trends%>%
  filter(total_jail_pop==max(total_jail_pop,na.rm=TRUE))%>%
  pull(total_jail_pop)

#What is the total populations of black race in jail?
highest_total_populations_black<- incarceration_trends%>%
  filter(black_jail_pop==max(black_jail_pop,na.rm=TRUE))%>%
  pull(black_jail_pop)

#What is the total populations of white race in jail?
highest_total_populations_white<- incarceration_trends%>%
  filter(white_jail_pop==max(white_jail_pop,na.rm=TRUE))%>%
  pull(white_jail_pop)

#What is the total populations of latinx race in jail?
highest_total_populations_latinx<- incarceration_trends%>%
  filter(latinx_jail_pop==max(latinx_jail_pop,na.rm=TRUE))%>%
  pull(latinx_jail_pop)

#What is the total populations of native race in jail?
highest_total_populations_native<- incarceration_trends%>%
  filter(native_jail_pop==max(native_jail_pop,na.rm=TRUE))%>%
  pull(native_jail_pop)




