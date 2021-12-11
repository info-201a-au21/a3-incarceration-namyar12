#load packages

library(dplyr)
library(ggplot2)
library(tidyr)
library(mapproj)

#load data

incarceration_data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

#Variable 1 - rate of male juvenile incarcerations
#Total number of incarcerations

total_incarcerations <- incarceration_data %>%
  summarise(all_incarcerations = sum(total_pop)) %>%
  pull(all_incarcerations)

#Total number of jail population

total_jail_population <- incarceration_data %>%
  filter(!is.na(total_jail_pop)) %>%
  summarise(jail_total = sum(total_jail_pop)) %>%
  pull(jail_total)

#Total number of male incarcerations

total_male_population <- incarceration_data %>%
  filter(!is.na(male_jail_pop)) %>%
  summarise(male_jail_total = sum(male_jail_pop)) %>%
  pull(male_jail_total)

#Total male adult incarcerations

total_male_adult <- incarceration_data %>%
  filter(!is.na(male_adult_jail_pop)) %>%
  summarise(total_male_adult = sum(male_adult_jail_pop)) %>%
  pull(total_male_adult)

#Total male juvenile incarnerations

total_male_juvenile <- incarceration_data %>%
  filter(!is.na(male_juvenile_jail_pop)) %>%
  summarise(total_male_juvenile = sum(male_juvenile_jail_pop)) %>%
  pull(total_male_juvenile)

#Ratio of male adult incarceration vs male juvenile incarcerations

ratio_adult_juvenile <- total_male_juvenile/total_male_adult

#Rate of male juvenile incarceration - 1

rate_male_juvenile <- (total_male_juvenile/total_jail_population*100)
roundoff_rate <- trunc(rate_male_juvenile)

#Variable 2 - state with highest/lowest male juvenile incarceration

#State with highest male juvenile incarceration 

state_highest <- incarceration_data %>%
  group_by(state) %>%
  summarise(highest = sum(male_juvenile_jail_pop))%>%
  arrange(desc(highest))
state_highest_juvenile <- state_highest[1,1]$state

#State with the lowest male juvenile incarceration 

state_lowest <- incarceration_data %>%
  group_by(state) %>%
  summarise(lowest = sum(male_juvenile_jail_pop))%>%
  arrange(desc(lowest))
state_highest_juvenile <- state_highest[51,1]$state

#Variable 3 - county with highest/lowest male juvenile incarceration

#County with the highest male adult incarceration 

county_highest <- incarceration_data %>%
  group_by(county_name) %>%
  summarise(highest = sum(male_juvenile_jail_pop))%>%
  arrange(desc(highest))
county_highest_juvenile <- county_highest[1,1]$county_name

#County with the lowest male incarceration 

county_lowest <- incarceration_data %>% 
  filter(!is.na(male_juvenile_jail_pop)) %>%
  group_by(county_name) %>%
  summarise(lowest = sum(male_juvenile_jail_pop))%>%
  arrange(desc(lowest))
county_lowest_juvenile <- county_lowest[51,1]$county_name

#Variable 4 - difference in sum of male juvenile incarcerations from 1970 to 2010

#Number of male juvenile incarceration in 1970

male_juvenile_yearly <- incarceration_data %>%
  group_by(year) %>%
  summarise(yearwise = sum(year)) %>%
  arrange(desc(yearwise))
male_juvenile_1970 <- male_juvenile_yearly[49,2]$year

#Number of male juvenile incarceration in 2018

male_juvenile_yearly <- incarceration_data %>%
  group_by(year) %>%
  summarise(yearwise = sum(year)) %>%
  arrange(desc(yearwise)) %>%
  pull

#Variable 5 - Total white vs. black incarceration numbers

#total number of black jail population

black_jail_total <- incarceration_data %>%
  filter(!is.na(black_jail_pop)) %>%
  summarise(total_black_jail_pop = sum(black_jail_pop)) %>%
  pull(total_black_jail_pop)

#total number of white incarceration

white_jail_total <- incarceration_data %>%
  filter(!is.na(white_jail_pop)) %>%
  summarise(total_white_jail_pop = sum(white_jail_pop)) %>%
  pull(total_white_jail_pop)

#Compare two variables 

#Total number of black incarceration in region - South

black_incarcerations_south <- incarceration_data %>%
  filter(!is.na(black_jail_pop)) %>% 
  group_by(region = "South") %>%
  summarise(total = sum(black_jail_pop))

#Total number of white incarceration in region - West 

white_incarcerations_CA <- incarceration_data %>%
  filter(!is.na(white_jail_pop)) %>% 
  group_by(county = "West") %>%
  summarise(total = sum(white_jail_pop))

#Trends over time chart 

bar_graph <- 






