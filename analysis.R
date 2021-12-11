#load packages

library(dplyr)
library(leaflet)
library(ggplot2)
library(tidyverse)
library(tidyr)
library(mapproj)

#load data

incarceration_data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

#Variable 1 - Total number of male incarcerations

total_incarcerations <- incarceration_data %>%
  summarise(all_incarcerations = sum(total_pop)) %>%
  pull(all_incarcerations)

#Total number of male incarcerations

total_male_population <- incarceration_data %>%
  filter(!is.na(male_jail_pop)) %>%
  summarise(male_jail_total = sum(male_jail_pop)) %>%
  pull(male_jail_total)

#Variable 2 - Rate of male juvenile incarcerations

#Total male juvenile incarnerations

total_male_juvenile <- incarceration_data %>%
  filter(!is.na(male_juvenile_jail_pop)) %>%
  summarise(total_male_juvenile = sum(male_juvenile_jail_pop)) %>%
  pull(total_male_juvenile)

#Total number of jail population

total_jail_population <- incarceration_data %>%
  filter(!is.na(total_jail_pop)) %>%
  summarise(jail_total = sum(total_jail_pop)) %>%
  pull(jail_total)

#Rate of male juvenile incarceration

rate_male_juvenile <- (total_male_juvenile/total_jail_population*100)
roundoff_rate <- trunc(rate_male_juvenile)

#Variable 3 - state with highest/lowest male juvenile incarceration

#State with highest male juvenile incarceration 

state_highest <- incarceration_data %>%
  filter(!is.na(male_juvenile_jail_pop)) %>%
  group_by(state) %>%
  summarise(total = sum(male_juvenile_jail_pop)) %>%
  pull(total) 

state_high_name <- incarceration_data %>%
  filter(!is.na(male_juvenile_jail_pop)) %>%
  group_by(state) %>%
  summarise(total = sum(male_juvenile_jail_pop)) %>%
  arrange(desc(total)) 
high_state_name <- state_high_name[1,1]$state

num_state_high <- max(state_highest)

#State with the lowest male juvenile incarceration 

num_state_low <- min(state_highest)

#Variable 4 - Total white vs. black incarceration numbers

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

fem <- incarceration_data %>%
  filter(is.na(female_jail_pop)) %>%
  summarise(total = sum(female_jail_pop)) %>%
  pull(total)

#Compare two variables - Variable 5

#Total number of black incarceration in region - South

black_incarcerations_south <- incarceration_data %>%
  filter(!is.na(black_jail_pop)) %>% 
  group_by(region = "South") %>%
  summarise(total = sum(black_jail_pop))

#Total number of white incarceration in region - South 

white_incarcerations_south <- incarceration_data %>%
  filter(!is.na(white_jail_pop)) %>% 
  group_by(region = "South") %>%
  summarise(total = sum(white_jail_pop))

#Variable comparison chart

south_incarcerations <- data.frame(x_axis = c("south_black_incarcerations", "south_white_incarcerations"),
                                   y_axis = c(black_incarcerations_south$total, white_incarcerations_south$total),
                                   z_axis = c("black", "white"))

bargraph <- ggplot(south_incarcerations) +
  geom_col(mapping = aes(x = x_axis , y = y_axis, fill = z_axis )) + 
  labs(title = "White vs Black incarcerations in South region", 
       x = "South", y = "Number of Incarcerations")

#Trend over time graph

year_trends <- incarceration_data %>%
  filter(year>2000) %>%
  filter(state == "CA" | state == "WY") %>%
  filter(!is.na(male_juvenile_jail_pop)) %>%
  group_by(state, year) %>%
  summarise(total = sum(male_juvenile_jail_pop))

line_graph_compare <- ggplot(year_trends) +
  geom_line(aes(year, total, color = state)) + labs(title = "Male Juvenile Jail Population in California and Wyoming",
                                      x = "Year",
                                      y = "Male Juvenile Jail Pop")

#Map 

blank_theme <- theme_bw()+
  theme ( 
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank (),
    panel.border = element_blank()
  )

male_jail_df <- incarceration_data %>%
  filter(year == "2015") %>%
  filter(!is.na(male_jail_pop)) %>%
  select(county_name, state, male_jail_pop, fips)

figure <- map_data("county") %>%
  unite(polyname, region, subregion, sep = ",") %>%
  left_join(county.fips, by = "polyname")

map_data <- figure %>%
  left_join(male_jail_df, by = "fips")

map <- ggplot(map_data) +
   geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = log(male_jail_pop),
     size = 0.1
   )) + 
   labs(
     title = "Male jail population 2020",x = "Longitude",
     y = "Latitude")  +
   coord_map() +
   labs(fill ="male jail population")
 
 

