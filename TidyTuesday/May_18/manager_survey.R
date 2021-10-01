library(tidyverse)

tuesdata <- tidytuesdayR::tt_load('2021-05-18')
tuesdata <- tidytuesdayR::tt_load(2021, week = 21)

survey <- tuesdata$survey

glimpse(survey)

#import coordinate data to supplement the survey 

coord <- read_csv("Downloads/worldcitiespop.csv") %>% 
  mutate(city = City) %>% 
  select(!City)
glimpse(coord)
coord %>%
  filter(Country == "us")

?left_join
df <- left_join(survey, coord)

df %>% 
  filter(Country == "us",
         state != "Hawaii") %>% 
  group_by(city, race) %>% 
  summarise(avg = mean(annual_salary), Latitude, Longitude) %>% 
  na.omit() %>% 
  ggplot(aes(x = Longitude, y = Latitude, color = avg)) +
  geom_point(aes(alpha = .001)) +
  scale_color_continuous(type = "viridis")+
  facet_wrap(~race)

survey %>% 
  filter(annual_salary < 500000)%>% 
  ggplot( aes(y = gender, x = annual_salary, )) +
  geom_boxplot()
