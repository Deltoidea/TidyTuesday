library(tidyverse)


dat <- read_csv(url("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"))

dat %>%  
  names()

df <- dat %>% 
  group_by(date, state) %>% 
  summarise(cases = sum(cases), deaths = sum(deaths)) 

df %>% 
  filter(state == "Utah") %>% 
  pivot_longer(cols = c(deaths, cases), names_to = "type", values_to = "count") %>%
  group_by(date) %>% 
  summarise(sum(count)) %>% 
  ggplot(aes(x = date, y = count, color = `type`)) +
  geom_bar()
  