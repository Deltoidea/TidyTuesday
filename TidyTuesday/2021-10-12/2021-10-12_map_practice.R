library(tidyverse)
library(tidytuesdayR)
library(janitor)

#load data

#df <- tt_load("2021-10-12")
#saveRDS(df, file = "./dataset.RDS")

df <- readRDS("./dataset.RDS")

df$`fish-stocks-within-sustainable-levels` %>% 
  drop_na() %>% 
  group_by(Year, Entity) %>% 
  summarise(mean = mean(`Share of fish stocks within biologically sustainable levels (FAO, 2020)`)) %>% 
  ggplot(aes(x = Year, y = mean, color = Entity)) +
  geom_line()

consumption <- df$`fish-and-seafood-consumption-per-capita` 
consumption %>%
  clean_names() %>% 
  split(.$entity) %>% 
  map(~ lm(fish_seafood_food_supply_quantity_kg_capita_yr_fao_2020~year, data = .x)) %>% 
  map(coef) %>% 
  map_dbl(min) %>% str()
