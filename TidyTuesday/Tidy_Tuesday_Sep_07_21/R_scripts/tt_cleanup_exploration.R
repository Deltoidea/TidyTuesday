library(tidyverse)
library(tidytuesdayR)

df <- tidytuesdayR::tt_load("2021-09-07")
tidytuesdayR::last_tuesday()

df$circuits %>% 
  glimpse
df$seasons
df$races
df$lap_times
df$status %>% 
  glimpse

df$