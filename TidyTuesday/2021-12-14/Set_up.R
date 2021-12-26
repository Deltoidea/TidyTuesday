library(tidyverse)
library(tidytuesdayR)
library(dbplyr)

data <- tt_load("2021-12-14",)

data$lyrics %>% 
  write_csv("./Lyrics.csv")
data$related_artists %>% 
  write_csv("Related_artists.csv")
data$studio_album_tracks %>% 
  write_csv("./Song_Observations.csv")
