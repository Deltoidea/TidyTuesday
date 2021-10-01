library(tidyverse)
library(tidytuesdayR)
library(caret)
library(tidymodels)
library(modelr)



data <- tt_load("2021-09-14")


bdf <- data$billboard
adf <- data$audio_features

bdf %>% names
adf %>% names
bdf$song %>% head()
adf$song %>% head

df <- left_join(bdf, adf, by = c("song", "performer"))
set.seed(123) 
dftrain <- sample_n(df, 10000, replace = FALSE)
set.seed(124)
dftest <-sample_n(df, 2500, replace = FALSE)


glimpse(df)
mod1 <- glm(data = dftrain, formula = peak_position~spotify_genre+danceability+energy+loudness+speechiness+acousticness)
mod2 <- glm(data = dftrain, formula = peak_position~spotify_genre+danceability+energy+loudness)
mod3 <- glm(data = dftrain, formula = peak_position~spotify_genre+spotify_track_duration_ms+danceability+liveness+instrumentalness+loudness+acousticness)
mean(mod1$residuals^2)
mean(mod2$residuals^2)
mean(mod3$residuals^2)
dftest <- add_predictions(dftest, model = mod1)
dftest %>% 
  ggplot(aes(x = weeks_on_chart, y = peak_position)) +
  geom_point() +
  geom_point
