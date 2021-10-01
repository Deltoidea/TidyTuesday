library(RCurl)
library(tidyverse)
library(tidytuesdayR)
library(GGally)
library(modelr)
library(patchwork)


df <- tt_load("2021-06-22")$parks


df
glimpse(df)
names(df)
df$year %>% unique() 
top10 <- df[
  with(df,order(rank)),
] %>% head(10)
bottom10 <- df[
  with(df,order(rank)),
] %>% tail(10)
df$spend_per_resident_data <-  df$spend_per_resident_data %>% 
  gsub(pattern = "\\$", replacement = "", .) %>% 
  as.numeric()
df$park_benches

 df %>% 
  select(rank, med_park_size_points, amenities_points, restroom_data) %>%
  ggpairs(cardinality_threshold = NULL)


mod1 <- glm(data = df, formula = total_pct~restroom_points + dogpark_points + spend_per_resident_data + splashground_points)
mod2 <- glm(data = df, formula = total_pct~restroom_points * dogpark_points * spend_per_resident_data * splashground_points)
mod3 <- lm(data = df, formula = total_pct~restroom_points * dogpark_points * spend_per_resident_data * splashground_points * spend_per_resident_points)



p1 <- add_predictions(df, mod1) %>%
  ggplot(aes(x=year, color=spend_per_resident_data)) +
  geom_point(aes(y=total_pct)) +
  geom_smooth(method = "lm",aes(y=pred))

p2 <- add_predictions(df, mod2) %>%
  ggplot(aes(x=year, color=spend_per_resident_data)) +
  geom_point(aes(y=total_pct)) +
  geom_smooth(method = "lm",aes(y=pred))

p3 <- add_predictions(df, mod3) %>%
  ggplot(aes(x=year, color=spend_per_resident_data)) +
  geom_point(aes(y=total_pct)) +
  geom_smooth(method = "lm",aes(y=pred))


p1 / p2 / p3

