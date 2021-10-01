library(tidyverse)
library(tidytuesdayR)
library(caret)
library(tidymodels)
library(GGally)


df <- tidytuesdayR::tt_load(2021, week = 35)
df$lemur_data %>% glimpse()
df$taxonomy
d <- append(x = df$lemur_data %>% names(), 
       values = df$taxonomy %>% names()) %>% 
  duplicated()
d
which(d == TRUE)
append(x = df$lemur_data %>% names(), 
       values = df$taxonomy %>% names())[55]
cdf <- left_join(df$lemur_data, df$taxonomy)


cdf %>% glimpse()
cdf %>%  
  filter(!is.na(age_at_death_y), age_at_death_y != 0) %>% 
  ggplot(aes(x = dob, y = age_at_death_y)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~taxon)

mod1 <-  glm(data = cdf, formula = age_at_death_y ~ dob*taxon)

glimpse(mod1)
mod1$residuals %>% plot()
mod1$fitted.values %>% plot
mod1$coefficients %>% plot
 
cdf %>% 
  filter(hybrid == "N",
         !is.na(dod)) %>% 
  group_by(latin_name, sex) %>% 
  summarise(dod = mean(age_at_death_y)) %>%
  pivot_wider(names_from = sex, values_from = dod) %>% 
  select(!ND) %>% 
ggparcoord(columns = 2:3,groupColumn = 1)
GGally::ggparcoord()


BiocManager::install("msa")
sessionInfo()
