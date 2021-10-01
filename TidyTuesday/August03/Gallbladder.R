library(tidyverse)
library(ggside)
library(gapminder)
library(factoextra)
library(cluster)

df <- read_csv("../../../../user/Downloads/cdph_ssi_adult_odp_2020h1.csv")

df %>% glimpse()
df$Year %>% unique()

df %>% 
  filter(Operative_Procedure == "Gallbladder surgery") %>% 
  group_by(Facility_Type) %>%
  summarise(avg = mean(Procedure_Count), avg_inf = mean(Infections_Reported), avg_pred_inf = mean(Infections_Predicted)) %>% 
  mutate(Predicton_accuracy = avg_pred_inf - avg_inf) %>% 
  ggplot(aes(x = Facility_Type, y = Predicton_accuracy)) +
  geom_segment( aes(x = Facility_Type, xend = Facility_Type, y=0, yend = Predicton_accuracy), color="grey") +
  geom_point( color="orange", size=4)


## modeling practice
set.seed(123)
df <- gapminder::gapminder_unfiltered
df$year %>% unique() %>% length
df[,4:length(df)] <- df[,4:length(df)] %>% scale
df %>% 
  mutate(year = as.factor(year)) %>%  
  na.omit() %>% 
  select(!continent) %>% 
  kmeans(., 3, iter.max = 2, nstart = 1)

