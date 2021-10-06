library(tidyverse)
library(tidytuesdayR)


df <- tt_load("2021-10-05")


df <- df$nurses


df %>% names

df %>% pivot_longer(cols = starts_with("Annual"), 
                    names_to = "Salary", 
                    values_to = "valueS") %>% 
  pivot_longer(cols = starts_with("Hourly"), 
               names_to = "Hourly", 
               values_to = "valueH")

regions <- read_csv(url("https://raw.githubusercontent.com/cphalpert/census-regions/master/us%20census%20bureau%20regions%20and%20divisions.csv")) %>% 
  select(State, Region, Division)

df %>% names

df <- df %>% pivot_longer(cols = starts_with("Annual"), 
                          names_to = "Salary", 
                          values_to = "valueS") %>% 
  pivot_longer(cols = starts_with("Hourly"), 
               names_to = "Hourly", 
               values_to = "valueH") 
df %>% left_join(., regions)
df
df %>% 
  group_by(State, Year) %>% 
  summarise(mean = mean(`Hourly Wage Avg`)) %>% 
  
  ggplot(aes(x = Year, y = mean, group = State)) + 
  geom_line(stat = "identity", alpha = 0.5) +
  geom_line(data = df %>% 
              group_by(State, Year) %>% 
              summarise(mean = mean(`Hourly Wage Avg`)) %>% 
              filter(State == "Utah"), aes(color = State)) +
  labs( y = " Mean Hourly Wage", title = "") +
  theme_classic() +
theme(legend.title = element_blank()) 
