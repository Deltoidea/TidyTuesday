library(tidyverse)
library(tidytuesdayR)

# 
# df <- tt_load("2021-10-05")
# 
# df <- df$nurses
# 
# regions <- read_csv(url("https://raw.githubusercontent.com/cphalpert/census-regions/master/us%20census%20bureau%20regions%20and%20divisions.csv")) %>% 
#   select(State, Region, Division)
# 
# write_csv(df, file = "./Nurses_with_region.csv")

df <- read_csv("Nurses_with_region.csv")
#Get inflation rate per year.

inflation_rate <- read_csv("./united-states-inflation-rate.csv", skip = 16) %>%
  mutate(Year = date %>% lubridate::year())
inflation_rate$inf_rate <- inflation_rate$`Inflation Rate (%)`/100 
inf_rate <- inflation_rate %>% 
  select(Year, inf_rate) %>% 
  filter(Year %in% c(df$Year %>%unique)) %>% 
  mutate(inf_rate = cumsum(x = inf_rate)+1)

df %>% 
  filter(Salary == "Annual Salary Avg") %>% 
  select(State, Year, valueS, Region) %>% 
  left_join(.,inf_rate) %>% 
  arrange(Year, State) %>%
  group_by(State) %>% 
  mutate(`Adjusted Salary` = valueS/(inf_rate)) %>%
  ungroup() %>% 
  distinct() %>% 
  pivot_longer(cols = c(valueS, `Adjusted Salary`)) %>% 
  group_by(Year, Region, name) %>% 
  summarise(value = mean(value)) %>% 
  na.omit() %>% 
  ggplot(aes(x = Year,  color = Region)) +
  geom_line(aes(y = value)) +
  facet_wrap(~name)
  
df$Salary %>% unique()
df %>% 
  ggplot(aes(x = Year, y = valueS, group = State)) + 
  geom_line(stat = "identity", alpha = 0.5) +
  geom_line(data = df %>%  
              filter(State %in% c("Utah", "California", "Washington", "Louisiana")), aes(color = State)) +
  labs( y = "", title = "", x = "") +
  theme_classic() +
theme(legend.po = "none",
      axis.text = element_blank(),
      axis.line = element_blank(),
      axis.ticks = element_blank(),  ) + coord_polar()


# https://www.macrotrends.net/countries/USA/united-states/inflation-rate-cpi

df$valueS
  