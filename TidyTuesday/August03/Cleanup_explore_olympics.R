library(tidytuesdayR)
library(tidyverse)


tidytuesdayR::last_tuesday()
df <- tt_load(x = "2021-08-03")
  
df <- df$athletes


glimpse(df)
df$grp_id %>% unique()
df %>% 
  mutate(medal = as.numeric(recode(medal, "Gold" = "1",
                        "Silver" = "2",
                        "Bronze" = "3"))) %>% 
  group_by(year, abb, medal) %>%
  summarise(count(medal))
glimpse(df)
df %>% 
  group_by(year, abb, medal) %>%
  summarise(medal_count = n()) %>% 
  filter(abb == "USA") %>% 
  ungroup() %>% 
  group_by(year) %>% 
  ggplot(aes(x = year, y = medal_count)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = medal)) + 
  scale_fill_manual(values = c("#B87333", "gold", "#C0C0C0")) +
  labs(title = "Total USA Medals by Year", y = "", x = "") + 
  theme_minimal() + 
  scale_x_continuous(breaks = c(seq.int(1980, 2016, 4)))
