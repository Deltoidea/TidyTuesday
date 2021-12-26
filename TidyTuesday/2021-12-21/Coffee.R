library(tidyverse)
library(tidytuesdayR)
library(data.table)
#Setting up environment:
data.table::

# data <- tt_load("2021-12-21")
# 
# 
# df <- data$starbucks
# df %>% write_csv("./Coffee.csv")

df <- read_csv("Coffee.csv")

glimpse(df)
df %>% 
  mutate(trans_fat_g = trans_fat_g %>% as.numeric())
df$trans_fat_g %>%
  as.numeric() %>%
  sample(size = 300) %>% 
  hist()
df %>% 
  unique()

df %>% 
  ggplot(aes(y = size, x = product_name, fill = size)) + geom_bar(stat = "identity")
  
df %>% 
  filter(serv_size_m_l > 0,
         caffeine_mg > 0) %>% 
  mutate(caffeine_per_ml = caffeine_mg/serv_size_m_l) %>% 
  group_by(product_name) %>% 
  summarise(caffeine = mean(caffeine_per_ml)*250) %>% 
  arrange(desc(caffeine)) %>% 
  ggplot(aes(y = reorder(product_name, caffeine), x = caffeine)) +
  geom_bar(stat = "identity", 
           position = "dodge")
  
  ggplot(aes(x = sugar_g, y = caffeine_mg/serv_size_m_l)) + 
  geom_point()

  
  df %>% 
    mutate(Type = if_else(grepl("tea", product_name, ignore.case = T), "Tea", 
                          if_else(grepl("coffee|caff|black|cold brew", product_name, ignore.case = T), 'Coffee', 
                                  if_else(grepl("Espresso|cappucc|macchia", product_name, ignore.case = T), "Espresso", "Other")))) %>% 
  filter(caffeine_mg >=50) %>% 
    ggplot(aes(y = product_name,
               color = serv_size_m_l,
               x = caffeine_mg)) + 
    geom_point() +
    facet_wrap(~size)
  