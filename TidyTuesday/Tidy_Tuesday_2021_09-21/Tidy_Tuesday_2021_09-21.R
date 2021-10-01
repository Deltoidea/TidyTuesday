library(tidytuesdayR)
library(tidyverse)
library(waffle)
library(ggbeeswarm)

df <- tt_load("2021-09-21")

df <- df$nominees

df %>%  names
df %>% head
df$type %>% 
  unique
df %>% 
  count(type, producer, year, sort = T) %>% 
  pull(producer) %>% 
  unique %>% 
  length

df %>% 
  pull(category) %>% 
  str_sub(end = -8) %>% 
  unique
?regex  
df %>%
  mutate(category = category %>% str_sub(end = -8),
         gender = ifelse(grepl("actress", category, ignore.case = TRUE), "Female",
                  ifelse(grepl("actor", category, ignore.case = TRUE), "Male", "Other"))) %>%
  count(gender, year) %>% 
  ggplot(aes(x = year, y = n, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge")


df %>% 
  mutate(category = category %>% str_sub(end = -8),
         category = ifelse(grepl("actress", category, ignore.case = TRUE), "Actress",
                  ifelse(grepl("actor", category, ignore.case = TRUE), "Actor", "Other"))) %>% 
  group_by(category, distributor) %>% 
  count(type) %>%
  group_by(distributor, category) %>% 
  summarise(tot = sum(n)) %>% 
  ungroup() %>% 
  filter(category != "Other") %>% 
  slice_max(order_by = tot, n = 20) %>%  
  pivot_wider(names_from = category, values_from = tot) %>% 
  mutate(order = Actor + Actress) %>% 
  arrange(order) %>%
  mutate(distributor = factor(distributor,ordered = T)) %>% 
  pivot_longer(cols = c(Actor, Actress), names_to = "Category", values_to = "tot") %>% 
  ggplot(aes(y = distributor, x = tot, fill = Category)) +
  geom_bar(stat="identity", aes(y=reorder(distributor, -order), x=tot, fill=Category), position = "dodge") +
  labs(y = "", x = "Number of Nominations and Wins", title = "Combination of Nominations and Wins by Major Distributor") + theme_bw()
  
ggsave(filename = "./graph.png",
       device = "png",
       height = 5,
       width = 7)

major_distributor <- df %>% 
  mutate(category = category %>% str_sub(end = -8),
         category = ifelse(grepl("actress", category, ignore.case = TRUE), "Actress",
                           ifelse(grepl("actor", category, ignore.case = TRUE), "Actor", "Other"))) %>% 
  group_by(category, distributor) %>% 
  count(type) %>%
  group_by(distributor, category) %>% 
  summarise(tot = sum(n)) %>% 
  ungroup() %>% 
  filter(category != "Other") %>% 
  slice_max(order_by = tot, n = 10) %>% 
  pull(distributor) %>% unique()



df %>% 
  filter(distributor == major_distributor) %>% 
  mutate(category = category %>% str_sub(end = -8)) %>% 
  group_by(distributor, year, type) %>% 
  tally() %>% 
  ggplot(aes(x = year, y = n, color = type)) +
  geom_beeswarm(priority = "random")+
  facet_wrap(~distributor)


df %>% 
  filter(distributor == major_distributor) %>% 
  group_by(distributor, year) %>% 
  count(type) %>%
  ungroup() %>% 
  group_by(distributor, type) %>% 
  mutate(cumulative = cumsum(n)) %>% 
  ggplot(aes(x = year, y = cumulative, color = distributor)) +
  geom_line(aes(linetype = type))
  
