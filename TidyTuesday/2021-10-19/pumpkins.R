library(tidyverse)
library(tidytuesdayR)
library(ggmap)
library(sf)
library(patchwork)
library(maps)


theme_set(theme_minimal())
#load data for 2021-10-19
world <- map_data("world")
# 
# data <- tt_load("2021-10-19")
# write_csv(data$pumpkins,"./pumpkins.csv")


pumpkins <- read_csv("pumpkins.csv")

#lat and long data
citymap <- read_csv("uscities.csv")
citymap <- citymap %>% 
  select(city, state_name, lat, lng) 

#explore a bit
pumpkins <- pumpkins[!is.na(pumpkins$weight_lbs),]
pumpkins %>% names

pumpkins %>% glimpse()
pumpkins$variety %>% unique()
hist(pumpkins$weight_lbs %>% as.numeric())

#convert some data types

# clean id and split into year and full type

pumpkins %>% 
  separate(col = id, into = c("year", "species"), sep = "-") %>% 
  mutate(species = recode(species,
                          "F" = "Field Pumpkin", 
                          "P" = "Giant Pumpkin", 
                          "S" = "Giant Squash", 
                          "W" = "Giant Watermelon", 
                          "L" = "Long Gourd", 
                          "T" = "Tomato"),
         across(c(ott, 
                  est_weight,
                  weight_lbs,
                  year,
                  place), 
                as.numeric),
         state_name = state_prov) -> pumpkins

 pumpkins %>% 
   count(species)
names(pumpkins)
names(citymap) 
 
pdf <-  pumpkins %>% 
   filter(country == "United States") %>% 
   left_join(citymap, ) %>% 
  mutate(long = lng) %>% 
  select(year, 
         species,
         place,
         weight_lbs,
         city,
         state_prov,
         ott,
         lat,
         long)
 pdf %>% 
   filter(species == "Giant Pumpkin") %>% 
   count(place) 

p1 <- pdf %>%  
  filter(lat < 50) %>% 
  ggplot(aes(y = lat, x = weight_lbs, color = species )) +
  geom_point() +
  geom_smooth(method = "lm", color = "black") +
  facet_wrap(~species,scales = "free") +
  theme(legend.position = "none",
        axis.text = element_blank(),
        plot.background = element_rect(fill = "white")) +
  labs(title = "Produce Size by Latitude",
       caption = "*Location information was downloaded from simplemaps.com",
       y = "Latitude", x = "Weight")
p1
sumpdf <- pdf %>% 
  filter(year == 2021) %>% 
  drop_na() %>% 
  mutate(place %in% 1:10) %>% 
  group_by(state_prov, species, place) %>%
  summarise(weight_lbs = mean(weight_lbs),
            lat = mean(lat),
            long = mean(long)) 
  
  
  
p2 <- ggplot() + 
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "white", fill = "gray", size = 0.1
  ) + geom_point(data = sumpdf, 
                 aes(x = long,
                     y = lat,
                     color = weight_lbs),
                 alpha = .8,
                 size = 2) +
  xlim(range(pdf$long,na.rm = T)) +
  ylim(range(pdf$lat, na.rm = T)) +
  facet_wrap(~species) +
  labs(title = "Produce Size by Latitude",
       caption = "*Location information was downloaded from simplemaps.com") +
  theme(axis.text = element_blank(), axis.title = element_blank())
p2  
  
scatterplot <- p1 + labs(title = "What Effect Does Latitude Have on Max Weight?",
                         x = "Latitude")

map <- p2 


ggsave(plot = scatterplot,
       "Scatterplot.png",
       device = "png",
       width = 7,
       height = 5)

ggsave(plot = map,
       "map.png",
       device = "png",
       width = 7,
       height = 5)
