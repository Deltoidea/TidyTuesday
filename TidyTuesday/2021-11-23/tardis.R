library(tidyverse)
library(SentimentAnalysis)
library(tidymodels)
library(caret)
library(rpart)
library(rpart.plot)
library(gbm)


# Load Datasets:
directors <- read_csv(file = url("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-23/directors.csv"))
episodes <- read_csv(file = url("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-23/episodes.csv"))
imdb <- read_csv(url("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-23/imdb.csv"))
writers <- read_csv(url("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-23/writers.csv"))

# Explore a bit.
writers_clean <- writers %>% mutate(story_number = story_number %>%
                     gsub("[^0-9.-]", "", .) %>%
                     as.numeric())
directors %>% 
  mutate(story_number = story_number %>%
           gsub("[^0-9.-]", "", .) %>%
           as.numeric()) %>% 
  pull(story_number) %>% 
  range() -> story_range
story_range[2] - story_range[1]
directors_clean <- directors %>% 
  mutate(story_number = story_number %>%
           gsub("[^0-9.-]", "", .) %>%
           as.numeric())
episodes_clean <- episodes %>%
  mutate(story_number = story_number %>%
           gsub("[^0-9.-]", "", .) %>%
           as.numeric()) %>% 
  select(!c(serial_title, production_code))

imdb_clean <- imdb %>%  
  mutate(air_date = air_date %>% lubridate::dmy(),
         ) %>% 
  rename(season_number = season,
         episode_number = ep_num,
         first_aired = air_date) 
                
df <- episodes_clean %>% 
  left_join(directors_clean) %>% 
  left_join(writers_clean) %>% 
  left_join(imdb_clean, by = "first_aired") %>% 
  distinct() %>% 
  filter(!is.na(rating.x)) %>% 
  select(rating_x, uk_viewers, type, first_aired) %>% 
  drop_na()
#Effect of Time of year on rating
df %>% 
  ggplot(aes(x = first_aired, 
             y = rating.x,
             color = first_aired %>% lubridate::year())) + 
  geom_point() +
  geom_smooth() +
  facet_wrap(~type)
director_freq <- df %>% 
  group_by(director) %>% 
  tally()
df %>% 
  group_by(director) %>%
  summarise(avg_rating = mean(rating.x)) %>% 
  na.omit() %>%
  left_join(director_freq) %>% 
  ggplot(aes(y = reorder(director, avg_rating), x = avg_rating, color = n)) +
  geom_bar(stat = "identity") +
  coord_cartesian(xlim = c(70,90))



              
 # Mess around with Sentiment Analysis:
sentiment <- analyzeSentiment(imdb$desc)
sentiment$SentimentLM
n_imdb <- cbind(imdb,sentiment$SentimentQDAP)
plot(n_imdb$ep_num,n_imdb$`sentiment$SentimentQDAP`)
ggplot(data = n_imdb, aes(x = ep_num, y = `sentiment$SentimentQDAP`)) + 
  geom_point() + 
  geom_smooth() +
  facet_wrap(~season) + 
  labs(y = "General Sentiment", x = "Episode Number")
?analyzeSentiment()

df$outcomes <- c(1:length(df$first_aired))
# Set training and testing divisions:
set.seed(1234)

partition <- createDataPartition(df$rating.x, p = .8, list = F) 

train <- df[partition[,1],]
test <- df[-partition[,1],]

nasum <- df %>% 
  summarise_all(~sum(is.na(.)))


# Make some models to compare

hist(df$uk_viewers)

tree <- rpart(uk_viewers ~ type*era+episode_number, data = train)
tree
Boston.boost <- gbm(uk_viewers ~ .,
                    data = train ,
                    distribution = "gaussian",
                    n.trees = 1000,
                 shrinkage = 0.01, 
                 interaction.depth = 4)
Boston.boost

summary(Boston.boost) #Summary gives a table of Variable Importance and a plot of Variable Importance
