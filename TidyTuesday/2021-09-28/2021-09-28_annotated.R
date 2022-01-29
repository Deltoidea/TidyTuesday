library(tidyverse)
library(tidytuesdayR)
library(lubridate)
library(directlabels)

#Read in the data:
papers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/papers.csv')
authors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/authors.csv')
programs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/programs.csv')
paper_authors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/paper_authors.csv')
paper_programs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/paper_programs.csv')
#Do a little exploration
glimpse(paper_authors)
glimpse(paper_programs)
programs %>% head
authors <- authors %>% 
  select(author, name)
papers %>% head

#Join datasets for plotting
left_join(papers, paper_authors) %>% 
  mutate(author_code = author, NULL) %>% 
  select(!author) %>% 
  left_join(., paper_programs) %>% 
  drop_na() %>% 
  left_join(.,programs) %>% 
  unite("date", month,year) %>% #Create functional date column
  mutate(date = date %>% lubridate::my()) %>% 
  group_by(year = floor_date(date, unit = "year"), Program = program_desc) %>% 
  tally(name = "Count") %>% drop_na %>% #Summarize data to create a Count column
  filter(year < "2021-01-01") %>% # Remove partial years
  ggplot(aes(x = Year, y  = Count, color = Program)) +
  geom_line(alpha = .5) +
  theme_bw()+
  labs(title = "Papers Published by Year")

# make a bumpchart


bumpplot <- left_join(papers, paper_authors) %>% 
  mutate(author_code = author, NULL) %>% 
  select(!author) %>% 
  left_join(., paper_programs) %>% 
  drop_na() %>% 
  left_join(.,programs) %>% 
  unite("date", month,year) %>% 
  mutate(date = date %>% lubridate::my()) %>% 
  group_by(year = floor_date(date, unit = "year") %>% year(), program_desc) %>% 
  tally() %>% drop_na %>%
  filter(year < "2021-01-01") %>%
  ungroup() %>% 
  group_by(year) %>% 
  arrange(year, n, program_desc) %>% 
  mutate(ranking = row_number()) %>% 
  ggplot(aes(x = year, y = ranking, group = program_desc)) +
  geom_line(aes(color = program_desc), size = 1) +
  geom_point(aes(color = program_desc, alpha = 1), size = 3) + 
  scale_y_reverse() + 
  geom_dl(aes(label = program_desc), method = list(dl.trans(x = x + 0.2), "last.points", cex = 0.8)) +
  theme_classic() +
  theme(legend.position = "none", plot.margin = margin(10,10,0,0)) + 
  labs(title = "Relative Ranking of NBER Programs Based On Papers Published.",
       y = "Ranking",
       caption = "Data obtained from the National Bureau of Economic Reasearch",
       x = "") +
  coord_cartesian(xlim=c(1970, 2060))+
  scale_x_continuous(breaks = c(1970, 1980, 1990, 2000, 2010, 2020 ))
bumpplot
ggsave(filename = "./bumpplot.png",
       plot = bumpplot,
       height = 5,
       width = 7,
       units = "in")
