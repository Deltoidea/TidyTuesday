library(tidyverse)
library(tabulizer)



tables <- tabulizer::extract_tables(file = "../../../Downloads/document.pdf", method = "stream", output = "data.frame")
str(tables)
tibble2 <- tables[2] %>% as.data.frame()
tibble2 %>% view
names(tibble2)


tibble %>% 
  select(colSums(!is.na(.)) > 0)
  
  
tibble2 <- tibble2[,colSums(is.na(tibble2))<nrow(tibble2)]  
  
tibble2 %>% pivot_longer(cols = 2:ncol(tibble2)) %>% pivot_wider(names_from = X, values_from = value)
