library(tidyverse)

df <- read.csv('data/total-data.csv')
df %>% select(1:50) %>% glimpse()
# df <- total_df %>% type.convert()

start_cols <- df %>% 
  select(c('skill_rating',
           starts_with('competitiveStats.careerStats.allHeroes'))
         ) %>% 
  is.na() %>% 
  colSums()

start_df <- df %>% 
  select(one_of(names(start_cols[start_cols == 0])))

pairs(start_df[1:4])

