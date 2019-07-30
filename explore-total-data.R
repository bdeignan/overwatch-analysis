library(tidyverse)

df <- read.csv('data/total-data-2.csv')
df %>% select(1:50) %>% glimpse()
# df <- total_df %>% type.convert()

# NEED TO FIX THIS, WE HAVE MISSING DATA
start_cols <- df %>% 
  select(c('skill_rating',
           starts_with('competitiveStats.careerStats.allHeroes'))
         ) %>% 
  is.na() %>% 
  colSums()

start_df <- df %>% 
  select(one_of(names(start_cols[start_cols == 0])))

## analysis
hist(start_df$skill_rating)
pairs(start_df[1:4])

pairs(start_df[c(1, 7:ncol(start_df))])

cor(start_df)
start_df$competitiveStats.careerStats.allHeroes.game.timePlayed %>% head()


start_df %>% 
  mutate(charlen=nchar(competitiveStats.careerStats.allHeroes.game.timePlayed %>% 
                         as.character()
                       )) %>%
  group_by(charlen) %>% 
  summarise(n())

start_df %>% 
  mutate(charlen=nchar(competitiveStats.careerStats.allHeroes.game.timePlayed %>% 
                         as.character()
  )) %>%
  filter(charlen==9) %>% 
  head()

start_df %>% 
  mutate(charlen=nchar(competitiveStats.careerStats.allHeroes.game.timePlayed %>% 
                         as.character()
  )) %>%
  filter(charlen==9) %>% 
  head() %>% 
  mutate(competitive_time_played = competitiveStats.careerStats.allHeroes.game.timePlayed %>% 
           as.character()) %>% 
  separate(competitive_time_played, c('hour', 'min', 'sec'), sep = ':')

  