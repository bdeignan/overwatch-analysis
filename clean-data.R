library(tidyverse)

df = read.csv('data/total-data-2.csv')

time_cols = df %>% 
  select(contains('time')) %>% 
  select(-contains('percentage')) %>% 
  colnames()

hero_time_cols = df %>% 
  select(starts_with('competitiveStats.careerStats')) %>%
  select(ends_with('timePlayed')) %>% 
  select(-contains('allHeroes')) %>% 
  colnames()

patterns = 'competitiveStats\\.careerStats\\.|\\.game\\.timePlayed'

# this fixes all important time cols
clean_df = df %>% 
  mutate_at(time_cols, funs(as.character)) %>% 
  mutate_at(time_cols, funs(ifelse(is.na(.), '00:00:00',.))) %>% 
  mutate_at(time_cols, funs(ifelse(. =='0', '00:00:00',.))) %>%
  mutate_at(time_cols, funs(ifelse(nchar(.)==5, paste('00:',.),.))) %>% 
  mutate_at(time_cols, funs(lubridate::period_to_seconds(lubridate::hms(.)) )) %>% 
  mutate(top_hero=apply(.[hero_time_cols], 1, function(x) names(x)[which.max(x)]) %>% 
           str_remove_all(., patterns))

# min_cols = clean_df %>% 
#   select(c('skill_rating',
#            starts_with('competitiveStats.careerStats.allHeroes'),
#            'top_hero',
#            'games_played', 
#            'rank'
#            )) %>% summarise_all(funs(min(.,na.rm=TRUE)))
# 
# na_cols = clean_df %>% 
#   select(c('skill_rating',
#            starts_with('competitiveStats.careerStats.allHeroes'),
#            'top_hero',
#            'games_played', 
#            'rank'
#   )) %>% is.na() %>% colSums()
# 
# # problematic column
# colnames(clean_df %>% 
#            select(c('skill_rating',
#                     starts_with('competitiveStats.careerStats.allHeroes'),
#                     'top_hero',
#                     'games_played', 
#                     'rank'
#            )))[(min_cols == 0) & (na_cols > 0)]
                                

clean_df_small <- clean_df %>% 
  select(c('skill_rating',
           starts_with('competitiveStats.careerStats.allHeroes'),
           'top_hero',
           'games_played', 
           'rank'
  )) %>%
  filter(!is.na(competitiveStats.careerStats.allHeroes.game.gamesLost)) %>% # one weird obs.
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0,.)))# replace na for all numerics

tank_heroes = c("roadhog", "reinhardt", "dVa", "wreckingBall", "orisa", "zarya", "winston")
damage_heroes = c("soldier76", "mccree", "junkrat", "widowmaker", "ashe", "bastion", 
                  "sombra", "torbjorn", "reaper", "doomfist", "symmetra", "genji",
                  "hanzo", "pharah", "mei", "tracer")
support_heroes = c("lucio", "ana", "moira", "mercy", "brigitte", "baptiste", "zenyatta")

clean_df_small$top_hero_type = NA
clean_df_small[match(tank_heroes, clean_df_small$top_hero), ]$top_hero_type = 'tank'
clean_df_small[match(damage_heroes, clean_df_small$top_hero), ]$top_hero_type = 'damage'
clean_df_small[match(support_heroes, clean_df_small$top_hero), ]$top_hero_type = 'support'

write_csv(clean_df_small, 'data/clean-data.csv')
