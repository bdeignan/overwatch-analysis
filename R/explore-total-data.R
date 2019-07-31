library(tidyverse)

df <- read.csv('data/total-data-2.csv')
df %>% select(1:50) %>% glimpse()
# df <- total_df %>% type.convert()

# NEED TO FIX THIS, WE HAVE MISSING DATA
# start_cols <- 
  
df %>% 
  select(c('skill_rating',
           starts_with('competitiveStats.careerStats.allHeroes'))
         ) %>% 
  is.na() %>% 
  colSums()

df %>% 
  select(c('skill_rating',
           starts_with('competitiveStats.careerStats.allHeroes'))
  ) %>% 
  summary()

df %>% 
  filter(competitiveStats.careerStats.allHeroes.combat.timeSpentOnFire=='00:20') %>% 
  mutate(fire = competitiveStats.careerStats.allHeroes.combat.timeSpentOnFire %>% as.character()
         ) %>% 
  separate(fire, c('hour', 'min', 'sec'), sep = ':') %>% 
  select(hour, min, sec)
  

df %>% 
  select(contains('time')) %>% 
  mutate_each(funs(as.character)) %>% 
  replace(is.na(.), '00:00:00') %>% 
  mutate_each(funs(nchar)) %>% 
  head()

df %>% 
  mutate(competitiveStats.careerStats.brigitte.heroSpecific.inspireUptimePercentage = 
           competitiveStats.careerStats.brigitte.heroSpecific.inspireUptimePercentage %>% 
           as.character() %>% ifelse(. == '0', '00:00:00', .)) %>% 
  mutate(ccc = competitiveStats.careerStats.brigitte.heroSpecific.inspireUptimePercentage %>% nchar()) %>% 
  filter(ccc==2) %>% 
  group_by(competitiveStats.careerStats.brigitte.heroSpecific.inspireUptimePercentage, ccc) %>% 
  summarise(n())

tmp = df %>% 
  select(contains('time')) %>% 
  select(-contains('percentage')) %>% 
  mutate_each(funs(as.character)) %>% 
  mutate_all(funs(ifelse(is.na(.), '00:00:00',.))) %>% 
  mutate_all(funs(ifelse(. =='0', '00:00:00',.))) %>%
  mutate_all(funs(ifelse(nchar(.)==5, paste('00:',.),.))) %>% 
  mutate_each(funs(nchar)) %>% 
  summarise_each(funs(min, max))

tmp %>% apply(., 1, FUN=min)
tmp %>% dim
which.min(tmp)

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
df %>% 
  mutate_at(time_cols, funs(as.character)) %>% 
  mutate_at(time_cols, funs(ifelse(is.na(.), '00:00:00',.))) %>% 
  mutate_at(time_cols, funs(ifelse(. =='0', '00:00:00',.))) %>%
  mutate_at(time_cols, funs(ifelse(nchar(.)==5, paste('00:',.),.))) %>% 
  mutate_at(time_cols, funs(lubridate::period_to_seconds(lubridate::hms(.)) )) %>% 
  mutate(top_hero=apply(.[hero_time_cols], 1, function(x) names(x)[which.max(x)]) %>% 
           str_remove_all(., patterns))


  
df %>% 
  select(starts_with('competitiveStats.careerStats')) %>%
  select(ends_with('timePlayed')) %>% 
  select(-contains('allHeroes')) %>% 
  mutate_each(funs(as.character)) %>% 
  mutate_all(funs(ifelse(is.na(.), '00:00:00',.))) %>% 
  mutate_all(funs(ifelse(. =='0', '00:00:00',.))) %>%
  mutate_all(funs(ifelse(nchar(.)==5, paste('00:',.),.))) %>% 
  mutate_all(funs(lubridate::period_to_seconds(lubridate::hms(.)) )) %>% 
  mutate(top_hero=apply(.[hero_time_cols], 1, function(x) names(x)[which.max(x)]) %>% 
           str_remove_all(., patterns)) %>% 
  group_by(top_hero) %>% 
  summarise(n()) %>% View

tmp$competitiveStats.careerStats.brigitte.heroSpecific.inspireUptimePercentage_min
  
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

  