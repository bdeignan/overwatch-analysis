library(tidyverse)
library(corrplot)

df <- read_csv('data/clean-data.csv')

# clean up column names

df %>% head()

boxplot(skill_rating ~ top_hero_type, data=df)

plot(skill_rating ~ games_played, data=df)


plot(skill_rating ~ competitiveStats.careerStats.allHeroes.matchAwards.medals, data=df)

df %>% select(top_hero_type,
              best.meleeFinalBlowsMostInGame,
              average.objectiveTimeAvgPer10Min) %>% 
  is.na() %>% colSums
  

M <- cor(df %>% select_if(is.numeric))

sr_cor = M[row.names(M) == 'skill_rating', !(colnames(M) %in% c('rank', 'skill_rating'))]

sr_cor[which.max(sr_cor)]
sr_cor[which.min(sr_cor)]

fit = lm(scale(skill_rating) ~ . -rank -top_hero_type,
         data=df
         )
summary(fit)
?scale
plot(fitted(fit), resid(fit))

