library(tidyverse)
library(corrplot)
library(car)

df <- read_csv('data/clean-data.csv')
df$win_rate = (df$game.gamesWon / df$game.gamesPlayed)

# clean up column names

df %>% head()

boxplot(skill_rating ~ top_hero_type, data=df)

plot(skill_rating ~ games_played, data=df)


plot(skill_rating ~ competitiveStats.careerStats.allHeroes.matchAwards.medals, data=df)

df %>% select(top_hero_type,
              best.meleeFinalBlowsMostInGame,
              average.objectiveTimeAvgPer10Min) %>% 
  is.na() %>% colSums
  

# Correlations ------------------------------------------------------------

num_feats = df %>% select_if(is.numeric) %>% select(-skill_rating) %>% colnames()
# linear

find_cor_sr <- function(data){
  M <- cor(data %>% select_if(is.numeric))
  M[row.names(M) == 'skill_rating', !(colnames(M) %in% c('rank', 'skill_rating'))]
}

lin_sr_cor = find_cor_sr(df)

# polynomial
poly2_sr_cor = df %>% mutate_at(num_feats, funs(.^2)) %>% find_cor_sr(.)
poly3_sr_cor = df %>% mutate_at(num_feats, funs(.^3)) %>% find_cor_sr(.)

# log
log_sr_cor = df %>% mutate_at(num_feats, funs(log(.))) %>% find_cor_sr(.)

# visualize
promising = c()
promising = c(promising, names(lin_sr_cor[which.max(lin_sr_cor)]))
promising = c(promising, names(lin_sr_cor[which.min(lin_sr_cor)]))
promising = c(promising, names(poly2_sr_cor[which.max(poly2_sr_cor)]))
promising = c(promising, names(poly2_sr_cor[which.min(poly2_sr_cor)]))
promising = c(promising, names(poly3_sr_cor[which.max(poly3_sr_cor)]))
promising = c(promising, names(poly3_sr_cor[which.min(poly3_sr_cor)]))
promising = c(promising, names(log_sr_cor[which.max(log_sr_cor)]))
promising = c(promising, names(log_sr_cor[which.min(log_sr_cor)]))

pair_vars = c(unique(promising), 'skill_rating')

pairs(df %>% select(one_of(pair_vars)))

par(mfrow=c(1,2))
plot(skill_rating ~ games_played, data = df)
plot(skill_rating ~ log(games_played), data = df)
# plot(skill_rating ~ game.gamesWon ^2, data = df)
# plot(skill_rating ~ game.gamesWon ^3, data = df)

# Model building ----------------------------------------------------------

fit = lm(skill_rating ~ . -rank -top_hero_type,
         data=df
         )
summary(fit)
plot(fitted(fit), resid(fit))

qqnorm(resid(fit))
qqline(resid(fit), col = "dodgerblue", lwd = 2)


fit2 = lm(skill_rating ~ top_hero_type : . + ., 
          data = df %>% select(best.meleeFinalBlowsMostInGame,
                                 average.objectiveTimeAvgPer10Min, games_played,
                               top_hero_type, skill_rating)
          )

summary(fit2)
shapiro.test(resid(fit2))

vif(fit2)
par(mfrow=c(1,1))
qqnorm(resid(fit2))
qqline(resid(fit2), col = "dodgerblue", lwd = 2)
plot(fitted(fit2), resid(fit2))
hist(rstandard(fit2))

##
pair_vars
fit3 = lm(skill_rating ~ top_hero : . + ., 
          data = df %>% select(one_of(c(pair_vars, 'top_hero'))) %>% 
            select(-game.gamesPlayed ) %>% 
            mutate(top_hero = relevel(top_hero %>% as.factor(), ref='reinhardt'))
)
summary(fit3)
unique(df$top_hero) %>% sort()

## kai's model
form = formula(skill_rating ~  
  assists.offensiveAssists + average.allDamageDoneAvgPer10Min + average.deathsAvgPer10Min + 
  average.eliminationsAvgPer10Min + average.healingDoneAvgPer10Min + 
  average.objectiveKillsAvgPer10Min + 
  average.objectiveTimeAvgPer10Min + average.soloKillsAvgPer10Min + 
  best.barrierDamageDoneMostInGame + best.eliminationsMostInGame + 
  best.healingDoneMostInGame + best.meleeFinalBlowsMostInGame + 
  best.offensiveAssistsMostInGame + best.timeSpentOnFireMostInGame + 
  combat.objectiveKills + matchAwards.cards + matchAwards.medalsBronze + 
  matchAwards.medalsGold + miscellaneous.teleporterPadsDestroyed + 
  assists.reconAssists + log(games_played) + top_hero_type + average.allDamageDoneAvgPer10Min:top_hero_type + 
  average.healingDoneAvgPer10Min:top_hero_type + average.objectiveTimeAvgPer10Min:top_hero_type + 
  average.soloKillsAvgPer10Min:top_hero_type)

fit4 = lm(form, data = df)
summary(fit4)
vif(fit4)



form2 = formula(skill_rating ~ top_hero + best.meleeFinalBlowsMostInGame + average.allDamageDoneAvgPer10Min + 
                  average.objectiveKillsAvgPer10Min + games_played + top_hero:average.allDamageDoneAvgPer10Min)
fit5 = lm(form2, data = df)
summary(fit5)
vif(fit5)
qqnorm(resid(fit5))
qqline(resid(fit5), col = "dodgerblue", lwd = 2)
plot(fitted(fit5), resid(fit5))
hist(rstandard(fit5))
shapiro.test(resid(fit5))
library(lmtest)
bptest(fit5)
