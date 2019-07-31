library(readr)
library(tidyverse)
library(corrplot)

clean_df <- read_csv("data/clean-data.csv")

# remove factor columns for cor() to work properly
predictors = colnames(clean_df)[1:60]
predictors = append(predictors, "games_played")
numerical_df = clean_df[predictors]
corr_mat = round(cor(numerical_df), 2)

# https://stackoverflow.com/questions/18275639/remove-highly-correlated-variables
corr_mat[upper.tri(corr_mat)] = 0
diag(corr_mat) = 0
df = numerical_df[,!apply(corr_mat,2,function(x) any(x > 0.90))]

# add factor column back in 
# predictors = append(predictors, "top_hero_type")
# fit = lm(as.formula(paste("skill_rating ~ ", paste(predictors[2:length(predictors)], collapse="+"))), data = clean_df)
# summary(fit)

fit = lm(skill_rating ~ ., data = clean_df)
summary(fit)

n = length(resid(fit))
back_bic = step(fit, direction = "backward", k = log(n))

fit_bic = lm(skill_rating ~ assists.defensiveAssists + assists.healingDone + 
               assists.offensiveAssists + average.allDamageDoneAvgPer10Min + 
               average.eliminationsAvgPer10Min + average.healingDoneAvgPer10Min + 
               average.objectiveKillsAvgPer10Min + average.soloKillsAvgPer10Min + 
               best.barrierDamageDoneMostInGame + best.eliminationsMostInGame + 
               best.meleeFinalBlowsMostInGame + best.offensiveAssistsMostInGame + 
               combat.objectiveKills + game.gamesLost + game.gamesTied + 
               game.gamesWon + game.timePlayed + matchAwards.medalsBronze + 
               matchAwards.medalsGold + miscellaneous.teleporterPadsDestroyed + 
               assists.reconAssists + games_played, data = clean_df)
summary(fit_bic)

library(faraway)
vif(fit_bic)



fit = lm(skill_rating ~ . + top_hero_type: ., 
         data = clean_df %>%
           select(predictors)
         )
summary(fit)

n = length(resid(fit))
back_bic = step(fit, direction = "backward", k = log(n))

fit_bic = lm(skill_rating ~ assists.defensiveAssists + assists.healingDone + 
               assists.offensiveAssists + average.allDamageDoneAvgPer10Min + 
               average.barrierDamageDoneAvgPer10Min + average.deathsAvgPer10Min + 
               average.eliminationsAvgPer10Min + average.healingDoneAvgPer10Min + 
               average.heroDamageDoneAvgPer10Min + average.objectiveKillsAvgPer10Min + 
               average.objectiveTimeAvgPer10Min + average.soloKillsAvgPer10Min + 
               best.barrierDamageDoneMostInGame + best.eliminationsMostInGame + 
               best.healingDoneMostInGame + best.meleeFinalBlowsMostInGame + 
               best.offensiveAssistsMostInGame + best.timeSpentOnFireMostInGame + 
               combat.objectiveKills + game.gamesLost + game.gamesTied + 
               games_played + game.timePlayed + matchAwards.cards + miscellaneous.teleporterPadsDestroyed + 
               assists.reconAssists + games_played + top_hero_type + average.allDamageDoneAvgPer10Min:top_hero_type + 
               average.barrierDamageDoneAvgPer10Min:top_hero_type + average.healingDoneAvgPer10Min:top_hero_type + 
               average.heroDamageDoneAvgPer10Min:top_hero_type + average.objectiveTimeAvgPer10Min:top_hero_type + 
               average.soloKillsAvgPer10Min:top_hero_type, data = clean_df)
summary(fit_bic)

library(faraway)
vif(fit_bic)

df <- read_csv("data/clean-data.csv")

pair_vars = c("best.meleeFinalBlowsMostInGame",
"average.objectiveTimeAvgPer10Min",
"average.allDamageDoneAvgPer10Min",
"average.objectiveKillsAvgPer10Min",
"games_played",
"game.gamesPlayed",
"skill_rating")
fit3 = lm(skill_rating ~ top_hero : . + ., 
          data = df %>% select(one_of(c(pair_vars, 'top_hero'))) %>% 
            select(-game.gamesPlayed ) %>% 
            mutate(top_hero = relevel(top_hero %>% as.factor(), ref='reinhardt'))
)
summary(fit3)

back_aic = step(fit3, direction = "backward")

fit4 = lm(skill_rating ~ top_hero + best.meleeFinalBlowsMostInGame + average.allDamageDoneAvgPer10Min + 
            average.objectiveKillsAvgPer10Min + games_played + top_hero:average.allDamageDoneAvgPer10Min, data = df)
summary(fit4)
