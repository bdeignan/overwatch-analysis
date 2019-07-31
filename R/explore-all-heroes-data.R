library(readr)

clean_df <- read_csv("data/clean-data.csv")

fit = lm(skill_rating ~ . - rank, data = all_heroes_data)
summary(fit)

n = length(resid(fit))
back_bic = step(fit, direction = "backward", k = log(n))

