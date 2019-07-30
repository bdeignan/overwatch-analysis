
library(readr)
# Seems to load with the correct type for time columns
all_heroes_data <- read_csv("data/all-heroes-data.csv")

fit = lm(skill_rating ~ . - rank, data = all_heroes_data)
summary(fit)

n = length(resid(fit))
back_bic = step(fit, direction = "backward", k = log(n))

