library(dplyr)

df <- read.csv("data/life-satisfaction-vs-life-expectancy.csv",
               stringsAsFactors = FALSE)
country_group <- df %>%
  group_by(Entity)

avg_life_exp <- country_group %>%
  summarize(avg_life_exp = mean(Life.expectancy..years., na.rm = TRUE))

avg_life_satisf <- country_group %>%
  summarize(avg_life_satisf = mean(Life.satisfaction..measured.from.lowest.0.to.highest.10.on.Cantril.Ladder.,
                                   na.rm=TRUE))