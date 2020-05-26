library(stringr)
library(dplyr)

df <- read.csv("data/life-satisfaction-vs-life-expectancy.csv",
               stringsAsFactors = FALSE)

df_filter <- df %>%
  filter(
  !is.na(
Life.satisfaction..measured.from.lowest.0.to.highest.10.on.Cantril.Ladder.)
  ) %>%
  filter(!is.na(Life.expectancy..years.)) %>%
  select(-Total.population..Gapminder.)

country_group <- df_filter %>%
  group_by(Entity)

avg_life_exp <- country_group %>%
summarize(avg_life_exp = round(mean(Life.expectancy..years., na.rm = TRUE), 2))

avg_life_satisf <- country_group %>%
  summarize(avg_life_satisf = round(mean(
    Life.satisfaction..measured.from.lowest.0.to.highest.10.on.Cantril.Ladder.,
                                   na.rm = TRUE), 2))

avg <- merge(avg_life_exp, avg_life_satisf)
