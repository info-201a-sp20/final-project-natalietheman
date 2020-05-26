library(stringr)
library(dplyr)

df <- read.csv("data/life-satisfaction-vs-life-expectancy.csv",
               stringsAsFactors = FALSE)

df_filter <- df %>%
filter(
!is.na
  (Life.satisfaction..measured.from.lowest.0.to.highest.10.on.Cantril.Ladder.)
) %>%
  filter(!is.na(Life.expectancy..years.)) %>%
  select(-Total.population..Gapminder.)

get_summary_info <- function(df) {
  length <- nrow(df)
  mean_le <- mean(df$Life.expectancy..years.)
  mean_ls <-
    mean(
  df$Life.satisfaction..measured.from.lowest.0.to.highest.10.on.Cantril.Ladder.)
  min_le <- min(df$Life.expectancy..years.)
  min_ls <-
    min(
  df$Life.satisfaction..measured.from.lowest.0.to.highest.10.on.Cantril.Ladder.)
  max_le <- max(df$Life.expectancy..years.)
  max_ls <-
    max(
  df$Life.satisfaction..measured.from.lowest.0.to.highest.10.on.Cantril.Ladder.)
  return(list(rows = length,
              life_exp_mean = round(mean_le, 2),
              life_sat_mean = round(mean_ls, 2),
              life_exp_min = round(min_le, 2),
              life_sat_min = round(min_ls, 2),
              life_exp_max = round(max_le, 2),
              life_sat_max = round(max_ls, 2))
  )
}

get_summary_info(df_filter)
