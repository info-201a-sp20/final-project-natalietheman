# Loads appropriate libraries
library(stringr)
library(dplyr)

# Turns the csv into a dataframe
df <- read.csv("data/life-satisfaction-vs-life-expectancy.csv",
               stringsAsFactors = FALSE)

# Filters the dataframe to have only useful information
df_filter <- df %>%
  filter(
  !is.na(
Life.satisfaction..measured.from.lowest.0.to.highest.10.on.Cantril.Ladder.)
  ) %>%
  filter(!is.na(Life.expectancy..years.)) %>%
  select(-Total.population..Gapminder.)

# Groups the dataframe by country
country_group <- df_filter %>%
  group_by(Entity)

# Calculates the average life expectancy based on country
avg_life_exp <- country_group %>%
summarize(avg_life_exp = round(mean(Life.expectancy..years., na.rm = TRUE), 2))

# Calculates the average life satisfaction based on country
avg_life_satisf <- country_group %>%
  summarize(avg_life_satisf = round(mean(
    Life.satisfaction..measured.from.lowest.0.to.highest.10.on.Cantril.Ladder.,
                                   na.rm = TRUE), 2))

# Combines the dataframes to create a dataframe consisting of both
avg <- merge(avg_life_exp, avg_life_satisf)
