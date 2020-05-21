library(dplyr)
library(plotly)

df <- read.csv("data/life-satisfaction-vs-life-expectancy.csv",
               stringsAsFactors = FALSE)

df_filter <- df %>%
  filter(!is.na(Life.expectancy..years.)) %>%
  select(-Total.population..Gapminder.)

country_group <- df_filter %>%
  group_by(Entity)

avg_life_exp <- country_group %>%
  summarize(avg_life_exp = mean(Life.expectancy..years., na.rm = TRUE))

avg_life_satisf <- country_group %>%
  summarize(avg_life_satisf =
mean(Life.satisfaction..measured.from.lowest.0.to.highest.10.on.Cantril.Ladder.,
    na.rm = TRUE))

avg <- merge(avg_life_exp, avg_life_satisf) %>%
  arrange(avg_life_exp)

year_group <- df_filter %>%
  group_by(Year)

life_exp_per_year <- year_group %>%
  summarize(avg_life_exp = mean(Life.expectancy..years.))

y <- list(title = "Average Life Expetancy")

plot_ly(life_exp_per_year,
  x = ~Year, y = ~avg_life_exp, mode = "line",
  text = ~ paste(
    "Year:", Year,
    "<br> Avg LE:", round(avg_life_exp, 2)
  ),
  hoverinfo = "text"
) %>%
  layout(yaxis = y)
