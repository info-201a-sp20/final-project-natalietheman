library(stringr)
library(dplyr)
library(ggplot2)
install.packages("plotly")
library("plotly")

df <- read.csv("data/life-satisfaction-vs-life-expectancy.csv", stringsAsFactors = FALSE)

df_filter <- df %>% 
  filter(!is.na(Life.satisfaction..measured.from.lowest.0.to.highest.10.on.Cantril.Ladder.)) %>% 
  filter(!is.na(Life.expectancy..years.)) %>% 
  select(-Total.population..Gapminder.)

country_group <- df_filter %>%
  group_by(Entity)

avg_life_exp <- country_group %>%
  summarize(avg_life_exp = mean(Life.expectancy..years., na.rm = TRUE))

avg_life_satisf <- country_group %>%
  summarize(avg_life_satisf = mean(Life.satisfaction..measured.from.lowest.0.to.highest.10.on.Cantril.Ladder.,
                                   na.rm = TRUE))

avg <- merge(avg_life_exp, avg_life_satisf)

y <- list(title = "Average Life Satisfaction")
x <- list(title= "Average Life Expectancy")

plot_ly(avg, x = ~avg_life_exp, y = ~avg_life_satisf, color = ~Entity) %>%
  add_trace(text = ~Entity) %>% 
  layout(xaxis = x, yaxis = y)


