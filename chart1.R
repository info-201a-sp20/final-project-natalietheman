library(stringr)
library(dplyr)
library(ggplot2)
library(plotly)

df <- read.csv("data/life-satisfaction-vs-life-expectancy.csv", stringsAsFactors = FALSE)

df_filter <- df %>% 
  filter(!is.na(Life.satisfaction..measured.from.lowest.0.to.highest.10.on.Cantril.Ladder.)) %>% 
  filter(!is.na(Life.expectancy..years.)) %>% 
  select(-Total.population..Gapminder.) 

country_group <- df_filter %>%
  group_by(Entity)

chart1 <- ggplot(data = df_filter, aes(group = Entity, 
                 x = Life.satisfaction..measured.from.lowest.0.to.highest.10.on.Cantril.Ladder.,
                 y = Life.expectancy..years.)) +
       geom_boxplot() +
       labs(x = "Life Satisfaction",
            y = "Life Expectancy") 
chart1

