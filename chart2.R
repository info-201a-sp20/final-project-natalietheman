library(dplyr)
library(plotly)
library(styler)

# read data file
df <- read.csv("data/life-satisfaction-vs-life-expectancy.csv",
               stringsAsFactors = FALSE)

# create function to create scatterplot chart
chart2 <- function(df) {
  
  # filter dataframe, remove NA values from life exp and life satisfaction
df_filter <- df %>%
  filter(!is.na(
    Life.satisfaction..measured.from.lowest.0.to.highest.10.on.Cantril.Ladder.)
    ) %>%
  filter(!is.na(Life.expectancy..years.)) %>%
  select(-Total.population..Gapminder.)

# group data by entity
country_group <- df_filter %>%
  group_by(Entity)

# get average life expectancy for each entity
avg_life_exp <- country_group %>%
  summarize(avg_life_exp = mean(Life.expectancy..years., na.rm = TRUE))

# get average life satisfaction for each entity
avg_life_satisf <- country_group %>%
summarize(
avg_life_satisf =
mean(Life.satisfaction..measured.from.lowest.0.to.highest.10.on.Cantril.Ladder.,
    na.rm = TRUE))

# merge together to form one df
avg <- merge(avg_life_exp, avg_life_satisf)

# plot scatterplot graph
y <- list(title = "Average Life Satisfaction")
x <- list(title = "Average Life Expectancy")

plot_ly(avg,
                   x = ~avg_life_exp,
                   y = ~avg_life_satisf,
                   color = ~Entity) %>%
  add_trace(text = ~Entity) %>%
  layout(title = "Avg Life Expectancy vs. Avg Life Satisfaction",
         xaxis = x, yaxis = y)
}
