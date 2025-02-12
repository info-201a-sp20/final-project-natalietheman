# Loads necessary libraries
library(dplyr)
library(ggplot2)

# Reads the csv and turns it into a dataframe
df <- read.csv("data/life-satisfaction-vs-life-expectancy.csv",
               stringsAsFactors = FALSE)

# Creates a function used to generate a barplot chart 
chart1 <- function(df) {

  # Filters the initial dataframe to preserve only useful data
df_filter <- df %>%
  filter(!is.na(
Life.satisfaction..measured.from.lowest.0.to.highest.10.on.Cantril.Ladder.
)) %>%
  filter(!is.na(Life.expectancy..years.)) %>%
  select(-Total.population..Gapminder.)

# Generates the boxplot based on data
plot <- ggplot(data = df_filter, aes(group = Entity,
       x =
 Life.satisfaction..measured.from.lowest.0.to.highest.10.on.Cantril.Ladder.,
       y = Life.expectancy..years.)) +
       geom_boxplot() +
       labs(x = "Life Satisfaction",
            y = "Life Expectancy", title = "Ranges of Life Expectancy Per Life
            Satisfaction")
return(plot)
}