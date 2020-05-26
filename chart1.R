library(dplyr)
library(ggplot2)


df <- read.csv("data/life-satisfaction-vs-life-expectancy.csv",
               stringsAsFactors = FALSE)
chart1 <- function(df) {
df <- rename(df, life_satis =
   Life.satisfaction..measured.from.lowest.0.to.highest.10.on.Cantril.Ladder.)

df_filter <- df %>%
  filter(!is.na(life_satis)) %>%
  filter(!is.na(Life.expectancy..years.)) %>%
  select(-Total.population..Gapminder.)

chart1 <- ggplot(data = df_filter, aes(group = Entity,
       x = life_satis,
       y = Life.expectancy..years.)) +
       geom_boxplot() +
       labs(x = "Life Satisfaction",
            y = "Life Expectancy")

return(chart1)
}
