library(dplyr)
library(plotly)

df <- read.csv("data/life-satisfaction-vs-life-expectancy.csv",
  stringsAsFactors = FALSE
)


life_exp_overtime <- function(df) { #function
  df_filter <- df %>% #filter out na values and get rid of population
    filter(!is.na(Life.expectancy..years.)) %>%
    select(-Total.population..Gapminder.)

  # group by country
  country_group <- df_filter %>%
    group_by(Entity)

  # summary table of avg life expectancy of each country
  avg_life_exp <- country_group %>%
    summarize(avg_life_exp = mean(Life.expectancy..years., na.rm = TRUE))

  # summary table of avg life satisfaction of each country
  avg_life_satisf <- country_group %>%
    summarize(
      avg_life_satisf =
mean(Life.satisfaction..measured.from.lowest.0.to.highest.10.on.Cantril.Ladder.,
          na.rm = TRUE
        )
    )

  # joined table of two summary tables from above
  avg <- merge(avg_life_exp, avg_life_satisf) %>%
    arrange(avg_life_exp)

  # group by year
  year_group <- df_filter %>%
    group_by(Year)

  # summary table of life expectancy per year, all countries combined
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
    layout(yaxis = y, title = "Average Life Expectancy Over The Years")
}
