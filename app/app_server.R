# Load appropriate packages
library(shiny)
library(dplyr)
library(leaflet)
library(stringr)
library(plotly)

# Read data
data <- read.csv("data/data.csv", stringsAsFactors = FALSE)
gdp_data <- read.csv("data/gdpPerState.csv", stringsAsFactors = FALSE)
mcd <- read.csv("data/McDonalds.csv")
ls_vs_le <- read.csv("data/life-satisfaction-vs-life-expectancy.csv",
  stringsAsFactors = FALSE
)

# Get long and lat for McD data frame
coords <- strsplit(as.character(mcd$geometry.coordinates), ",")
for (i in 1:length(coords)) {
  coords[[i]][1] <- str_sub(coords[[i]][1], 2, -1)
  coords[[i]][2] <- str_sub(coords[[i]][2], 1, -2)
}

lats <- c()
longs <- c()
for (i in 1:length(coords)) {
  longs <- c(longs, coords[[i]][1])
  lats <- c(lats, coords[[i]][2])
}

lats <- as.numeric(lats)
longs <- as.numeric(longs)

mcd <- mutate(mcd, lat = lats)
mcd <- mutate(mcd, lon = longs)

# Get annual gdp
q1 <- as.numeric(gsub(",", "", gdp_data$X2018Q1))
q2 <- as.numeric(gsub(",", "", gdp_data$X2018Q2))
q3 <- as.numeric(gsub(",", "", gdp_data$X2018Q3))
q4 <- as.numeric(gsub(",", "", gdp_data$X2018Q4))

gdp <- q1 + q2 + q3 + q4
gdp_data$sums <- gdp

# Get state sums
state_sums <- gdp_data %>% select(State, sums)
state_total <- data %>% select(State, totalScore)
gdp_state <- full_join(state_sums, state_total, by = "State")

# mcd dataframe filter
states <- mcd %>%
  distinct(properties.subDivision) %>%
  arrange(properties.subDivision)

# Create state vector
state_vec <- states$properties.subDivision

# Add state abbreviation column
data$state_abb <- state.abb[match(data$State, state.name)]

# Creating new dataframe that contains only relevant data
state_group <- mcd %>%
  group_by(properties.subDivision)
mickey <- state_group %>%
  summarize(num_mcd = n())
colnames(mickey) <- c("state_abb", "num_mcd")

# Selecting relevant rows
donald <- data %>%
  select(state_abb, totalScore)

# Join tables
mcd_total <- full_join(mickey, donald, by = "state_abb")
mcd_total <- na.omit(mcd_total)

server <- function (input, output) {
  output$state_gdp <- renderText({
    return(paste0("GDP: $", gdp_data %>%
           filter(State == input$gdp_states) %>%
           pull(sums)))
  })
  # Render GDP rank
  output$state_rank_gdp <- renderText({
    return(paste0("State GDP Rank: ", gdp_data %>%
      arrange(-sums) %>%
      mutate(id = row_number()) %>%
      filter(State == input$gdp_states) %>%
        pull(id)))
  })
  # Render state happiness rank
  output$state_rank_happiness <- renderText({
    return(paste0("State Happiness Rank: ", data %>%
      filter(State == input$gdp_states) %>%
        pull(overall)))
  })
  # Render number of McDonald's
  output$num_mcd <- renderText({
    return(paste0(
      "# of McDonald's: ",
      mcd %>%
        filter(properties.subDivision == input$states) %>%
        tally()
    ))
  })
  # Render state ranking
  output$state_rank <- renderText({
    return(paste0(
      "State Ranking: ",
      data %>%
        filter(state_abb == input$states) %>%
        pull(overall)
    ))
  })
  # Render happiness score out of 100
  output$state_happiness <- renderText({
    return(paste0(
      "State Happiness Scores (based on various metrics): ",
      data %>%
        filter(state_abb == input$states) %>%
        pull(totalScore),
      "/100.00"
    ))
  })
  # Render gdp data table
  output$gdp_data <- renderTable({
    new_data <- gdp_data %>%
      new_data()
  })
  # Render plot for gdp vs happiness
  output$gdp_data <- renderPlotly ({
    plot <- plot_ly(gdp_state, x = ~totalScore,
                    y = ~sums, type = "scatter") %>%
      add_trace(text = gdp_state$State, hoverinfo = "text",
                showlegend = F) %>%
      layout(
        xaxis = list(title = "Happiness Score"),
        yaxis = list(title = "GDP"),
        title = "GDP per State vs. Happiness Score"
      )
    plot
  })
  # Tweak dataframe to arrange by state
  new_gdp <- gdp_data %>%
    select(State, sums) %>%
    arrange(State)
  # Tweak dataframe to arrange by state
  new_data <- data %>%
    select(State, totalScore) %>%
    arrange(State)
  # Join dataframes
  newTable <- full_join(new_gdp, new_data)
  # Create mcdonald map
  output$mcd_map <- renderLeaflet({
    temp <- mcd %>%
      filter(properties.subDivision == input$states)

    leaflet(data = temp) %>%
      addProviderTiles("Stamen.TonerLite") %>%
      addCircleMarkers(
        lat = ~lat,
        lng = ~lon,
        radius = 0.05,
        fillOpacity = 0.1,
        popup = paste(
          "City:", mcd$properties.addressLine3
        )
      )
  })
  # Create mcdonald scatterplot
  output$mcd_scatter <- renderPlotly({
    plot <- plot_ly(
      data = mcd_total, x = ~totalScore, y = ~num_mcd,
      type = "scatter", mode = "markers"
    ) %>%
      add_trace(text = mcd_total$state_abb, hoverinfo = "text",
                showlegend = F) %>%
      layout(
        xaxis = list(title = "Happiness Score"),
        yaxis = list(title = "Number of McDonald's"),
        title = "Number of McDonald's per state vs. Happiness Score"
      )
    plot
  })
  # Create life slider
  output$life_slider <- renderPlotly({
    df_filter <- ls_vs_le %>% # filter out na values and get rid of population
      filter(!is.na(Life.expectancy..years.)) %>%
      select(-Total.population..Gapminder.)
    # group by country
    country_group <- df_filter %>%
      group_by(Entity)
    # summary table of avg life expectancy of each country
    avg_life_exp <- country_group %>%
      summarize(avg_life_exp = mean(Life.expectancy..years., na.rm = TRUE))
    # group by year
    year_group <- df_filter %>%
      group_by(Year)
    # summary table of life expectancy per year, all countries combined
    life_exp_per_year <- year_group %>%
      summarize(avg_life_exp = mean(Life.expectancy..years.))
    y <- list(title = "Average Life Expectancy")
    plot_ly(life_exp_per_year,
      x = ~Year, y = ~avg_life_exp, mode = "line",
      text = ~ paste(
        "Year:", Year,
        "<br> Avg LE:", round(avg_life_exp, 2)
      ),
      hoverinfo = "text"
    ) %>%
      layout(yaxis = y, title = "Average Life Expectancy Over The Years")
  })
  # Generate life text
  output$life_ex_year <- renderText({
    ls_vs_le %>%
    filter(!is.na(Life.expectancy..years.)) %>%
    select(-Total.population..Gapminder.) %>%
    group_by(Year) %>%
    summarize(avg_life_exp = mean(Life.expectancy..years., na.rm = TRUE)) %>%
    filter(Year == input$life_exp_input) %>%
    pull(2) %>%
    round(2)
  })
  # Create life expectancy plot
  output$life_expectancy <- renderPlot({
    # Filters the initial dataframe to preserve only useful data
    df_filter <- ls_vs_le %>%
      filter(!is.na(
Life.satisfaction..measured.from.lowest.0.to.highest.10.on.Cantril.Ladder.
      )) %>%
      filter(!is.na(Life.expectancy..years.)) %>%
      select(-Total.population..Gapminder.)

    # Generates the boxplot based on data
    plot <- ggplot(data = df_filter, aes(
      group = Entity,
      x =
Life.satisfaction..measured.from.lowest.0.to.highest.10.on.Cantril.Ladder.,
      y = Life.expectancy..years.
    )) +
      geom_boxplot() +
      labs(
        x = "Life Satisfaction",
        y = "Life Expectancy", title = "Ranges of Life Expectancy Per Life
            Satisfaction"
      )
    print(plot)
  })
}
