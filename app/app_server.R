library(shinythemes)
library(shiny)
library(dplyr)
library(leaflet)
library(stringr)
library(plotly)

data <- read.csv("data/data.csv", stringsAsFactors = FALSE)
gdp_data <- read.csv("data/gdpPerState.csv", stringsAsFactors = FALSE)
mcd <- read.csv("data/McDonalds.csv")

coords <- strsplit(as.character(mcd$geometry.coordinates), ",")
for (i in 1:length(coords)) {
  coords[[i]][1] <- str_sub(coords[[i]][1], 2, -1)
  coords[[i]][2] <- str_sub(coords[[i]][2], 1, -2)
}

lats <- c();
longs <- c();
for (i in 1:length(coords)) {
  longs <- c(longs, coords[[i]][1])
  lats <- c(lats, coords[[i]][2])
}

lats <- as.numeric(lats)
longs <- as.numeric(longs)

mcd <- mutate(mcd, lat = lats)
mcd <- mutate(mcd, lon = longs)

ls_vs_le <- read.csv("data/life-satisfaction-vs-life-expectancy.csv",
                     stringsAsFactors = FALSE
)
q1 <- as.numeric(gsub(",","",gdp_data$X2018Q1))
q2 <- as.numeric(gsub(",","",gdp_data$X2018Q2))
q3 <- as.numeric(gsub(",","",gdp_data$X2018Q3))
q4 <- as.numeric(gsub(",","",gdp_data$X2018Q4))

gdp <- q1 + q2 + q3 + q4
gdp_data$sums <- gdp

# page two
spongebob <- gdp_data %>% select(State, sums)
patrick <- data %>% select(State, totalScore)
squidward <- full_join(spongebob, patrick, by = "State")

states <- mcd %>%
  distinct(properties.subDivision) %>%
  arrange(properties.subDivision)
state_vec <- states$properties.subDivision

data$state_abb <- state.abb[match(data$State, state.name)]

state_group <- mcd %>%
  group_by(properties.subDivision)
mickey <- state_group %>%
  summarize(num_mcd = n())
colnames(mickey) <- c("state_abb", "num_mcd")

donald <- data %>%
  select(state_abb, totalScore)

goofy <- full_join(mickey, donald, by = "state_abb")
goofy <- na.omit(goofy)

server <- function(input, output) {
  
  
  
  
  output$stateGdp <- renderText({
    return(paste0("GDP: $", gdp_data %>% filter(State == input$gdpstates) %>%
                    pull(sums)))
  })
  
  output$stateRankGDP <- renderText({
    return(paste0("State GDP Rank: ", gdp_data %>%
                    arrange(-sums) %>% mutate(id = row_number()) %>%
                    filter(State == input$gdpstates) %>% pull(id)))
  })
  
  output$stateRankHappiness <- renderText({
    return(paste0("State Happiness Rank: ", data %>%
                    filter(State == input$gdpstates) %>% pull(overall)))
  })
  
  output$stateHappiness2 <- renderText({
    
  })
  
  output$numMcD <- renderText({
    return(paste0("# of McDonald's: ",
                  mcd %>% filter(properties.subDivision == input$states) %>%
                    tally()))
  })
  
  output$stateRank <- renderText({
    return(paste0("State Ranking: ",
                  data %>% filter(state_abb == input$states) %>% pull(overall)))
  })
  
  output$stateHappiness <- renderText({
    return(paste0("State Happiness Scores (based on various metrics): ",
                  data %>% filter(state_abb == input$states) %>% pull(totalScore),
                  "/100.00"))
  })
  
  output$ronald <- renderUI({
    
  })
  
  output$gdpData <- renderTable({
    newData <- gdp_data %>% 
      newData
  })
  
  output$gdpData <- renderPlotly({
    plot <- plot_ly(squidward, x = ~totalScore, y = ~sums, type = "scatter") %>%
      add_trace(text = squidward$State, hoverinfo = "text",showlegend = F) %>%
      layout(xaxis = list(title = "Happiness Score"),
             yaxis = list(title = "GDP"),
             title = "GDP per State vs. Happiness Score")
    plot
  })
  
  new_gdp <- gdp_data %>% 
    select(State, sums) %>% 
    arrange(State)
  
  new_data <- data %>% 
    select(State, totalScore) %>% 
    arrange(State)
  
  newTable <- full_join(new_gdp, new_data)
  
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
          "City:", mcd$properties.addressLine3)
        #color = "Reds"
      )
  })
  
  output$mcd_scatter <- renderPlotly({
    plot <- plot_ly(data = goofy, x = ~totalScore, y = ~num_mcd,
                    type = "scatter", mode = "markers") %>%
      add_trace(text = goofy$state_abb, hoverinfo = "text", showlegend = F) %>%
      layout(xaxis = list(title = "Happiness Score"),
             yaxis = list(title = "Number of McDonald's"),
             title = "Number of McDonald's per state vs. Happiness Score")
    plot
  })
  
  output$life_slider <- renderPlotly({
    df_filter <- ls_vs_le %>% #filter out na values and get rid of population
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
  
  output$life_ex_year <- renderText({
    ls_vs_le %>% filter(!is.na(Life.expectancy..years.)) %>%
      select(-Total.population..Gapminder.) %>% 
      group_by(Year) %>% 
      summarize(avg_life_exp = mean(Life.expectancy..years., na.rm = TRUE)) %>% 
      filter(Year == input$zoomies) %>% 
      pull(2) %>% 
      round(2)
  })
  
  output$life_expectancy <- renderPlot({
    # Filters the initial dataframe to preserve only useful data
    df_filter <- ls_vs_le %>%
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
    print(plot)
  })
}