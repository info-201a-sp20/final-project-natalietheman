#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(leaflet)
library(stringr)
library(plotly)

# source("../return_sum_info.R", chdir = TRUE)
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


# page one
page_one <- tabPanel(
    "Project Overview",
    titlePanel("Project Overview"),
    mainPanel(
        h1("Questions"),
        p(strong("1. How does GDP per capita relate to happiness within the United States?")),
        a("GDP Link", href = "https://github.com/info-201a-sp20/final-project-natalietheman/blob/master/data/gdpPerState.csv"),
        p("The link above is referencing the data set that we used to explore the first question.
          This data set was obtained from the United States Bureau of Economic Analysis."),
        p(strong("2. How does the number of McDonald's relate to happiness within the United States?")),
        a("McDonalds Link", href = "https://github.com/info-201a-sp20/final-project-natalietheman/blob/master/data/McDonalds.csv"),
        p("The link above is referencing the data set that we used to explore the second question.
          This data set was obtained from a kaggle user that gathered the data himself."),
        p(strong("3. How does the life expectancy per country relate to the levels of happiness across the globe?")),
        a("Life expenctancy Link", href = "https://github.com/info-201a-sp20/final-project-natalietheman/blob/master/data/life-satisfaction-vs-life-expectancy.csv"),
        p("The link above is referencing the data set that we used to explore the third question.
          This data set was obtained from the World Happiness Report, European Commission, World Value
          Survey, and Pew Global Attitudes Survey.")
    )
)

page_two <- tabPanel(
    "GDP",
    titlePanel("GDP"),
    plotOutput(outputId = "gdpData")
)

page_three <- tabPanel(
    "McDonald's",
    titlePanel("McDonald's"),
    mainPanel(
      leafletOutput(outputId = "mcd_map")
    )
)

page_four <- tabPanel(
    "Life Expectancy",
    titlePanel("Life Expectancy"),
    plotlyOutput("life_slider")
    
)


page_five <- tabPanel(
    "pg5",
    titlePanel("page 5")
)



# Define UI for application that draws a histogram
# Multi-page layout??
ui <- navbarPage(
    "Happiness or smthing",
    page_one,
    page_two,
    page_three,
    page_four,
    page_five
)
# 
# 
#     # Application title
#     titlePanel("Happiness wooo"),
# 
#     # Sidebar with a slider input for number of bins
#     sidebarLayout(
#         sidebarPanel(
#             sliderInput("bins",
#                         "Number of bins:",
#                         min = 1,
#                         max = 50,
#                         value = 30)
#         ),
# 
#         # Show a plot of the generated distribution
#         mainPanel(
#            plotOutput("distPlot")
#         )
#     )
# )

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$gdpData <- renderTable({
        newData <- gdp_data %>% 
        newData
    })
    
    
    ####GET THESE MEANS DONE
    
    q1 <- as.numeric(gsub(",","",gdp_data$X2018Q1))
    q2 <- as.numeric(gsub(",","",gdp_data$X2018Q2))
    q3 <- as.numeric(gsub(",","",gdp_data$X2018Q3))
    q4 <- as.numeric(gsub(",","",gdp_data$X2018Q4))

    gdp <- q1 + q2 + q3 + q4
    gdp_data$sums <- gdp
    
    ######
    
    
    
    output$mcd_map <- renderLeaflet({
      
      leaflet(data = mcd) %>%
        addProviderTiles("Stamen.TonerLite") %>%
        addCircleMarkers(
          lat = ~lat,
          lng = ~lon,
          radius = 0.05,
          fillOpacity = 0.1,#color = "Reds"
        )
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
    
    
    # output$distPlot <- renderPlot({
    #     # generate bins based on input$bins from ui.R
    #     x    <- faithful[, 2]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # 
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white')
    # })
}

# Run the application 
shinyApp(ui = ui, server = server)

