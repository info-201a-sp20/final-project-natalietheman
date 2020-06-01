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
# source("../return_sum_info.R", chdir = TRUE)
data <- read.csv("data/data.csv", stringsAsFactors = FALSE)
gdp_data <- read.csv("data/gdpPerState.csv", stringsAsFactors = FALSE)


# page one
page_one <- tabPanel(
    "Project Overview",
    titlePanel("Project Overview"),
    mainPanel(
        h1("Questions"),
        p(strong("1. How does GDP per capita relate to happiness within the United States?")),
        a("gdp link", href = "https://github.com/info-201a-sp20/final-project-natalietheman/blob/master/data/gdpPerState.csv"),
        
        p(strong("2. How does the number of McDonald's relate to happiness within the United States?
")),
        a("mcd link", href = "https://github.com/info-201a-sp20/final-project-natalietheman/blob/master/data/McDonalds.csv"),
        
        p(strong("3. How does the life expectancy per country relate to the levels of happiness across the globe?")),
        a("lifeexp link", href = "https://github.com/info-201a-sp20/final-project-natalietheman/blob/master/data/life-satisfaction-vs-life-expectancy.csv")
    )
)

page_two <- tabPanel(
    "GDP",
    titlePanel("GDP"),
    plotOutput(outputId = "gdpData")
)

page_three <- tabPanel(
    "McDonald's",
    titlePanel("McDonald's")
)

page_four <- tabPanel(
    "Life Expectancy",
    titlePanel("Life Expectancy"),
    
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
        newData <- data %>% 
            select(State)
        newData
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
