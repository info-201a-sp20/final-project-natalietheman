# Load appropriate packages
library(shinythemes)
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

# page one (overview)
page_one <- tabPanel(
  "Project Overview",
  titlePanel("Project Overview"),
  mainPanel(
    h1("Purpose"),
    p("The purpose of this project is to analyze the various relationships
    between
      happiness and other characteristics in order to determine how to best
      improve
      society. Happiness is important as it allows people to have a
      motivation to continue
      forward and helps people function daily. Happiness also
      influences your health and
      combats stress."),
    h1("Questions"),
    p(strong("1. How does GDP per capita relate to happiness within
             the United States?")),
    p("The link below references the dataset that we used to explore
    the first question.
          This data set was obtained from the United States Bureau of
      Economic Analysis."),
    a("GDP Link", href =
"https://github.com/info-201a-sp20/final-project-natalietheman/blob/
master/data/gdpPerState.csv"),

    p(strong("2. How does the number of McDonald's relate to happiness
             within the United States?")),
    p("The link below references the data set that we used to explore
    the second question.
          This data set was obtained from a kaggle user that gathered
      the data himself."),
    a("McDonald's Link",
href = "https://github.com/info-201a-sp20/final-project-natalietheman/
blob/master/data/McDonalds.csv"),
    p(strong("3. How does the life expectancy per country relate to
             the levels of happiness across the globe?")),
    p("The link below references the data set that we used to explore
    the third question.
          This data set was obtained from the World Happiness Report,
          European Commission, World Value
          Survey, and Pew Global Attitudes Survey."),
    a("Life Expectancy Link",
href = "https://github.com/info-201a-sp20/final-project-natalietheman/
blob/master/data/life-satisfaction-vs-life-expectancy.csv")
  ),
  sidebarPanel(
    # Fun image representing happiness
    img(src = "office.gif", width = "250px")
  )
)

# remove commas from numbers to convert to numeric and calculate
# sum for each state
q1 <- as.numeric(gsub(",", "", gdp_data$X2018Q1))
q2 <- as.numeric(gsub(",", "", gdp_data$X2018Q2))
q3 <- as.numeric(gsub(",", "", gdp_data$X2018Q3))
q4 <- as.numeric(gsub(",", "", gdp_data$X2018Q4))

# Create column to represent cumulative gdp
gdp <- q1 + q2 + q3 + q4
gdp_data$sums <- gdp

# Gdp per state calculated
state_sums <- gdp_data %>% select(State, sums)
state_total <- data %>% select(State, totalScore)
gdp_state <- full_join(state_sums, state_total, by = "State")

# page two (GDP)
page_two <- tabPanel(
  "GDP",
  h1(id = "gdp-heading", "GDP"),
  tags$style(HTML("#gdp-heading{color: green;}")),
  tags$head(tags$style(
    HTML('
         #gdp-sidebar {
         background-color: #228B22;
         }
         body, label, input, button, select {
         font-family: "Arial";
         }')
  )),
  sidebarLayout(
    sidebarPanel(
      id = "gdp-sidebar",
      selectInput(
        inputId = "gdp_states",
        label = "Pick a state!",
        choices = state.name
      ),
      textOutput(outputId = "state_gdp"),
      textOutput(outputId = "state_rank_gdp"),
      textOutput(outputId = "state_rank_happiness")
    ),
    mainPanel(
      plotlyOutput(outputId = "gdp_data"),
      p("As you can see from the plot above, there's generally no
      correlation, but
      maybe an argueably weak positive correlation between happiness
      score and GDP.
      If you were to look at North Dakota and West Virginia, you can
      see the disparity
      of happiness score, yet note how small a difference exists in
      the GDP. This example
      seems to not be out of the ordinary as GDP doesn't seem to dictate
      happiness score much.")
    )
  )
)

# mcd df filter
states <- mcd %>%
  distinct(properties.subDivision) %>%
  arrange(properties.subDivision)
# create state vector
state_vec <- states$properties.subDivision

# add state abbreviation column
data$state_abb <- state.abb[match(data$State, state.name)]

# page three (McD's)
page_three <- tabPanel(
  "McDonald's",
  h1(id = "mcd-heading", "McDonald's"),
  tags$style(HTML("#mcd-heading{color: red;}")),
  tags$head(tags$style(
    HTML('
         #mcd-sidebar {
         background-color: #dec4de;
         }
         body, label, input, button, select {
         font-family: "Arial";
         }')
  )),
  sidebarLayout(
    sidebarPanel(
      id = "mcd-sidebar",
      selectInput(
        inputId = "states",
        label = "Pick a state!",
        choices = state_vec
      ),
      textOutput(outputId = "num_mcd"),
      textOutput(outputId = "state_rank"),
      textOutput(outputId = "state_happiness"),
      img(src = "dankronald.gif", width = "250px")
    ),
    mainPanel(
      leafletOutput(outputId = "mcd_map"),
      p(),
      plotlyOutput(outputId = "mcd_scatter"),
      p("As you can see by flipping through the various states,
      the number
      of McDonald's doesn't seem to have any correlation to the
      happiness levels
      in each state. A clear example of this is Utah and West
      Virginia. In addition,
      the scatterplot above demonstrates that there is no distinct
      positive or
      negative correlation, leading us to conclude that there
      is no relationship
      between the number of McDonald's and happiness levels.")
    )
  )
)

# page four (Life Expectancy)
page_four <- tabPanel(
  "Life Expectancy",
  h1(id = "life-heading", "Life Expectancy"),
  tags$style(HTML("#life-heading{color: blue;}")),
  numericInput(
    "life_exp_input",
    "Choose a Year to see the Life Expectancy!",
    1950,
    1950,
    2015,
    1
  ),
  p("Life Expectancy In the Above Year:"),
  textOutput("life_ex_year"),
  plotlyOutput("life_slider"),
  p("As the graph above demonstrates, the life expectancy
  has increased over
    the years. This can likely be attributed to medicinal
    advances and easier
    access to healthcare. As societies progress, life expectancy
    has increased
    which will likely lead to an increase in life satisfaction as well."),
  plotOutput(outputId = "life_expectancy"),
  p("As the graph above demonstrates, there is a noticeable
  trend that seems
    to cluster the life expectancy higher as life satisfaction
    also increases.
    There appears to be a positive correlation between life
    satisfaction and
    life expectancy from the graph. There are obviously outliers
    such as the boxplot
    near 5.075 which reaches the range of 70-74 years for a relatively
    low life
    satisfaction, however, this does not detract from the overall
    positive correlation.")
)

# page five (Summary)
page_five <- tabPanel(
  "Summary",
  titlePanel("Takeaways"),
  p("Looking at the data analysis present from the charts, the
  takeaways that can
      be derived from each question are explained below:"),
  tags$ul(
    tags$li("For the first question revolving around the relationship
    between GDP and life satisfaction,
    we were able to deduce that there is no real relationship
    between GDP and life satsifaction.
    There is a mild positive correlation between the two
    (very weak), which you could theoretically
    attribute to the relationship between financially successful
    states potentially having more resources,
    thus better accomodating their citizens. But this doesn't seem
    to be the case in general, as
    the relationship seems to be rather weak. For example, the GDP
    difference between North Dakota
    and West Virginia is 85,422 (which is rather small), however,
    their difference in life satisfaction is 32.2 (on a scale of 100)
    which shows how a massive disparity in life satisfaction has no real
    relationship to GDP.
    The implications of this is that it demonstrates how the more economically
    successful states
    aren't necessarily happier, demonstrating a lack of link between money
            and happiness."),
    tags$li("For the second question revolving around the relationship
    between the number
    of McDonald's and happiness, there appears to be no correlation.
    Despite what McDonald's
    may advertise, their happy meals do not seem to provide any
    extra happiness. To display
    this lack of relationship, we can take the example states of
    Utah and West Virginia, who are
    respectively the 2nd and 50th ranked states in terms of
    happiness. In Utah, the number of McDonald's
    is 116 whereas in West Virginia the number of McDonald's
    is 104, showing a difference of 12
    McDonald's restaurants. This disparity is relatively small
    and not large enough to show a relationship
    between the number of McDonald's and happiness levels. The implication
    of this insight
    is that more accessible fast food does not necessarily improve happiness,
    so happiness
    may be potentially more closely tied to quality of food instead of
            accessibility."),
    tags$li("For the third question revolving around the relationship
    between life expectancy
    and life satisfaction, we were able to deduce that as life expectancy
    increased,
    as did life satisfaction. We believe these two to be related as
    societies that
    help their citizens to live longer generally have better amenities
    and services,
    leading to a higher quality of life and thus higher life satisfaction.
    If we were to look
    at the boxplot near 5.075 in terms of life satisfaction, we can see that
    it has a life
    expectancy range near 70-74 years, which is an outlier but it does not
    detract from
    the overall positive linear trend of life satisfaction and life expectancy
    both increasing.
    The implication of this insight is that we can be derive that a healthier
    population is
    more likely to be happier as well, so to boost happiness we can provide
    easier access
    to healthcare or provide more healthcare options.")
  )
)

# reorganize mcd info
state_group <- mcd %>%
  group_by(properties.subDivision)
mickey <- state_group %>%
  summarize(num_mcd = n())
colnames(mickey) <- c("state_abb", "num_mcd")

# Choose columns
donald <- data %>%
  select(state_abb, totalScore)

# Joining columns together
mcd_total <- full_join(mickey, donald, by = "state_abb")
mcd_total <- na.omit(mcd_total)

ui <- navbarPage(
  theme = shinytheme("united"),
  "Happiness",
  page_one,
  page_two,
  page_three,
  page_four,
  page_five
)
