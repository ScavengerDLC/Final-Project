#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(plotly)
library(ggplot2)
library(lubridate)
library(shinyFeedback)
library(lubridate)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Election Data App"),
  
  # Sidebar with a slider input for number of bins 
    
    # Pannel
    mainPanel(
      tabsetPanel(
        ### HomePage
        tabPanel("Homepage", textOutput(outputId = "landing")),
        
        ### Historical Polling Averages
        tabPanel("Historical Polling Averages", plotOutput("votePlot"),
                 sliderInput("year_max",
                             "Upper Bound Year:",
                             min = 1968,
                             max = 2024,
                             value = 2024,
                             step = 4),
                 sliderInput("year_min",
                             "Lower Bound Year:",
                             min = 1968,
                             max = 2024,
                             value = 2016,
                             step = 4)),
        
        ### sentiment Over an Election Cycle Compared to Poll
        tabPanel("Sentiment vs Polls over An Election Cycle", 
                 plotOutput("pollTrend"),
                 plotOutput("sentimentTrend"),
                 sliderInput("year_election",
                             "Select Year:",
                             min = 1968,
                             max = 2024,
                             value = 2016,
                             step = 4))
                 )
      
      
    )
  )


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ### Homepage message
  output$landing <- renderText("Welcome to Chris Fierro's Election Data App (Coded in R).
                               There are various interactive graphs found in the various
                               tabs. Polling Data comes from 538 polling averages.
                               Sentiment is gathered from NYTimes articles that 
                               cover the candidates in each year. NYTimes search
                               API was used to find articles.")
  
  
  
  output$votePlot <- renderPlot({
    # Set Year bounds to filter for
    year_max <- input$year_max
    
    year_min <- input$year_min
    
    # Read CSV
    df <- read.csv("model_dataset.csv")
    
    # Filter based on min and max
    df <- df |> filter(year >= year_min & year <= year_max)
    
    # Make Chart
    df |> 
      ggplot(aes(x = pct_estimate, y = vote_share)) +
      geom_point() +
      geom_smooth(aes(color = "one"), method = lm, formula = y ~ x) +
      geom_line(aes(x=50, color = "two")) +
      geom_line(aes(y=50, color = "two")) +
      scale_color_manual(values = c("#6c008c", "#ba8900")) +
      labs(title = "Vote Estimate from Polls Compared to Actual Vote Results",
           subtitle = "2016-2024") +
      xlab("Estimated Percent of Vote") +
      ylab("Actual Percent of Vote") +
      theme(
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey70"),
        panel.grid.minor = element_line(color = "grey85"),
        legend.position = "none"
      )
  })
  
  output$pollTrend <- renderPlot({
    election_year <- input$year_election
    
    national <- read.csv("national_data.csv")
    
    national <- national |> mutate(date = ymd(date))
    
    national |>
      filter(year == election_year) |>
      group_by(party) |>
      ggplot() +
      geom_line(aes(x = date, y = pct_estimate, color = party)) +
      scale_color_manual(values = c("blue", "red")) +
      labs(title = "538 Election Estimate",
           subtitle = paste0("National Numbers, ", election_year)) +
      xlab("Date") +
      ylab("Estimated Candidate Support") +
      theme(
        panel.grid.major = element_line(color = "grey60"),
        panel.grid.minor = element_line(color = "grey75")
      )
  })
  
  output$sentimentTrend <- renderPlot({
    election_year <- input$year_election
    
    national <- read.csv("national_data.csv")
    
    national <- national |> mutate(date = ymd(date))
    
    national |>
      filter(year == election_year, !is.na(daily_bing)) |>
      group_by(party) |>
      ggplot() +
      geom_line(aes(x = date, y = daily_bing, color = party)) +
      scale_color_manual(values = c("blue", "red")) +
      labs(title = "NYTimes Average Bing Sentiment for Each Candidate",
           subtitle = paste0(election_year)) +
      xlab("Date") +
      ylab("Bing Sentiment") +
      theme(
        panel.grid.major = element_line(color = "grey60"),
        panel.grid.minor = element_line(color = "grey75")
      )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
