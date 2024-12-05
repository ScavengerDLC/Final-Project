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
library(lubridate)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Estimated Vote Share Vs. Actual Vote Share"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("year_max",
                  "Upper Bound Year:",
                  min = 1968,
                  max = 2024,
                  value = 2024),
      sliderInput("year_min",
                  "Lower Bound Year:",
                  min = 1968,
                  max = 2024,
                  value = 2016)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("votePlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  output$votePlot <- renderPlotly({
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
}

# Run the application 
shinyApp(ui = ui, server = server)
