

# Load the Required Libraries
install.packages("shiny")
library(shiny)
library(dplyr)
library(ggplot2)


#Load the Data
df.football.transfer <- read.csv(file = 'data/football-transfers.csv')


#Create the User Interface
ui <- fluidPage(
  titlePanel("Football Transfers Descriptive Statistics"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Choose a season and a window"),
      selectInput("season", "Season:", unique(df.football.transfer$season)),
      selectInput("window", "Window:", unique(df.football.transfer$window))
    ),
    
    mainPanel(
      h4("Descriptive Statistics"),
      verbatimTextOutput("player_count"),
      verbatimTextOutput("market_value_mean"),
      verbatimTextOutput("average_age"),
      verbatimTextOutput("average_fee")
    )
  )
)



#Create the Server Function

server <- function(input, output) {
  
  season_data <- reactive({
    df.football.transfer %>% filter(season == input$season, window == input$window)
  })
  
  output$player_count <- renderPrint({
    paste("Number of Football Players:", nrow(season_data()))
  })
  
  output$market_value_mean <- renderPrint({
    paste("Average Market Value:", mean(season_data()$market_value))
  })
  
  output$average_age <- renderPrint({
    paste("Average Age:", mean(season_data()$age))
  })
  
  output$average_fee <- renderPrint({
    paste("Average Transfer Fee:", mean(season_data()$fee))
  })
}


#load the app 
shinyApp(ui, server)

