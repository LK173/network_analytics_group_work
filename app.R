#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(DT)

# 2. LOAD DATASET ----

# Define the data's file path
filepath <- file.path("data", "football-transfers.csv")

# Read the CSV file to a dataframe
data <- read.csv(filepath)


# Compute descriptive statistical values
number.observations <- nrow(data)
number.countries <- length(unique(data$country))
number.clubs <- length(unique(data$club))
number.leagues <- length(unique(data$league))
avg.market.value <- format(mean(data$market_value, na.rm = TRUE), big.mark = ",")
std.market.value <- format(sd(data$market_value, na.rm = TRUE), big.mark = ",")
max.market.value <- format(max(data$market_value, na.rm = TRUE), big.mark = ",")
avg.fee <- format(mean(data$fee, na.rm = TRUE), big.mark = ",")
std.fee <- format(sd(data$fee, na.rm = TRUE), big.mark = ",")
column.names <- toString(names(data))

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Football Trade Network"),
  
  dashboardSidebar(
    selectInput(
      "country_filter",
      "Countries",
      choices = unique(data$country)
    )
  ),
  
  dashboardBody(
    # Create a tabset with three tabs
    tabsetPanel(
      tabPanel("Descriptive analysis", 
               fluidRow(
               valueBox(number.observations, "Observations", icon = icon("list")),
               valueBox(number.countries, "Countries", icon = icon("list")),
               valueBox(number.clubs, "Clubs", icon = icon("list")),
               valueBox(number.leagues, "Leagues", icon = icon("list")),
               valueBox(max.market.value, "Max. Market Value", icon = icon("list")),
               valueBox(avg.market.value, "Avg. Market Value", icon = icon("list")),
               valueBox(std.market.value, "Std. Market Value", icon = icon("list")),
               valueBox(avg.fee, "Avg. Transfer Fee", icon = icon("list")),
               valueBox(std.fee, "Std. Transfer Fee", icon = icon("list"))
               ),
               
               fluidRow(
                 column(12, offset = 0,
                   h2("Preview of the data and variables"),
                   dataTableOutput("data_preview")       
                 )
                 
               ),
      ),
      tabPanel("Network Exploration", 
               # Contents of second tab
      ),
      tabPanel("Network Analysis", 
               # Contents of third tab
      )
    )
  )
)

# Define server
server <- function(input, output) {
  
  output$data_preview = renderDataTable(data, options = list(scrollX = TRUE, pageLength = 5))
    
}

# Run the application 
shinyApp(ui = ui, server = server)
