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
library(shinythemes)
library(ggplot2)
library(igraph)
library(visNetwork)
library(ggmap)
library(RgoogleMaps)


# set the bounding box for the map
bbox <- c(-180, -60, 180, 80)

# retrieve the map
map <- get_stamenmap(bbox, zoom = 2, maptype = "toner-lite")

# LOAD DATA ----

# NEW LOAD DATA

df.football.transfer <- read.csv(file = '../data/football-transfers.csv')

data <- df.football.transfer


# PREDEFINED VARIABLES ----

# Create the variable overview
variable_names <- c("season", "window", "country", "country_lat", "country_lon",
                    "league", "club", "movement", "dealing_club", "dealing_country",
                    "dealing_country_lat", "dealing_country_lon", "name", "age",
                    "nationality", "position", "market_value", "fee")

variable_desc <- c("The year of the season in which the trade took place.",
                   "The season of the year in which the trade took place.", 
                   "The country to which the player was transferred.", 
                   "The receiving country's latitude.", 
                   "The receiving country's longitude.", 
                   "The league to which the player was transferred.", 
                   "The club to which the player was transferred.", 
                   "The direction of the transfer record entry (in/out).", 
                   "The club from which the player was transferred.", 
                   "The country from which the player was transferred.", 
                   "The dealing country's latitude.", 
                   "The dealing country's longitude",
                   "The name of the player that was transferred.",
                   "The age of the player that was transferred.",
                   "The nationality of the player that was transferred.",
                   "The position of the player that was transferred.",
                   "The market value of the player that was transferred",
                   "The transfer fee of the player that was transferred")

dt.available.vars <- data.frame(Variable = variable_names, Description = variable_desc)

# Define descriptive statistics
number.observations <- nrow(data)
number.countries <- length(unique(data$country))
number.clubs <- length(unique(data$club))
number.leagues <- length(unique(data$league))
avg.market.value <- format(mean(data$market_value, na.rm = TRUE), big.mark = ",", scientific = FALSE)
std.market.value <- format(sd(data$market_value, na.rm = TRUE), big.mark = ",", scientific = FALSE)
max.market.value <- format(max(data$market_value, na.rm = TRUE), big.mark = ",", scientific = FALSE)
min.market.value <- format(min(data$market_value, na.rm = TRUE), big.mark = ",", scientific = FALSE)
avg.fee <- format(mean(data$fee, na.rm = TRUE), big.mark = ",", scientific = FALSE)
std.fee <- format(sd(data$fee, na.rm = TRUE), big.mark = ",", scientific = FALSE)
max.fee <- format(max(data$fee, na.rm = TRUE), big.mark = ",", scientific = FALSE)
min.fee <- format(min(data$fee, na.rm = TRUE), big.mark = ",", scientific = FALSE)


# Create data frame for calculations
dt.summary.table <- data.frame(
  "Metric" = c(
    "Number of Observations",
    "Number of Countries",
    "Number of Clubs",
    "Number of Leagues",
    "Average Market Value",
    "Standard Deviation of Market Value",
    "Maximum Market Value",
    "Minimum Market Value",
    "Average Fee",
    "Standard Deviation of Fee",
    "Maximum Fee",
    "Minimum Fee"
  ),
  "Value" = c(
    number.observations,
    number.countries,
    number.clubs,
    number.leagues,
    avg.market.value,
    std.market.value,
    max.market.value,
    min.market.value,
    avg.fee,
    std.fee,
    max.fee,
    min.fee
  )
)


# UI SECTION ----
ui <- navbarPage(
  title = "Football Talent Scout Europe",
  theme = shinytheme("cosmo"),
  
  
  # 1. INTRODUCTION PAGE ----
  tabPanel(title = "Introduction", 
           fluidRow(
             column(12,
                    h2("Welcome to our Football Talent Scout App!"),
                    p("This is our introductory page where we present you the business
                       value of this app.")
             )
           )
  ),
  
  # 2. DESCRIPTIVE STATISTICS PAGE ----
  navbarMenu(title = "Descriptive Statistics",
             
             # 2.1 EXPLORING THE DATASET
             tabPanel(title = "Exploring the dataset",
                      
                      fluidRow(
                        column(12,
                               h2("Welcome to our Football Talent Scout App!"),
                               p("In this section we will provide an overview
                                 and description of the variables that are 
                                 available in the dataset as well as a preview 
                                 of the raw dataset."),
                               p("For further information about the dataset
                                 please refer to this <LINK> Website"),
                               br()
                        )
                      ),
                      
                      wellPanel(
                        # 2.1.1 Table with variables
                        wellPanel(
                          h3("Available Variables in the dataset"),
                          tableOutput("available.vars")
                        ),
                        # 2.1.2 Preview of raw data
                        wellPanel(
                          h3("Preview of the raw dataset"),
                          dataTableOutput("data.preview")
                        )
                      )
                      
             ),
             
             # 2.2 GENERAL STATISTICS
             tabPanel(title = "Summary Statistics", 
                      fluidRow(
                        column(12,
                               h2("General Statistics"),
                               p("In this section we will provide general statistics 
                                 on the data by using the most relevant metrics."),
                               br()
                        )
                      ),
                      # 2.2.1 Summary Statistics
                      wellPanel(
                        
                        wellPanel(
                          fluidRow(
                            column(6,
                                   h3("Filter the dataset"),
                                   selectInput("summary.f1", "Countries", choices = c("Select All", unique(data$country))),
                                   sliderInput("season.statistics", "Season", min = min(data$season), max = max(data$season), value = c(min(data$season), max(data$season)), step = 1)
                            ),
                            column(6,
                                   h3("Summary Statistics"),
                                   tableOutput("summary.table")
                            )
                          )
                        ),
                        
                        wellPanel(
                          fluidRow(
                            column(12,
                                   h3("Transfers per Country"),
                                   plotOutput("transfers.per.country")
                            )
                          ),
                          fluidRow(
                            column(12,
                                   h3("Avg. Market Value per Country"),
                                   plotOutput("avg.per.country")
                            )
                          )
                        )
                        
                      )
             )
  ),
  
  
  # 3. NETWORK EXPLORATION PAGE ----
  navbarMenu(title = "Network Exploration",
             # 3.1 Subpage
             tabPanel(title = "Subpage 3.1", 
                      fluidRow(
                        column(4,
                               wellPanel(
                                 h3("Filter the dataset"),
                               ),
                        ),
                        column(8,
                               # 2.1.2 Preview on raw data
                               wellPanel(
                                 h3("Summary Statistics"),
                               )
                        )
                      )
             ),

             # TEST TEST TEST TEST
             # 3.2 Subpage
             tabPanel(title = "Subpage 3.2 Details about the whole football Transfer Network")

  
  # NETWORK ANALYSIS PAGE ----
  navbarMenu(title = "Network Analysis",
             tabPanel(title = "Subpage 4.1", "This is the first subpage of Item 4"),
             tabPanel(title = "Subpage 4.2", "This is the second subpage of Item 4")
  )
  

  
# End of UI      
)


# SERVER SECTION ----
server <- function(input, output) {
  
  # 2. DESCRIPTIVE STATISTICS
  # 2.1.1 Table with available variables
  output$available.vars <- renderTable({dt.available.vars})
  
  # 2.1.2 Data Preview
  output$data.preview = renderDataTable(head(data, 100), options = list(scrollX = TRUE, pageLength = 5, searching = FALSE))  
  
  # 2.2.1 Summary statistics table
  output$summary.table <- renderTable({
    dt.summary.table
  })
  
  # 2.2.2 Distribution: Transfers per Country
  dt.season.statistics <- reactive({
    data %>%
      filter(season >= input$season.statistics[1] & season <= input$season.statistics[2])
  })
  
  transfers.per.country <- reactive({
    dt.season.statistics() %>% 
      group_by(country) %>% 
      summarise(n = n()) %>% 
      ggplot(aes(x = country, y = n)) + 
      geom_bar(stat = "identity", fill = "blue", color = "white") +
      labs(x = "Country", y = "Number of Transfers") +
      theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1))
  })
  output$transfers.per.country <- renderPlot({
    transfers.per.country()
  })
  
  # 2.2.2 Distribution: Avg. Market Value per Country
  avg.per.country <- reactive({
    dt.season.statistics() %>% 
      group_by(country) %>% 
      summarise(avg_market_value = mean(market_value, na.rm = TRUE)) %>% 
      ggplot(aes(x = country, y = avg_market_value)) + 
      geom_bar(stat = "identity", fill = "blue", color = "white") +
      labs(x = "Country", y = "Average Market Value (in Millions)") +
      theme(axis.text.x = element_text(size =12, angle = 45, hjust = 1))
  })
  output$avg.per.country <- renderPlot({
    avg.per.country()
  })

  # 3. NETWORK EXPLORATION
  # 3.2 Directed Graph

  
  
# End of server    
}

# Run the application 
shinyApp(ui = ui, server = server)
