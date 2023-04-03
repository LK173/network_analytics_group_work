library(shiny)
library(dplyr)
library(shinythemes)
library(ggplot2)
library(plotly)


# LOAD DATA ----

# Define the data's file path
#filepath <- file.path("data", "football-transfers.csv")

# Read the CSV file to a dataframe
data <- read.csv(file = '../data/football-transfers.csv')


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
  
  
  # 3. NETWORK EXPLORATION PAGE ----
  navbarMenu(title = "Network Exploration",
             # 3.1 Subpage
             tabPanel(title = "Subpage 3.1",
                      fluidRow(
                        column(4,
                               wellPanel(
                                 h3("Filter the dataset"),
                                 selectInput("season_3_1", "Select a season:",
                                             choices = 2004:2021, selected = 2021, width = "100%"),
                                 uiOutput("country_ui_3_1"),
                                 uiOutput("position_ui_3_1")
                               )),
                        column(8,
                               wellPanel(
                                 h3("Summary Statistics"),
                                 plotlyOutput("map")
                               )
                        )
                      )
             ),
             
             # 3.2 Subpage
             tabPanel(title = "Subpage 3.2", "This is the second subpage of Item 3")
             
  ),
  
  
  # NETWORK ANALYSIS PAGE ----
  navbarMenu(title = "Network Analysis",
             tabPanel(title = "Subpage 4.1", "This is the first subpage of Item 4"),
             tabPanel(title = "Subpage 4.2", "This is the second subpage of Item 4")
  )
  
  
  
  # End of UI      
)


# SERVER SECTION ----
server <- function(input, output) {
  # 3.1 World Map
  # filter the data based on the slider input
  
  selected.club_3_1 <- reactiveVal()
  graph_selected_club_3_1 <- reactiveVal()
  
  season.data_3_1 <- reactive({
    data %>%
      filter(season == input$season_3_1) %>%
      filter(country == input$country) %>%
      filter(if (input$position != "All") position_category == input$position else TRUE)
  })
  
  # Reactive function to update country choices based on selected season
  country.choices_3_1 <- reactive({
    data %>%
      filter(season == input$season_3_1) %>%
      distinct(country) %>%
      pull(country)
  })
  
  # Update the country input based on the selected season
  output$country_ui_3_1 <- renderUI({
    country.filtered_3_1 <- data %>% filter(season == input$season_3_1) %>% distinct(country) %>% pull(country)
    selectInput("country", "Country:", choices = country.filtered_3_1)
  })
  
  output$position_ui_3_1 <- renderUI({
    req(input$country) # Ensure input$country is available before proceeding
    position.filtered_3_1 <- data %>%
      filter(season == input$season_3_1, country == input$country) %>%
      distinct(position_category) %>%
      pull(position_category)
    selectInput("position", "Position:", choices = c("All", position.filtered_3_1))
  })
  
  output$map <- renderPlotly({
    world_map <- ggplot() +
      borders("world", colour = "lightgrey", fill = "white") +
      coord_equal(xlim = c(-180, 180), ylim = c(-90, 90))
    
    p <- world_map
    
    # Count the number of times each edge exists
    edge_counts <- season.data_3_1() %>%
      group_by(dealing_country_lat, dealing_country_lon, country_lat, country_lon) %>%
      summarise(weight = n())
    
    # Add arrows to the map for each directed edge in the dataset with color based on the weight
    p <- p + geom_segment(aes(x = dealing_country_lon, y = dealing_country_lat, xend = country_lon, yend = country_lat, color = weight), data = edge_counts, arrow = arrow(length = unit(0.2, "cm"))) +
      scale_color_gradient(low = "blue", high = "red") # Set the color gradient
    
    p <- p + geom_point(aes(x = country_lon, y = country_lat, label = country), data = season.data_3_1(), color = "green", size = 2)
    
    # Add points for each node in the dataset
    p <- p + geom_point(aes(x = country_lon, y = country_lat, label = country), data = season.data_3_1(), color = "red", size = 2)
    
    # Customize the appearance of the plot
    p <- p + theme_void() + theme(legend.position = "none")
    
    # Show the plot
    ggplotly(p)
  })
  
  # End of server    
}

# Run the application 
shinyApp(ui = ui, server = server)
