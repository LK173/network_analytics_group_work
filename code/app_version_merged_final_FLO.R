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


# set the bounding box for the map----------------------------------------------
bbox <- c(-180, -60, 180, 80)

# retrieve the map
map <- get_stamenmap(bbox, zoom = 2, maptype = "toner-lite")

# LOAD DATA---------------------------------------------------------------------

# NEW LOAD DATA

df.football.transfer <- read.csv(file = '../data/football-transfers.csv')

data <- df.football.transfer


# PREDEFINED VARIABLES----------------------------------------------------------

# Create the variable overview
variable.names <- c("season", "window", "country", "country_lat", "country_lon",
                    "league", "club", "movement", "dealing_club", 
                    "dealing_country", "dealing_country_lat", 
                    "dealing_country_lon", "name", "age",
                    "nationality", "position", "market_value", "fee")

variable.desc <- c("The year of the season in which the trade took place.",
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

df.available.vars <- data.frame(Variable = variable.names, 
                                Description = variable.desc)

# Define descriptive statistics
number.observations <- nrow(data)
number.countries <- length(unique(data$country))
number.clubs <- length(unique(data$club))
number.leagues <- length(unique(data$league))
avg.market.value <- format(mean(data$market_value, na.rm = TRUE), 
                           big.mark = ",", scientific = FALSE)
std.market.value <- format(sd(data$market_value, na.rm = TRUE), 
                           big.mark = ",", scientific = FALSE)
max.market.value <- format(max(data$market_value, na.rm = TRUE), 
                           big.mark = ",", scientific = FALSE)
min.market.value <- format(min(data$market_value, na.rm = TRUE), 
                           big.mark = ",", scientific = FALSE)
avg.fee <- format(mean(data$fee, na.rm = TRUE), big.mark = ",", 
                  scientific = FALSE)
std.fee <- format(sd(data$fee, na.rm = TRUE), big.mark = ",", 
                  scientific = FALSE)
max.fee <- format(max(data$fee, na.rm = TRUE), big.mark = ",", 
                  scientific = FALSE)
min.fee <- format(min(data$fee, na.rm = TRUE), big.mark = ",", 
                  scientific = FALSE)


# Create data frame for calculations
df.summary.table <- data.frame(
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


# UI SECTION -------------------------------------------------------------------
ui <- navbarPage(
  title = "Football Talent Scout International",
  theme = shinytheme("cosmo"),
  
  
  # 1. INTRODUCTION PAGE -------------------------------------------------------
  tabPanel(title = "Introduction", 
           fluidRow(
             column(1),
             column(10, 
                    h2("Welcome to Football Talent Scout International!"),
                    tags$br(),
                    p("Welcome to Football Talent Scout International, your 
                    go-to app for global football scouting."),
                    p("Discover comprehensive data on past football transfers, 
                    including performance, market value, transfer fees, and club 
                    information."),
                    p("Our app offers tailored features for Football Talent 
                    Scouts. The Descriptive Stats tab presents an overview of 
                    the data set, with filters such as season, year, and 
                    country. The Network Exploration tab showcases an 
                    interactive map, illustrating football transfer trends over 
                    time. The Network Analysis tab allows users to delve into 
                    the network and predict potential trades based on historical 
                    data."),
                    p("Benefit from valuable insights to help identify top 
                    talent using customizable metrics. Although our network 
                    analysis uses a reduced data set and isn't updated 
                    automatically, we're committed to continuously improving our 
                    app and providing the latest information."),
                    p("Football Talent Scout International is designed for a 
                    seamless user experience, catering to both veteran scouts 
                    and newcomers. Gain a competitive edge with our 
                    cutting-edge app."),
                    p("Thank you for choosing Football Talent Scout 
                    International. We're here to help you uncover the next 
                    generation of football prodigies!"),
                    tags$br(),
                    img(src = "football-stadium-unsplash.jpg", height = "auto", 
                        width = "100%", style = "display: block; margin-left: 
                        auto; margin-right: auto;"),
                    p("Image source: Unsplash (open-source)")
             ),
             column(1)
           )
  ),
  
  # 2. DESCRIPTIVE STATISTICS PAGE ---------------------------------------------
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
                               p("In this section we will provide general 
                               statistics on the data by using the most relevant
                                 metrics."),
                               br()
                        )
                      ),
                      # 2.2.1 Summary Statistics
                      wellPanel(
                        wellPanel(
                          fluidRow(
                            column(6,
                                   h3("Filter the dataset"),
                                   sliderInput("season.statistics", 
                                               "Season", min = min(data$season),
                                               max = max(data$season), 
                                               value = c(min(data$season), 
                                                         max(data$season)), 
                                               step = 1, sep =""),
                                   selectInput("summary.f1", "Countries", 
                                               choices = c("Select All", 
                                                           unique(data$country))
                                               )
                            ),
                            column(6,
                                   style = "border-left: 1px solid #999; 
                                   padding-left: 20px; padding-right: 20px;",
                                   h3("Summary Statistics"),
                                   tableOutput("summary.table")
                            )
                          )
                        ),
                        
                        wellPanel(
                          fluidRow(
                            column(12,
                                   uiOutput("transfers_title"),
                                   plotOutput("top.transfers")
                            )
                          ),
                          fluidRow(
                            column(12,
                                   uiOutput("market_value_title"),
                                   plotOutput("top.market.value")
                            )
                          )
                        )
                        
                      )
             )
  ),
  
  
  # 3. NETWORK EXPLORATION PAGE ------------------------------------------------
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

             # 3.2 Subpage
             tabPanel(title = "Subpage 3.2 Details about the whole football 
                      Transfer Network",
                      fluidPage(
                        titlePanel("Overview of the Football Transfers Network 
                                   by Country"),
                        
                        wellPanel(
                          wellPanel(
                            fluidRow(
                              column(6,
                                     style = "border-right: 1px solid #999; 
                                     padding-left: 20px; padding-right: 20px;",
                                     h3("Choose a season, country, club, 
                                        position"),
                                     selectInput("season_3_2", 
                                                 "Select a season:",
                                                 choices = 2004:2021, 
                                                 selected = 2021, 
                                                 width = "100%"),
                                     uiOutput("country_ui_3_2"),
                                     uiOutput("position_ui_3_2"),
                                     tags$br(),
                                     tags$br(),
                                     h4("Explanations for the Usage of the 
                                        Dashboard"),
                                     tags$p("This page helps to get an overview 
                                            of transfers by countries."),
                                     tags$br(),
                                     tags$p("Use the following filters:"),
                                     tags$ul(
                                       tags$li("Use filters on the left to 
                                               select a season/year and position
                                               to view the transfer network of 
                                               the country."),
                                       tags$li("Highlight and filter a node by 
                                                clicking on 
                                                one in the interactive graph to 
                                                see connections and centrality 
                                                measures of these node."),
                                       tags$li("Zoom into the graph to see 
                                               player names and transfer 
                                               direction.")
                                     ),
                                     tags$br(),
                                     tags$p("For more detailed information 
                                            about statistical measures, degree 
                                            distribution as well as in and out 
                                            degrees see the tables below.")
                              ),
                              column(6,
                                     h3("Interactive Transfer Network Graph"),
                                     visNetworkOutput("directed_graph"),
                              )
                            )
                          )
                          ),
                        
                        wellPanel(
                          tags$h3("Statistical Measures"),
                          tags$p("The statistical measures of the transfer 
                                 network are displayed below, including average 
                                 degree, clustering coefficient, and average 
                                 path length."),
                          p(uiOutput("graph_summary")),
                          tags$br(),
                          h4(textOutput("transfer_network_summary_headline")),
                          p(uiOutput("transfer_network_summary_details"))
                        ),
                        
                        wellPanel(
                          tags$h3("Centrality Measures Table"),
                          tags$p("The table below shows centrality measures for 
                                 the filtered graph, including degree, 
                                 closeness, betweenness, eigenvector, and 
                                 PageRank."),
                          tags$br(),
                          dataTableOutput("centrality_table")
                        ),
                        
                        wellPanel(
                          tags$h3("Degree Distribution Histogram"),
                          tags$p("The histogram below displays the degree 
                                 distribution of the selected country's transfer
                                 network."),
                          tags$br(),
                          plotOutput("degree_distribution")
                        ),
                        
                        wellPanel(
                          fluidRow(
                            column(6,
                                   style = "border-right: 1px solid #999; 
                                   padding-left: 20px; padding-right: 20px;",
                                   tags$h3("In-degree Table"),
                                   tags$p("The table below shows the 
                                          in-degree of each club in the 
                                          transfer network."),
                                   tags$br(),
                                   dataTableOutput("in_degree_table")),
                            column(6,
                                   tags$h3("Out-degree Table"),
                                   tags$p("The table below shows the 
                                          out-degree of each club in the 
                                          transfer network."),
                                   tags$br(),
                                   dataTableOutput("out_degree_table"))
                            )
                          )
                        )
                      )
             ),
  
  # NETWORK ANALYSIS PAGE-------------------------------------------------------
  navbarMenu(title = "Network Analysis",
             tabPanel(title = "4.1 Football Transfers Map",
                      fluidRow(
                        column(12,
                               h2("Football Transfers Map by several metrics"),
                               p("In this section, we will visualize the 
                                 football transfers data on a map."),
                               br()
                        )
                      ),
                      wellPanel(
                        wellPanel(
                          fluidRow(
                            column(4,
                                   h3("Filters"),
                                   selectInput("season_4_1", "Select a season:",
                                               choices = 2004:2021, 
                                               selected = 2021, width = "100%"),
                                   selectInput("metric_4_1", "Select metric:",
                                               c("Highest Average Market Value",
                                                 "Highest Average Performance", 
                                                 "Highest Average Fee")),
                                   selectInput("position_4_1", 
                                               "Select position:",
                                               c("All", "Goalkeeper", "Defense",
                                                 "Midfield", "Attack"))
                            ),
                            column(8,
                                   style = "border-left: 1px solid #999; 
                                   padding-left: 10px;",
                                   h3("Football Transfers Map"),
                                   plotOutput("football_map", width = "100%", 
                                              height = "800px")
                            )
                          )
                        ),
                        wellPanel(
                          fluidRow(
                            column(12,
                                   h3("Top Countries Table"),
                                   dataTableOutput("top_countries_table")
                            )
                          )
                        )
                      )
             ),
             
             tabPanel(title = "4.2 Football Transfers by Performance Index",
                      fluidPage(
                        titlePanel("Football Transfers Network Exploration by 
                                   Self-Developed Performance Index"),
                        
                        wellPanel(
                          fluidRow(
                            column(4,
                                   h3("Choose a Season, Country, Club, 
                                      Position"),
                                   selectInput("season_4_2", "Select a Season:",
                                               choices = 2004:2021, 
                                               selected = 2021, width = "100%"),
                                   uiOutput("country_ui_4_2"),
                                   uiOutput("club_ui_4_2"),
                                   uiOutput("position_ui_4_2"),
                                   tags$br(),
                                   tags$br(),
                                   h4("Explanations for the Usage of the 
                                      Dashboard"),
                                   tags$p("We've developed a performance 
                                          index for football players, 
                                          considering age, market value, and 
                                          transfer fee. The ideal player 
                                          is young, with high market value, and 
                                          a transfer fee of 0, indicating a free
                                          transfer."),
                                   tags$ul(
                                     tags$li("Use the filters to select the 
                                             time, country, clubs, and position,
                                             or choose all."),
                                     tags$li("You can also filter by club to 
                                             view clubs from the selected year 
                                             and country.")
                                   ),
                                   tags$br(),
                                   tags$strong("Top Graph:"),
                                   tags$p("The interactive graph displays the 
                                          top ten incoming transfers for the 
                                          selected club, revealing player 
                                          sources. Adjust the view by dragging 
                                          nodes. Use the top-right filter to 
                                          highlight a node and the position 
                                          filter to search for specific 
                                          positions needed by the club."),
                                   tags$br(),
                                   tags$strong("Bottom Graph:"),
                                   tags$p("The scatterplot shows the relation 
                                          between age and performance index, 
                                          with positions as colored markers. 
                                          This helps identify the distribution 
                                          of players and areas for 
                                          improvement."),
                            ),
                            column(8,
                                   style = "border-left: 1px solid #999; 
                                   padding-left: 20px; padding-right: 20px;",
                                   h3(textOutput("headline_4_2_1")),
                                   visNetworkOutput("performers_graph"),
                                   tags$br(),
                                   tags$br(),
                                   h3("Distribution of Performance across 
                                      Position and Age"),
                                   plotOutput("scatter_plot")
                            )
                          )
                        ),
                        
                        wellPanel(
                          h3(textOutput("headline_4_2_2")),
                          tags$p("The table provides detailed information on the
                                 transfers from the filtered graphs above and 
                                 can be filtered in the same way."),
                          tags$br(),
                          dataTableOutput("performers_table")
                          )
                        )
             ),
             
             tabPanel(title = "4.3 Football Transfers Predictions 
                      (Based on Season 2017-2021)",
                      fluidPage(
                        titlePanel("Football Transfers Predictions 
                                   (Based on Season 2017-2021)"),
                        tags$p("Welcome to the Football Transfers 
                               Prediction dashboard!"),
                        tags$p("This dashboard allows you to predict the most 
                               likely transfers of football players between 
                               clubs using cocitation analysis. Enjoy exploring
                               the Football Transfers Prediction dashboard and 
                               use it to your advantage when scouting new 
                               talent!"),
                        tags$br(),
                        
                        wellPanel(
                          wellPanel(
                            fluidRow(
                              column(6,
                                     style = "border-right: 1px solid #999; 
                                     padding-left: 20px; padding-right: 20px;",
                                     h3("Choose a country, club, position,
                                        no. of players"),
                                     selectInput("country_4_3", "Country:",
                                                 unique(data$country)),
                                     uiOutput("club_ui_4_3"),
                                     selectInput("position_4_3", "Position:",
                                                 c("All", "Goalkeeper", 
                                                   "Defense", "Midfield", 
                                                   "Attack")),
                                     selectInput("num_rows", "Number of players 
                                                 to display:", 
                                                 choices = c(10, 20, 30, 50)),
                                     tags$br(),
                                     tags$br(),
                                     h4("Explanations for the Usage of 
                                        the Dashboard"),
                                     tags$p("Use the filters to:"),
                                     tags$ul(
                                       tags$li("Country: filter the displayed 
                                               clubs by country."),
                                       tags$li("Club: select a specific club 
                                               from the dynamically updated list
                                               of clubs based on the selected 
                                               country."),
                                       tags$li("Position: filter players by 
                                               their position on the field or 
                                               select All to display players in
                                               all positions."),
                                       tags$li("Number of players: select the 
                                               number of players you want to see
                                               in the table displayed.")
                                     ),
                                     tags$br(),
                                     tags$strong("Prediction Graph:"),
                                     tags$p("Directed graph representing the top
                                            5 predicted transfers for the 
                                            selected club. The vertices 
                                            represent the clubs, and the 
                                            directed edges show the predicted 
                                            transfer probabilities. The chart is
                                            interactive, allowing you to select 
                                            a club to view more information.")
                              ),
                              column(6,
                                     h3("Next Possible Transfers"),
                                     visNetworkOutput("prediction_graph",
                                                      width = "100%", 
                                                      height = "500px")
                              )
                            )
                          )
                        ),
                        
                        wellPanel(
                          fluidRow(
                            column(12,
                                   uiOutput("table_title"),
                                   tags$p("Table with the best players of the 5 
                                          predicted clubs or the best players of
                                          the club selected from the prediction 
                                          chart. The table shows the player's 
                                          name, age, position, nationality, 
                                          performance, market value, transfer 
                                          fee and dealing club. The players are 
                                          sorted according to their performance 
                                          by the performance index."),
                                   tags$br(),
                                   tableOutput("player_table"))
                          )
                        )
                      )
             )
  )

# End of UI---------------------------------------------------------------------
)


# SERVER SECTION----------------------------------------------------------------
server <- function(input, output) {
  
  # 2. DESCRIPTIVE STATISTICS---------------------------------------------------
  # 2.1.1 Table with available variables----------------------------------------
  output$available.vars <- renderTable({df.available.vars})
  
  # 2.1.2 Data Preview
  output$data.preview = renderDataTable(head(data, 100), 
                                        options = list(scrollX = TRUE, 
                                                       pageLength = 5, 
                                                       searching = FALSE))  
  
  # 2.2.1 Summary statistics table----------------------------------------------
  output$summary.table <- renderTable({
    filtered_data <- dt.season.statistics() %>%
      filter(if (input$summary.f1 != 
                 "Select All") country == input$summary.f1 else TRUE)
    
    number.observations <- nrow(filtered_data)
    number.countries <- length(unique(filtered_data$country))
    number.clubs <- length(unique(filtered_data$club))
    number.players <- length(unique(filtered_data$name))
    max.age <- format(round(max(filtered_data$age, na.rm = TRUE), 0), 
                      big.mark = ",", scientific = FALSE)
    min.age <- format(round(min(filtered_data$age, na.rm = TRUE), 0), 
                      big.mark = ",", scientific = FALSE)
    avg.age <- format(round(mean(filtered_data$age, na.rm = TRUE), 0), 
                      big.mark = ",", scientific = FALSE)
    std.age <- format(round(sd(filtered_data$age, na.rm = TRUE), 0), 
                      big.mark = ",", scientific = FALSE)
    max.performance <- format(round(max(filtered_data$normalized_performance,
                                        na.rm = TRUE), 4), big.mark = ",", 
                              scientific = FALSE)
    min.performance <- format(round(min(filtered_data$normalized_performance, 
                                        na.rm = TRUE), 4), big.mark = ",", 
                              scientific = FALSE)
    avg.performance <- format(round(mean(filtered_data$normalized_performance, 
                                         na.rm = TRUE), 4), big.mark = ",", 
                              scientific = FALSE)
    std.performance <- format(round(sd(filtered_data$normalized_performance, 
                                       na.rm = TRUE), 4), big.mark = ",", 
                              scientific = FALSE)
    max.market.value <- format(max(filtered_data$market_value, na.rm = TRUE), 
                               big.mark = ",", scientific = FALSE)
    min.market.value <- format(min(filtered_data$market_value, na.rm = TRUE), 
                               big.mark = ",", scientific = FALSE)
    avg.market.value <- format(mean(filtered_data$market_value, na.rm = TRUE), 
                               big.mark = ",", scientific = FALSE)
    std.market.value <- format(sd(filtered_data$market_value, na.rm = TRUE), 
                               big.mark = ",", scientific = FALSE)
    max.fee <- format(max(filtered_data$fee, na.rm = TRUE), big.mark = ",", 
                      scientific = FALSE)
    min.fee <- format(min(filtered_data$fee, na.rm = TRUE), big.mark = ",", 
                      scientific = FALSE)
    avg.fee <- format(mean(filtered_data$fee, na.rm = TRUE), big.mark = ",", 
                      scientific = FALSE)
    std.fee <- format(sd(filtered_data$fee, na.rm = TRUE), big.mark = ",", 
                      scientific = FALSE)
    
    df.summary.table <- data.frame(
      "Metric" = c(
        "Number of Observations",
        "Number of Countries",
        "Maximum Age",
        "Minimum Age",
        "Average Age",
        "Standard Deviation Age",
        "Maximum Performance",
        "Minimum Performance",
        "Average Performance",
        "Standard Deviation Performance",
        "Number of Clubs",
        "Number of Players",
        "Maximum Market Value",
        "Minimum Market Value",
        "Average Market Value",
        "Standard Deviation of Market Value",
        "Maximum Fee",
        "Minimum Fee",
        "Average Fee",
        "Standard Deviation of Fee"
      ),
      "Value" = c(
        number.observations,
        number.countries,
        max.age,
        min.age,
        avg.age,
        std.age,
        max.performance,
        min.performance,
        avg.performance,
        std.performance,
        number.clubs,
        number.players,
        max.market.value,
        min.market.value,
        avg.market.value,
        std.market.value,
        max.fee,
        min.fee,
        avg.fee,
        std.fee
      )
    )
    # Split the data frame into two halves--------------------------------------
    n <- nrow(df.summary.table)
    half_n <- ceiling(n / 2)
    df.summary.table.1 <- df.summary.table[1:half_n, ]
    df.summary.table.2 <- df.summary.table[(half_n + 1):n, ]
    
    # Adjust column names for the second half
    colnames(df.summary.table.2) <- c("Metric ", "Value ")
    
    # Combine both halves using cbind()
    df.summary.table.wide <- cbind(df.summary.table.1, df.summary.table.2)
    
    # Render the table
    df.summary.table.wide
  })
  
  # 2.2.2 Distribution: Top Transfers-------------------------------------------
  output$transfers_title <- renderUI({
    if (input$summary.f1 == "Select All") {
      h3("Top 20 Countries by Number of Transfers")
    } else {
      h3("Top 20 Clubs by Number of Transfers")
    }
  })
  
  dt.season.statistics <- reactive({
    data %>%
      filter(season >= input$season.statistics[1] & 
               season <= input$season.statistics[2])
  })
  
  # Top transfers---------------------------------------------------------------
  top.transfers.countries <- reactive({
    if (input$summary.f1 == "Select All") {
      dt.season.statistics() %>%
        group_by(country) %>%
        summarise(n = n()) %>%
        arrange(desc(n)) %>%
        top_n(20, n) %>%
        ggplot(aes(x = reorder(country, -n), y = n)) +
        geom_bar(stat = "identity", fill = "#0066cc", color = "white") +
        labs(x = "Country", y = "Number of Transfers") +
        theme_minimal() +
        theme(plot.margin = margin(15, 15, 15, 15),
              plot.background = element_rect(fill = "#F5F5F5"),
              panel.background = element_rect(fill = "#F5F5F5"),
              axis.title.x = element_text(size = 14, face = "bold", 
                                          margin = margin(t = 10)),
              axis.title.y = element_text(size = 14, face = "bold", 
                                          margin = margin(r = 10)),
              axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
              panel.grid.major = element_line(color = "gray85"),
              panel.grid.minor = element_line(color = "gray95"),
              axis.line = element_blank())
    } else {
      return(NULL)
    }
  })
  
  top.transfers.clubs <- reactive({
    if (input$summary.f1 != "Select All") {
      dt.season.statistics() %>%
        filter(country == input$summary.f1) %>%
        group_by(club) %>%
        summarise(n = n()) %>%
        arrange(desc(n)) %>%
        top_n(20, n) %>%
        ggplot(aes(x = reorder(club, -n), y = n)) +
        geom_bar(stat = "identity", fill = "#0066cc", color = "white") +
        labs(x = "Club", y = "Number of Transfers") +
        theme_minimal() +
        theme(plot.margin = margin(15, 15, 15, 15),
              plot.background = element_rect(fill = "#F5F5F5"),
              panel.background = element_rect(fill = "#F5F5F5"),
              axis.title.x = element_text(size = 14, face = "bold", 
                                          margin = margin(t = 10)),
              axis.title.y = element_text(size = 14, face = "bold", 
                                          margin = margin(r = 10)),
              axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
              panel.grid.major = element_line(color = "gray85"),
              panel.grid.minor = element_line(color = "gray95"),
              axis.line = element_blank())
    } else {
      return(NULL)
    }
  })
  
  output$top.transfers <- renderPlot({
    if (input$summary.f1 == "Select All") {
      top.transfers.countries()
    } else {
      top.transfers.clubs()
    }
  })
  
  # 2.2.2 Distribution: Top Market Value
  output$market_value_title <- renderUI({
    if (input$summary.f1 == "Select All") {
      h3("Top 20 Countries by Average Market Value")
    } else {
      h3("Top 20 Clubs by Average Market Value")
    }
  })
  
  # Top market value
  top.market.value.countries <- reactive({
    if (input$summary.f1 == "Select All") {
      dt.season.statistics() %>%
        group_by(country) %>%
        summarise(avg_market_value = mean(market_value, na.rm = TRUE)) %>%
        arrange(desc(avg_market_value)) %>%
        top_n(20, avg_market_value) %>%
        ggplot(aes(x = reorder(country, -avg_market_value), 
                   y = avg_market_value)) +
        geom_bar(stat = "identity", fill = "#0066cc", color = "white") +
        labs(x = "Country",
             y = "Average Market Value") +
        theme_minimal() +
        theme(plot.margin = margin(15, 15, 15, 15),
              plot.background = element_rect(fill = "#F5F5F5"),
              panel.background = element_rect(fill = "#F5F5F5"),
              axis.title.x = element_text(size = 14, face = "bold", 
                                          margin = margin(t = 10)),
              axis.title.y = element_text(size = 14, face = "bold", 
                                          margin = margin(r = 10)),
              axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
              panel.grid.major = element_line(color = "gray85"),
              panel.grid.minor = element_line(color = "gray95"),
              axis.line = element_blank())
    } else {
      return(NULL)
    }
  })
  
  top.market.value.clubs <- reactive({
    if (input$summary.f1 != "Select All") {
      dt.season.statistics() %>%
        filter(country == input$summary.f1) %>%
        group_by(club) %>%
        summarise(avg_market_value = mean(market_value, na.rm = TRUE)) %>%
        arrange(desc(avg_market_value)) %>%
        top_n(20, avg_market_value) %>%
        ggplot(aes(x = reorder(club, -avg_market_value), 
                   y = avg_market_value)) +
        geom_bar(stat = "identity", fill = "#0066cc", color = "white") +
        labs(x = "Club",
             y = "Average Market Value") +
        theme_minimal() +
        theme(plot.margin = margin(15, 15, 15, 15),
              plot.background = element_rect(fill = "#F5F5F5"),
              panel.background = element_rect(fill = "#F5F5F5"),
              axis.title.x = element_text(size = 14, face = "bold", 
                                          margin = margin(t = 10)),
              axis.title.y = element_text(size = 14, face = "bold", 
                                          margin = margin(r = 10)),
              axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
              panel.grid.major = element_line(color = "gray85"),
              panel.grid.minor = element_line(color = "gray95"),
              axis.line = element_blank())
    } else {
      return(NULL)
    }
  })
  
  output$top.market.value <- renderPlot({
    if (input$summary.f1 == "Select All") {
      top.market.value.countries()
    } else {
      top.market.value.clubs()
    }
  })
  

  # Network Exploration---------------------------------------------------------
  
  # 3.2 Overview about the football Transfers Network by country----------------
  
  selected.club_3_2 <- reactiveVal()
  graph_selected_club_3_2 <- reactiveVal()
  
  
  season.data_3_2 <- reactive({
    data %>%
      filter(season == input$season_3_2) %>%
      filter(country == input$country) %>%
      filter(if (input$position != "All") position_category == 
               input$position else TRUE)
  })
  
  # Reactive function to update country choices based on selected season--------
  country.choices_3_2 <- reactive({
    data %>%
      filter(season == input$season_3_2) %>%
      distinct(country) %>%
      pull(country)
  })
  
  
  # Update the country input based on the selected season-----------------------
  output$country_ui_3_2 <- renderUI({
    country.filtered_3_2 <- data %>% filter(season == input$season_3_2) %>% 
      distinct(country) %>% pull(country)
    selectInput("country", "Country:", choices = country.filtered_3_2)
  })
  
  output$position_ui_3_2 <- renderUI({
    req(input$country) # Ensure input$country is available before proceeding
    position.filtered_3_2 <- data %>%
      filter(season == input$season_3_2, country == input$country) %>%
      distinct(position_category) %>%
      pull(position_category)
    selectInput("position", "Position:", choices = c("All", 
                                                     position.filtered_3_2))
  })
  
  
  # Create the directed graph---------------------------------------------------
  directed.graph <- reactive({
    g.directed <- graph_from_data_frame(season.data_3_2()[, c("dealing_club", 
                                                              "club", "name")],
                                        directed = TRUE)
    df.nodes <- data.frame(id = V(g.directed)$name, label = V(g.directed)$name)
    df.edges <- data.frame(from = get.edgelist(g.directed)[, 1], 
                           to = get.edgelist(g.directed)[, 2],
                           label = E(g.directed)$name)
    
    visNetwork(df.nodes, df.edges, width = "100%", height = "600px") %>%
      visEdges(arrows = "to", font = list(size = 10, face = "sans-serif"), 
               label = df.edges$label) %>%
      visNodes(
        color = list(
          highlight = list(
            background = "red",
            border = "red"
          )
        )
      ) %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visEvents(select = 
                  "function(properties) { Shiny.setInputValue('selectedClub', 
                properties.nodes[0]); }") %>%
      visEvents(deselectNode = 
                  "function() { Shiny.setInputValue('directed_graph_unselected',
                true, {priority: 'event'}); }")
  })
  
  
  # Observe the selected club from the directed_graph and update the 
  # selected_club reactive variable---------------------------------------------
  observeEvent(input$selectedClub, {
    selected.club_3_2(input$selectedClub)
  })
  
  # Observe the selected club from the directed_graph and update the 
  # selected_club reactive variable---------------------------------------------
  observeEvent(input$selectedClub, {
    graph_selected_club_3_2(input$selectedClub)
  })
  
  # Observe the deselected club from the directed_graph and reset the 
  # selected_club reactive variable---------------------------------------------
  observeEvent(input$directed_graph_unselected, {
    selected.club_3_2(NULL)
  })
  
  # Observe the deselected club from the directed_graph and reset the 
  # selected_club reactive variable---------------------------------------------
  observeEvent(input$directed_graph_unselected, {
    graph_selected_club_3_2(NULL)
  })
  
  # Render the directed graph---------------------------------------------------
  output$directed_graph <- renderVisNetwork({
    directed.graph()
  })
  
  # Render the number of nodes and edges----------------------------------------
  output$graph_summary <- renderUI({
    g.directed <- graph_from_data_frame(season.data_3_2()[, c("dealing_club",
                                                              "club", "name")],
                                        directed = TRUE)
    num_nodes <- gorder(g.directed)
    num_edges <- gsize(g.directed)
    
    tags$ul(
      tags$li(paste("Number of nodes:", num_nodes)),
      tags$li(paste("Number of edges:", num_edges))
    )
  })
  
  # Calculate centrality measures for the filtered graph
  centrality.measures <- reactive({
    g.directed <- graph_from_data_frame(season.data_3_2()[, c("dealing_club",
                                                              "club", "name")],
                                        directed = TRUE)
    degree.centrality <- degree(g.directed, mode = "all")
    closeness <- closeness(g.directed)
    betweenness <- round(betweenness(g.directed), 4)
    eigenvector <- round(evcent(g.directed)$vector, 4)
    pr <- page_rank(g.directed)$vector
    pr_norm <- scale(pr)
    
    # Normalized centrality measures for better interpretations-----------------
    normalized.degree <- round(degree(g.directed, normalized = TRUE), 4)
    normalized.closeness <- round(closeness(g.directed, normalize = TRUE), 4)
    normalized.betweenness <- round(betweenness(g.directed, normalize = TRUE),
                                    4)
    normalized.pr <- round(scale(pr,center = min(pr), 
                                 scale = max(pr) - min(pr)),4)
    
    # Create a dataframe with centrality measures-------------------------------
    club_names <- V(g.directed)$name
    
    df.centrality.measures_clubs <- data.frame(
      club_name = club_names,
      degree = degree.centrality,
      normalized_degree = normalized.degree,
      normalized_closeness = normalized.closeness,
      normalized_betweenness = normalized.betweenness,
      eigenvector = eigenvector,
      normalized_pagerank = normalized.pr
    )
    
    return(df.centrality.measures_clubs)
  })
  
  # Render the centrality measures table----------------------------------------
  output$centrality_table <- renderDataTable({
    df.centrality <- centrality.measures()
    
    if (!is.null(selected.club_3_2()) && selected.club_3_2() != "") {
      df.centrality <- df.centrality %>% 
        filter(club_name == selected.club_3_2())
    }
    
    # Update column names
    colnames(df.centrality) <- gsub("_", " ", colnames(df.centrality))
    colnames(df.centrality) <- tools::toTitleCase(colnames(df.centrality))
    
    df.centrality
  }, options = list(pageLength = 10, autoWidth = TRUE, searching = FALSE))
  
  # Render the degree distribution histogram
  output$degree_distribution <- renderPlot({
    g.directed <- graph_from_data_frame(season.data_3_2()[, c("dealing_club", 
                                                              "club", "name")], 
                                        directed = TRUE)
    degree.subgraph <- degree(g.directed)
    ggplot() +
      geom_histogram(aes(x = degree.subgraph, y = ..count..), 
                     binwidth = 1, fill = "#0066cc", color = "white") +
      scale_x_continuous(breaks = seq(min(degree.subgraph),
                                      max(degree.subgraph), 1)) +
      labs(x = "Degree", y = "Number of clubs",
           title = paste("Degree Distribution of ", input$country)) +
      theme_minimal() +
      theme(plot.margin = margin(15, 15, 15, 15),
            plot.background = element_rect(fill = "#F5F5F5"),
            panel.background = element_rect(fill = "#F5F5F5"),
            axis.title.x = element_text(size = 14, face = "bold", 
                                        margin = margin(t = 10)),
            axis.title.y = element_text(size = 14, face = "bold", 
                                        margin = margin(r = 10)),
            axis.text.x = element_text(size = 12),
            panel.grid.major = element_line(color = "gray85"),
            panel.grid.minor = element_line(color = "gray95"),
            axis.line = element_blank())
  })
  
  # Render the statistical measures---------------------------------------------
  transfer_network_stats <- reactive({
    g.directed <- graph_from_data_frame(season.data_3_2()[, c("dealing_club", 
                                                              "club", "name")],
                                        directed = TRUE)
    avg.degree <- mean(degree(g.directed))
    clustering.coeff <- transitivity(g.directed, type = "average")
    avg.path.length <- average.path.length(g.directed)
    
    list(avg_degree = avg.degree,
         clustering_coeff = clustering.coeff,
         avg_path_length = avg.path.length)
  })
  
  output$transfer_network_summary_headline <- renderText({
    paste("Statistical summary of the transfer network of", input$country, ":")
  })
  
  output$transfer_network_summary_details <- renderUI({
    stats <- transfer_network_stats()
    tags$ul(
      tags$li(paste("Avg. degree of the directed graph:", 
                    round(stats$avg_degree, 4))),
      tags$li(paste("Clustering coefficient of the directed graph:", 
                    round(stats$clustering_coeff, 4))),
      tags$li(paste("Avg. path length of the directed graph:", 
                    round(stats$avg_path_length, 4)))
    )
  })
  
  # Render the in-degree table--------------------------------------------------
  output$in_degree_table <- renderDataTable({
    g.directed <- graph_from_data_frame(season.data_3_2()[, c("dealing_club", 
                                                              "club", "name")], 
                                        directed = TRUE)
    in.degree <- data.frame(club = V(g.directed)$name, 
                            in.degree = degree(g.directed, mode = "in"))
    
    if (!is.null(selected.club_3_2()) && selected.club_3_2() != "") {
      in.degree <- in.degree %>% filter(club == selected.club_3_2())
    }
    
    # Update column names-------------------------------------------------------
    colnames(in.degree) <- gsub("\\.", " ", colnames(in.degree))
    colnames(in.degree) <- tools::toTitleCase(colnames(in.degree))
    
    in.degree
  }, options = list(pageLength = 10, autoWidth = TRUE, searching = FALSE))
  
  # Render the out-degree table-------------------------------------------------
  output$out_degree_table <- renderDataTable({
    g.directed <- graph_from_data_frame(season.data_3_2()[, c("dealing_club", 
                                                              "club", "name")], 
                                        directed = TRUE)
    out.degree <- data.frame(club = V(g.directed)$name, 
                             out.degree = degree(g.directed, mode = "out"))
    
    if (!is.null(selected.club_3_2()) && selected.club_3_2() != "") {
      out.degree <- out.degree %>% filter(club == selected.club_3_2())
    }
    
    # Update column names-------------------------------------------------------
    colnames(out.degree) <- gsub("\\.", " ", colnames(out.degree))
    colnames(out.degree) <- tools::toTitleCase(colnames(out.degree))
    
    out.degree
  }, options = list(pageLength = 10, autoWidth = TRUE, searching = FALSE))
  
  
  # 4. NETWORK Analysis---------------------------------------------------------
  
  # 4.1 Performance Map---------------------------------------------------------
  
  
  # Filter the data by season and position--------------------------------------
  df_filtered <- reactive({
    df <- data %>%
      filter(season == input$season_4_1)
    
    if (input$position_4_1 != "All") {
      df <- df %>% filter(position_category == input$position_4_1)
    }
    
    return(df)
  })
  
  # Create a summary table of the top 10 dealing countries by selected metric---
  df_map <- reactive({
    metric_col <- case_when(
      input$metric_4_1 == "Highest Average Market Value" ~ "market_value",
      input$metric_4_1 == 
        "Highest Average Performance" ~ "normalized_performance",
      input$metric_4_1 == "Highest Average Fee" ~ "fee"
    )
    
    df_filtered() %>% 
      group_by(dealing_country_lat, dealing_country_lon, dealing_country) %>% 
      summarise(avg_metric = mean(!!sym(metric_col)), n_transfers = n(), 
                .groups = 'drop') %>% 
      filter(!is.na(avg_metric)) %>% 
      arrange(desc(avg_metric)) %>%
      head(10)
  })
  
  # create the top countries table
  output$top_countries_table <- renderDataTable({
    df_map() %>%
      mutate(n_transfers = sapply(dealing_country, function(x) {
        sum(df_filtered()$dealing_country == x)
      })) %>%
      mutate(avg_age = sapply(dealing_country, function(x) {
        mean(df_filtered()$age[df_filtered()$dealing_country == x], 
             na.rm = TRUE)
      })) %>%
      mutate(avg_age = round(avg_age, 0), avg_metric = round(avg_metric, 4)) %>%
      select(dealing_country, n_transfers, avg_age, avg_metric) %>%
      rename(Country = dealing_country, No_Transfers = n_transfers, 
             Average_Age = avg_age, !!input$metric_4_1 := avg_metric)
  }, options = list(lengthChange = FALSE))
  
  # create the map
  output$football_map <- renderPlot({
    ggmap(map, width = 5000, height = 3000) + 
      geom_point(data = df_map(), aes(x = dealing_country_lon, 
                                      y = dealing_country_lat, 
                                      size = avg_metric), alpha = 0.6, 
                 color = "#FF5733") +
      scale_size_continuous(range = c(1, 5)) +
      labs(size = paste(input$metric, "(in millions of euros)"),
           title = paste("Top Ten Dealing Countries by", input$metric_4_1, 
                         "- Season", input$season_4_1)) +
      theme_void() +
      theme(plot.title = element_text(size = 16, face = "bold", 
                                      margin = margin(b = 20, t = 30)),
            legend.position = "bottom",
            legend.text = element_text())
  })
  
  # 4.2 Performance index
  
  season.data_4_2 <- reactive({
    data %>%
      filter(season == input$season_4_2) %>%
      filter(country == input$country) %>%
      filter(if (input$position != "All") position_category == 
               input$position else TRUE)
  })
  
  # Reactive function to update country choices based on selected season--------
  
  country.choices_4_2 <- reactive({
    data %>%
      filter(season == input$season_4_2) %>%
      distinct(country) %>%
      pull(country)
  })
  
  # Reactive function to update position choices based on 
  # selected season, country, and club------------------------------------------
  position.choices_4_2 <- reactive({
    data %>%
      filter(season == input$season_4_2, country == input$country, 
             club == input$club) %>%
      distinct(position_category) %>%
      pull(position_category)
  })

  
  # Update the position input based on the selected 
  # season, country, and club---------------------------------------------------
  output$position_ui_4_2 <- renderUI({
    req(input$club) # Ensure input$club is available before proceeding
    position.filtered_4_2 <- data %>%
      filter(season == input$season_4_2, country == input$country, 
             club == input$club) %>%
      distinct(position_category) %>%
      pull(position_category)
    selectInput("position", "Position:", choices = c("All", 
                                                     position.filtered_4_2))
  })
  
  # Update the country input based on the selected season-----------------------
  output$country_ui_4_2 <- renderUI({
    countries.filtered_4_2 <- data %>% filter(season == input$season_4_2) %>% 
      distinct(country) %>% pull(country)
    selectInput("country", "Country:", choices = countries.filtered_4_2)
  })
  
  output$club_ui_4_2 <- renderUI({
    req(input$country)
    clubs.filtered_4_2 <- data %>% filter(season == input$season_4_2, 
                                          country == input$country) %>% 
      distinct(club) %>% pull(club)
    selectInput("club", "Club:", choices = clubs.filtered_4_2)
  })
  
  # Plot directed graph interactive of top performers---------------------------
  df.performers <- reactive({
    season.filtered_4_2 <- season.data_4_2() %>% 
      filter(country == input$country, club == input$club)
    
    # Filter for top performers based on age and performance score, 
    # and keep only top 20------------------------------------------------------
    performers.top <- season.filtered_4_2[order(-season.filtered_4_2$normalized_performance), ][1:10, ]
    
    # Create a new dataframe with only the relevant columns---------------------
    performers.df <- subset(performers.top, select=c("dealing_club", "club",
                                                     "name"))
    
    # Create the graph----------------------------------------------------------
    g.perform <- graph_from_data_frame(performers.df, directed = TRUE)
    
    # Remove nodes with the name "NA"-------------------------------------------
    g.perform <- delete.vertices(g.perform, which(V(g.perform)$name == "NA"))
    
    # Convert graph to edges and nodes data frames------------------------------
    df.nodes <- data.frame(id = V(g.perform)$name, label = V(g.perform)$name)
    df.edges <- data.frame(from = get.edgelist(g.perform)[, 1], to = get.edgelist(g.perform)[, 2],
                           label = E(g.perform)$name)
    
    # Create a visNetwork object instead of using the base igraph plot----------
    visNetwork(df.nodes, df.edges, width = "100%", height = "600px") %>%
      visEdges(arrows = "to", font = list(size = 10, face = "sans-serif"), 
               label = df.edges$label) %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visInteraction(zoomView = FALSE,
                     dragView = FALSE) %>%
      visLayout(randomSeed = 42)
  })
  
  output$performers_graph <- renderVisNetwork({
    df.performers()
  })
  
  # Create a table of detailed informations about top perfomers-----------------
  filtered.data_4_2 <- reactive({
    season.filtered_4_2 <- season.data_4_2() %>% 
      filter(country == input$country, club == input$club)
    
    # Filter for top performers based on age and performance score, 
    # and keep only top 20------------------------------------------------------
    df.performers.top <- season.filtered_4_2[order(-season.filtered_4_2$normalized_performance), ][1:10, ]
    
    # Create a new dataframe with only the relevant columns
    df.performers <- 
      subset(df.performers.top, select=c("season", "country", 
                                         "club", "dealing_club", "name",
                                         "age", "position_category", 
                                         "nationality", 
                                         "normalized_performance"))
    
    # Reset the row names/index
    rownames(df.performers) <- NULL
    df.performers
  })
  
  # Create a scatter plot of market values by position and age 
  # based on the filtered data--------------------------------------------------
  output$scatter_plot <- renderPlot({
    filtered.data_4_2() %>%
      mutate(position_category = factor(position_category, 
                                        levels = c("Goalkeeper", "Defense",
                                                   "Midfield", "Attack"))) %>%
      ggplot(aes(x = age, y = normalized_performance, 
                 color = position_category)) +
      geom_point(size = 2, alpha = 0.6) +
      scale_color_brewer(palette = "Set1", name = "Position") +
      scale_y_continuous(labels = scales::comma) +
      labs(x = "Age", y = "Normalized Performance") +
      theme_minimal() +
      theme(plot.margin = margin(15, 15, 15, 15),
            plot.background = element_rect(fill = "#F5F5F5"),
            panel.background = element_rect(fill = "#F5F5F5"),
            axis.title.x = element_text(size = 14, face = "bold", 
                                        margin = margin(t = 10)),
            axis.title.y = element_text(size = 14, face = "bold", 
                                        margin = margin(r = 10)),
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12),
            panel.grid.major = element_line(color = "gray85"),
            panel.grid.minor = element_line(color = "gray95"),
            axis.line = element_blank(),
            legend.position = "bottom",
            legend.title = element_text(face = "bold", size = 10),
            legend.text = element_text(size = 10))
  })
  
  output$performers_table <- renderDataTable({
    dt <- filtered.data_4_2()
    
    # Capitalize column names and remove "_"------------------------------------
    colnames(dt) <- gsub("_", " ", colnames(dt))
    colnames(dt) <- tools::toTitleCase(colnames(dt))
    
    dt
  },
  options = list(
    searching = FALSE,
    lengthChange = FALSE,
    paging = FALSE,
    pageLength = 10
  ))
  
  # Create club-specific headline for network graph-----------------------------
  output$headline_4_2_1 <- renderText({
    req(input$club)
    paste("Top Ten Transfers to", input$club, "by Normalized Performance Index")
  })
  
  # Create club-specific headline for table-------------------------------------
  output$headline_4_2_2 <- renderText({
    req(input$club)
    paste("Transfer Insights for Top Performing Players of", input$club)
  })
  
  
  # 4.3 Prediction
  

  season.data_4_3 <- reactive({
    data %>% filter(season >= 2011 & season <= 2021)
  })
  
  
  output$club_ui_4_3 <- renderUI({
    clubs_filtered <- season.data_4_3() %>% 
      filter(country == input$country_4_3) %>% select(club)
    selectInput("club", "Club:", choices = unique(clubs_filtered))
  })
  
  df_top_5_pred_dealing_clubs <- reactiveVal()
  selected_club <- reactiveVal()
  
  output$prediction_graph <- renderVisNetwork({
    req(input$club)
    
    season_filtered <- season.data_4_3() %>% 
      select(c("dealing_club", "club", "name"))
    
    # Create directed graph ----------------------------------------------------
    g.season_filtered <- graph_from_data_frame(season_filtered[,
                                                               c("dealing_club", 
                                                                 "club")], 
                                               directed = TRUE)
    
    # Get predicted edges ----------------------------------------------------------
    m.predicted.edges <- as.matrix(cocitation(g.season_filtered) * 
                                     (1 - get.adjacency(g.season_filtered)))
    
    df.predicted.edges <- as.data.frame(which(m.predicted.edges > 0, 
                                              arr.ind = TRUE))
    
    colnames(df.predicted.edges) <- c("dealing_club", "club")
    
    df.predicted.edges$transfer_weight <- 
      m.predicted.edges[m.predicted.edges > 0]
    
    df.predicted.edges$dealing_club <- 
      rownames(m.predicted.edges)[df.predicted.edges$dealing_club]
    
    df.predicted.edges$club <- 
      rownames(m.predicted.edges)[df.predicted.edges$club]
    
    # Remove self-loops where dealing club and club are the same ---------------
    df.predicted.edges <- subset(df.predicted.edges, dealing_club != club)
    
    # Normalize the transfer_weight column -------------------------------------
    max.transfer.weight <- max(df.predicted.edges$transfer_weight)
    
    df.predicted.edges$transfer_prob <- 
      round(df.predicted.edges$transfer_weight / max.transfer.weight, 2)
    
    # Filter edges to only include those with selected club as target ----------
    df.predicted.edges <- df.predicted.edges[df.predicted.edges$club == 
                                               input$club, ]
    df.predicted.edges <- df.predicted.edges[df.predicted.edges$dealing_club != 
                                               input$club, ]
    
    # Keep only the relevant columns -------------------------------------------
    df.predicted.edges <- subset(df.predicted.edges,
                                 select=c("dealing_club", "club", 
                                          "transfer_prob"))
    
    # Remove the index column by setting row.names to NULL ---------------------
    row.names(df.predicted.edges) <- NULL
    
    # Sort edges by transfer probability ---------------------------------------
    df.predicted.edges <- 
      df.predicted.edges[order(df.predicted.edges$transfer_prob, 
                               decreasing = TRUE), ]
    df.top.5.pred.dealing.clubs <- df.predicted.edges[1:5, ]
    df_top_5_pred_dealing_clubs(df.top.5.pred.dealing.clubs)
    rownames(df.top.5.pred.dealing.clubs) <- NULL
    
    # Create directed graph from df.top.5.pred.dealing.clubs
    g.test <- 
      graph_from_data_frame(df.top.5.pred.dealing.clubs[, 
                                                        c("dealing_club", 
                                                          "club", 
                                                          "transfer_prob")], 
                            directed = TRUE)
    
    # Set edge weights to transfer_probability----------------------------------
    E(g.test)$weight <- df.top.5.pred.dealing.clubs$transfer_prob
    
    # Set vertex colors for chosen club and dealing clubs-----------------------
    V(g.test)$color <- ifelse(V(g.test)$name == input$club, "red", "blue")
    
    # Create nodes data frame---------------------------------------------------
    nodes <- data.frame(id = V(g.test)$name, 
                        label = V(g.test)$name, 
                        color = V(g.test)$color)
    
    # Add the transfer_prob as the label for the edges--------------------------
    edges <- data.frame(from = get.edgelist(g.test)[, 1],
                        to = get.edgelist(g.test)[, 2],
                        label = paste(round(E(g.test)$weight, digits = 2)),
                        value = E(g.test)$weight)
    
    # Create color gradient-----------------------------------------------------
    edge.color.gradient <- colorRampPalette(c("yellow", "orange", "red"))
    
    # Determine number of unique edge weights-----------------------------------
    num.unique.weights <- length(unique(E(g.test)$weight))
    
    # Generate color vector based on edge weights-------------------------------
    edge.colors <- edge.color.gradient(num.unique.weights)
    
    # Add 'color' column to the edges data frame--------------------------------
    edges$color <- edge.colors[as.numeric(cut(E(g.test)$weight, breaks = num.unique.weights))]
    
    # Create visNetwork graph---------------------------------------------------
    visNetwork(nodes, edges) %>%
      visEdges(smooth = TRUE,
               width = 1,
               label = paste(round(E(g.test)$weight, digits = 2)), 
               arrows = list(to = list(enabled = TRUE)),
               color = list(color = edges$color),
               font = list(size = 26)) %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visPhysics(solver = "barnesHut",
                 barnesHut = list(gravitationalConstant = -25000,
                                  centralGravity = 0.5)) %>%
      visInteraction(zoomView = FALSE,
                     dragView = FALSE)
  })
  
  table_title <- reactive({
    if (!is.null(selected_club()) && selected_club() %in% 
        df_top_5_pred_dealing_clubs()$dealing_club) {
      paste("Top Players in", selected_club())
    } else {
      "Top Players in 5 Predicted Dealing Clubs"
    }
  })
  
  output$table_title <- renderUI({
    h3(table_title())
  })
  
  # Observe the selected club from the prediction_graph and update the 
  # selected_club reactive variable---------------------------------------------
  observeEvent(input$prediction_graph_selected, {
    selected_club(input$prediction_graph_selected)
  })
  
  output$player_table <- renderTable({
    
    top_5_clubs <- df_top_5_pred_dealing_clubs()$dealing_club
    player_data <- season.data_4_3() %>%
      filter(dealing_club %in% top_5_clubs) %>%
      filter(if (input$position_4_3 != "All") position_category == 
               input$position_4_3 else TRUE) %>%
      select(name, age, position, nationality, normalized_performance, 
             market_value, fee, dealing_club) %>%
      arrange(desc(normalized_performance))
    
    # Filter player data based on the selected club or 
    # show data for all top 5 clubs---------------------------------------------
    if (!is.null(selected_club()) && selected_club() %in% top_5_clubs) {
      player_data <- player_data %>% filter(dealing_club == selected_club())
    }
    
    
    # Rename columns------------------------------------------------------------
    player_data <- player_data %>%
      rename(
        "Player Name" = name,
        "Age" = age,
        "Position" = position,
        "Nationality" = nationality,
        "Performance" = normalized_performance,
        "Market Value" = market_value,
        "Transfer Fee" = fee,
        "Dealing Club" = dealing_club
      )
    
    # Remove decimals from market_value and fee columns-------------------------
    player_data$`Market Value` <- format(player_data$`Market Value`, 
                                         nsmall = 0, scientific = FALSE)
    player_data$`Transfer Fee` <- format(player_data$`Transfer Fee`, 
                                         nsmall = 0, scientific = FALSE)
    
    # Display the selected number of rows---------------------------------------
    player_data <- head(player_data, n = input$num_rows)
    
    player_data
  })
  
# End of server-----------------------------------------------------------------
}

# Run the application-----------------------------------------------------------
shinyApp(ui = ui, server = server)
