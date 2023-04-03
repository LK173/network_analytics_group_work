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
library(plotly)



# set the bounding box for the map----------------------------------------------
bbox <- c(-180, -60, 180, 80)

# retrieve the map
map <- get_stamenmap(bbox, zoom = 2, maptype = "toner-lite")

# LOAD DATA---------------------------------------------------------------------

# NEW LOAD DATA

df.football.transfer <- read.csv(file = '../data/football-transfers.csv')

df.transfers.network <- df.football.transfer


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
number.observations <- nrow(df.transfers.network)
number.countries <- length(unique(df.transfers.network$country))
number.clubs <- length(unique(df.transfers.network$club))
number.leagues <- length(unique(df.transfers.network$league))
avg.market.value <- format(mean(df.transfers.network$market_value, na.rm = TRUE), 
                           big.mark = ",", scientific = FALSE)
std.market.value <- format(sd(df.transfers.network$market_value, na.rm = TRUE), 
                           big.mark = ",", scientific = FALSE)
max.market.value <- format(max(df.transfers.network$market_value, na.rm = TRUE), 
                           big.mark = ",", scientific = FALSE)
min.market.value <- format(min(df.transfers.network$market_value, na.rm = TRUE), 
                           big.mark = ",", scientific = FALSE)
avg.fee <- format(mean(df.transfers.network$fee, na.rm = TRUE), big.mark = ",", 
                  scientific = FALSE)
std.fee <- format(sd(df.transfers.network$fee, na.rm = TRUE), big.mark = ",", 
                  scientific = FALSE)
max.fee <- format(max(df.transfers.network$fee, na.rm = TRUE), big.mark = ",", 
                  scientific = FALSE)
min.fee <- format(min(df.transfers.network$fee, na.rm = TRUE), big.mark = ",", 
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
                                               "Season", 
                                               min = min(
                                                 df.transfers.network$season),
                                               max = max(
                                                 df.transfers.network$season), 
                                               value = c(min(
                                                 df.transfers.network$season), 
                                                         max(
                                                  df.transfers.network$season)), 
                                               step = 1, sep =""),
                                   selectInput("summary.f1", "Countries", 
                                               choices = c("Select All",
                                                           unique(
                                                  df.transfers.network$country))
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
                                   uiOutput("transfers.title"),
                                   plotOutput("top.transfers")
                            )
                          ),
                          fluidRow(
                            column(12,
                                   uiOutput("market.value.title"),
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
             tabPanel(title = "3.1 International Transfer Map",
                      fluidRow(
                        column(3,
                               wellPanel(
                                 h3("Filter the dataset"),
                                 selectInput("season.3.1", "Select a season:",
                                             choices = 2004:2021, 
                                             selected = 2021, width = "100%"),
                                 uiOutput("country.ui.3.1"),
                                 uiOutput("position.ui.3.1"),
                                 tags$br(),
                                 tags$p("Disclaimer: Noticable loading times
                                        are possible for displaying the data
                                        due to a large number of transfers 
                                        in the dataset."),
                                 tags$br(),
                                 h4("Explanations for the Usage of 
                                        the Worldmap"),
                                 tags$p("Hover over the nodes to see the name, 
                                        longitude, and latitude of the 
                                        country."),
                                 tags$p("The weight of the edges is represented 
                                        by gradient color:"),
                                 tags$ul(
                                   tags$li("Blue: Low weight = few 
                                           connections"),
                                   tags$li("Red: High weight = many 
                                           connections")),
                                 tags$p("Use the filters to:"),
                                 tags$ul(
                                   tags$li("Season: Filter the data by transfer 
                                           season."),
                                   tags$li("Country: Filter the displayed 
                                               nodes by country."),
                                   tags$li("Position: Filter players by 
                                               their position on the field or 
                                               select All to display players in
                                               all positions.")
                                 )
                               )),
                        column(9,
                               wellPanel(
                                 h3(textOutput("world.map.headline")),
                                 plotlyOutput("map", height = "600px", 
                                              width = "100%")
                               )
                        )
                      )
             ),

             # 3.2 Subpage
             tabPanel(title = "3.2 Details about Football 
                      Transfers Network",
                      fluidPage(
                        titlePanel("Overview of the Football Transfers Network 
                                   by Country"),
                        
                        wellPanel(
                          wellPanel(
                            fluidRow(
                              column(4,
                                     h3("Choose a Season, Country, Club, 
                                        Position"),
                                     selectInput("season.3.2", 
                                                 "Select a season:",
                                                 choices = 2004:2021, 
                                                 selected = 2021, 
                                                 width = "100%"),
                                     selectInput("country.3.2", "Country:", 
                                                 choices = NULL),
                                     selectInput("position.3.2", "Position:", 
                                                 choices = c("All")),
                                     tags$br(),
                                     tags$p("Disclaimer: Noticable loading times
                                        are possible for displaying the data
                                        due to a large number of transfers 
                                        in the dataset."),
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
                                     tags$br(),
                                     tags$p("For more detailed information 
                                            about statistical measures of the
                                            football transfers network, degree 
                                            distribution as well as in and out 
                                            degrees see the tables below.")
                              ),
                              column(8,
                                     style = "border-left: 1px solid #999; 
                                     padding-left: 20px; padding-right: 20px;",
                                     h3("Interactive Transfer Network Graph"),
                                     visNetworkOutput("directed_graph",
                                                      height = "600px"),
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
                          p(uiOutput("graph.summary")),
                          tags$br(),
                          h4(textOutput("transfer.network.summary.headline")),
                          p(uiOutput("transfer.network.summary.details")),
                          p(uiOutput("transfer.network.interpretation"))
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
                          plotOutput("degree.distribution")
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
                                   dataTableOutput("in.degree.table")),
                            column(6,
                                   tags$h3("Out-degree Table"),
                                   tags$p("The table below shows the 
                                          out-degree of each club in the 
                                          transfer network."),
                                   tags$br(),
                                   dataTableOutput("out.degree.table"))
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
                               h2("Football Transfers Map by Several Metrics"),
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
                                   selectInput("season.4.1", "Select a Season:",
                                               choices = 2004:2021, 
                                               selected = 2021, width = "100%"),
                                   selectInput("metric.4.1", "Select metric:",
                                               c("Highest Average Market Value",
                                                 "Highest Average Performance", 
                                                 "Highest Average Fee")),
                                   selectInput("position.4.1", 
                                               "Select position:",
                                               c("All", "Goalkeeper", "Defense",
                                                 "Midfield", "Attack")),
                                   tags$br(),
                                   tags$p("Disclaimer: Noticable loading times
                                        are possible for displaying the data
                                        due to a large number of transfers 
                                        in the dataset."),
                                   tags$br(),
                                   tags$p("Introducing Football Talent Scout 
                                          Europe's Interactive Word Map: This 
                                          world map helps showcase the top 10 
                                          countries based on a selected metric 
                                          (highest average market value, highest 
                                          average performance, or highest 
                                          average fee)."),
                                   tags$p("In order to make the most out of this 
                                          application use the following 
                                          filters:"),
                                   tags$ul(
                                     tags$li("Season: Select your preferred 
                                             season range by adjusting the 
                                             slider."),
                                     tags$li("Metric Selector: Choose the 
                                             ranking metric you prefer from the 
                                             available options."),
                                     tags$li("Position: Filter the data by 
                                             players' positions, selecting from 
                                             available options."),
                                     tags$li("Position: Filter the data by 
                                             players' positions, selecting from 
                                             available options.")
                                   ),
                                   tags$p("The interactive world map is a useful 
                                         tool for analyzing and visualizing 
                                         the most attractive countries based 
                                         on the user's selected criteria, such 
                                         as for international scouting networks, 
                                         foreign locations, or building youth 
                                         academies abroad."),
                                   tags$p("The table below the map displays 
                                         information about the 10 countries with 
                                         the most transfers, including the name 
                                         of the country, the number of 
                                         transfers, the average age of the 
                                         transferred players, and the average 
                                         value of the selected metric.")
                            ),
                            column(8,
                                   style = "border-left: 1px solid #999; 
                                   padding-left: 10px;",
                                   h3(textOutput("football.map.headline")),
                                   plotOutput("football.map", width = "100%", 
                                              height = "600px")
                            )
                          )
                        ),
                        wellPanel(
                          fluidRow(
                            column(12,
                                   h3("Top Countries Table"),
                                   dataTableOutput("top.countries.table")
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
                                   selectInput("season.4.2", "Select a Season:",
                                               choices = 2004:2021, 
                                               selected = 2021, width = "100%"),
                                   selectInput("country.4.2", "Country:", 
                                               choices = NULL),
                                   selectInput("club.4.2", "Club:", 
                                               choices = NULL),
                                   selectInput("position.4.2", "Position:", 
                                               choices = c("All")),
                                   tags$br(),
                                   tags$p("Disclaimer: Noticable loading times
                                        are possible for displaying the data
                                        due to a large number of transfers 
                                        in the dataset."),
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
                                   h3(textOutput("headline.4.2.1")),
                                   visNetworkOutput("performers_graph"),
                                   tags$br(),
                                   tags$br(),
                                   h3("Distribution of Performance across 
                                      Position and Age"),
                                   plotOutput("scatter.plot")
                            )
                          )
                        ),
                        
                        wellPanel(
                          h3(textOutput("headline.4.2.2")),
                          tags$p("The table provides detailed information on the
                                 transfers from the filtered graphs above and 
                                 can be filtered in the same way."),
                          tags$br(),
                          dataTableOutput("performers.table")
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
                              column(4,
                                     style = "border-right: 1px solid #999; 
                                     padding-left: 20px; padding-right: 20px;",
                                     h3("Choose a Country, Club, Position,
                                        No. of Players"),
                                     selectInput("country.4.3", "Country:", 
                                                 unique(
                                                df.transfers.network$country)),
                                     selectInput("club.4.3", "Club:", 
                                                 choices = NULL),
                                     tags$br(),
                                     tags$p("Disclaimer: Noticable loading times
                                        are possible for displaying the data
                                        due to a large number of transfers 
                                        in the dataset."),
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
                              column(8,
                                     h3("Next Possible Transfers"),
                                     visNetworkOutput("prediction.graph",
                                                      width = "100%", 
                                                      height = "500px")
                              )
                            )
                          )
                        ),
                        wellPanel(
                          fluidRow(
                            column(6,
                                   selectInput("position.4.3", "Position:",
                                               c("All", "Goalkeeper", 
                                                 "Defense", "Midfield", 
                                                 "Attack"))
                                   ),
                            column(6,
                                   selectInput("num.rows", 
                                               "Number of players to display:",
                                               choices = c(10, 20, 30, 50)),
                                   )
                          )
                        ),
                        
                        wellPanel(
                          fluidRow(
                            column(12,
                                   uiOutput("table.title"),
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
                                   tableOutput("player.table"))
                          )
                        )
                      )
             )
  )

# End of UI---------------------------------------------------------------------
)


# SERVER SECTION----------------------------------------------------------------
server <- function(input, output, session) {
  
  # 2. DESCRIPTIVE STATISTICS---------------------------------------------------
  # 2.1.1 Table with available variables----------------------------------------
  output$available.vars <- renderTable({df.available.vars})
  
  # 2.1.2 Data Preview
  output$data.preview = renderDataTable(head(df.transfers.network, 100), 
                                        options = list(scrollX = TRUE, 
                                                       pageLength = 5, 
                                                       searching = FALSE))  
  
  # 2.2.1 Summary statistics table----------------------------------------------
  output$summary.table <- renderTable({
    df.filtered.data <- season.statistics() %>%
      filter(if (input$summary.f1 != 
                 "Select All") country == input$summary.f1 else TRUE)
    
    number.observations <- nrow(df.filtered.data)
    number.countries <- length(unique(df.filtered.data$country))
    number.clubs <- length(unique(df.filtered.data$club))
    number.players <- length(unique(df.filtered.data$name))
    max.age <- format(round(max(df.filtered.data$age, na.rm = TRUE), 0), 
                      big.mark = ",", scientific = FALSE)
    min.age <- format(round(min(df.filtered.data$age, na.rm = TRUE), 0), 
                      big.mark = ",", scientific = FALSE)
    avg.age <- format(round(mean(df.filtered.data$age, na.rm = TRUE), 0), 
                      big.mark = ",", scientific = FALSE)
    std.age <- format(round(sd(df.filtered.data$age, na.rm = TRUE), 0), 
                      big.mark = ",", scientific = FALSE)
    max.performance <- format(round(max(df.filtered.data$normalized_performance,
                                        na.rm = TRUE), 4), big.mark = ",", 
                              scientific = FALSE)
    min.performance <- format(round(min(df.filtered.data$normalized_performance, 
                                        na.rm = TRUE), 4), big.mark = ",", 
                              scientific = FALSE)
    avg.performance <- 
      format(round(mean(df.filtered.data$normalized_performance, 
                                         na.rm = TRUE), 4), big.mark = ",", 
                              scientific = FALSE)
    std.performance <- format(round(sd(df.filtered.data$normalized_performance, 
                                       na.rm = TRUE), 4), big.mark = ",", 
                              scientific = FALSE)
    max.market.value <- format(max(df.filtered.data$market_value, na.rm = TRUE), 
                               big.mark = ",", scientific = FALSE)
    min.market.value <- format(min(df.filtered.data$market_value, na.rm = TRUE), 
                               big.mark = ",", scientific = FALSE)
    avg.market.value <- 
      format(mean(df.filtered.data$market_value, na.rm = TRUE), 
                               big.mark = ",", scientific = FALSE)
    std.market.value <- format(sd(df.filtered.data$market_value, na.rm = TRUE), 
                               big.mark = ",", scientific = FALSE)
    max.fee <- format(max(df.filtered.data$fee, na.rm = TRUE), big.mark = ",", 
                      scientific = FALSE)
    min.fee <- format(min(df.filtered.data$fee, na.rm = TRUE), big.mark = ",", 
                      scientific = FALSE)
    avg.fee <- format(mean(df.filtered.data$fee, na.rm = TRUE), big.mark = ",", 
                      scientific = FALSE)
    std.fee <- format(sd(df.filtered.data$fee, na.rm = TRUE), big.mark = ",", 
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
    half.n <- ceiling(n / 2)
    df.summary.table.1 <- df.summary.table[1:half.n, ]
    df.summary.table.2 <- df.summary.table[(half.n + 1):n, ]
    
    # Adjust column names for the second half
    colnames(df.summary.table.2) <- c("Metric ", "Value ")
    
    # Combine both halves using cbind()
    df.summary.table.wide <- cbind(df.summary.table.1, df.summary.table.2)
    
    # Render the table
    df.summary.table.wide
  })
  
  # 2.2.2 Distribution: Top Transfers-------------------------------------------
  output$transfers.title <- renderUI({
    if (input$summary.f1 == "Select All") {
      h3("Top 20 Countries by Number of Transfers")
    } else {
      h3("Top 20 Clubs by Number of Transfers")
    }
  })
  
  season.statistics <- reactive({
    df.transfers.network %>%
      filter(season >= input$season.statistics[1] & 
               season <= input$season.statistics[2])
  })
  
  # Top transfers---------------------------------------------------------------
  top.transfers.countries <- reactive({
    if (input$summary.f1 == "Select All") {
      season.statistics() %>%
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
      season.statistics() %>%
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
  output$market.value.title <- renderUI({
    if (input$summary.f1 == "Select All") {
      h3("Top 20 Countries by Average Market Value")
    } else {
      h3("Top 20 Clubs by Average Market Value")
    }
  })
  
  # Top market value
  top.market.value.countries <- reactive({
    if (input$summary.f1 == "Select All") {
      season.statistics() %>%
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
      season.statistics() %>%
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
  
  # 3.1 Overview about the football Transfers Network by country----------------
  
  # 3.1 World Map
  # filter the data based on the slider input-----------------------------------
  
  selected.club.3.1 <- reactiveVal()
  graph.selected.club.3.1 <- reactiveVal()
  
  season.data.3.1 <- reactive({
    df.transfers.network %>%
      filter(season == input$season.3.1) %>%
      filter(country == input$country) %>%
      filter(if (input$position != "All") position_category == 
               input$position else TRUE)
  })
  
  # Reactive function to update country choices based on selected season
  country.choices.3.1 <- reactive({
    df.transfers.network %>%
      filter(season == input$season.3.1) %>%
      distinct(country) %>%
      pull(country)
  })
  
  # Update the country input based on the selected season-----------------------
  output$country.ui.3.1 <- renderUI({
    df.country.filtered.3.1 <- df.transfers.network %>% 
      filter(season == input$season.3.1) %>% distinct(country) %>% pull(country)
    selectInput("country", "Country:", choices = df.country.filtered.3.1)
  })
  
  output$position.ui.3.1 <- renderUI({
    req(input$country) # Ensure input$country is available before proceeding----
    df.position.filtered.3.1 <- df.transfers.network %>%
      filter(season == input$season.3.1, country == input$country) %>%
      distinct(position_category) %>%
      pull(position_category)
    selectInput("position", "Position:", choices = c("All", 
                                                     df.position.filtered.3.1))
  })
  
  output$world.map.headline <- renderText({
    paste("International Soccer Player Transfers to ", input$country,
          " in Season", input$season.3.1)
  })
  
  output$map <- renderPlotly({
    world.map <- ggplot() +
      borders("world", colour = "lightgrey", fill = "white") +
      coord_equal(xlim = c(-45, 65), ylim = c(15, 80))
    
    p <- world.map
    
    # Count the number of times each edge exists--------------------------------
    edge.counts <- season.data.3.1() %>%
      group_by(dealing_country_lat, dealing_country_lon, country_lat, 
               country_lon) %>% summarise(weight = n())
    
    # Add arrows to the map for each directed edge in the dataset--------------- 
    # with color based on the weight
    p <- p + geom_segment(aes(x = dealing_country_lon, y = dealing_country_lat,
                              xend = country_lon, yend = country_lat, 
                              color = weight), data = edge.counts, 
                          arrow = arrow(length = unit(0.2, "cm"))) +
      scale_color_gradient(name = "Edge weight", low = "blue", high = "red")
    
    # Add labels containing the edge weight value-------------------------------
    p <- p + geom_text(aes(x = (dealing_country_lon + country_lon)/2,
                           y = (dealing_country_lat + country_lat)/2, 
                           label = weight), data = edge.counts, 
                       size = 4, color = "black", fontface = "heavy")
    
    p <- p + geom_point(aes(x = dealing_country_lon, 
                            y = dealing_country_lat, label = dealing_country),
                        data = season.data.3.1(), color = "lightblue", size = 1)
    
    # Add points for each node in the dataset-----------------------------------
    p <- p + geom_point(aes(x = country_lon, y = country_lat, 
                            label = country), data = season.data.3.1(), 
                        color = "red", size = 1)
    
    # Customize the appearance of the plot--------------------------------------
    p <- p + theme_void() + theme(legend.position = "top")
    
    # Show the plot-------------------------------------------------------------
    ggplotly(p)
  })
  
  
  # 3.2 Overview about the football Transfers Network by country----------------
  
  selected.club.3.2 <- reactiveVal()
  graph.selected.club.3.2 <- reactiveVal()
  
  
  season.data.3.2 <- reactive({
    df.transfers.network %>%
      filter(season == input$season.3.2) %>%
      filter(country == input$country.3.2) %>%
      filter(if (input$position.3.2 != "All") 
        position_category == input$position.3.2 else TRUE)
  })
  
  # Reactive function to update country choices based on selected season--------
  country.choices.3.2 <- reactive({
    df.transfers.network %>%
      filter(season == input$season.3.2) %>%
      distinct(country) %>%
      pull(country)
  })
  
  # Observe the selected season and update the country input--------------------
  observe({
    req(input$season.3.2)
    updateSelectInput(session, "country.3.2", "Country:", 
                      choices = country.choices.3.2())
  })
  
  # Observe the selected country and update the position input------------------
  observe({
    req(input$country.3.2)
    df.position.filtered.3.2 <- df.transfers.network %>%
      filter(season == input$season.3.2, country == input$country.3.2) %>%
      distinct(position_category) %>%
      pull(position_category)
    updateSelectInput(session, "position.3.2", "Position:", 
                      choices = c("All", df.position.filtered.3.2))
  })
  
  
  # Create the directed graph---------------------------------------------------
  directed.graph <- reactive({
    g.directed <- graph_from_data_frame(season.data.3.2()[, c("dealing_club", 
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
  # selected.club reactive variable---------------------------------------------
  observeEvent(input$selectedClub, {
    selected.club.3.2(input$selectedClub)
  })
  
  # Observe the selected club from the directed_graph and update the 
  # selected.club reactive variable---------------------------------------------
  observeEvent(input$selectedClub, {
    graph.selected.club.3.2(input$selectedClub)
  })
  
  # Observe the deselected club from the directed_graph and reset the 
  # selected.club reactive variable---------------------------------------------
  observeEvent(input$directed_graph_unselected, {
    selected.club.3.2(NULL)
  })
  
  # Observe the deselected club from the directed_graph and reset the 
  # selected.club reactive variable---------------------------------------------
  observeEvent(input$directed_graph_unselected, {
    graph.selected.club.3.2(NULL)
  })
  
  # Render the directed graph---------------------------------------------------
  output$directed_graph <- renderVisNetwork({
    directed.graph()
  })
  
  # Render the number of nodes and edges----------------------------------------
  output$graph.summary <- renderUI({
    g.directed <- graph_from_data_frame(season.data.3.2()[, c("dealing_club",
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
    g.directed <- graph_from_data_frame(season.data.3.2()[, c("dealing_club",
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
    club.names <- V(g.directed)$name
    
    df.centrality.measures.clubs <- data.frame(
      club_name = club.names,
      degree = degree.centrality,
      normalized_degree = normalized.degree,
      normalized_closeness = normalized.closeness,
      normalized_betweenness = normalized.betweenness,
      eigenvector = eigenvector,
      normalized_pagerank = normalized.pr
    )
    
    return(df.centrality.measures.clubs)
  })
  
  # Render the centrality measures table----------------------------------------
  output$centrality_table <- renderDataTable({
    df.centrality <- centrality.measures()
    
    if (!is.null(selected.club.3.2()) && selected.club.3.2() != "") {
      df.centrality <- df.centrality %>% 
        filter(club_name == selected.club.3.2())
    }
    
    # Update column names
    colnames(df.centrality) <- gsub("_", " ", colnames(df.centrality))
    colnames(df.centrality) <- tools::toTitleCase(colnames(df.centrality))
    
    df.centrality
  }, options = list(pageLength = 10, autoWidth = TRUE, searching = FALSE))
  
  # Render the degree distribution histogram
  output$degree.distribution <- renderPlot({
    g.directed <- graph_from_data_frame(season.data.3.2()[, c("dealing_club", 
                                                              "club", "name")], 
                                        directed = TRUE)
    degree.subgraph <- degree(g.directed)
    ggplot() +
      geom_histogram(aes(x = degree.subgraph, y = ..count..), 
                     binwidth = 1, fill = "#0066cc", color = "white") +
      scale_x_continuous(breaks = seq(min(degree.subgraph),
                                      max(degree.subgraph), 1)) +
      labs(x = "Degree", y = "Number of clubs",
           title = paste("Degree Distribution of ", input$country.3.2)) +
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
  transfer.network.stats <- reactive({
    g.directed <- graph_from_data_frame(season.data.3.2()[, c("dealing_club", 
                                                              "club", "name")],
                                        directed = TRUE)
    avg.degree <- mean(degree(g.directed))
    clustering.coeff <- transitivity(g.directed, type = "average")
    avg.path.length <- average.path.length(g.directed)
    diameter <- diameter(g.directed)
    
    list(avg_degree = avg.degree,
         clustering_coeff = clustering.coeff,
         avg_path_length = avg.path.length,
         diameter = diameter)
  })
  
  output$transfer.network.summary.headline <- renderText({
    paste("Statistical summary of the transfer network of", 
          input$country.3.2, ":")
  })
  
  output$transfer.network.summary.details <- renderUI({
    stats <- transfer.network.stats()
    tags$ul(
      tags$li(paste("Avg. degree of the directed graph:", 
                    round(stats$avg_degree, 4))),
      tags$li(paste("Clustering coefficient of the directed graph:", 
                    round(stats$clustering_coeff, 4))),
      tags$li(paste("Avg. path length of the directed graph:", 
                    round(stats$avg_path_length, 4))),
      tags$li(paste("Diameter of the directed graph:", 
                    stats$diameter))
    )
  })
  
  output$transfer.network.interpretation <- renderUI({
    stats <- transfer.network.stats()
    paste("Given the selected filters at the top, the average degree of a node 
          is", round(stats$avg_degree, 4), ". This means that on average, a 
          node has", round(stats$avg_degree, 4), "connections to other nodes. 
          The average clustering coefficient of the network is", 
          round(stats$clustering_coeff, 4), ". A high clustering coefficient 
          of 1 means that the nodes in the network tend to form tightly 
          connected groups or clusters. In contrast, a low average clustering 
          coefficient closer to 0 suggests that the nodes in the network are 
          more sparsely connected, and that there are fewer tightly knit 
          communities within the network. The average path length in the network
          is", round(stats$avg_path_length, 4), ". Thus, the average shortest 
          distance between pairs of nodes in the network is", 
          round(stats$avg_path_length, 4), ". The diameter of the network is", 
          stats$diameter, ". Thus, maximum distance between any pair of nodes 
          in the network is", stats$diameter, ".")
  })
  
  # Render the in-degree table--------------------------------------------------
  output$in.degree.table <- renderDataTable({
    g.directed <- graph_from_data_frame(season.data.3.2()[, c("dealing_club", 
                                                              "club", "name")], 
                                        directed = TRUE)
    in.degree <- data.frame(club = V(g.directed)$name, 
                            in.degree = degree(g.directed, mode = "in"))
    
    if (!is.null(selected.club.3.2()) && selected.club.3.2() != "") {
      in.degree <- in.degree %>% filter(club == selected.club.3.2())
    }
    
    # Update column names-------------------------------------------------------
    colnames(in.degree) <- gsub("\\.", " ", colnames(in.degree))
    colnames(in.degree) <- tools::toTitleCase(colnames(in.degree))
    
    in.degree
  }, options = list(pageLength = 10, autoWidth = TRUE, searching = FALSE))
  
  # Render the out-degree table-------------------------------------------------
  output$out.degree.table <- renderDataTable({
    g.directed <- graph_from_data_frame(season.data.3.2()[, c("dealing_club", 
                                                              "club", "name")], 
                                        directed = TRUE)
    out.degree <- data.frame(club = V(g.directed)$name, 
                             out.degree = degree(g.directed, mode = "out"))
    
    if (!is.null(selected.club.3.2()) && selected.club.3.2() != "") {
      out.degree <- out.degree %>% filter(club == selected.club.3.2())
    }
    
    # Update column names-------------------------------------------------------
    colnames(out.degree) <- gsub("\\.", " ", colnames(out.degree))
    colnames(out.degree) <- tools::toTitleCase(colnames(out.degree))
    
    out.degree
  }, options = list(pageLength = 10, autoWidth = TRUE, searching = FALSE))
  
  
  # 4. NETWORK Analysis---------------------------------------------------------
  
  # 4.1 Performance Map---------------------------------------------------------
  
  
  # Filter the data by season and position--------------------------------------
  filtered.season.position <- reactive({
    df.season.position <- df.transfers.network %>%
      filter(season == input$season.4.1)
    
    if (input$position.4.1 != "All") {
      df.season.position <- df.season.position %>% 
        filter(position_category == input$position.4.1)
    }
    
    return(df.season.position)
  })
  
  # Create a summary table of the top 10 dealing countries by selected metric---
  map.4.1 <- reactive({
    metric.col <- case_when(
      input$metric.4.1 == "Highest Average Market Value" ~ "market_value",
      input$metric.4.1 == 
        "Highest Average Performance" ~ "normalized_performance",
      input$metric.4.1 == "Highest Average Fee" ~ "fee"
    )
    
    filtered.season.position() %>% 
      group_by(dealing_country_lat, dealing_country_lon, dealing_country) %>% 
      summarise(avg_metric = mean(!!sym(metric.col)), n_transfers = n(), 
                .groups = 'drop') %>% 
      filter(!is.na(avg_metric)) %>% 
      arrange(desc(avg_metric)) %>%
      head(10)
  })
  
  # create the top countries table----------------------------------------------
  output$top.countries.table <- renderDataTable({
    map.4.1() %>%
      mutate(n_transfers = sapply(dealing_country, function(x) {
        sum(filtered.season.position()$dealing_country == x)
      })) %>%
      mutate(avg_age = sapply(dealing_country, function(x) {
        mean(filtered.season.position()$age[
          filtered.season.position()$dealing_country == x], na.rm = TRUE)
      })) %>%
      mutate(avg_age = round(avg_age, 0), avg_metric = round(avg_metric, 4)) %>%
      select(dealing_country, n_transfers, avg_age, avg_metric) %>%
      rename(Country = dealing_country, No_Transfers = n_transfers, 
             Average_Age = avg_age, !!input$metric.4.1 := avg_metric)
  }, options = list(lengthChange = FALSE))
  
  # create the map--------------------------------------------------------------
  output$football.map.headline <- renderText({
    paste("Top Ten Dealing Countries by", input$metric.4.1, "- Season", 
          input$season.4.1)
  })
  
  output$football.map <- renderPlot({
    ggmap(map, width = 5000, height = 3000) + 
      geom_point(data = map.4.1(), aes(x = dealing_country_lon, 
                                      y = dealing_country_lat, 
                                      size = avg_metric), alpha = 0.6, 
                 color = "#FF5733") +
      scale_size_continuous(range = c(1, 5)) +
      labs(size = paste(input$metric, "(in millions of euros)")) +
      theme_void() +
      theme(plot.title = element_text(size = 16, face = "bold", 
                                      margin = margin(b = 20, t = 30)),
            legend.position = "bottom",
            legend.text = element_text())
  })
  
  # 4.2 Performance index-------------------------------------------------------
  
  season.data.4.2 <- reactive({
    df.transfers.network %>%
      filter(season == input$season.4.2, country == input$country.4.2) %>%
      filter(if (input$position.4.2 != "All") position_category == 
               input$position.4.2 else TRUE)
  })
  
  # Reactive function to update country choices based on selected season--------
  country.choices.4.2 <- reactive({
    df.transfers.network %>%
      filter(season == input$season.4.2) %>%
      distinct(country) %>%
      pull(country)
  })
  
  # Reactive function to update position choices based on 
  # selected season, country, and club------------------------------------------
  position.choices.4.2 <- reactive({
    df.transfers.network %>%
      filter(season == input$season.4.2, country == input$country.4.2, 
             club == input$club.4.2) %>%
      distinct(position_category) %>%
      pull(position_category)
  })
  
  observe({
    updateSelectInput(session, "country.4.2", "Country:", 
                      choices = country.choices.4.2())
  })
  
  observe({
    req(input$country.4.2)
    clubs.filtered.4.2 <- df.transfers.network %>% 
      filter(season == input$season.4.2, country == input$country.4.2) %>% 
      distinct(club) %>% pull(club)
    updateSelectInput(session, "club.4.2", "Club:", 
                      choices = clubs.filtered.4.2)
  })
  
  observe({
    req(input$club.4.2)
    position.filtered.4.2 <- position.choices.4.2()
    updateSelectInput(session, "position.4.2", "Position:", 
                      choices = c("All", position.filtered.4.2))
  })
  
  # Plot directed graph interactive of top performers---------------------------
  df.performers <- reactive({
    season.filtered.4.2 <- season.data.4.2() %>% 
      filter(country == input$country.4.2, club == input$club.4.2)
    
    
    # Filter for top performers based on age and performance score, 
    # and keep only top 20------------------------------------------------------
    df.performers.top <- 
      season.filtered.4.2[order(-season.filtered.4.2$normalized_performance),
                          ][1:10, ]
    
    # Create a new dataframe with only the relevant columns---------------------
    performers.df <- subset(df.performers.top, select=c("dealing_club", "club",
                                                     "name"))
    
    # Create the graph----------------------------------------------------------
    g.perform <- graph_from_data_frame(performers.df, directed = TRUE)
    
    # Remove nodes with the name "NA"-------------------------------------------
    g.perform <- delete.vertices(g.perform, which(V(g.perform)$name == "NA"))
    
    # Convert graph to edges and nodes data frames------------------------------
    df.nodes <- data.frame(id = V(g.perform)$name, label = V(g.perform)$name)
    df.edges <- data.frame(from = get.edgelist(g.perform)[, 1], 
                           to = get.edgelist(g.perform)[, 2],
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
  filtered.data.4.2 <- reactive({
    season.filtered.4.2 <- 
      season.data.4.2() %>% filter(country == input$country.4.2, club ==
                                     input$club.4.2)
    
    # Filter for top performers based on age and performance score, 
    # and keep only top 20------------------------------------------------------
    df.performers.top <- 
      season.filtered.4.2[order(-season.filtered.4.2$normalized_performance),
                          ][1:10, ]
    
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
  output$scatter.plot <- renderPlot({
    filtered.data.4.2() %>%
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
  
  output$performers.table <- renderDataTable({
    df.4.2 <- filtered.data.4.2()
    
    # Capitalize column names and remove "_"------------------------------------
    colnames(df.4.2) <- gsub("_", " ", colnames(df.4.2))
    colnames(df.4.2) <- tools::toTitleCase(colnames(df.4.2))
    
    df.4.2
  },
  options = list(
    searching = FALSE,
    lengthChange = FALSE,
    paging = FALSE,
    pageLength = 10
  ))
  
  # Create club-specific headline for network graph-----------------------------
  output$headline.4.2.1 <- renderText({
    req(input$club)
    paste("Top Ten Transfers to", input$club, "by Normalized Performance Index")
  })
  
  # Create club-specific headline for table-------------------------------------
  output$headline.4.2.2 <- renderText({
    req(input$club)
    paste("Transfer Insights for Top Performing Players of", input$club)
  })
  
  
  # 4.3 Prediction
  

  season.data.4.3 <- reactive({
    df.transfers.network %>% filter(season >= 2017 & season <= 2021)
  })
  
  # Update club input when country changes
  observe({
    req(input$country.4.3)
    clubs_filtered <- season.data.4.3() %>% 
      filter(country == input$country.4.3) %>% select(club)
    updateSelectInput(session, "club.4.3", "Club:", 
                      choices = unique(clubs_filtered))
  })
  
  top.5.pred.dealing.clubs <- reactiveVal()
  selected.club.4.3 <- reactiveVal()
  
  output$prediction.graph <- renderVisNetwork({
    req(input$club.4.3)
    
    season_filtered <- season.data.4.3() %>% 
      select(c("dealing_club", "club", "name"))
    
    # Create directed graph ----------------------------------------------------
    g.season_filtered <- graph_from_data_frame(season_filtered[,
                                                               c("dealing_club", 
                                                                 "club")], 
                                               directed = TRUE)
    
    # Get predicted edges ------------------------------------------------------
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
                                               input$club.4.3, ]
    df.predicted.edges <- df.predicted.edges[df.predicted.edges$dealing_club != 
                                               input$club.4.3, ]
    
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
    top.5.pred.dealing.clubs <- df.predicted.edges[1:5, ]
    top.5.pred.dealing.clubs(top.5.pred.dealing.clubs)
    rownames(top.5.pred.dealing.clubs) <- NULL
    
    # Create directed graph from top.5.pred.dealing.clubs--------------------
    g.top.clubs <- 
      graph_from_data_frame(top.5.pred.dealing.clubs[, c("dealing_club", 
                                                            "club", 
                                                            "transfer_prob")],
                                    directed = TRUE)
    
    # Set edge weights to transfer_probability
    E(g.top.clubs)$weight <- top.5.pred.dealing.clubs$transfer_prob
    
    # Set vertex colors for chosen club and dealing clubs
    V(g.top.clubs)$color <- 
      ifelse(V(g.top.clubs)$name == input$club.4.3, "red", "blue")
    
    # Create nodes data frame---------------------------------------------------
    df.nodes <- data.frame(id = V(g.top.clubs)$name, 
                        label = V(g.top.clubs)$name, 
                        color = V(g.top.clubs)$color)
    
    # Add the transfer_prob as the label for the edges--------------------------
    df.edges <- data.frame(from = get.edgelist(g.top.clubs)[, 1],
                        to = get.edgelist(g.top.clubs)[, 2],
                        label = paste(round(E(g.top.clubs)$weight, digits = 2)),
                        value = E(g.top.clubs)$weight)
    
    # Create color gradient-----------------------------------------------------
    edge.color.gradient <- colorRampPalette(c("yellow", "orange", "red"))
    
    # Determine number of unique edge weights-----------------------------------
    num.unique.weights <- length(unique(E(g.top.clubs)$weight))
    
    # Generate color vector based on edge weights-------------------------------
    edge.colors <- edge.color.gradient(num.unique.weights)
    
    # Add 'color' column to the edges data frame--------------------------------
    df.edges$color <- edge.colors[as.numeric(cut(E(g.top.clubs)$weight, 
                                                 breaks = num.unique.weights))]
    
    # Create visNetwork graph---------------------------------------------------
    visNetwork(df.nodes, df.edges) %>%
      visEdges(smooth = TRUE,
               width = 1,
               label = paste(round(E(g.top.clubs)$weight, digits = 2)), 
               arrows = list(to = list(enabled = TRUE)),
               color = list(color = df.edges$color),
               font = list(size = 26)) %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visPhysics(solver = "barnesHut",
                 barnesHut = list(gravitationalConstant = -25000,
                                  centralGravity = 0.5)) %>%
      visInteraction(zoomView = FALSE,
                     dragView = FALSE)
  })
  
  table.title <- reactive({
    if (!is.null(selected.club.4.3()) && selected.club.4.3() %in% 
        top.5.pred.dealing.clubs()$dealing_club) {
      paste("Top Players in", selected.club.4.3())
    } else {
      "Top Players in 5 Predicted Dealing Clubs"
    }
  })
  
  output$table.title <- renderUI({
    h3(table.title())
  })
  
  # Observe the selected club from the prediction.graph and update the 
  # selected.club.4.3 reactive variable---------------------------------------------
  observeEvent(input$prediction.graph.selected, {
    selected.club.4.3(input$prediction.graph.selected)
  })
  
  output$player.table <- renderTable({
    
    top.5.clubs <- top.5.pred.dealing.clubs()$dealing_club
    player.data <- season.data.4.3() %>%
      filter(dealing_club %in% top.5.clubs) %>%
      filter(if (input$position.4.3 != "All") position_category == 
               input$position.4.3 else TRUE) %>%
      select(name, age, position, nationality, normalized_performance, 
             market_value, fee, dealing_club) %>%
      arrange(desc(normalized_performance))
    
    # Filter player data based on the selected club or 
    # show data for all top 5 clubs---------------------------------------------
    if (!is.null(selected.club.4.3()) && selected.club.4.3() %in% top.5.clubs) {
      player.data <- player.data %>% filter(dealing_club == selected.club.4.3())
    }
    
    
    # Rename columns------------------------------------------------------------
    player.data <- player.data %>%
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
    player.data$`Market Value` <- format(player.data$`Market Value`, 
                                         nsmall = 0, scientific = FALSE)
    player.data$`Transfer Fee` <- format(player.data$`Transfer Fee`, 
                                         nsmall = 0, scientific = FALSE)
    
    # Display the selected number of rows---------------------------------------
    player.data <- head(player.data, n = input$num.rows)
    
    player.data
  })
  
# End of server-----------------------------------------------------------------
}

# Run the application-----------------------------------------------------------
shinyApp(ui = ui, server = server)
