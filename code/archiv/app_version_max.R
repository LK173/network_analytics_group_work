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

             # 3.2 Subpage
             tabPanel(title = "Subpage 3.2 Details about the whole football Transfer Network",
                      fluidPage(
                        titlePanel("Overview about the football Transfers Network by country"),
                        
                        sidebarLayout(
                          sidebarPanel(
                            h4("Choose a season, country, club, position"),
                            selectInput("season_3_2", "Select a season:",
                                        choices = 2004:2021, selected = 2021, width = "100%"),
                            uiOutput("country_ui_3_2"),
                            uiOutput("position_ui_3_2")
                          ),
                          
                          mainPanel(
                            visNetworkOutput("directed_graph"),
                            verbatimTextOutput("graph_summary"),
                            verbatimTextOutput("graph_measures"),
                            dataTableOutput("centrality_table"),
                            plotOutput("degree_distribution"),
                            fluidRow(
                              column(6, dataTableOutput("in_degree_table")),
                              column(6, dataTableOutput("out_degree_table"))
                            )
                          )
                        )
                      )
                    )
             ),
  
  # NETWORK ANALYSIS PAGE ----
  navbarMenu(title = "Network Analysis",
             tabPanel(title = "4.1 Football Transfers Map",
                      fluidPage(
                        titlePanel("Football Transfers Map by several metrics"),
                        sidebarLayout(
                          sidebarPanel(
                            h4("Filters"),
                            selectInput("season_4_1", "Select a season:",
                                        choices = 2004:2021, selected = 2021, width = "100%"),
                            selectInput("metric_4_1", "Select metric:",
                                        c("Highest Average Market Value", "Highest Average Performance", "Lowest Average Fee")),
                            selectInput("position_4_1", "Select position:",
                                        c("All", "Goalkeeper", "Defender", "Midfielder", "Forward"))
                          ),
                          
                          mainPanel(
                            plotOutput("football_map", width = "80%", height = "800px"),
                            dataTableOutput("top_countries_table")
                          )
                        )
                      )
                    ),
             
             tabPanel(title = "4.2 Football Transfers by performance index",
                      fluidPage(
                        titlePanel("Football Transfers Network Exploration by self-developed performance index "),
                        sidebarLayout(
                          sidebarPanel(
                            h4("Choose a season, country, club, position"),
                            selectInput("season_4_2", "Select a season:",
                                        choices = 2004:2021, selected = 2021, width = "100%"),
                            uiOutput("country_ui_4_2"),
                            uiOutput("club_ui_4_2"),
                            uiOutput("position_ui_4_2")
                          ),
                          
                          mainPanel(
                            tags$h4("Top 10 transfers to chosen club by normalized performance index",
                                    style = "font-weight: bold; font-size: 24px; text-align: center"),
                            fluidRow(
                              column(6, visNetworkOutput("performers_graph")),
                              column(6, plotOutput("scatter_plot"))
                            ),
                            tags$h4("Detailed informations about the transfers displayed above",
                                    style = "font-weight: bold; font-size: 24px; text-align: center"),
                            dataTableOutput("performers_table")
                          )
                        )
                      )
             ),
             
             tabPanel(title = "4.3 Football Transfers Predictions (Based on Season 2017-2021)",
                      fluidPage(
                        titlePanel("Football Transfers Predictions (Based on Season 2017-2021)"),
                        
                        sidebarLayout(
                          sidebarPanel(
                            h4("Choose a country, club, position and number of players to show"),
                            selectInput("country_4_3", "Country:", unique(data$country)),
                            uiOutput("club_ui_4_3"),
                            selectInput("position_4_3", "Position:", c("All", "Goalkeeper", "Defense", "Midfield", "Attack")),
                            selectInput("num_rows", "Number of players to display:", choices = c(10, 20, 30, 50))
                          ),
                          
                          mainPanel(
                            h4("Next Possible Transfers"),
                            visNetworkOutput("prediction_graph", width = "70%", height = "500px"),
                            uiOutput("table_title"),
                            tableOutput("player_table")
                          )
                        )
                      )
             )
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

  # Network Exploration
  
  
  # 3.2 Overview about the football Transfers Network by country
  
  selected.club_3_2 <- reactiveVal()
  graph_selected_club_3_2 <- reactiveVal()
  
  
  season.data_3_2 <- reactive({
    data %>%
      filter(season == input$season_3_2) %>%
      filter(country == input$country) %>%
      filter(if (input$position != "All") position_category == input$position else TRUE)
  })
  
  # Reactive function to update country choices based on selected season
  country.choices_3_2 <- reactive({
    data %>%
      filter(season == input$season_3_2) %>%
      distinct(country) %>%
      pull(country)
  })
  
  
  # Update the country input based on the selected season
  output$country_ui_3_2 <- renderUI({
    country.filtered_3_2 <- data %>% filter(season == input$season_3_2) %>% distinct(country) %>% pull(country)
    selectInput("country", "Country:", choices = country.filtered_3_2)
  })
  
  output$position_ui_3_2 <- renderUI({
    req(input$country) # Ensure input$country is available before proceeding
    position.filtered_3_2 <- data %>%
      filter(season == input$season_3_2, country == input$country) %>%
      distinct(position_category) %>%
      pull(position_category)
    selectInput("position", "Position:", choices = c("All", position.filtered_3_2))
  })
  
  
  # Create the directed graph
  directed.graph <- reactive({
    g.directed <- graph_from_data_frame(season.data_3_2()[, c("dealing_club", "club", "name")], directed = TRUE)
    df.nodes <- data.frame(id = V(g.directed)$name, label = V(g.directed)$name)
    df.edges <- data.frame(from = get.edgelist(g.directed)[, 1], to = get.edgelist(g.directed)[, 2],
                           label = E(g.directed)$name)
    
    visNetwork(df.nodes, df.edges, width = "100%", height = "600px") %>%
      visEdges(arrows = "to", font = list(size = 10, face = "sans-serif"), label = df.edges$label) %>%
      visNodes(
        color = list(
          highlight = list(
            background = "red",
            border = "red"
          )
        )
      ) %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visEvents(select = "function(properties) { Shiny.setInputValue('selectedClub', properties.nodes[0]); }") %>%
      visEvents(deselectNode = 
                  "function() { Shiny.setInputValue('directed_graph_unselected', true, {priority: 'event'}); }")
  })
  
  
  # Observe the selected club from the directed_graph and update the selected_club reactive variable
  observeEvent(input$selectedClub, {
    selected.club_3_2(input$selectedClub)
  })
  
  # Observe the selected club from the directed_graph and update the selected_club reactive variable
  observeEvent(input$selectedClub, {
    graph_selected_club_3_2(input$selectedClub)
  })
  
  # Observe the deselected club from the directed_graph and reset the selected_club reactive variable
  observeEvent(input$directed_graph_unselected, {
    selected.club_3_2(NULL)
  })
  
  # Observe the deselected club from the directed_graph and reset the selected_club reactive variable
  observeEvent(input$directed_graph_unselected, {
    graph_selected_club_3_2(NULL)
  })
  
  # Render the directed graph
  output$directed_graph <- renderVisNetwork({
    directed.graph()
  })
  
  # Render the number of nodes and edges
  output$graph_summary <- renderText({
    g.directed <- graph_from_data_frame(season.data_3_2()[, c("dealing_club", "club", "name")], directed = TRUE)
    num_nodes <- gorder(g.directed)
    num_edges <- gsize(g.directed)
    paste("Number of nodes:", num_nodes, "\nNumber of edges:", num_edges)
  })
  # Calculate centrality measures for the filtered graph
  centrality.measures <- reactive({
    g.directed <- graph_from_data_frame(season.data_3_2()[, c("dealing_club", "club", "name")], directed = TRUE)
    degree.centrality <- degree(g.directed, mode = "all")
    closeness <- closeness(g.directed)
    betweenness <- round(betweenness(g.directed), 4)
    eigenvector <- round(evcent(g.directed)$vector, 4)
    pr <- page_rank(g.directed)$vector
    pr_norm <- scale(pr)
    
    # Normalized centrality measures for better interpretations---------------------
    normalized.degree <- round(degree(g.directed, normalized = TRUE), 4)
    normalized.closeness <- round(closeness(g.directed, normalize = TRUE), 4)
    normalized.betweenness <- round(betweenness(g.directed, normalize = TRUE), 4)
    normalized.pr <- round(scale(pr,center = min(pr), 
                                 scale = max(pr) - min(pr)),4)
    
    # Create a dataframe with centrality measures
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
  
  # Render the centrality measures table
  output$centrality_table <- renderDataTable({
    df.centrality <- centrality.measures()
    
    if (!is.null(selected.club_3_2()) && selected.club_3_2() != "") {
      df.centrality <- df.centrality %>% filter(club_name == selected.club_3_2())
    }
    df.centrality
  }, options = list(searching = FALSE))
  
  # Render the degree distribution histogram
  output$degree_distribution <- renderPlot({
    g.directed <- graph_from_data_frame(season.data_3_2()[, c("dealing_club", "club", "name")], directed = TRUE)
    degree.subgraph <- degree(g.directed)
    hist(degree.subgraph,
         main = paste("Degree Distribution of ", input$country),
         xlab = "Degree", ylab = "Number of clubs", col = "blue", border = "white")
  })
  
  # Render the statistical measures
  output$graph_measures <- renderText({
    g.directed <- graph_from_data_frame(season.data_3_2()[, c("dealing_club", "club", "name")], directed = TRUE)
    avg.degree <- mean(degree(g.directed))
    clustering.coeff <- transitivity(g.directed, type = "average")
    avg.path.length <- average.path.length(g.directed)
    
    paste("Statistical summary of the transfer network of", input$country, ":",
          "\nAvg. degree of the directed graph:", round(avg.degree,4),
          "\nClustering coefficient of the directed graph:", round(clustering.coeff,4),
          "\nAvg. path length of the directed graph:", round(avg.path.length,4))
  })
  
  # Render the in-degree table
  output$in_degree_table <- renderDataTable({
    g.directed <- graph_from_data_frame(season.data_3_2()[, c("dealing_club", "club", "name")], directed = TRUE)
    in.degree <- data.frame(club = V(g.directed)$name, in.degree = degree(g.directed, mode = "in"))
    
    if (!is.null(selected.club_3_2()) && selected.club_3_2() != "") {
      in.degree <- in.degree %>% filter(club == selected.club_3_2())
    }
    
    in.degree
  }, options = list(searching = FALSE))
  
  # Render the out-degree table
  output$out_degree_table <- renderDataTable({
    g.directed <- graph_from_data_frame(season.data_3_2()[, c("dealing_club", "club", "name")], directed = TRUE)
    out.degree <- data.frame(club = V(g.directed)$name, out.degree = degree(g.directed, mode = "out"))
    
    if (!is.null(selected.club_3_2()) && selected.club_3_2() != "") {
      out.degree <- out.degree %>% filter(club == selected.club_3_2())
    }
    
    out.degree
  }, options = list(searching = FALSE))
  
  
  # 4. NETWORK Analysis
  
  # 4.1 Performance Map
 
  
  # filter the data by season and position
  df_filtered <- reactive({
    df <- data %>%
      filter(season == input$season_4_1)
    
    if (input$position_4_1 != "All") {
      df <- df %>% filter(position_category == input$position_4_1)
    }
    
    return(df)
  })
  
  # create a summary table of the top 10 dealing countries by selected metric
  df_map <- reactive({
    metric_col <- case_when(
      input$metric_4_1 == "Highest Average Market Value" ~ "market_value",
      input$metric_4_1 == "Highest Average Performance" ~ "normalized_performance",
      input$metric_4_1 == "Lowest Average Fee" ~ "fee"
    )
    
    df_filtered() %>% 
      group_by(dealing_country_lat, dealing_country_lon, dealing_country) %>% 
      summarise(avg_metric = mean(!!sym(metric_col)), n_transfers = n(), .groups = 'drop') %>% 
      filter(!is.na(avg_metric)) %>% 
      arrange(ifelse(input$metric_4_1 == "Lowest Average Fee", avg_metric, desc(avg_metric))) %>%
      head(10)
  })
  
  # create the top countries table
  output$top_countries_table <- renderDataTable({
    df_map() %>%
      mutate(n_transfers = sapply(dealing_country, function(x) {
        sum(df_filtered()$dealing_country == x)
      })) %>%
      mutate(avg_age = sapply(dealing_country, function(x) {
        mean(df_filtered()$age[df_filtered()$dealing_country == x], na.rm = TRUE)
      })) %>%
      select(dealing_country, n_transfers, avg_age, avg_metric) %>%
      rename(Country = dealing_country, No_Transfers = n_transfers, Average_Age = avg_age, !!input$metric_4_1 := avg_metric)
  })
  
  # create the map
  output$football_map <- renderPlot({
    ggmap(map, width = 5000, height = 3000) + 
      geom_point(data = df_map(), aes(x = dealing_country_lon, y = dealing_country_lat, size = avg_metric), alpha = 0.6, color = "#FF5733") +
      scale_size_continuous(range = c(1, 5)) +
      labs(size = paste(input$metric, "(in millions of euros)"),
           title = paste("Top Ten Dealing Countries by", input$metric_4_1, "- Season", input$season_4_1)) +
      theme_void() +
      theme(plot.title = element_text(size = 16, face = "bold", margin = margin(b = 20, t = 30)),
            legend.position = "bottom",
            legend.text = element_text())
  })
  
  # 4.2 Performance index
  
  season.data_4_2 <- reactive({
    data %>%
      filter(season == input$season_4_2) %>%
      filter(country == input$country) %>%
      filter(if (input$position != "All") position_category == input$position else TRUE)
  })
  
  # Reactive function to update country choices based on selected season
  
  country.choices_4_2 <- reactive({
    data %>%
      filter(season == input$season_4_2) %>%
      distinct(country) %>%
      pull(country)
  })
  
  # Reactive function to update position choices based on selected season, country, and club
  position.choices_4_2 <- reactive({
    data %>%
      filter(season == input$season_4_2, country == input$country, club == input$club) %>%
      distinct(position_category) %>%
      pull(position_category)
  })

  
  # Update the position input based on the selected season, country, and club
  output$position_ui_4_2 <- renderUI({
    req(input$club) # Ensure input$club is available before proceeding
    position.filtered_4_2 <- data %>%
      filter(season == input$season_4_2, country == input$country, club == input$club) %>%
      distinct(position_category) %>%
      pull(position_category)
    selectInput("position", "Position:", choices = c("All", position.filtered_4_2))
  })
  
  # Update the country input based on the selected season
  output$country_ui_4_2 <- renderUI({
    countries.filtered_4_2 <- data %>% filter(season == input$season_4_2) %>% distinct(country) %>% pull(country)
    selectInput("country", "Country:", choices = countries.filtered_4_2)
  })
  
  output$club_ui_4_2 <- renderUI({
    req(input$country) # Ensure input$country is available before proceeding
    clubs.filtered_4_2 <- data %>% filter(season == input$season_4_2, country == input$country) %>% distinct(club) %>% pull(club)
    selectInput("club", "Club:", choices = clubs.filtered_4_2)
  })
  
  # Plot directed graph interactive of top performers
  df.performers <- reactive({
    season.filtered_4_2 <- season.data_4_2() %>% filter(country == input$country, club == input$club)
    
    # Filter for top performers based on age and performance score, and keep only top 20
    performers.top <- season.filtered_4_2[order(-season.filtered_4_2$normalized_performance), ][1:10, ]
    
    # Create a new dataframe with only the relevant columns
    performers.df <- subset(performers.top, select=c("dealing_club", "club", "name"))
    
    # Create the graph
    g.perform <- graph_from_data_frame(performers.df, directed = TRUE)
    
    # Remove nodes with the name "NA"
    g.perform <- delete.vertices(g.perform, which(V(g.perform)$name == "NA"))
    
    # Convert graph to edges and nodes data frames
    df.nodes <- data.frame(id = V(g.perform)$name, label = V(g.perform)$name)
    df.edges <- data.frame(from = get.edgelist(g.perform)[, 1], to = get.edgelist(g.perform)[, 2],
                           label = E(g.perform)$name)
    
    # Create a visNetwork object instead of using the base igraph plot
    visNetwork(df.nodes, df.edges, width = "100%", height = "600px") %>%
      visEdges(arrows = "to", font = list(size = 10, face = "sans-serif"), label = df.edges$label) %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visLayout(randomSeed = 42)
  })
  
  output$performers_graph <- renderVisNetwork({
    df.performers()
  })
  
  # Create a table of detailed informations about top perfomers
  filtered.data_4_2 <- reactive({
    season.filtered_4_2 <- season.data_4_2() %>% filter(country == input$country, club == input$club)
    
    # Filter for top performers based on age and performance score, and keep only top 20
    df.performers.top <- season.filtered_4_2[order(-season.filtered_4_2$normalized_performance), ][1:10, ]
    
    # Create a new dataframe with only the relevant columns
    df.performers <- subset(df.performers.top, select=c("season", "country", "club", "dealing_club",
                                                        "name", "age", "position_category", "nationality",
                                                        "normalized_performance"))
    
    # Reset the row names/index
    rownames(df.performers) <- NULL
    df.performers
  })
  
  # Create a scatter plot of market values by position and age based on the filtered data
  output$scatter_plot <- renderPlot({
    filtered.data_4_2() %>%
      ggplot(aes(x = age, y = normalized_performance, color = position_category)) +
      geom_point(size = 2, alpha = 0.6) +
      scale_color_brewer(palette = "Set1") +
      scale_y_continuous(labels = scales::comma) +
      labs(x = "Age", y = "Normalized Performance",
           title = "Performance by Position and Age")+
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
            axis.title = element_text(face = "bold"),
            legend.title = element_text(face = "bold"),
            legend.text = element_text(face = "bold"))
  })
  output$performers_table <- renderDataTable({
    filtered.data_4_2() 
  },
  options = list(
    searching = FALSE,
    lengthChange = FALSE,
    paging = FALSE,
    pageLength = 10
  ))
  
  # 4.3 Prediction
  

  season.data_4_3 <- reactive({
    data %>% filter(season >= 2011 & season <= 2021)
  })
  
  
  output$club_ui_4_3 <- renderUI({
    clubs_filtered <- season.data_4_3() %>% filter(country == input$country_4_3) %>% select(club)
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
    
    df.predicted.edges$transfer_weight <- m.predicted.edges[m.predicted.edges > 0]
    
    df.predicted.edges$dealing_club <- 
      rownames(m.predicted.edges)[df.predicted.edges$dealing_club]
    
    df.predicted.edges$club <- 
      rownames(m.predicted.edges)[df.predicted.edges$club]
    
    # Remove self-loops where dealing club and club are the same ---------------
    df.predicted.edges <- subset(df.predicted.edges, dealing_club != club)
    
    # Normalize the transfer_weight column -------------------------------------
    max.transfer.weight <- max(df.predicted.edges$transfer_weight)
    
    df.predicted.edges$transfer_prob <- round(df.predicted.edges$transfer_weight / 
                                                max.transfer.weight, 2)
    
    # Filter edges to only include those with selected club as target --------------
    df.predicted.edges <- df.predicted.edges[df.predicted.edges$club == 
                                               input$club, ]
    df.predicted.edges <- df.predicted.edges[df.predicted.edges$dealing_club != 
                                               input$club, ]
    
    # Keep only the relevant columns -----------------------------------------------
    df.predicted.edges <- subset(df.predicted.edges,
                                 select=c("dealing_club", "club", "transfer_prob"))
    
    # Remove the index column by setting row.names to NULL -------------------------
    row.names(df.predicted.edges) <- NULL
    
    # Sort edges by transfer probability -------------------------------------------
    df.predicted.edges <- df.predicted.edges[order(df.predicted.edges$transfer_prob, 
                                                   decreasing = TRUE), ]
    df.top.5.pred.dealing.clubs <- df.predicted.edges[1:5, ]
    df_top_5_pred_dealing_clubs(df.top.5.pred.dealing.clubs)
    rownames(df.top.5.pred.dealing.clubs) <- NULL
    
    # Create directed graph from df.top.5.pred.dealing.clubs
    g.test <- graph_from_data_frame(df.top.5.pred.dealing.clubs[, c("dealing_club", "club", "transfer_prob")],
                                    directed = TRUE)
    
    # Set edge weights to transfer_probability
    E(g.test)$weight <- df.top.5.pred.dealing.clubs$transfer_prob
    
    # Set vertex colors for chosen club and dealing clubs
    V(g.test)$color <- ifelse(V(g.test)$name == input$club, "red", "blue")
    
    # Create nodes data frame
    nodes <- data.frame(id = V(g.test)$name, 
                        label = V(g.test)$name, 
                        color = V(g.test)$color)
    
    # Add the transfer_prob as the label for the edges
    edges <- data.frame(from = get.edgelist(g.test)[, 1],
                        to = get.edgelist(g.test)[, 2],
                        label = paste(round(E(g.test)$weight, digits = 2)),
                        value = E(g.test)$weight)
    
    # Create color gradient
    edge.color.gradient <- colorRampPalette(c("yellow", "orange", "red"))
    
    # Determine number of unique edge weights
    num.unique.weights <- length(unique(E(g.test)$weight))
    
    # Generate color vector based on edge weights
    edge.colors <- edge.color.gradient(num.unique.weights)
    
    # Add 'color' column to the edges data frame
    edges$color <- edge.colors[as.numeric(cut(E(g.test)$weight, breaks = num.unique.weights))]
    
    # Create visNetwork graph
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
    if (!is.null(selected_club()) && selected_club() %in% df_top_5_pred_dealing_clubs()$dealing_club) {
      paste("Top Players in", selected_club())
    } else {
      "Top Players in 5 Predicted Dealing Clubs"
    }
  })
  
  output$table_title <- renderUI({
    h4(table_title())
  })
  
  # Observe the selected club from the prediction_graph and update the selected_club reactive variable
  observeEvent(input$prediction_graph_selected, {
    selected_club(input$prediction_graph_selected)
  })
  
  output$player_table <- renderTable({
    
    top_5_clubs <- df_top_5_pred_dealing_clubs()$dealing_club
    player_data <- season.data_4_3() %>%
      filter(dealing_club %in% top_5_clubs) %>%
      filter(if (input$position_4_3 != "All") position_category == input$position_4_3 else TRUE) %>%
      select(name, age, position, nationality, normalized_performance, market_value, fee, dealing_club) %>%
      arrange(desc(normalized_performance))
    
    # Filter player data based on the selected club or show data for all top 5 clubs
    if (!is.null(selected_club()) && selected_club() %in% top_5_clubs) {
      player_data <- player_data %>% filter(dealing_club == selected_club())
    }
    
    
    # Rename columns
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
    
    # Remove decimals from market_value and fee columns
    player_data$`Market Value` <- format(player_data$`Market Value`, nsmall = 0, scientific = FALSE)
    player_data$`Transfer Fee` <- format(player_data$`Transfer Fee`, nsmall = 0, scientific = FALSE)
    
    # Display the selected number of rows
    player_data <- head(player_data, n = input$num_rows)
    
    player_data
  })
  
# End of server    
}

# Run the application 
shinyApp(ui = ui, server = server)
