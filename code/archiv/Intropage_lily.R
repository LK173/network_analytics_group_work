library(shiny)

ui <- fluidPage(
  tags$head(
    tags$style(
      HTML("
        .jumbotron {
          background-color: #f8f9fa;
          padding-top: 20px;
          padding-bottom: 20px;
          margin-bottom: 0;
        }
        
        .text-center {
          text-align: center;
        }
      ")
    )
  ),
  
  # Jumbotron section with header and text
  tags$div(
    class = "jumbotron text-center",
    tags$h1("Welcome to Football Talent Scout Europe!"),
    tags$p("Welcome to Football Talent Scout Europe, the ultimate app for Football Talent Scouts. Our app provides a comprehensive overview of past football transfers in Europe, including information on market value, transfer fees, receiving and trading clubs, and more.
Our app offers a variety of functionalities that make it an essential tool for any Football Talent Scout. The descriptive statistics tab offers general summary statistics about the data set, with relevant filters such as season, year, and country. With our network exploration tab, you can view an interactive map that displays the movement of football transfers over time. The third tab, network analysis, allows users to interactively explore the network and predict potential new trades based on historical data.
Our app also provides a wealth of information to help talent scouts identify the most important factors to focus on when scouting for new talent. For example, you can easily determine which countries are most important for identifying new players.
While our network analysis is based on a reduced data set, our app provides users with valuable insights into past football transfers and offers predictions for potential new trades. It is important to note that our data set is not automatically updated to the current date, but we are constantly working to improve our app and provide the most up-to-date information possible.
Overall, our app is structured to provide a seamless user experience. Whether you are an experienced talent scout or just starting out, Football Talent Scout Europe is the perfect app to help you stay ahead of the competition.
Thank you for choosing Football Talent Scout Europe. We hope our app will help you discover the next generation of European football stars!")
  )
)

server <- function(input, output, session) {}

shinyApp(ui, server)
