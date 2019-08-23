library(shiny)
library(tidyverse)
library(data.table)
library(ggthemes)

table <- fread("data/nba_rating.csv")
table <- table %>%
  mutate(TEAM_NAME = as.character(TEAM_NAME)) %>%
  mutate(GAME_DATE = as.Date(GAME_DATE))

 mean <- 107.9268

ui <- fluidPage(
  titlePanel("NBA app ratings"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("team", "Select team:",
                  choices = list("Atlanta Hawks",
                                 "Boston Celtics",
                                 "Brooklyn Nets",
                                 "Charlotte Hornets",
                                 "Chicago Bulls",
                                 "Cleveland Cavaliers",
                                 "Dallas Mavericks",
                                 "Denver Nuggets",
                                 "Detroit Pistons",
                                 "Golden State Warriors",
                                 "Houston Rockets",
                                 "Indiana Pacers",
                                 "LA Clippers",
                                 "Los Angeles Lakers",
                                 "Memphis Grizzlies",
                                 "Miami Heat",
                                 "Milwaukee Bucks",
                                 "Minnesota Timberwolves",
                                 "New Orleans Pelicans",
                                 "New York Knicks",
                                 "Oklahoma City Thunder",
                                 "Orlando Magic",
                                 "Philadelphia 76ers",
                                 "Phoenix Suns",
                                 "Portland Trail Blazers",
                                 "Sacramento Kings",
                                 "San Antonio Spurs",
                                 "Toronto Raptors",
                                 "Utah Jazz",
                                 "Washington Wizards"),
                  selected = "Toronto Raptors"),
      
      selectInput("rating", "Select rating:",
                  choices = list("E_OFF_RATING",
                                 "E_DEF_RATING",
                                 "E_NET_RATING"),
                  selected = "E_NET_RATING"),
      
      dateRangeInput("time", "Select date range",
                     start = "2018-10-16",
                     end = "2019-04-10",
                     min = "2018-10-16",
                     max = "2019-04-10")
    ),
    
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("Plot", plotOutput("dallas")),
        tabPanel("Table", dataTableOutput("data"))
      )
      ) 
  )

)

# Define server logic ----
server <- function(input, output) {
  
  nba <- reactive({
    args <- switch(input$team,
                   "Atlanta Hawks" = "Atlanta Hawks",
                   "Boston Celtics" = "Boston Celtics",
                   "Brooklyn Nets" = "Brooklyn Nets",
                   "Charlotte Hornets" = "Charlotte Hornets",
                   "Chicago Bulls" = "Chicago Bulls",
                   "Cleveland Cavaliers" = "Cleveland Cavaliers",
                   "Dallas Mavericks" = "Dallas Mavericks",
                   "Denver Nuggets" = "Denver Nuggets",
                   "Detroit Pistons" = "Detroit Pistons",
                   "Golden State Warriors" = "Golden State Warriors",
                   "Houston Rockets" = "Houston Rockets",
                   "Indiana Pacers" = "Indiana Pacers",
                   "LA Clippers" = "LA Clippers",
                   "Los Angeles Lakers" = "Los Angeles Lakers",
                   "Memphis Grizzlies" = "Memphis Grizzlies",
                   "Miami Heat" = "Miami Heat",
                   "Milwaukee Bucks" = "Milwaukee Bucks",
                   "Minnesota Timberwolves" = "Minnesota Timberwolves",
                   "New Orleans Pelicans" = "New Orleans Pelicans",
                   "New York Knicks" = "New York Knicks",
                   "Oklahoma City Thunder" = "Oklahoma City Thunder",
                   "Orlando Magic" = "Orlando Magic",
                   "Philadelphia 76ers" = "Philadelphia 76ers",
                   "Phoenix Suns" = "Phoenix Suns",
                   "Portland Trail Blazers" = "Portland Trail Blazers",
                   "Sacramento Kings" = "Sacramento Kings",
                   "San Antonio Spurs" = "San Antonio Spurs",
                   "Toronto Raptors" = "Toronto Raptors",
                   "Utah Jazz" = "Utah Jazz",
                   "Washington Wizards" = "Washington Wizards")

    table %>%
      filter(TEAM_NAME == args) %>%
      filter(GAME_DATE >= input$time[1] & GAME_DATE <= input$time[2])
  })
  
  output$dallas <- renderPlot({
    data <- nba()
    
    ggplot(data,
           aes_string(data$GAME_DATE, y = input$rating)) +
      geom_line(color = ifelse(data[,input$rating] > mean, data$col1, "black") ) +
      geom_point() +
      theme_tufte() +
      labs(title = paste0(input$team," ",
                          input$rating, " ",
                          "in season 2018-19")) +
      theme(axis.title.x = element_blank(),
            plot.title = element_text(hjust = 0.5))
  })
  
  output$data <- renderDataTable({
    nba()
  })
    
}

# Run the app ----
shinyApp(ui = ui, server = server)
