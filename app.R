library(shiny)
library(tidyverse)
library(data.table)
library(ggthemes)

table <- fread("F:/R_20190720/Shiny/nba/data/dallas_rating.csv")
table <- table %>%
  mutate(TEAM_NAME = as.character(TEAM_NAME)) %>%
  mutate(GAME_DATE = as.Date(GAME_DATE))
  # filter(GAME_DATE >= "2019-01-01")
# source("helpers.R")

ui <- fluidPage(
  titlePanel("NBA app ratings"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("team", "Select team:",
                  choices = list("Dallas Maverikcs")),
      
      selectInput("rating", "Select rating:",
                  choices = list("E_OFF_RATING",
                                 "E_DEF_RATING",
                                 "E_NET_RATING"),
                  selected = "E_NET_RATING"),
      
      dateRangeInput("time", "Select date range",
                     start = "2018-10-17",
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

  output$dallas <- renderPlot({
    ggplot(table,
           aes_string(x = table$GAME_DATE, y = input$rating)) +
      geom_line() +
      theme_tufte()
  })
  
  
  
  output$data <- renderDataTable({
    table[table$GAME_DATE >= input$time[1] & table$GAME_DATE <= input$time[2],]
  })
    
}

# Run the app ----
shinyApp(ui = ui, server = server)
