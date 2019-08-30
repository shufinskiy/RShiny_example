library(shiny)
library(tidyverse)
library(data.table)
library(ggthemes)
library(zoo)
library(plotly)

table <- fread("data/nba_rating.csv")
table <- table %>%
  mutate(TEAM_NAME = as.character(TEAM_NAME)) %>%
  mutate(GAME_DATE = as.Date(GAME_DATE))

mean <- 107.9268

source("helpers.R")

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
      
      br(),
      br(),
      selectInput("rating", "Select rating:",
                  choices = list("E_OFF_RATING",
                                 "E_DEF_RATING",
                                 "E_NET_RATING",
                                 "OFF & DEF"),
                  selected = "E_NET_RATING"),

      checkboxInput("roll", "Game Roling", value = FALSE),
      
      numericInput("number", "N-Game Rolling:",
                   value = 10, min = 1, max = 82, step = 1),
      helpText("N-Game Rolling works only\nwith checkbox 'Game Roling' enabled"),
      
      br(),
      br(),
      
      dateRangeInput("time", "Select date range",
                     start = "2018-10-16",
                     end = "2019-04-10",
                     min = "2018-10-16",
                     max = "2019-04-10"),
      
      br(),
      br(),
      
      downloadButton("download", "Save the data"),
      
      br(),
      br(),
      
      downloadButton("downloadplot", "Save the plot")
      
    ),
    
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("Plot", plotOutput("dallas")),
        tabPanel("Table", dataTableOutput("data")),
        tabPanel("Interactive plot", plotlyOutput("plotly_chart",
                                                  width = "auto",
                                                  height = "600px"))
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
    if(input$roll) {
      data <- rollmean_func(nba(), input$number)
    } else {
      data <- nba()
    }
    
    if(input$rating %in% c("E_OFF_RATING",
                           "E_DEF_RATING",
                           "E_NET_RATING")) {
      ggplot(data,
             aes_string(data$GAME_DATE, y = input$rating)) +
        geom_line(color = ifelse(data[,input$rating] > mean, data$col1, "black") ) +
        geom_point() +
        theme_tufte() +
        labs(title = paste0(ifelse(input$roll, 
                                   paste0(input$number,"-Game Rolling "), 
                                   ""), 
                            input$team, " ",
                            switch(input$rating,
                                   "E_OFF_RATING" = "estimate off_rating",
                                   "E_DEF_RATING" = "estimate def_rating",
                                   "E_NET_RATING" = "estimate net_rating"),
                            " ", "in season 2018-19")) +
        theme(axis.title.x = element_blank(),
              plot.title = element_text(hjust = 0.5))
    } else {
      data_two_plots(data)
    }

  })
  
  output$data <- renderDataTable({
    
    if(input$roll) {
      data <- rollmean_func(nba(), input$number)
    } else {
      data <- nba()
    }
  })
  
  output$plotly_chart <- renderPlotly({
    if(input$roll) {
      data <- rollmean_func(nba(), input$number)
    } else {
      data <- nba()
    }
    
    if(input$rating %in% c("E_OFF_RATING",
                           "E_DEF_RATING",
                           "E_NET_RATING")) {
      plot_ly()%>%
        add_trace(data = data,
                  type = 'scatter',
                  mode = 'lines+markers',
                  x = data$GAME_DATE,
                  y = switch (input$rating,
                              "E_OFF_RATING" = data$E_OFF_RATING,
                              "E_DEF_RATING" = data$E_DEF_RATING,
                              "E_NET_RATING" = data$E_NET_RATING
                  ),
                  text = ~data$TEAM_NAME,
                  color = I(data$col1)) %>%
        layout(title = paste0(ifelse(input$roll, 
                                     paste0(input$number,"-Game Rolling "), 
                                     ""), 
                              input$team, " ",
                              switch(input$rating,
                                     "E_OFF_RATING" = "estimate off_rating",
                                     "E_DEF_RATING" = "estimate def_rating",
                                     "E_NET_RATING" = "estimate net_rating"),
                              " ", "in season 2018-19"),
               yaxis = list(title = switch(input$rating,
                                           "E_OFF_RATING" = "estimate off_rating",
                                           "E_DEF_RATING" = "estimate def_rating",
                                           "E_NET_RATING" = "estimate net_rating"),
                            zeroline = FALSE))
    } else {
      data_two_plots(data) %>%
        ggplotly() %>%
        layout(legend = list(orientation = "h",   
                             xanchor = "center",
                             x = 0.5, y = -Inf))
    }
    
  })
  
  output$download <- downloadHandler(
    filename = function(){
      paste0("data_", Sys.Date(), ".csv")
    },
    content = function(name){
      write.csv(nba(), name, row.names = FALSE)
    }, contentType = "text/csv"
  )
  
  output$downloadplot <- downloadHandler(
    filename = function(){
      paste0("plot_", Sys.Date(), ".png")
    },
    content = function(name){
      ggsave(name, plot = last_plot(), height = 5, width = 7, units = "in")
    }, contentType = "image/png"
  )
    
}

# Run the app ----
shinyApp(ui = ui, server = server)
