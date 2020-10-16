library(ggplot2)  
library(shiny)
library(htmltools)
library(shinydashboard)
library(shinythemes) 
library(dplyr) 
library(lubridate)
library(tidyr)
library(tidyverse)
library(scales)
library(data.table)
library(zoo)
library(ggrepel)
library(rsconnect)

setwd("~/GitHub/Sports-Data/College Football/Revised R Shiny Conference Look")

conf_performance <- read.csv('cfb conf.csv')


ui <- shinyUI(fluidPage(  
  titlePanel("Conference Performance"),  
  sidebarLayout(  
    sidebarPanel(
      selectInput("conference", "Select a Conference",
                  c('Big Ten', 'Big 12', 'ACC', 'PAC-12', 'SEC', 'AAC', 'MWC', 'MAC', 'CUSA', 'Big 8', 'WAC', 'Big East')),
      sliderInput("season", "Select a Season:",
                  min = 1936, max = 2019,
                  value = 1999),
      sliderInput('ranking', 'Select a Rank:',
                  min = 1, max = 5,
                  value = 3),
      selectInput("running", "Select a Measure for Running Calc:",
                  c('sum', 'mean', 'median', 'max', 'min')),
      sliderInput("variable", "Select a Variable for Running Calculation:",
                  min = 3, max = 10,
                  value = 5),
      width=2),
    mainPanel(
      plotOutput("Plot", width = "100%")  
    )  
  )  
))

server <- shinyServer(function(input, output) {  
  output$Plot <- renderPlot({  
    
    team_conf <- conf_performance %>%
      replace(is.na(.), 0) %>%
      arrange(Team, 
              Season) %>%
      select(Team, 
             Season,
             Conf,
             Total.Wins,
             Total.Losses,
             Total.Ties,
             Conf.Wins,
             Conf.Losses,
             Conf.Ties) %>%
      mutate(Total.Games = Total.Wins + Total.Losses + Total.Ties,
             Total.Conf.Games = Conf.Wins + Conf.Losses + Conf.Ties,
             Pct.Win = Total.Wins/Total.Games) %>%
      group_by(Team) %>%
      mutate(Rolling.Wins = rollapplyr(Total.Wins, input$variable, input$running, partial = TRUE),
             Rolling.Losses = rollapplyr(Total.Losses, input$variable, input$running, partial = TRUE),
             Rolling.Ties = rollapplyr(Total.Ties, input$variable, input$running, partial = TRUE),
             Rolling.Total.Games = rollapplyr(Total.Games, input$variable, input$running, partial = TRUE)) %>%
      mutate(Rolling.Conf.Wins = rollapplyr(Conf.Wins, input$variable, input$running, partial = TRUE),
             Rolling.Conf.Losses = rollapplyr(Conf.Losses, input$variable, input$running, partial = TRUE),
             Rolling.Conf.Ties = rollapplyr(Conf.Ties, input$variable, input$running, partial = TRUE),
             Rolling.Conf.Total.Games = rollapplyr(Total.Conf.Games, input$variable, input$running, partial = TRUE)) %>%
      ungroup() %>%
      mutate(Rolling.Pct.Won = Rolling.Wins/Rolling.Total.Games,
             Rolling.Conf.Pct.Won = Rolling.Conf.Wins/Rolling.Conf.Total.Games) %>%
      filter(Season >= 1936)
    
    top_teams <- team_conf %>%
      filter(Conf == input$conference,
             Season == input$season) %>%
      select(Team,
             Rolling.Wins) %>%
      arrange(desc(Rolling.Wins)) %>%
      filter(dense_rank(desc(Rolling.Wins)) <= input$ranking) %>%
      select(Team)
    
    team_conf %>%
      filter(Conf == input$conference) %>%
      ggplot(aes(x = Season,
                 y = Rolling.Wins,
                 group = Team)) +
      geom_line(alpha = 0.2, 
                color = 'gray',
                size = 1) +
      geom_line(data = subset(team_conf, Team %in% top_teams$Team & Conf == input$conference),
                mapping = aes(x = Season,
                              y = Rolling.Wins,
                              group = Team,
                              color = Team),
                alpha = 0.5,
                size = 2) +
      geom_line(data = subset(team_conf, Team %in% top_teams$Team & Season <= input$season & Conf == input$conference),
                mapping = aes(x = Season,
                              y = Rolling.Wins,
                              group = Team,
                              color = Team),
                size = 2) +
      geom_vline(xintercept = input$season,
                 linetype = 'dashed') +
      labs(title = paste('Winningest Teams in the ', input$conference, ' as of ', input$season, sep = ''),
           subtitle = paste('Based on a Rolling ', input$variable, ' Season ', input$running, '.', sep = ''),
           y = '',
           x = '') +
      theme(plot.title = element_text(face = 'bold', size = 18, family = 'Arial'),
            legend.position = 'top',
            legend.background=element_blank(),
            legend.key=element_blank(),
            legend.text = element_text(size = 12, family = 'Arial'),
            legend.title = element_text(size = 12, family = 'Arial'),
            plot.subtitle = element_text(size = 15, family = 'Arial'),
            plot.caption = element_text(size = 12, family = 'Arial'),
            axis.title = element_text(size = 12, family = 'Arial'),
            axis.text = element_text(size = 12, family = 'Arial'),
            strip.text = ggplot2::element_text(size = 12, hjust = 0, face = 'bold', color = 'black', family = 'Arial'),
            strip.background = element_rect(fill = NA),
            panel.background = ggplot2::element_blank(),
            axis.line = element_line(colour = "#222222", linetype = "solid"),
            panel.grid.major.y = element_line(colour = "#c1c1c1", linetype = "dashed"),
            panel.grid.major.x = element_blank()) 
    
    
    
  }, height=700,
  width = 1150)
})

shinyApp(ui=ui, server=server)