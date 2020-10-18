# College Football Conference Dashboard
# Created by Alex Elfering

# This dashboard is designed to examine how well college football teams perform over the short-term and long-term by conference

# Data Sources Used: College Football Reference


# Load packages
list.of.packages <- c("ggplot2", 
                      "shiny", 
                      'htmltools', 
                      'shinydashboard', 
                      'shinythemes', 
                      'dplyr', 
                      'tidyr', 
                      'tidyverse', 
                      'scales', 
                      'zoo',
                      'ggrepel',
                      'rsconnect',
                      'DT')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(ggplot2)  
library(shiny)
library(htmltools)
library(shinydashboard)
library(shinythemes) 
library(dplyr) 
library(tidyr)
library(tidyverse)
library(scales)
library(zoo)
library(ggrepel)
library(rsconnect)
library(DT)

# loading data source
conf_performance <- read.csv('cfb conf.csv' , fileEncoding="UTF-8-BOM")

# building the r shiny dashboard
ui <- shinyUI(fluidPage(  
  titlePanel("Conference Performance by Team since 1936"),  
  sidebarLayout(  
    sidebarPanel(
      selectInput("conference", "Select a Conference",
                  sort(unique(conf_performance$Conf))),
      sliderInput("season", "Select a Season:",
                  min = 1936, max = 2019,
                  value = 2019),
      sliderInput('ranking', 'Select a Rank:',
                  min = 1, max = 5,
                  value = 3),
      selectInput("running", "Select a Measure for Running Calc:",
                  c('sum', 'mean', 'median')),
      sliderInput("variable", "Select a Variable for Running Calculation:",
                  min = 3, max = 10,
                  value = 5),
      sliderInput("axis", "Select a Variable to Format Axis:",
                  min = 1, max = 20,
                  value = 5),
      width=2),
    mainPanel(
      plotOutput("Plot", width = "100%")  ,
      DT::dataTableOutput('dt')
    )  
  )  
))

server <- shinyServer(function(input, output) { 
  # time series graph
  output$Plot <- renderPlot({  
    
    # this data frame builds the running calculation for both the regular season and conference play
    # teams that move between conferences (Nebraska from Big 12 -> Big Ten) should have their conference play record start over again
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
      # regular season
      group_by(Team) %>%
      mutate(Rolling.Wins = rollapplyr(Total.Wins, input$variable, input$running, partial = TRUE),
             Rolling.Losses = rollapplyr(Total.Losses, input$variable, input$running, partial = TRUE),
             Rolling.Ties = rollapplyr(Total.Ties, input$variable, input$running, partial = TRUE),
             Rolling.Total.Games = rollapplyr(Total.Games, input$variable, input$running, partial = TRUE)) %>%
      ungroup() %>%
      # conference play
      group_by(Conf,
               Team) %>%
      mutate(Rolling.Conf.Wins = rollapplyr(Conf.Wins, input$variable, input$running, partial = TRUE),
             Rolling.Conf.Losses = rollapplyr(Conf.Losses, input$variable, input$running, partial = TRUE),
             Rolling.Conf.Ties = rollapplyr(Conf.Ties, input$variable, input$running, partial = TRUE),
             Rolling.Conf.Total.Games = rollapplyr(Total.Conf.Games, input$variable, input$running, partial = TRUE)) %>%
      ungroup() %>%
      mutate(Rolling.Pct.Won = Rolling.Wins/Rolling.Total.Games,
             Rolling.Conf.Pct.Won = Rolling.Conf.Wins/Rolling.Conf.Total.Games) %>%
      # filtering for the beginning of the AP Polling era
      filter(Season >= 1936)
    
    fill_missing_time_series <- team_conf %>%
      filter(Conf == input$conference) %>%
      group_by(Team) %>%
      complete(Season = seq(min(Season), max(Season), by = 1)) %>%
      ungroup() %>%
      fill(Conf)
    
    top_teams <- team_conf %>%
      filter(Conf == input$conference,
             Season == input$season) %>%
      select(Team,
             Rolling.Wins) %>%
      arrange(desc(Rolling.Wins)) %>%
      filter(dense_rank(desc(Rolling.Wins)) <= input$ranking) %>%
      select(Team)
    
    beg_season <- min(fill_missing_time_series$Season)
    end_season <- max(fill_missing_time_series$Season)
    
    fill_missing_time_series %>%
      filter(Conf == input$conference) %>%
      ggplot(aes(x = Season,
                 y = Rolling.Wins,
                 group = Team)) +
      geom_line(alpha = 0.2, 
                color = 'gray',
                size = 1) +
      geom_line(data = subset(fill_missing_time_series, Team %in% top_teams$Team & Conf == input$conference),
                mapping = aes(x = Season,
                              y = Rolling.Wins,
                              group = Team,
                              color = Team),
                alpha = 0.5,
                size = 2) +
      geom_line(data = subset(fill_missing_time_series, Team %in% top_teams$Team & Season <= input$season & Conf == input$conference),
                mapping = aes(x = Season,
                              y = Rolling.Wins,
                              group = Team,
                              color = Team),
                size = 2) +
      geom_vline(xintercept = input$season,
                 linetype = 'dashed') +
      scale_x_continuous(limits=c(beg_season, end_season),
                         breaks = seq(beg_season, end_season, by = input$axis)) +
      labs(title = paste('Winningest Teams in the ', input$conference, ' as of ', input$season, sep = ''),
           subtitle = paste('Based on a Rolling ', input$variable, ' Season ', input$running, '.', sep = ''),
           y = '',
           x = '',
           caption = 'Visualization by Alex Elfering | Data provided by College Football Reference') +
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
    
    
    
  }, height = 'auto', width = 'auto')
  # summary table
  output$dt <- DT::renderDataTable({
    
    team_conf <- conf_performance %>%
      replace(is.na(.), 0) %>%
      arrange(Team, 
              Season) %>%
      select(Team, 
             Season,
             Conf,
             Post,
             Total.Wins,
             Total.Losses,
             Total.Ties,
             Conf.Wins,
             Conf.Losses,
             Conf.Ties) %>%
      mutate(Total.Games = Total.Wins + Total.Losses + Total.Ties,
             Total.Conf.Games = Conf.Wins + Conf.Losses + Conf.Ties,
             Pct.Win = Total.Wins/Total.Games,
             Finish.Ranked = ifelse(Post > 0, 1, 0),
             Finished.Top.10 = ifelse(Post > 0 & Post <= 10, 1, 0)) %>%
      group_by(Team) %>%
      complete(Season = seq(min(Season), max(Season), by = 1)) %>%
      mutate(Rolling.Wins = round(rollapplyr(Total.Wins, input$variable, input$running, partial = TRUE, na.rm = TRUE),1),
             Rolling.Losses = round(rollapplyr(Total.Losses, input$variable, input$running, partial = TRUE, na.rm = TRUE),1),
             Rolling.Ties = round(rollapplyr(Total.Ties, input$variable, input$running, partial = TRUE, na.rm = TRUE),1),
             Rolling.Total.Games = rollapplyr(Total.Games, input$variable, input$running, partial = TRUE),
             Rolling.Ranked = rollapplyr(Finish.Ranked, input$variable, sum, partial = TRUE),
             Rolling.Top.10 = rollapplyr(Finished.Top.10, input$variable, sum, partial = TRUE)) %>%
      ungroup() %>%
      group_by(Team, Conf) %>%
      mutate(Rolling.Conf.Wins = round(rollapplyr(Conf.Wins, input$variable, input$running, partial = TRUE, na.rm = TRUE),1),
             Rolling.Conf.Losses = round(rollapplyr(Conf.Losses, input$variable, input$running, partial = TRUE, na.rm = TRUE),1),
             Rolling.Conf.Ties = round(rollapplyr(Conf.Ties, input$variable, input$running, partial = TRUE, na.rm = TRUE),1),
             Rolling.Conf.Total.Games = rollapplyr(Total.Conf.Games, input$variable, input$running, partial = TRUE)) %>%
      ungroup() %>%
      mutate(Rolling.Pct.Won = Rolling.Wins/Rolling.Total.Games,
             Rolling.Conf.Pct.Won = Rolling.Conf.Wins/Rolling.Conf.Total.Games) %>%
      filter(Season >= 1936,
             !is.na(Total.Games))
    
    tbl_test <- team_conf %>%
      filter(Conf == input$conference,
             Season == input$season) %>%
      arrange(desc(Rolling.Pct.Won)) %>%
      mutate(Rolling.Ties = ifelse(Rolling.Ties == 0, NA, Rolling.Ties),
             Rolling.Conf.Ties = ifelse(Rolling.Conf.Ties == 0, NA, Rolling.Conf.Ties)) %>%
      unite(Record, c('Rolling.Wins', 'Rolling.Losses', 'Rolling.Ties'), sep = '-', na.rm = TRUE) %>%
      unite(Conf.Record, c('Rolling.Conf.Wins', 'Rolling.Conf.Losses', 'Rolling.Conf.Ties'), sep = '-', na.rm = TRUE) %>%
      select(Team,
             Record,
             Conf.Record,
             Rolling.Ranked,
             Rolling.Top.10)
    
    datatable(tbl_test, 
              rownames = FALSE,
              extensions = 'Buttons', 
              colnames = c('Team',
                           'Regular Season Record',
                           'Conference Play Record',
                           'Finished Ranked in Top 25',
                           'Finished Ranked in Top 10'),
              caption = htmltools::tags$caption(
                style = 'caption-side: bottom; text-align: left;',
                htmltools::em(paste(input$conference, ' Conference Records between ', input$season-input$variable, ' and ', input$season, '. Conference Play also includes conference championships. Averages and medians are rounded. Teams are dense ranked based on percent of wins. Visualization and design by Alex Elfering. Data Source: College Football Reference.', sep = ''))),
              options = list(paging = FALSE,
                             dom = 'Bfrtip',
                             buttons = c('copy', 'csv', 'excel')))
    
    
  })
})

shinyApp(ui=ui, server=server)