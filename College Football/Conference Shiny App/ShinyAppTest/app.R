#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

list.of.packages <- c("ggplot2", 
                      "dplyr", 
                      'tidyverse', 
                      'tidyr', 
                      'lubridate', 
                      'data.table', 
                      'zoo', 
                      'ggrepel', 
                      'directlabels', 
                      'reactable',
                      'shiny',
                      'htmltools',
                      'shinydashboard',
                      'shinythemes')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(ggplot2)
library(dplyr)
library(tidyverse)
library(tidyr)
library(lubridate)
library(data.table)
library(zoo)
library(ggrepel)
library(directlabels)
library(reactable)
library(shiny)
library(htmltools)
library(shinydashboard)
library(shinythemes)

cfb_conf <- read.csv('cfb conf.csv')
conf_records <- read.csv('conf_records.csv')
championships <- read.csv('cfb champions.csv')

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("How Has Each College Football Conference Performed Over Time?"),
    # The filters on the side panel
    sidebarLayout(
        sidebarPanel(
            sliderInput("range", "Select a Season:",
                        min = 1936, max = 2019,
                        value = 2015),
            sliderInput("Variable",
                        "Select How Many Seasons to Look Back:",
                        min = 1,
                        max = 20,
                        value = 10),
            h5("Table created by Alex Elfering"),
            h5("Data: College-Football Reference"),
            width=2),
        mainPanel(reactableOutput(outputId = "table")))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$table <- renderReactable({
        orange_pal <- function(x) rgb(colorRamp(c(NA, "#ff9500"))(x), maxColorValue = 255)
        green_pal <- function(x) rgb(colorRamp(c(NA, "#00a123"))(x), maxColorValue = 255)
        blue_pal <- function(x) rgb(colorRamp(c(NA, "#008cff"))(x), maxColorValue = 255)
        
        sticky_style <- list(position = "sticky", left = 0, background = "#fff", zIndex = 1,
                             borderRight = "1px solid #eee")
        
        bar_chart <- function(label, width = "100%", height = "16px", fill = "#00bfc4", background = NULL) {
            bar <- div(style = list(background = fill, width = width, height = height))
            chart <- div(style = list(flexGrow = 1, marginLeft = "8px", background = background), bar)
            div(style = list(display = "flex", alignItems = "center"), label, chart)
        }
        
        # Pull Championship Data
        championships$National.Champion <- gsub("\\s*\\([^\\)]+\\)","",as.character(championships$National.Champion))
        data.frame(championships,do.call(rbind,strsplit(championships$National.Champion,",")))
        
        mark1 <- championships %>%
            mutate(National.Champion = ifelse(National.Champion == 'Miami', 'Miami (FL)', National.Champion)) %>%
            separate(National.Champion, into = paste("C", 1:4, sep = '-'), sep = ', ') %>%
            select(Year,
                   `C-1`,
                   `C-2`,
                   `C-3`)
        
        one_champ <- mark1 %>%
            mutate(Won.Championship = 1) %>%
            select(Year,
                   Champion = `C-1`,
                   Won.Championship) %>%
            filter(!is.na(Champion))
        
        two_champ <- mark1 %>%
            mutate(Won.Championship = 1) %>%
            select(Year,
                   Champion = `C-2`,
                   Won.Championship) %>%
            filter(!is.na(Champion))
        
        three_champ <- mark1 %>%
            mutate(Won.Championship = 1) %>%
            select(Year,
                   Champion = `C-3`,
                   Won.Championship) %>%
            filter(!is.na(Champion))
        
        cfb_championships <- bind_rows(one_champ, two_champ, three_champ)
        
        diff <- max(input$Season)-min(input$Season)
        
        # Conference Records
        rolling.bowl.wins <- conf_records %>%
            select(Conference,
                   Year,
                   Bowl.Wins = W.1,
                   Bowl.Losses = L.1,
                   Bowl.Ties = T.1) %>%
            arrange(Year) %>%
            group_by(Conference) %>%
            mutate(Rolling.Bowl.Wins = rollapplyr(Bowl.Wins, input$Variable, sum, partial = TRUE),
                   Rolling.Bowl.Losses = rollapplyr(Bowl.Losses, input$Variable, sum, partial = TRUE),
                   Rolling.Bowl.Ties = rollapplyr(Bowl.Ties, input$Variable, sum, partial = TRUE)) %>%
            mutate(Bowl.Record = paste(Rolling.Bowl.Wins,
                                       Rolling.Bowl.Losses,
                                       Rolling.Bowl.Ties,
                                       sep = '-')) %>%
            select(Conference,
                   Year,
                   Bowl.Record)
        
        # Rolling Wins by Team and by Conference
        cfb_team_data <- left_join(cfb_conf, cfb_championships, by = c('Season' = 'Year', 'Team' = 'Champion'))    
        
        #       Find the rolling number of wins, losses, and ties by team and aggregate by conference
        team.rolling.wins <- cfb_team_data %>%
            replace(is.na(.), 0) %>%
            mutate(AP_Post_Ranked = ifelse(Post > 0, 1, 0),
                   AP_Top_10 = ifelse(Post <= 10 & Post != 0, 1, 0)) %>%
            arrange(Season) %>%
            group_by(Team) %>%
            # this solves for the App States of football who went to FCS in 1981 but rejoined FBS later
            complete(Season = seq(min(Season), 2019, by = 1)) %>%
            mutate(Conf = ifelse(is.na(Conf), 'Did not compete in FBS', Conf),
                   Notes = ifelse(is.na(Notes), '', Notes)) %>%
            replace(is.na(.), 0) %>%
            mutate(Rolling.Wins = rollapplyr(Total.Wins, input$Variable, sum, partial = TRUE),
                   Rolling.Losses = rollapplyr(Total.Losses, input$Variable, sum, partial = TRUE),
                   Rolling.Ties = rollapplyr(Total.Ties, input$Variable, sum, partial = TRUE),
                   Rolling.Championships = rollapplyr(Won.Championship, input$Variable, sum, partial = TRUE)) %>%
            ungroup() %>%
            select(Team,
                   Season,
                   Conf,
                   Rolling.Wins,
                   Rolling.Losses,
                   Rolling.Ties,
                   AP_Post_Ranked,
                   AP_Top_10,
                   Rolling.Championships) %>%
            mutate(Pct.Won = Rolling.Wins/(Rolling.Wins + Rolling.Losses + Rolling.Ties)) %>%
            # Removing teams that did not compete on the FBS level
            filter(!Conf %in% c('Did not compete in FBS')) %>%
            # Here is where we aggregate the team results by conference
            group_by(Season,
                     Conf) %>%
            mutate(Total.Wins = sum(Rolling.Wins),
                   Total.Losses = sum(Rolling.Losses),
                   Total.Ties = sum(Rolling.Ties),
                   Total.Pct = Total.Wins/(Total.Wins + Total.Losses + Total.Ties),
                   Teams = n_distinct(Team),
                   Total.Championships = sum(Rolling.Championships)) %>%
            mutate(Conf_AP_Rank = sum(AP_Post_Ranked),
                   Conf_AP_10 = sum(AP_Top_10)) %>%
            mutate(Tm.Rank = dense_rank(desc(Pct.Won))) %>%
            # Return the team who has won the most games by conference overall
            # Some conferences return multiple teams per season...we will solve for that below
            filter(Tm.Rank == 1) %>%
            mutate(Rows = row_number()) %>%
            ungroup()
        
        #     if a conference has more than one leader, this concatenates them
        leader.concat <- team.rolling.wins %>%
            distinct(Season,
                     Conf,
                     Team,
                     Rolling.Wins,
                     Rolling.Losses,
                     Rolling.Ties,
                     Rows) %>%
            mutate(Team_Name = paste(Team, ' (', Rolling.Wins, '-', Rolling.Losses, '-', Rolling.Ties, ') ', sep = '')) %>%
            distinct(Season,
                     Conf,
                     Team_Name,
                     Rows) %>%
            group_by(Season,
                     Conf) %>%
            mutate(Rows = as.character(Rows)) %>%
            spread(Rows, Team_Name) %>%
            unite(Leaders, -c('Season', 'Conf'), sep = ', ', na.rm = TRUE) %>%
            distinct(Season,
                     Conf,
                     Leaders)
        
        conf.rolling.records <- team.rolling.wins %>%
            distinct(Season,
                     Conf,
                     Teams,
                     Total.Wins,
                     Total.Losses,
                     Total.Ties,
                     Total.Pct,
                     Conf_AP_Rank,
                     Conf_AP_10,
                     Total.Championships) %>%
            group_by(Conf) %>%
            mutate(Rolling.AP.Rank = rollapplyr(Conf_AP_Rank, input$Variable, sum, partial = TRUE)/input$Variable,
                   Rolling.AP.10 = rollapplyr(Conf_AP_10, input$Variable, sum, partial = TRUE)/input$Variable) %>%
            ungroup()
        
        #       Joining the various data frames together
        conf.leaders.records <- inner_join(conf.rolling.records, leader.concat, by = c('Season' = 'Season', 'Conf' = 'Conf'))
        conf.leader.bowls <- inner_join(conf.leaders.records, rolling.bowl.wins, by = c('Season' = 'Year', "Conf" = "Conference"))
        
        #       Formatting columns for data table
        conf.record.table <- conf.leader.bowls %>%
            mutate(Earlier = (Season-input$Variable)+1) %>%
            filter(Season == input$range) %>%
            mutate(Period = paste(Earlier, Season, sep = '-'),
                   Conf_Record = paste(Total.Wins, Total.Losses, Total.Ties, sep = '-')
            ) %>%
            arrange(Conf) %>%
            select(Conference = Conf,
                   Period,
                   `Teams` = Teams,
                   `Percent Won` = Total.Pct,
                   `Conference Record` = Conf_Record,
                   `Bowl Record` = Bowl.Record,
                   `Championships` = Total.Championships,
                   `Average in AP 25` = Rolling.AP.Rank,
                   `Average in AP 10` = Rolling.AP.10,
                   `Winningest Team(s)` = Leaders) %>%
            replace(is.na(.), 0)
        
        reactable(conf.record.table,
                  pagination = FALSE,
                  outlined = TRUE,
                  highlight = TRUE,
                  striped = TRUE,
                  resizable = TRUE,
                  wrap = TRUE,
                  defaultSorted = 'Conference',
                  defaultColDef = colDef(headerClass = "header", 
                                         align = "left"),
                  style = list(fontFamily = 'Arial', 
                               fontSize = '14px'),
                  theme = reactableTheme(
                      headerStyle = list(
                          "&:hover[aria-sort]" = list(background = "hsl(0, 0%, 96%)"),
                          "&[aria-sort='ascending'], &[aria-sort='descending']" = list(background = "hsl(0, 0%, 96%)"),
                          borderColor = "#555"
                      )),
                  columns = list(Conference = colDef(minWidth = 110,
                                                     style = sticky_style,
                                                     headerStyle = sticky_style),
                                 Period = colDef(minWidth = 90),
                                 Teams = colDef(minWidth = 70),
                                 `Conference Record` = colDef(minWidth = 110),
                                 `Bowl Record` = colDef(minWidth = 110),
                                 `Percent Won` = colDef(minWidth = 80,
                                                        format = colFormat(percent = TRUE, digits = 2),
                                                        style = function(value) {
                                                            normalized <- (value - min(conf.record.table$`Percent Won`)) / (max(conf.record.table$`Percent Won`) - min(conf.record.table$`Percent Won`))
                                                            color <- orange_pal(normalized)
                                                            list(background = color)
                                                        }),
                                 Championships = colDef(minWidth = 130,
                                                        style = function(value) {
                                                            normalized <- (value - min(conf.record.table$Championships)) / (max(conf.record.table$Championships) - min(conf.record.table$Championships))
                                                            color <- orange_pal(normalized)
                                                            list(background = color)
                                                        }),
                                 `Average in AP 25` = colDef(minWidth = 90,
                                                             align = "left", cell = function(value) {
                                                                 width <- paste0(value / max(conf.record.table$`Average in AP 25`) * 100, "%")
                                                                 label <- format(round(value, 2), nsmall = 2)
                                                                 bar_chart(label, width = width)
                                                             }),
                                 `Average in AP 10` = colDef(minWidth = 90,
                                                             align = "left", cell = function(value) {
                                                                 width <- paste0(value / max(conf.record.table$`Average in AP 25`) * 100, "%")
                                                                 label <- format(round(value, 2), nsmall = 2)
                                                                 bar_chart(label, width = width)
                                                             }),
                                 `Winningest Team(s)` = colDef(minWidth = 350,
                                                               class = "border-left")
                  ))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
