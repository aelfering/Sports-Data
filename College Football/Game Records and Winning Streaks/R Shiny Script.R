# This script identifies when the last time a team finished with a particular record, as well as winning streaks that are snapped in the latest season
# Script by Alex Elfering

# Last Updated: 

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
                      'DT',
                      'stringr')
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
library(stringi)

#setwd("~/GitHub/Sports-Data/College Football/Game Records and Winning Streaks")

cfb_games <- read.csv('Games teams CFB.csv', fileEncoding="UTF-8-BOM")
cfb_conferences <- read.csv('cfb conf.csv', fileEncoding="UTF-8-BOM")


#### Data cleaning  ####
colnames(cfb_games) <- c('Season', 'Rk', 'Wk', 'Date', 'Day', 'Team', 'Team.Pts', 'Location', 'Opponent', 'Opp.Pts', 'Notes')

rank_patterns <- paste('\\(', 1:25, '\\)', sep = '')

cfb_games <- cfb_games %>%
  mutate(Team = stri_trim_both(str_remove_all(Team, paste(rank_patterns, collapse = "|"))),
         Opponent = stri_trim_both(str_remove_all(Opponent, paste(rank_patterns, collapse = "|"))))

# find the fbs teams
cfb_select <- dplyr::select(cfb_conferences, Season, Conf, Team) %>%
  mutate(Team = case_when(Team == 'UTEP' ~ 'Texas-El Paso',
                          Team == 'UAB' ~ 'Alabama-Birmingham',
                          Team == 'BYU' ~ "Brigham Young",
                          Team == 'UCF' ~ "Central Florida",
                          Team == "LSU" ~ "Louisiana State",
                          Team == "Ole Miss" ~ "Mississippi",
                          Team == "Pitt" ~ "Pittsburgh",
                          Team == "USC" ~ 'Southern California',
                          Team == 'SMU' ~ "Southern Methodist",
                          Team == 'UTSA' ~ 'Texas-San Antonio',
                          TRUE ~ Team))

# all fbs teams in the latest season 
fbs_teams <- cfb_games %>%
  filter(Season == max(Season)) %>%
  inner_join(cfb_select) %>%
  distinct(Conf, 
           Team)

# expand games
opp_select <- cfb_games %>%
  select(Season,
         Rk,
         Wk,
         Date,
         Day,
         Team = Opponent,
         Team.Pts = Opp.Pts,
         Location,
         Opponent = Team,
         Opp.Pts = Team.Pts,
         Notes)
bind_all_games <- bind_rows(cfb_games, opp_select)



####  Find Historic Records ####
# calculate the running games won and lost by team per season
running_games <- bind_all_games %>%
  # played teams
  filter(!is.na(Team.Pts)) %>%
  arrange(Season,
          Wk) %>%
  mutate(Wins = ifelse(Team.Pts > Opp.Pts, 1, 0),
         Ties = ifelse(Team.Pts == Opp.Pts, 1, 0),
         Loses = ifelse(Team.Pts < Opp.Pts, 1, 0)) %>%
  # cumulative sum of games won and lost
  group_by(Season,
           Team) %>%
  mutate(Rolling.Wins = cumsum(Wins),
         Rolling.Losses = cumsum(Loses),
         Rolling.Ties = cumsum(Ties)) %>%
  ungroup() %>%
  group_by(Team) %>%
  # cauclating winning and losing streaks ongoing 
  mutate(Win.Streak = ave(Wins, cumsum(Wins==0), FUN = seq_along) - 1,
         Lose.Streak = ave(Loses, cumsum(Loses==0), FUN = seq_along) - 1,
         Streak.Status = ifelse(Win.Streak > 0, 
                                paste('W', Win.Streak, sep = ''), 
                                paste('L', Lose.Streak, sep = ''))) %>%
  ungroup()

# return the latest records of each team in the latest season 
latest_season_records <- running_games %>%
  filter(Season == max(Season)) %>%
  mutate(Result = ifelse(Team.Pts > Opp.Pts, 'W', 'L'),
         Latest_Game = paste(Result, ' ', Team.Pts, '-', Opp.Pts, ' vs ', Opponent, sep = '')) %>%
  group_by(Team) %>%
  slice(which.max(Wk)) %>%
  select(Team, 
         Season,
         Streak.Status,
         Latest_Game,
         Rolling.Wins,
         Rolling.Losses) %>%
  arrange(desc(Rolling.Wins), 
          Rolling.Losses) %>%
  unite(Record, c('Rolling.Wins', 'Rolling.Losses'), sep = '-', na.rm = TRUE)

# what is each team's next game?
next_game <- bind_all_games %>%
  filter(is.na(Team.Pts)) %>%
  group_by(Team) %>%
  slice(which.min(Wk)) %>%
  ungroup() %>%
  left_join(latest_season_records, by = c('Season' = 'Season', 'Opponent' = 'Team')) %>%
  mutate(Record = ifelse(!Opponent %in% fbs_teams$Team, '*', Record),
         Record = ifelse(is.na(Record), ' (0-0)', paste(' (', Record, ')', sep = '')),
         Record = ifelse(grepl('\\*', Record), gsub("[()]", "", Record),  Record)) %>%
  mutate(Next_Game = paste('vs ', Opponent, Record, sep = '')) %>%
  select(Team, 
         Next_Game)

# what is every fbs team's record of all time?
all_records <- running_games %>%
  filter(Season < max(Season)) %>%
  select(Season,
         Team, 
         Rolling.Wins,
         Rolling.Losses,
         Rolling.Ties) %>%
  arrange(desc(Rolling.Wins), 
          Rolling.Losses) %>%
  unite(Record, c('Rolling.Wins', 'Rolling.Losses'), sep = '-', na.rm = TRUE) %>%
  group_by(Team,
           Record) %>%
  summarise(Last_Season = max(Season),
            Seasons = n_distinct(Season))

# do any team records break team history?
# if a team goes 4-0 for the first time ever, they will show up here
team_record_break <- latest_season_records %>%
  left_join(all_records) %>%
  mutate(Season_Gap = Season-Last_Season) %>%
  left_join(next_game, by = c('Team' = 'Team')) %>%
  filter(is.na(Last_Season)) %>%
  mutate(Last_Season = 'First Time') %>%
  inner_join(fbs_teams, by = c('Team' = 'Team')) %>%
  select(Season,
         Team,
         Conf,
         Record,
         Last_Season,
         Streak.Status,
         Latest_Game,
         Next_Game)

# when was the last time that a team had a specific record?
# if a team is 4-0 for the first time since 1960, they will show up here
team_performance <- latest_season_records %>%
  left_join(all_records) %>%
  mutate(Season_Gap = Season-Last_Season) %>%
  left_join(next_game, by = c('Team' = 'Team')) %>%
  inner_join(fbs_teams) %>%
  filter(Team %in% unique(fbs_teams$Team)) %>%
  arrange(desc(Season_Gap)) %>%
  filter(!is.na(Last_Season),
         Season_Gap >= 5) %>%
  mutate(Next_Game = ifelse(is.na(Next_Game), 'Season Finished', Next_Game),
         Last_Season = as.character(Last_Season)) %>%
  select(Season,
         Team,
         Conf,
         Record,
         Last_Season,
         Streak.Status,
         Latest_Game,
         Next_Game)

performance_bind <- bind_rows(team_record_break, team_performance)





####  Broken Winning Streaks  ####
game_streaks <- bind_all_games %>%
  # played teams
  filter(!is.na(Team.Pts)) %>%
  arrange(Season,
          Wk) %>%
  mutate(Wins = ifelse(Team.Pts > Opp.Pts, 1, 0),
         Ties = ifelse(Team.Pts == Opp.Pts, 1, 0),
         Loses = ifelse(Team.Pts < Opp.Pts, 1, 0)) %>%
  group_by(Team,
           Opponent) %>%
  # cauclating winning and losing streaks ongoing 
  mutate(Win.Streak = ave(Wins, cumsum(Wins==0), FUN = seq_along) - 1,
         Lose.Streak = ave(Loses, cumsum(Loses==0), FUN = seq_along) - 1) %>%
  ungroup()

flag_streak_broken <- game_streaks %>%
  group_by(Team,
           Opponent) %>%
  mutate(Streak.Broken = lag(Lose.Streak),
         Series.Number = row_number(),
         Last.Win = (Series.Number-Streak.Broken)-1,
         Major.Streak.Broken = ifelse(Streak.Broken > 2 & Win.Streak > 0, 1, 0)) %>%
  ungroup() 

series_numbers <- flag_streak_broken %>%
  distinct(Team,
           Opponent,
           Series.Number,
           Season)

current_streaks_broken <- flag_streak_broken %>%
  filter(Season == max(Season),
         Major.Streak.Broken == 1) %>%
  select(Season,
         Team,
         Opponent,
         Team.Pts,
         Opp.Pts,
         Streak.Broken,
         Last.Win) %>%
  inner_join(series_numbers, by = c('Last.Win' = 'Series.Number', 'Team' = 'Team', 'Opponent' = 'Opponent')) %>%
  unite(Final_Score, c('Team.Pts', 'Opp.Pts'), sep = '-') %>%
  select(Season = Season.x,
         Team,
         Opponent,
         Final_Score,
         Streak.Broken,
         Last_Win = Season.y) %>%
  arrange(desc(Streak.Broken))





#### R Shiny App  ####

# building the r shiny dashboard
ui <- shinyUI(fluidPage(  
  titlePanel("College Football Season Team Progress Dashboard"),  
  mainPanel(
    print(paste('Code by Alex Elfering | Data Source: College Football Reference | Last Updated:', Sys.time(), sep = '' )),
    tabsetPanel(
      id = 'dataset',
      tabPanel("When was the Last Time a Team Held a Specific Record?", DT::dataTableOutput('records')),
      tabPanel("Busted Series Winning Streaks", DT::dataTableOutput('streaks'))
    )
  )
))

server <- shinyServer(function(input, output) { 
  output$records <- DT::renderDataTable({
    
    datatable(performance_bind,
              extensions = 'Buttons', 
              colnames = c('Season',
                           'Team',
                           'Conference',
                           'Record',
                           'First Time Since',
                           'Streak',
                           'Latest Opponent',
                           'Next Opponent'),
              caption = htmltools::tags$caption(
                style = 'caption-side: bottom; text-align: center;',
                htmltools::em('* Indicates an FCS Team')),
              options = list(paging = FALSE,
                             dom = 'Bfrtip',
                             scroller = TRUE,
                             buttons = c('copy', 'csv', 'excel')))
    
    
    
  })
  output$streaks <- DT::renderDataTable({
    
    datatable(current_streaks_broken,
              extensions = 'Buttons', 
              colnames = c('Season',
                           'Team',
                           'Opponent',
                           'Final Score',
                           'Streak Broken',
                           'Last Season Won'),
              options = list(paging = FALSE,
                             dom = 'Bfrtip',
                             buttons = c('copy', 'csv', 'excel')))
    
  })
})

shinyApp(ui=ui, server=server)