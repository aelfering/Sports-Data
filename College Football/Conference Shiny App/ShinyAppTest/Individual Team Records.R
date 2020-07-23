#### Package Loading  ####
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
                      'htmltools',
                      'stringi')
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
library(htmltools)
library(stringi)

setwd("~/Documents/GitHub/Sports-Data/College Football/Conference Shiny App/ShinyAppTest")

####  Loading data  ####
cfb_team_games <- read.csv('Games teams CFB.csv')

####  Create rank strings and function to trim white space ####
seq <- 1:25
seasons <- paste('(', seq, ')', sep = "")

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

####  Identify winning and losing games ####
winning_games <- cfb_team_games %>%
  distinct(Season,
           Wk,
           Date,
           Winner,
           Loser,
           Pts,
           Pts.1) %>%
  select(Season,
         Wk,
         Date,
         Team = Winner,
         Opponent = Loser,
         Team.Pts = Pts,
         Opp.Pts = Pts.1) %>%
  mutate(Team = stri_replace_all_fixed(Team, pattern = seasons, replacement = '', vectorize_all = FALSE),
         Team = trim(Team)) %>%
  mutate(Opp.Rank = str_extract(Opponent, '(?<=\\()[0-9-]+(?=\\))')) %>%
  replace(is.na(.), 0)

losing_games <- cfb_team_games %>%
  distinct(Season,
           Wk,
           Date,
           Winner,
           Loser,
           Pts,
           Pts.1) %>%
  select(Season,
         Wk,
         Date,
         Team = Loser,
         Opponent = Winner,
         Team.Pts = Pts.1,
         Opp.Pts = Pts) %>%
  mutate(Team = stri_replace_all_fixed(Team, pattern = seasons, replacement = '', vectorize_all = FALSE),
         Team = trim(Team)) %>%
  mutate(Opp.Rank = str_extract(Opponent, '(?<=\\()[0-9-]+(?=\\))')) %>%
  replace(is.na(.), 0)

all_games <- bind_rows(winning_games, losing_games)

####  Overall and ranked wins rolling sum ####
variable <- 10

# How well do teams perform against teams overall?
overall_team_record <- all_games %>%
  # Removes occassional dupes
  distinct(Season,
           Date,
           Team,
           Opponent,
           Team.Pts,
           Opp.Pts,
           Opp.Rank) %>%
  mutate(Wins = ifelse(Team.Pts > Opp.Pts, 1, 0),
         Loses = ifelse(Opp.Pts > Team.Pts, 1, 0),
         Ties = ifelse(Opp.Pts == Team.Pts, 1, 0)) %>%
  group_by(Season,
           Team) %>%
  summarise(Overall.Wins = sum(Wins),
            Overall.Losses = sum(Loses),
            Overall.Ties = sum(Ties)) %>%
  ungroup() %>%
  group_by(Team) %>%
  mutate(Rolling.Overall.Wins = rollapplyr(Overall.Wins, variable, sum, partial = TRUE),
         Rolling.Overall.Losses = rollapplyr(Overall.Losses, variable, sum, partial = TRUE),
         Rolling.Overall.Ties = rollapplyr(Overall.Ties, variable, sum, partial = TRUE)) %>%
  ungroup()

# How well do teams perform against ranked opponents?
ranked_record <- all_games %>%
  # Removes occassional dupes
  distinct(Season,
           Date,
           Team,
           Opponent,
           Team.Pts,
           Opp.Pts,
           Opp.Rank) %>%
  filter(Opp.Rank > 0) %>%
  mutate(Wins = ifelse(Team.Pts > Opp.Pts, 1, 0),
         Loses = ifelse(Opp.Pts > Team.Pts, 1, 0),
         Ties = ifelse(Opp.Pts == Team.Pts, 1, 0)) %>%
  group_by(Season,
           Team) %>%
  summarise(Ranked.Wins = sum(Wins),
            Ranked.Losses = sum(Loses),
            Ranked.Ties = sum(Ties)) %>%
  ungroup() %>%
  group_by(Team) %>%
  # Complete missing seasons among teams and replace those missing values with NA
  mutate(Rolling.Ranked.Wins = rollapplyr(Ranked.Wins, 10, sum, partial = TRUE),
         Rolling.Ranked.Losses = rollapplyr(Ranked.Losses, 10, sum, partial = TRUE),
         Rolling.Ranked.Ties = rollapplyr(Ranked.Ties, 10, sum, partial = TRUE)) %>%
  ungroup()

overall_and_ranked_wins <- left_join(overall_team_record, ranked_record, by = c('Season' = 'Season', 'Team' = 'Team'))

overall_names_fixed <- overall_and_ranked_wins %>%
  # Adjusting specific team names with acronyms
  mutate(Team = ifelse(Team == 'Southern California', 'USC', Team),
         Team = ifelse(Team == 'Central Florida', 'UCF', Team),
         Team = ifelse(Team == 'Texas-San Antonio', 'UTSA', Team),
         Team = ifelse(Team == 'Texas-El Paso', 'UTEP', Team),
         Team = ifelse(Team == 'Alabama-Birmingham', 'UAB', Team),
         Team = ifelse(Team == 'Southern Methodist', 'SMU', Team),
         Team = ifelse(Team == 'Pittsburgh', 'Pitt', Team),
         Team = ifelse(Team == 'Mississippi', 'Ole Miss', Team),
         Team = ifelse(Team == 'Louisiana State', 'LSU', Team),
         Team = ifelse(Team == 'Brigham Young', 'BYU', Team))