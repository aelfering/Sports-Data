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

####  Create game records ####

head(all_games)

# game outcomes
outcome_season_records <- all_games %>%
  mutate(Wins = ifelse(Team.Pts > Opp.Pts, 1, 0),
         Loses = ifelse(Team.Pts < Opp.Pts, 1, 0),
         Ties = ifelse(Team.Pts == Opp.Pts, 1, 0)) %>%
  arrange(Team,
          Wk) %>%
  group_by(Season,
           Team) %>%
  mutate(Season.Wins = cumsum(Wins),
         Total.Wins = cumsum(Wins),
         Season.Losses = cumsum(Loses),
         Season.Ties = cumsum(Ties),
         Total.Games = Season.Wins + Season.Losses + Season.Ties,
         Won.6 = max(Season.Wins) >= 6) %>%
  ungroup()

# How many times has a team gone a specific record?
outcome_pivot <- outcome_season_records %>%
  # Look at seasons after overtime introduced
  filter(Season > 1995) %>%
  unite(Team.Record, c('Season.Wins', 'Season.Losses'), sep = '-') %>%
  select(Season,
         Team,
         Total.Games,
         Won.6,
         Team.Record) %>%
  group_by(Season,
           Team) %>%
  filter(max(Total.Games) >= 6) %>%
  #spread(Total.Games, 
  #       Team.Record) %>%
  #ungroup() %>%
  mutate(Team_Season = paste(Team, Season, sep = '_')) %>%
  group_by(Won.6,
           Total.Games,
           Team.Record) %>%
  summarise(Teams = n_distinct(Team_Season)) %>%
  ungroup() %>%
  group_by(Team.Record) %>%
  mutate(Total.Outcomes = sum(Teams)) %>%
  ungroup() %>%
  mutate(Game.Outcome = Teams/Total.Outcomes)

overall_freq <- outcome_pivot %>%
  group_by(Team.Record,
           Total.Games) %>%
  summarise(n = sum(Teams)) %>%
  ungroup() %>%
  arrange(Total.Games) %>%
  group_by(Total.Games) %>%
  mutate(Total.N = sum(n)) %>%
  ungroup() %>%
  mutate(Pct.Freq = n/Total.N) %>%
  select(Team.Record,
         Total.Games,
         Pct.Freq)

overall_pivot_freq <- inner_join(outcome_pivot,
                                 overall_freq,
                                 by = c('Team.Record' = 'Team.Record',
                                        'Total.Games' = 'Total.Games'))

mark1 <- overall_pivot_freq %>%
  select(Team.Record,
         Total.Games,
         Pct.Freq,
         Won.6,
         Game.Outcome) %>%
  group_by(Total.Games,
           Team.Record) %>%
  spread(Won.6,
         Game.Outcome) %>%
  arrange(Total.Games) %>%
  select(Team.Record,
         Total.Games,
         Pct.Freq,
         Won.6 = `TRUE`,
         Lost.More = `FALSE`)



#