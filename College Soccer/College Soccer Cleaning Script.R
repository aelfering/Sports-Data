# College Soccer Data Cleaning

library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)
library(reshape2)
library(stringi)
library(stringr)

soccer <- read.csv('College Soccer.csv')

# Separating Score from the Result
soccer$Clean.Score <- gsub("[^ -~]", '', soccer$Score) 
soccer$Just.Score <- str_sub(soccer$Clean.Score, 1, str_length(soccer$Clean.Score)-3)
soccer$Result <- str_sub(soccer$Clean.Score,-3,-1)
soccer$Result <- gsub("\\(", "", soccer$Result)
soccer$Result <- gsub("\\)", "", soccer$Result)

# Separating vs/at Opponent Name
soccer$Clean.Opponent <- gsub("[^ -~]", '', soccer$Opponent) 
soccer$Opponent.Name <- substring(soccer$Clean.Opponent, 3)
soccer$Location <- substring(soccer$Clean.Opponent, 1, 2)

soccer$Opponent.Name <- gsub("\\*", "", soccer$Opponent.Name)

soccer_with_scores <- separate(data = soccer, col = Just.Score, into = c("Team.Score", "Opp.Score"), sep = "-")

# Scrub the team name to remove college/university where appropriate and add additional columns for analysis
soccer_games_cleaned <- soccer_with_scores %>%
  mutate(Team = gsub("University", "", Team)) %>%
  select(Month, Date, Season, Team, Opponent.Name, Location, Gender, Division, Result, Team.Score, Opp.Score) %>%
  mutate(Wins = ifelse(Result == "W", 1, 0),
         Loses = ifelse(Result == "L", 1, 0),
         Ties = ifelse(Result == "D", 1, 0),
         Team.Score = as.numeric(Team.Score),
         Opp.Score = as.numeric(Opp.Score)) %>%
  group_by(Team, Division, Gender) %>%
  mutate(Game.Number = row_number()) %>%
  ungroup() %>%
  as.data.frame()

# Scrub Opponent Name

college_opp_names <- soccer_games_cleaned %>%
  filter(grepl("College", Opponent.Name)) %>%
  filter(!Opponent.Name %in% c('Siena College', 'Boston College')) %>%
  gsub('College', '', Opponent.Name) %>%
  distinct(Opponent.Name)

uni_opp_names <- soccer_games_cleaned %>%
  filter(grepl("University", Opponent.Name)) %>%
  filter(!Opponent.Name %in% c('University of the Southwest')) %>%
  distinct(Opponent.Name)

keep_names <- c('')

soccer_games_cleaned %>%
  group_by(Team, Gender, Division) %>%
  summarise(Goals.For = sum(Team.Score),
            Goals.Against = sum(Opp.Score)) %>%
  mutate(Diff = Goals.For - Goals.Against) %>%
  ungroup() %>%
  arrange(Diff) %>%
  as.data.frame()
  




