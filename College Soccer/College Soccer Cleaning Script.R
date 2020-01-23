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

soccer_games_cleaned %>%
  mutate(Margin = Team.Score - Opp.Score) %>%
  group_by(Gender) %>%
  summarise(Mean.Margin = mean(Margin)) %>%
  ungroup()

head(soccer_games_cleaned)

plus_minus_soccer <- soccer_games_cleaned %>%
  group_by(Team, Gender) %>%
  summarise(Goals.For = sum(Team.Score),
            Goals.Against = sum(Opp.Score),
            Total.Wins = sum(Wins),
            Total.Losses = sum(Loses),
            Total.Ties = sum(Ties),
            Total.Games = max(Game.Number))%>%
  ungroup() %>%
  mutate(Plus.Minus = Goals.For - Goals.Against,
         Pct.Won = Total.Wins/Total.Games) %>%
  group_by(Gender) %>%
  mutate(Plus.Minus.Rank = dense_rank(desc(Plus.Minus))) %>%
  ungroup()

write.csv(plus_minus_soccer, file = 'college soccer plus minus.csv')
  
  



















  




