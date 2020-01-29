# College Soccer Data Cleaning

library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)
library(reshape2)
library(stringi)
library(stringr)

soccer <- read.csv('College Soccer.csv')
conferences <- read.csv('Soccer Conference Data.csv')
united_rankings <- read.csv('United Soccer Coach Rankings.csv') #Final united soccer coach rankings thanks to NCAA

####  Data Tidying ####
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

####  Cleaning United Soccer Coach Rankings ####
ranks_united <- united_rankings %>%
  mutate(School = gsub("\\(", "", School)) %>%
  mutate(School = gsub("\\)", "", School)) %>%
  mutate(School = gsub(' [[:digit:]]+', '', School)) %>%
  mutate(School = gsub("Saint Mary's CA", "St. Mary's (CA)", School)) %>%
  mutate(School = gsub("Southern California", "USC", School)) %>%
  mutate(School = gsub("North Carolina State", "NC State", School)) %>%
  mutate(School = gsub("Southern Methodist", "SMU", School)) %>%
  select(United.Soccer.Coaches.Ranking,
         School,
         Gender)

####  Data Manipulation ####
# Adding additional columns such as a flag for win, loss, and tie as well as game number
soccer_games_cleaned <- soccer_with_scores %>%
  select(Month, Date, Season, Team, Opponent.Name, Location, Gender, Division, Result, Team.Score, Opp.Score) %>%
  mutate(Wins = ifelse(Result == "W", 1, 0),
         Loses = ifelse(Result == "L", 1, 0),
         Ties = ifelse(Result == "D", 1, 0),
         Team.Score = as.numeric(Team.Score),
         Opp.Score = as.numeric(Opp.Score)) %>%
  group_by(Team, Division, Gender) %>%
  mutate(Game.Number = row_number(),
         Shut.Out = ifelse(Team.Score == 0, 1, 0),
         Shut.Outs = ifelse(Opp.Score == 0, 1, 0)) %>%
  ungroup() %>%
  as.data.frame()

# Adding conferences
soccer_games_team_conference <- left_join(soccer_games_cleaned, conferences, by = c('Team' = 'School', 'Gender' = 'Gender'))
soccer_games_opp_conference <- left_join(soccer_games_team_conference, conferences, by = c('Opponent.Name' = 'School', 'Gender' = 'Gender'))

soccer_games_basic <- soccer_games_opp_conference %>%
  select(Month, 
         Date,
         Season,
         Team,
         Opponent.Name,
         Location,
         Gender,
         Division,
         Result,
         Team.Score,
         Opp.Score,
         Wins,
         Loses,
         Ties,
         Game.Number,
         Shut.Out,
         Shut.Outs,
         Team.Conference = Conference.x,
         Opp.Conference = Conference.y)

####  Plus-Minus Scores ####
# Creating plus-minus scores
plus_minus_soccer <- soccer_games_basic %>%
  group_by(Team, Team.Conference, Gender) %>%
  summarise(Goals.For = sum(Team.Score),
            Goals.Against = sum(Opp.Score),
            Total.Wins = sum(Wins),
            Total.Losses = sum(Loses),
            Total.Ties = sum(Ties),
            Total.Games = max(Game.Number),
            Total.Shut.Out = sum(Shut.Out),
            Total.Shut.Outs = sum(Shut.Outs))%>%
  ungroup() %>%
  mutate(Plus.Minus = Goals.For - Goals.Against,
         Pct.Won = Total.Wins/Total.Games) %>%
  group_by(Gender) %>%
  mutate(Plus.Minus.Rank = dense_rank(desc(Plus.Minus))) %>%
  ungroup()

####  Conference Play ####
# Need more time with this section...please stay tuned!
soccer_games_cleaned_conf <- left_join(soccer_games_cleaned, conferences, by = c('Team' = 'School', 'Gender' = 'Gender'))
soccer_games_opp_conf <- left_join(soccer_games_cleaned_conf, conferences, by = c('Opponent.Name' = 'School', 'Gender' = 'Gender'))

conf_rec <- soccer_games_opp_conf %>%
  filter(Conference.x == Conference.y) %>%
  group_by(Team, 
           Conference.x, 
           Gender) %>%
  summarise(Conf.Wins = sum(Wins),
            Conf.Losses = sum(Loses),
            Conf.Ties = sum(Ties),
            Conf.Goals.For = sum(Team.Score),
            Conf.Goals.Against = sum(Opp.Score)) %>%
  ungroup() %>%
  mutate(Total.Games = Conf.Wins + Conf.Losses + Conf.Ties) %>%
  group_by(Conference.x,
           Gender) %>%
  mutate(Pct.Conf = Conf.Wins/Total.Games,
         Conf.Plus.Minus = Conf.Goals.For - Conf.Goals.Against) %>%
  mutate(Conf.Rank = dense_rank(desc(Pct.Conf))) %>%
  ungroup() %>%
  as.data.frame()

####  Estimating Overall Opponent Strength of Schedule ####
opponent_record <- dplyr::select(plus_minus_soccer,
                                 Team,
                                 Gender,
                                 Goals.For,
                                 Goals.Against, 
                                 Total.Wins,
                                 Total.Losses,
                                 Total.Ties)

opponent_game_record <- left_join(soccer_games_cleaned, opponent_record, by = c('Opponent.Name' = 'Team',
                                                                                'Gender' = 'Gender'))

opponent_game_record[is.na(opponent_game_record)] <- 0

distinct_games <- opponent_game_record %>%
  #to adjust for rematches
  group_by(Team, Gender, Opponent.Name) %>%
  summarise(Game.Number = max(Game.Number)) %>%
  ungroup() %>%
  as.data.frame()

distinct_matches <- inner_join(opponent_game_record, distinct_games, by = c('Gender' = 'Gender',
                                                                  'Team' = 'Team',
                                                                  'Opponent.Name' = 'Opponent.Name',
                                                                  'Game.Number' = 'Game.Number'))

sos_overall <- distinct_matches %>%
  filter(Result == 'W') %>%
  group_by(Team, 
           Gender) %>%
  summarise(Opp.Wins = sum(Total.Wins),
            Opp.Losses = sum(Total.Losses),
            Opp.Ties = sum(Total.Ties),
            Opp.Goals.For = sum(Goals.For),
            Opp.Goals.Against = sum(Goals.Against)) %>%
  ungroup() %>%
  mutate(Total.Games = Opp.Wins + Opp.Losses + Opp.Ties) %>%
  mutate(Opp.Pct.Won = Opp.Wins/Total.Games) %>%
  as.data.frame() %>%
  select(Team,
         Gender,
         Opp.Wins,
         Opp.Losses,
         Opp.Ties,
         Opp.Goals.For,
         Opp.Goals.Against,
         Opp.Pct.Won) 

college_soccer_data <- left_join(plus_minus_soccer, sos_overall, by = c('Gender' = 'Gender',
                                                                        'Team' = 'Team'))

#### Ranking the top five conferences by gender who won the most games ####
conference_performance <- college_soccer_data %>%
  group_by(Team.Conference,
           Gender) %>%
  summarise(Total.Wins = sum(Total.Wins),
            Total.Losses = sum(Total.Losses),
            Total.Ties = sum(Total.Ties),
            Total.Games = sum(Total.Games)) %>%
  ungroup() %>%
  mutate(Pct.Won = Total.Wins/Total.Games) %>%
  group_by(Gender) %>%
  mutate(Rank.Pct.Won = dense_rank(desc(Pct.Won))) %>%
  ungroup() %>%
  arrange(Gender, 
          Rank.Pct.Won) %>%
  select(Team.Conference,
         Gender,
         Conference.Pct.Won = Pct.Won,
         Rank.Pct.Won)

soccer_conf_games <- left_join(college_soccer_data, conference_performance, by = c('Team.Conference' = 'Team.Conference',
                                                                                   'Gender' = 'Gender'))

####  Exporting the dataset ####
write.csv(soccer_conf_games, file = 'college soccer team conference performance.csv')
