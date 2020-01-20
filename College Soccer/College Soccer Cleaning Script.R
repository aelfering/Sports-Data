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

# Next we need to scrub the team and opponent names to remove "University" and "College" where appropriate
# We won't do this for schools like Boston College, Colorado College, or College of Charleston...what would they be without College?!

clean_these_names <- c('Iona College', 'Middle Georgia College', 'Central Baptist College', 'Central Baptist College', 
                       'Jarvis Christian College', 'Boston University', 'Troy University', 'Lamar University',
                       'Hampton University', 'Ohio University', 'King University', 'Columbia International University',
                       'Union University', 'Southern University')

non_college_uni_opp_names <- dplyr::filter(soccer_games_cleaned, !Opponent.Name %in% clean_these_names)

scrub_college_uni_opp_names <- soccer_games_cleaned %>%
  filter(Opponent.Name %in% clean_these_names) %>%
  mutate(Opponent.Name = gsub("College|University", "", Opponent.Name))

opponent_names_cleaned <- bind_rows(non_college_uni_opp_names, scrub_college_uni_opp_names)

opponent_names_cleaned %>%
  filter(grepl("College|University", Team)) %>%
  distinct(Team)

# Now cleaning team names
non_college_uni_team_names <- dplyr::filter(opponent_names_cleaned, !Team %in% c('Hampton University', 'Southern University'))      

scrub_team_names <- opponent_names_cleaned %>%
  filter(Team %in% c('Hampton University', 'Southern University')) %>%
  mutate(Team = gsub('University', '', Team))

# Team and Opponent names scrubbed
clean_team_opponent_names <- bind_rows(non_college_uni_team_names, scrub_team_names)
  




















  




