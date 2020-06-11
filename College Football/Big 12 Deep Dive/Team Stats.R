# Big 12 Box Stats

library(ggplot2)
library(tidyr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(stringr)

# Load the Data
big_12_stats <- read.csv('data.csv')

#### Recreate basic win-loss data ####   
team1 <- big_12_stats %>%
  distinct(game_id,
           school,
           conference,
           homeAway,
           points) %>%
  filter(conference == 'Big 12') %>%
  arrange(school,
          game_id)

team2 <- big_12_stats %>%
  distinct(game_id,
           school,
           conference,
           homeAway,
           points) %>%
  arrange(school,
          game_id)

team_1_2_join <- inner_join(team1, team2, by = c('game_id' = 'game_id'))

full_scores <- team_1_2_join %>%
  filter(school.x != school.y) %>%
  select(game_id,
         school = school.x,
         sch_conf = conference.x,
         location = homeAway.x,
         school_pts = points.x,
         opponent = school.y,
         opp_conf = conference.y,
         opponent_pts = points.y) %>%
  arrange(school,
          game_id)

wins_losses <- full_scores %>%
  mutate(wins = ifelse(school_pts > opponent_pts, 1, 0),
         loses = ifelse(school_pts < opponent_pts, 1, 0)) %>%
  group_by(school) %>%
  summarise(total_wins = sum(wins),
            total_losses = sum(loses),
            points_for = sum(school_pts),
            points_agt = sum(opponent_pts)) %>%
  ungroup() %>%
  mutate(plus_minus = points_for - points_agt) %>%
  # Arrange the teams by total wins
  arrange(desc(total_wins))

