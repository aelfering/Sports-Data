# Big 12 Box Stats

library(ggplot2)
library(tidyr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(stringr)

# Load the Data
big_12_stats <- read.csv('data.csv')

# Recreate basic win-loss data
one_side <- big_12_stats %>%
  distinct(game_id,
           school,
           conference,
           homeAway,
           points) %>%
  filter(conference == 'Big 12') %>%
  arrange(school,
          game_id)

other_side <- big_12_stats %>%
  distinct(game_id,
           school,
           conference,
           homeAway,
           points) %>%
  arrange(school,
          game_id)

mark1 <- inner_join(one_side, other_side, by = c('game_id' = 'game_id'))

full_scores <- mark1 %>%
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

