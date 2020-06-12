# Big 12 Box Stats

library(ggplot2)
library(tidyr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(stringr)

# Load the Data
big_12_reg <- read.csv('2019 Reg Season Stats.csv')
big_12_post <- read.csv('2019 Post Season Stats.csv')

big_12_schedule <- read.csv('2019 Schedule.csv')

full_big_12_stats <- bind_rows(big_12_reg, big_12_post)

# create a dataframe of the game_id and dates
sched_dates <- big_12_schedule %>%
  select(game_id = id,
         date = start_date) %>%
  mutate(date = substr(date, 1, 10),
         date = ymd(date)) %>%
  distinct(game_id,
           date)

full_big_12_stats_dates <- inner_join(sched_dates, full_big_12_stats, by = c('game_id' = 'game_id'))

####  Recreate basic win-loss data ####   
team1 <- full_big_12_stats_dates %>%
  distinct(date,
           game_id,
           school,
           conference,
           homeAway,
           points) %>%
  filter(conference == 'Big 12') %>%
  arrange(school,
          date)

team2 <- full_big_12_stats_dates %>%
  distinct(date,
           game_id,
           school,
           conference,
           homeAway,
           points) %>%
  arrange(school,
          date)

team_1_2_join <- inner_join(team1, team2, by = c('game_id' = 'game_id', 'date' = 'date'))

full_scores <- team_1_2_join %>%
  filter(school.x != school.y) %>%
  select(date,
         school = school.x,
         sch_conf = conference.x,
         location = homeAway.x,
         school_pts = points.x,
         opponent = school.y,
         opp_conf = conference.y,
         opponent_pts = points.y) %>%
  arrange(school,
          date) %>%
  group_by(school) %>%
  mutate(game_no = row_number()) %>%
  ungroup() #%>%
  #filter(game_no <= 12)

wins_losses <- full_scores %>%
  mutate(wins = ifelse(school_pts > opponent_pts, 1, 0),
         loses = ifelse(school_pts < opponent_pts, 1, 0)) %>%
  group_by(school) %>%
  summarise(total_wins = sum(wins),
            total_losses = sum(loses),
            points_for = sum(school_pts),
            points_agt = sum(opponent_pts),
            avg_points_for = mean(school_pts),
            avg_points_agt = mean(opponent_pts)) %>%
  ungroup() %>%
  mutate(plus_minus = points_for - points_agt) %>%
  # Arrange the teams by total wins
  arrange(desc(total_wins))


####  How did the Big 12 perform by other statistics? ####
head(full_big_12_stats_dates)

# How many first downs did each team get? What was the average, and for the conference overall?s
total_first_downs <- full_big_12_stats_dates %>%
  filter(conference == 'Big 12') %>%
  filter(grepl('firstDowns', stat_category)) %>%
  mutate(stat = as.character(stat),
         stat = as.integer(stat),
         avg_1st_downs = mean(stat)) %>%
  group_by(school) %>%
  mutate(school_avg_1st_downs = mean(stat),
         highest_1st_downs = max(stat),
         least_first_downs = min(stat)) %>%
  slice(which.max(date)) %>%
  ungroup() %>%
  select(school,
         avg_1st_downs,
         school_avg_1st_downs,
         highest_1st_downs,
         least_first_downs)

# How efficient are teams at converting on the third and fourth down?
# Tom Osborne talked about 45% conversion rate for success

downs <- full_big_12_stats_dates %>%
  filter(conference == 'Big 12') %>%
  filter(stat_category %in% c('fourthDownEff', 'thirdDownEff'))

downs_sep <- separate(data = mark1, col = stat, into = c("conversions", "attempts"), sep = "\\-")

downs_conversion_games <- downs_sep %>%
  mutate(conversions = as.character(conversions),
         conversions = as.integer(conversions),
         attempts = as.character(attempts),
         attempts = as.integer(attempts)) %>%
  group_by(school,
           date,
           stat_category) %>%
  summarise(conversions = sum(conversions),
            attempts = sum(attempts)) %>%
  ungroup() %>%
  mutate(pct_conversion = conversions/attempts) %>%
  arrange(desc(stat_category),
          desc(pct_conversion))

downs_conversion_pct <- downs_sep %>%
  mutate(conversions = as.character(conversions),
         conversions = as.integer(conversions),
         attempts = as.character(attempts),
         attempts = as.integer(attempts)) %>%
  group_by(school,
           stat_category) %>%
  summarise(conversions = sum(conversions),
            attempts = sum(attempts)) %>%
  ungroup() %>%
  mutate(pct_conversion = conversions/attempts) %>%
  arrange(desc(stat_category),
          desc(pct_conversion))

# How many completions did each team make for every attempt?
completionAttempts <- full_big_12_stats %>%
  filter(conference == 'Big 12') %>%
  filter(stat_category %in% c('completionAttempts'))

big_12_cmpAtt <- separate(data = completionAttempts, col = stat, into = c("completions", "attempts"), sep = "\\-")

passes_completed <- big_12_cmpAtt %>%
  mutate(completions = as.character(completions),
         completions = as.integer(completions),
         attempts = as.character(attempts),
         attempts = as.integer(attempts)) %>%
  group_by(school) %>%
  summarise(completions = sum(completions),
            attempts = sum(attempts)) %>%
  ungroup() %>%
  mutate(pct = completions/attempts)

# How many touchdowns resulted from passes?
passes_touchdown <- full_big_12_stats %>%
  filter(conference == 'Big 12') %>%
  filter(stat_category %in% c('passingTDs')) %>%
  mutate(passingTD = as.character(stat),
         passingTD = as.integer(passingTD)) %>%
  group_by(school) %>%
  summarise(passingTDs = sum(passingTD))

# How many interceptions were thrown per attempt?
passes_intercepted <- full_big_12_stats %>%
  filter(conference == 'Big 12') %>%
  filter(stat_category %in% c('interceptions')) %>%
  mutate(interceptions = as.character(stat),
         interceptions = as.integer(interceptions)) %>%
  group_by(school) %>%
  summarise(interceptions = sum(interceptions))

# Bring all the passes, touchdowns, and interceptions together
passes_touchdowns <- inner_join(passes_completed, passes_touchdown, by = c('school' = 'school'))
passes_touchdowns_int <- inner_join(passes_touchdowns, passes_intercepted, by = c('school' = 'school'))

passes_touchdowns_int %>%
  mutate(touchdown_ratio = passingTDs/interceptions,
         pct_interceptions = interceptions/attempts,
         ratio_rank = dense_rank(desc(touchdown_ratio)),
         interception_rank = dense_rank(desc(pct_interceptions))) %>%
  arrange(desc(touchdown_ratio))


