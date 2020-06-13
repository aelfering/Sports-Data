# More complex Big 12 Deep Dive

library(ggplot2)
library(dplyr)
library(tidyverse)
library(tidyr)
library(lubridate)
library(data.table)

# Pinpoint the WD
setwd("~/Documents/GitHub/Sports-Data/College Football/Big 12 Deep Dive")

# List the schedule and team statistics files
schedules <- list.files("~/Documents/GitHub/Sports-Data/College Football/Big 12 Deep Dive/Schedules", pattern = "*.csv", full.names = TRUE)
team_stats <- list.files("~/Documents/GitHub/Sports-Data/College Football/Big 12 Deep Dive/Team Stats", pattern = "*.csv", full.names = TRUE)

schedules_df <- as.data.frame(rbindlist(lapply(schedules, fread)))
team_stats_df <- as.data.frame(rbindlist(lapply(team_stats, fread)))

# Clean the schedule data.frame
head(schedules_df)

schedule_dates <- schedules_df %>%
  mutate(date = substr(start_date, 1, 10),
         date = ymd(date)) %>%
  select(game_id = id,
         season,
         date) %>%
  distinct(game_id,
           season,
           date)

# join the schedules data frame with stats
team_stats_with_dates <- inner_join(schedule_dates, team_stats_df, by = c('game_id' = 'game_id'))

####  Recreate wins and losses to validate the data  ####
team_points <- team_stats_with_dates %>%
  distinct(school,
           conference,
           season,
           date,
           game_id,
           points)

big_12_points <- subset(team_points, conference == 'Big 12')
points_join <- inner_join(big_12_points, team_points, by = c('date' = 'date', 'game_id' = "game_id", 'season' = 'season'))

# Clean up the data frame to remove redundancy
clean_points <- points_join %>%
  filter(school.x != school.y) %>%
  select(season,
         date,
         team = school.x,
         opponent = school.y,
         tm.conf = conference.x,
         opp.conf = conference.y,
         team.pts = points.x,
         opp.pts = points.y)

# Conference play
big_12_play <- points_join %>%
  filter(school.x != school.y) %>%
  select(season,
         date,
         team = school.x,
         opponent = school.y,
         tm.conf = conference.x,
         opp.conf = conference.y,
         team.pts = points.x,
         opp.pts = points.y) %>%
  filter(tm.conf == opp.conf)

# Find wins and losses by season and team
clean_points %>%
  mutate(wins = ifelse(team.pts > opp.pts, 1, 0),
         loses = ifelse(team.pts < opp.pts, 1, 0)) %>%
  group_by(season,
           team) %>%
  summarise(wins = sum(wins),
            losses = sum(loses)) %>%
  ungroup()

big_12_play %>%
  mutate(wins = ifelse(team.pts > opp.pts, 1, 0),
         loses = ifelse(team.pts < opp.pts, 1, 0)) %>%
  group_by(season,
           team) %>%
  summarise(wins = sum(wins),
            losses = sum(loses)) %>%
  ungroup()

####  What is the touchdown-interception ratio by team each season? ####
passingTD <- team_stats_with_dates %>%
  filter(stat_category == 'passingTDs') %>%
  select(season,
         game_id,
         date,
         school,
         conference,
         homeAway,
         stat) %>%
  mutate(stat = as.character(stat),
         passingTD = as.integer(stat)) %>%
  select(season,
         game_id,
         date,
         school,
         conference,
         homeAway,
         passingTD)

passingINT <- team_stats_with_dates %>%
  filter(stat_category == 'interceptions') %>%
  select(season,
         game_id,
         date,
         school,
         conference,
         homeAway,
         stat) %>%
  mutate(stat = as.character(stat),
         passingINT = as.integer(stat)) %>%
  select(season,
         game_id,
         date,
         school,
         conference,
         homeAway,
         passingINT)

passing_td_int <- inner_join(passingTD, 
                             passingINT, 
                             by = c('season' = 'season',
                                    'game_id' = 'game_id',
                                    'date' = 'date',
                                    'school' = 'school',
                                    'homeAway' = 'homeAway',
                                    'conference' = 'conference'))

ratio_calc <- passing_td_int %>%
  filter(conference == 'Big 12') %>%
  group_by(season,
           school) %>%
  summarise(passingTDs = sum(passingTD),
            interceptions = sum(passingINT)) %>%
  ungroup() %>%
  mutate(ratio = passingTDs/interceptions)

ggplot(ratio_calc,
       aes(x = season,
           y = ratio,
           group = school)) +
  geom_bar(stat = 'identity',
           position = 'identity',
           fill = 'orange') +
  geom_hline(yintercept = 0) +
  facet_wrap(~school)



####  What is the conversion rate on the third and fourth down by team each season? ####
downs <- team_stats_with_dates %>%
  filter(stat_category %in% c('thirdDownEff', 'fourthDownEff'),
         conference == 'Big 12') %>%
  select(game_id,
         season,
         date,
         school,
         stat_category,
         stat)

downs_sep <- separate(downs, stat, into = c('conversions', 'attempts'), sep = "\\-")

downs_sum <- downs_sep %>%
  mutate(conversions = as.character(conversions),
         conversions = as.integer(conversions),
         attempts = as.character(attempts),
         attempts = as.integer(attempts)) %>%
  group_by(season,
           school) %>%
  summarise(conversions = sum(conversions),
            attempts = sum(attempts)) %>%
  ungroup() %>%
  mutate(pct_conversion = conversions/attempts)

ggplot(downs_sum,
       aes(x = season,
           y = pct_conversion,
           group = school)) +
  geom_bar(stat = 'identity',
           position = 'identity') +
  facet_wrap(~school)
####  What is the completion rate of passes? How does that relate to ratio of interceptions and sacks per attempt?  ####
head(team_stats_with_dates)

# Completions and Attempts
cmpAtt <- team_stats_with_dates %>%
  filter(stat_category == 'completionAttempts')

cmpAtt_sep <- separate(cmpAtt, stat, into = c('completions', 'attempts'), sep = "\\-")

cmpAtt_stats <- cmpAtt_sep %>%
  mutate(completions = as.character(completions),
         completions = as.integer(completions),
         attempts = as.character(attempts),
         attempts = as.integer(attempts)) %>%
  filter(conference == 'Big 12') %>%
  group_by(season,
           school) %>%
  summarise(completions = sum(completions),
            attempts = sum(attempts)) %>%
  ungroup() %>%
  mutate(pct_completion = completions/attempts)

ggplot(cmpAtt_stats,
       aes(x = season,
           y = pct_completion,
           group = school)) +
  geom_line() +
  facet_wrap(~school)

interceptions <- team_stats_with_dates %>%
  filter(stat_category == 'interceptions',
         conference == 'Big 12') %>%
  mutate(stat = as.character(stat),
         stat = as.integer(stat)) %>%
  group_by(season,
           school) %>%
  summarise(interceptions = sum(stat)) %>%
  ungroup()

passingTDs <- team_stats_with_dates %>%
  filter(stat_category == 'passingTDs',
         conference == 'Big 12') %>%
  mutate(stat = as.character(stat),
         stat = as.integer(stat)) %>%
  group_by(season,
           school) %>%
  summarise(passingTDs = sum(stat)) %>%
  ungroup()

# To find the number of sacks is a little tricky
# You need to find what the opponents sacks were to find the school's sacks
big12sacks <- team_stats_with_dates %>%
  filter(stat_category == 'sacks',
         conference == 'Big 12') %>%
  mutate(stat = as.character(stat),
         stat = as.integer(stat)) %>%
  group_by(season,
           game_id,
           date,
           school) %>%
  summarise(sacks = sum(stat)) %>%
  ungroup()

allsacks <- team_stats_with_dates %>%
  filter(stat_category == 'sacks') %>%
  mutate(stat = as.character(stat),
         stat = as.integer(stat)) %>%
  group_by(season,
           game_id,
           date,
           school) %>%
  summarise(sacks = sum(stat)) %>%
  ungroup()

sacks_join <- inner_join(big12sacks, allsacks, by = c('season' = 'season', "game_id" = "game_id", "date" = "date"))

sacks_df_clean <- sacks_join %>%
  filter(school.x != school.y) %>%
  select(season,
         game_id,
         date,
         school = school.x,
         school.sacks = sacks.x,
         opponent = school.y,
         opponent.sacks = sacks.y) %>%
  group_by(season,
           school) %>%
  summarise(sacks_allowed = sum(opponent.sacks)) %>%
  ungroup()

# Join all of the stats together
cmpAtt_int <- inner_join(cmpAtt_stats, interceptions, by = c('season' = 'season', "school" = "school"))
cmpAtt_int_TDs <- inner_join(cmpAtt_int, passingTDs, by = c('season' = 'season', "school" = "school"))
offensiveLine_stats <- left_join(cmpAtt_int_TDs, sacks_df_clean, by = c('season' = 'season', "school" = "school"))

offensive_drive <- offensiveLine_stats %>%
  mutate(pct_interceptions = interceptions/attempts,
         pct_touchdowns = passingTDs/attempts,
         pct_sacks = sacks_allowed/attempts)

