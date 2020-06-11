# CFB Player Analysis
# Sand Boxing 

# Code Developed by Alex Elfering
# initially published 8 June 2020

library(ggplot2)
library(grid)
library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)

# Load the data
cfb_passing <- read.csv('cfb passing.csv')
rosters <- read.csv('cfb team rosters.csv')
seasons <- read.csv('cfb seasons.csv')

# Joining player position, season, and stats together 
roster_cfb_stats <- subset(rosters, 
                           Pos == 'QB' & Summary != '')

cfb_passing_with_seasons <- inner_join(seasons, 
                                       cfb_passing, 
                                       by = c('Date' = 'Date'))

cfb_passing_qbs <- inner_join(roster_cfb_stats, 
                              cfb_passing_with_seasons, 
                              by = c('School' = 'School', 'Player' = 'Player', 'Season' = 'Season'))

####  Overall Statistics  ####
freq_players <- cfb_passing_qbs %>%
  group_by(Player,
           School) %>%
  summarise(Beg_Season = min(Season),
            Fin_Season = max(Season),
            Games = n_distinct(Date),
            Cmp = sum(Cmp),
            Att = sum(Att),
            Yds = sum(Yds),
            TD = sum(TD),
            Int = sum(Int),
            Avg_Pct = mean(Pct)) %>%
  mutate(Pct_Cmp = Cmp/Att,
         Cmp_per_Game = Cmp/Games,
         Att_per_Game = Att/Games,
         Yds_per_Game = Yds/Games,
         Yds_per_Att = Yds/Att,
         Yds_per_Cmp = Yds/Cmp,
         TD_per_Game = TD/Games,
         Int_per_Game = Int/Games,
         Int_per_Att = Int/Att,
         TD_per_Att = TD/Att) %>%
  filter(Att > 100) %>%
  arrange(desc(Games))

avg_td_att <- mean(freq_players$TD_per_Att)
avg_cmp_att <- mean(freq_players$Pct_Cmp)
avg_int_att <- mean(freq_players$Int_per_Att)

ggplot(freq_players,
       aes(TD_per_Att,
           group = Player)) +
  geom_histogram(color = 'white')

ggplot(freq_players,
       aes(Int_per_Att,
           group = Player)) +
  geom_histogram(color = 'white') +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = avg_int_att,
             linetype = 'dashed')

ggplot(freq_players,
       aes(x = Pct_Cmp,
           y = Int_per_Att,
           group = Player)) +
  geom_point(alpha = 0.6,
             color = 'maroon',
             size = 2) +
  geom_vline(xintercept = avg_cmp_att,
             linetype = 'dashed',
             size = 1) +
  geom_hline(yintercept = avg_int_att,
             linetype = 'dashed',
             aes(size = Att))



####  What are overall statistics by class? ####
class_standings <- cfb_passing_qbs %>%
  group_by(Player,
           Class) %>%
  mutate(Attempts = sum(Att)) %>%
  filter(Attempts >= 100) %>%
  group_by(Class) %>%
  summarise(Games = n(),
            Cmp = sum(Cmp),
            Att = sum(Att),
            Yds = sum(Yds),
            TD = sum(TD),
            Int = sum(Int),
            Avg_Pct = mean(Pct)) %>%
  ungroup() %>%
  mutate(Pct_Cmp = Cmp/Att,
         Cmp_per_Game = Cmp/Games,
         Att_per_Game = Att/Games,
         Yds_per_Game = Yds/Games,
         TD_per_Game = TD/Games,
         Int_per_Game = Int/Games,
         Int_per_Att = Int/Att,
         TD_per_Att = TD/Att) 

####  Mark #1 ####
str(cfb_passing_qbs)

qb_running <- cfb_passing_qbs %>%
  mutate(Date = ymd(Date)) %>%
  arrange(Player, Date) %>%
  group_by(Player) %>%
  mutate(Running_Att = cumsum(Att),
         Running_Cmp = cumsum(Cmp)) %>%
  ungroup() %>%
  #filter(Running_Att >= 100) %>%
  group_by(Player) %>%
  mutate(Game_No = row_number(),
         Rolling_Att = rollapplyr(Att, 6, sum, partial = TRUE),
         Rolling_Cmp = rollapplyr(Cmp, 6, sum, partial = TRUE),
         Rolling_TD = rollapplyr(TD, 6, sum, partial = TRUE),
         Rolling_Int = rollapplyr(Int, 6, sum, partial = TRUE),
         Rolling_YDs = rollapplyr(Yds, 6, sum, partial = TRUE),
         Rolling_Avg_YDs = rollapplyr(Yds, 6, mean, partial = TRUE),
         Rolling_Pct = Rolling_Cmp/Rolling_Att,
         Rolling_TD_Pct = Rolling_TD/Rolling_Att,
         Rolling_Int_Pct = Rolling_Int/Rolling_Att) %>%
  filter(n() > 1) %>%
  ungroup() %>%
  mutate(Running_Pct = Running_Cmp/Running_Att) %>%
  select(Season,
         Game_No,
         Player,
         School,
         Date,
         Opponent,
         Att,
         Cmp,
         Yds,
         Int,
         TD,
         Running_Att,
         Running_Cmp,
         Running_Pct,
         Rolling_Att,
         Rolling_Cmp,
         Rolling_Pct,
         Rolling_YDs,
         Rolling_Avg_YDs,
         Rolling_TD_Pct,
         Rolling_Int_Pct)

ggplot(subset(qb_running, 
              Player == 'Jake Browning\\jake-browning-1'),
       aes(x = Game_No,
           y = Rolling_Pct,
           group = Player)) +
  geom_line(size = 1,
            aes(group = Season)) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = 'Percent of Passes Completed Every 6 Games for Adrian Martinez',
       subtitle = 'After the 100th Cumulative Attempt')

ggplot(subset(qb_running, 
              Player == 'Jake Browning\\jake-browning-1'),
       aes(x = Game_No,
           y = Rolling_Avg_YDs,
           group = Player)) +
  geom_line(size = 1,
            aes(group = Season))

ggplot(subset(qb_running, 
              Player == 'Jake Browning\\jake-browning-1'),
       aes(x = Game_No,
           y = Rolling_TD_Pct,
           group = Player)) +
  geom_line(size = 1,
            aes(group = Season)) +
  scale_y_continuous(labels = scales::percent)

ggplot(subset(qb_running, 
              Player == 'Jake Browning\\jake-browning-1'),
       aes(x = Game_No,
           y = Rolling_Int_Pct,
           group = Player)) +
  geom_line(size = 1,
            aes(group = Season)) +
  scale_y_continuous(labels = scales::percent)