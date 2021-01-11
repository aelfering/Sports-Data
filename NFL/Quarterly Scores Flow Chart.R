library(ggplot2)
library(tidyverse)
library(lubridate)
library(htmlwidgets)
library(htmltools)

nfl_quarterly_scores <- read.csv('NFL Quarterly Scores Games.csv',
                                 fileEncoding="UTF-8-BOM")

nfl_prb_538 <- read.csv('nfl_elo.csv')

games_30_pct_won <- nfl_prb_538 %>%
  filter(score1 > score2) %>%
  mutate(date = mdy(date)) %>%
  select(date,
         season,
         team1,
         team2,
         elo_prob1) %>%
  filter(elo_prob1 <= 0.4)


team_pts <- nfl_quarterly_scores %>%
  filter(Year >= 1974) %>%
  select(Year,
         Date,
         Tm,
         Opp,
         G.,
         Result,
         OT,
         Q1,
         Q2,
         Q3,
         Q4) %>%
  pivot_longer(cols = c(Q1, Q2, Q3, Q4),
               names_to = 'Quarter',
               values_to = 'Points.Scored')

opp_pts <- nfl_quarterly_scores %>%
  filter(Year >= 1974) %>%
  select(Year,
         Date,
         Tm,
         Opp,
         G.,
         Result,
         OT,
         OQ1,
         OQ2,
         OQ3,
         OQ4) %>%
  pivot_longer(cols = c(OQ1, OQ2, OQ3, OQ4),
               names_to = 'Quarter',
               values_to = 'Opp.Points.Scored') %>%
  mutate(Quarter = gsub('O', '', Quarter))

mark1 <- team_pts %>%
  #filter(#OT != 'OT',
  #       Tm == 'KAN') %>%
  inner_join(opp_pts) %>%
  mutate(Date = mdy(Date)) %>%
  inner_join(games_30_pct_won,
             by = c('Date' = 'date',
                    'Year' = 'season',
                    'Tm' = 'team1',
                    'Opp' = 'team2')) %>%
  group_by(Year,
           Date,
           Tm,
           Opp,
           G.) %>%
  mutate(Rolling.Team.Pts = cumsum(Points.Scored),
         Rolling.Oppt.Pts = cumsum(Opp.Points.Scored)) %>%
  #ungroup() %>%
  mutate(Margin = Rolling.Team.Pts-Rolling.Oppt.Pts) %>%
  mutate(Result = case_when(Margin > 0 ~ paste('Ahead in the ', Quarter, sep = ''),
                            Margin == 0 ~ paste('Tied in the ', Quarter, sep = ''),
                            Margin < 0 ~ paste('Behind in the ', Quarter, sep = '')),
         Result.1 = lead((Result))) %>%
  ungroup() %>%
  group_by(Result,
           Result.1) %>%
  summarise(Freq = n()) %>%
  ungroup() %>%
  mutate(Result.1 = case_when(Result == 'Ahead in the Q4' ~ 'Win',
                              Result == 'Behind in the Q4' ~ 'Lose',
                              Result == 'Tied in the Q4' ~ 'Overtime',
                              TRUE ~ Result.1))

mark2 <- team_pts %>%
  filter(OT == 'OT') %>%
  inner_join(opp_pts) %>%
  mutate(Date = mdy(Date)) %>%
  inner_join(games_30_pct_won,
             by = c('Date' = 'date',
                    'Year' = 'season',
                    'Tm' = 'team1',
                    'Opp' = 'team2')) %>%
  select(Year,
         Date,
         Tm,
         Opp,
         G.,
         Result, 
         OT) %>%
  mutate(Score = gsub('L ', '', Result),
         Score = gsub('W ', '', Score)) %>%
  separate(Score,
           into = c('Team.Pts', 'Opp.Pts'),
           sep = '-') %>%
  mutate(Team.Pts = as.numeric(Team.Pts),
         Opp.Pts = as.numeric(Opp.Pts),
         Result = 'Overtime',
         Result.1 = case_when(Team.Pts > Opp.Pts ~ 'Win',
                              Team.Pts < Opp.Pts ~ 'Lose',
                              Team.Pts == Opp.Pts ~ 'Tie')) %>%
  group_by(Result,
           Result.1) %>%
  summarise(Freq = n()) %>%
  ungroup()

mark3 <- bind_rows(mark1, mark2)

Nodes.Names <- data.frame(Result.1 = c('Ahead in the Q1',
                                       'Behind in the Q1',
                                       'Tied in the Q1',
                                       'Ahead in the Q2',
                                       'Behind in the Q2',
                                       'Tied in the Q2',
                                       'Ahead in the Q3',
                                       'Behind in the Q3',
                                       'Tied in the Q3',
                                       'Ahead in the Q4',
                                       'Behind in the Q4',
                                       'Tied in the Q4',
                                       'Win',
                                       'Lose',
                                       'Tie',
                                       'Overtime')) %>%
  mutate(Source = seq(0, 15, 1))

mark4 <- mark3 %>%
  inner_join(Nodes.Names,
             by = c('Result' = 'Result.1')) %>%
  inner_join(Nodes.Names,
             by = c('Result.1' = 'Result.1'))

r <- sankeyNetwork(Links = as.data.frame(mark4), 
                   Nodes = as.data.frame(Nodes.Names), 
                   Source = "Source.x",
                   Target = "Source.y", 
                   Value = "Freq", 
                   NodeID = "Result.1",
                   units = "games", 
                   fontSize = 20, 
                   nodeWidth = 20,
                   iterations = 0)

r
















margin_groups <- team_pts %>%
  filter(Year >= 1974) %>%
  inner_join(opp_pts) %>%
  mutate(Date = mdy(Date)) %>%
  inner_join(games_30_pct_won,
             by = c('Date' = 'date',
                    'Year' = 'season',
                    'Tm' = 'team1',
                    'Opp' = 'team2')) %>%
  group_by(Year,
           Date,
           Tm,
           Opp,
           G.) %>%
  mutate(Rolling.Team.Pts = cumsum(Points.Scored),
         Rolling.Oppt.Pts = cumsum(Opp.Points.Scored)) %>%
  #ungroup() %>%
  mutate(Margin = Rolling.Team.Pts-Rolling.Oppt.Pts) %>%
  mutate(Result.Des = case_when(Margin == 0 ~ paste('Tied in the ', Quarter, sep = ''),
                                Margin > 0 & Margin < 7 ~ paste('Ahead by < 1 TD in the ', Quarter, sep = ''),
                                Margin >= 7 & Margin < 14 ~ paste('Ahead 1-2 TDs in the ', Quarter, sep = ''),
                                Margin >= 14 ~ paste('Ahead +2 TDs in the ', Quarter, sep = ''),
                                Margin < 0 & Margin > -7 ~ paste('Behind by < 1 TD in the ', Quarter, sep = ''),
                                Margin >= -7 & Margin > -14 ~ paste('Behind 1-2 TDs in the ', Quarter, sep = ''),
                                Margin >= -14 ~ paste('Behind +2 TDs in the ', Quarter, sep = '')),
         Result.1 = lead((Result.Des))) %>%
  ungroup() %>%
  group_by(Result.Des,
           Result.1) %>%
  summarise(Freq = n()) %>%
  ungroup() %>%
  mutate(Result.1 = case_when(Result.Des == 'Ahead by < 1 TD in the Q4' ~ 'Win',
                              Result.Des == 'Ahead 1-2 TDs in the Q4' ~ 'Win',
                              Result.Des == 'Ahead +2 TDs in the Q4' ~ 'Win',
                              Result.Des == 'Behind by < 1 TD in the Q4' ~ 'Lose',
                              Result.Des == 'Behind 1-2 TDs in the Q4' ~ 'Lose',
                              Result.Des == 'Behind +2 TDs in the Q4' ~ 'Lose',
                              Result.Des == 'Tied in the Q4' ~ 'Overtime',
                              TRUE ~ Result.1))

mark2 <- team_pts %>%
  filter(OT == 'OT') %>%
  inner_join(opp_pts) %>%
  mutate(Date = mdy(Date)) %>%
  inner_join(games_30_pct_won,
             by = c('Date' = 'date',
                    'Year' = 'season',
                    'Tm' = 'team1',
                    'Opp' = 'team2')) %>%
  select(Year,
         Date,
         Tm,
         Opp,
         G.,
         Result, 
         OT) %>%
  mutate(Score = gsub('L ', '', Result),
         Score = gsub('W ', '', Score)) %>%
  separate(Score,
           into = c('Team.Pts', 'Opp.Pts'),
           sep = '-') %>%
  mutate(Team.Pts = as.numeric(Team.Pts),
         Opp.Pts = as.numeric(Opp.Pts),
         Result.Des = 'Overtime',
         Result.1 = case_when(Team.Pts > Opp.Pts ~ 'Win',
                              Team.Pts < Opp.Pts ~ 'Lose',
                              Team.Pts == Opp.Pts ~ 'Tie')) %>%
  group_by(Result.Des,
           Result.1) %>%
  summarise(Freq = n()) %>%
  ungroup()





Nodes.Names <- data.frame(Result.1 = c('Ahead +2 TDs in the Q1',
                                       'Ahead 1-2 TDs in the Q1',
                                       'Ahead by < 1 TD in the Q1',
                                       'Tied in the Q1',                                       
                                       'Behind by < 1 TD in the Q1',
                                       'Behind 1-2 TDs in the Q1',
                                       'Behind +2 TDs in the Q1',
                                       
                                       'Ahead +2 TDs in the Q2',
                                       'Ahead 1-2 TDs in the Q2',
                                       'Ahead by < 1 TD in the Q2',
                                       'Tied in the Q2',                                       
                                       'Behind by < 1 TD in the Q2',
                                       'Behind 1-2 TDs in the Q2',
                                       'Behind +2 TDs in the Q2',
                                       
                                       'Ahead +2 TDs in the Q3',
                                       'Ahead 1-2 TDs in the Q3',
                                       'Ahead by < 1 TD in the Q3',
                                       'Tied in the Q3',                                       
                                       'Behind by < 1 TD in the Q3',
                                       'Behind 1-2 TDs in the Q3',
                                       'Behind +2 TDs in the Q3',
                                       
                                       'Ahead +2 TDs in the Q4',
                                       'Ahead 1-2 TDs in the Q4',
                                       'Ahead by < 1 TD in the Q4',
                                       'Tied in the Q4',                                       
                                       'Behind by < 1 TD in the Q4',
                                       'Behind 1-2 TDs in the Q4',
                                       'Behind +2 TDs in the Q4',
                                       
                                       'Win',
                                       'Lose',
                                       'Overtime')) %>%
  mutate(Source = seq(0, 30, 1))

mark3 <- bind_rows(margin_groups, mark2)



mark4 <- mark3 %>%
  inner_join(Nodes.Names,
             by = c('Result.Des' = 'Result.1')) %>%
  inner_join(Nodes.Names,
             by = c('Result.1' = 'Result.1'))

r <- sankeyNetwork(Links = as.data.frame(mark4), 
                   Nodes = as.data.frame(Nodes.Names), 
                   Source = "Source.x",
                   Target = "Source.y", 
                   Value = "Freq", 
                   NodeID = "Result.1",
                   units = "games", 
                   fontSize = 20, 
                   iterations = 0,
                   nodeWidth = 20)

r




















