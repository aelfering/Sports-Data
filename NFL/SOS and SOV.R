library(ggplot2)
library(tidyr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(stringr)

nfl_overview <- read.csv('OverviewNFL.csv')

nfl_clean <- nfl_overview %>%
  select(Tm,
         Season = Year,
         Date,
         Location = Result,
         Opp,
         Week,
         G = G.,
         Result = Result.1) %>%
  mutate(Score = substring(Result, 2),
         Result = substring(Result, 1, 1),
         Date = mdy(Date)) %>%
  mutate(Wins = ifelse(Result == 'W', 1, 0),
         Loses = ifelse(Result == 'L', 1, 0),
         Ties = ifelse(Result == 'T', 1, 0)) %>%
  # Labeling Franchises
  mutate(Franchise = ifelse((Season <= 1987 & Tm == 'STL') | (Tm == 'PHO' | Tm == 'ARI'), 'Arizona Cardinals', NA),
         Franchise = ifelse(Tm == 'ATL', 'Atlanta Falcons', Franchise),
         Franchise = ifelse((Season <= 1983 & Tm == 'BAL') | (Tm == 'IND'), 'Indianapolis Colts', Franchise),
         Franchise = ifelse(Season >= 1996 & Tm == 'BAL', 'Baltimore Ravens', Franchise),
         Franchise = ifelse(Tm == 'BUF', 'Buffalo Bills', Franchise),
         Franchise = ifelse(Tm %in% c('BOS', 'NWE'), 'New England Patriots', Franchise),
         Franchise = ifelse(Tm == 'CAR', 'Carolina Panthers', Franchise),
         Franchise = ifelse(Tm == 'CHI', 'Chicago Bears', Franchise),
         Franchise = ifelse(Tm == 'CIN', 'Cincinnati Bengals', Franchise),
         Franchise = ifelse(Tm == 'CLE', 'Cleveland Brown', Franchise),
         Franchise = ifelse(Tm == 'DAL', 'Dallas Cowboys', Franchise),
         Franchise = ifelse(Tm == 'DEN', 'Denver Broncos', Franchise),
         Franchise = ifelse(Tm == 'DET', 'Detroit Lions', Franchise),
         Franchise = ifelse(Tm == 'GNB', 'Green Bay Packers', Franchise),
         Franchise = ifelse((Season <= 1996 & Tm == 'HOU') | Tm == 'TEN', 'Tennessee Titans', Franchise),
         Franchise = ifelse(Tm == 'JAX', 'Jacksonville Jaguars', Franchise),
         Franchise = ifelse(Tm == 'KAN', 'Kansas City Chiefs', Franchise),
         Franchise = ifelse(Tm %in% c('SDG', 'LAC'), 'Los Angeles Chargers', Franchise),
         Franchise = ifelse(Tm %in% c('LAR', 'RAM') | (Tm == 'STL' & Season >= 1995), 'Los Angeles Rams', Franchise),
         Franchise = ifelse(Tm == 'MIA', 'Miami Dolphins', Franchise),
         Franchise = ifelse(Tm == 'MIN', 'Minnesota Vikings', Franchise),
         Franchise = ifelse(Tm == 'NOR', 'New Orleans Saints', Franchise),
         Franchise = ifelse(Tm == 'NYG', 'New York Giants', Franchise),
         Franchise = ifelse(Tm == 'NYJ', 'New York Jets', Franchise),
         Franchise = ifelse(Tm %in% c('OAK', 'RAI'), 'Oakland Raiders', Franchise),
         Franchise = ifelse(Tm == 'PHI', 'Philadelphia Eagles', Franchise),
         Franchise = ifelse(Tm == 'PIT', 'Pittsburgh Steelers', Franchise),
         Franchise = ifelse(Tm == 'SEA', 'Seattle Seahawks', Franchise),
         Franchise = ifelse(Tm == 'SFO', 'San Francisco 49ers', Franchise),
         Franchise = ifelse(Tm == 'TAM', 'Tampa Bay Buceneers', Franchise),
         Franchise = ifelse(Tm == 'WAS', 'Washington Redskins', Franchise),
         Franchise = ifelse(Tm == 'HOU' & Season >= 2002, 'Houston Texans', Franchise))

nfl_score_clean <- separate(data = nfl_clean, col = Score, into = c("Tm.Points", "Opp.Points"), sep = "\\-")
nfl_score_int <- dplyr::mutate(nfl_score_clean,
                               Tm.Points = as.integer(Tm.Points),
                               Opp.Points = as.integer(Opp.Points))

####  Basic SOS and SOV Ratings ####
total_wins <- nfl_score_int %>%
  group_by(Season,
           Tm) %>%
  summarise(Total_Wins = sum(Wins),
            Total_Losses = sum(Loses),
            Total_Ties = sum(Ties),
            Tm_Points = sum(Tm.Points),
            Opp_Points = sum(Opp.Points)) %>%
  ungroup() %>%
  mutate(Total_Pct_Won = Total_Wins/(Total_Wins + Total_Losses + Total_Ties))

all_games <- nfl_score_clean %>%
  group_by(Season,
           Franchise,
           Tm,
           Opp) %>%
  #slice(which.max(Date)) %>%
  ungroup()

opp_record <- inner_join(all_games, total_wins, by = c('Season' = 'Season', 'Opp' = 'Tm'))

strength_victory_metric <- opp_record %>%
  filter(Result == 'W') %>%
  group_by(Season,
           Franchise) %>%
  summarise(Opp_Wins = sum(Total_Wins),
            Opp_Losses = sum(Total_Losses),
            Opp_Ties = sum(Total_Ties),
            Opp_Points_For = mean(Tm_Points),
            Opp_Points_Against = mean(Opp_Points)) %>%
  ungroup() %>%
  mutate(Strength_Schedule = Opp_Wins/(Opp_Wins + Opp_Losses + Opp_Ties)) %>%
  group_by(Season) %>%
  mutate(Avg_Strength_Schedule = mean(Strength_Schedule)) %>%
  arrange(Season,
          desc(Strength_Schedule))

strength_schedule_metric <- opp_record %>%
  group_by(Season,
           Franchise) %>%
  summarise(Opp_Wins = sum(Total_Wins),
            Opp_Losses = sum(Total_Losses),
            Opp_Ties = sum(Total_Ties),
            Opp_Points_For = mean(Tm_Points),
            Opp_Points_Against = mean(Opp_Points)) %>%
  ungroup() %>%
  mutate(Strength_Schedule = Opp_Wins/(Opp_Wins + Opp_Losses + Opp_Ties)) %>%
  group_by(Season) %>%
  mutate(Avg_Strength_Schedule = mean(Strength_Schedule)) %>%
  arrange(Season,
          desc(Strength_Schedule))

ggplot(strength_schedule_metric,
       aes(x = Season, 
           y = Strength_Schedule,
           group = Franchise)) +
  scale_y_continuous(labels = scales::percent) +
  geom_hline(yintercept = 0.5, 
             color = 'gray',
             size = 1, 
             linetype = 'dashed') +
  geom_line(aes(group = Franchise),
            size = 1) +
  facet_wrap(~Franchise,
             ncol = 4) + 
  theme(plot.title = element_text(face = 'bold', size = 18, family = 'Arial'),
        plot.subtitle = element_text(size = 15, family = 'Arial'),
        plot.caption = element_text(size = 10, family = 'Arial'),
        axis.title = element_text(size = 10, family = 'Arial'),
        axis.text = element_text(size = 10, family = 'Arial'),
        strip.text = ggplot2::element_text(size = 10, hjust = 0, face = 'bold', color = 'black', family = 'Arial'),
        strip.background = element_rect(fill = NA),
        panel.background = ggplot2::element_blank(),
        axis.line = element_line(colour = "#222222", linetype = "solid"),
        panel.grid.major.y = ggplot2::element_line(color = "#dedede", linetype = 'dashed'),
        panel.grid.major.x = element_blank()) 


####  Can we see how well that victory aged over the course of a season? ####