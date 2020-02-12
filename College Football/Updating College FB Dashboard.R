library(dplyr)
library(tidyr)
library(tidyverse)
library(reshape2)

cfb_games <- read.csv('cfb games.csv')
conferences <- read.csv('cfb conferences.csv')

####  Play Index ####


####  Conferences Manipulation ####
head(conferences)

conf_no_school_abbreviations <- conferences %>%
  mutate(School = gsub('LSU', 'Louisiana State', School)) %>%
  mutate(School = gsub('UTSA', 'Texas-San Antonio', School)) %>%
  mutate(School = gsub('UTEP', 'Texas-El Paso', School)) %>%
  mutate(School = gsub('USC', 'Southern California', School)) %>%
  mutate(School = gsub('UCF', 'Central Florida', School)) %>%
  mutate(School = gsub('UAB', 'Alabama-Birmingham', School)) %>%
  mutate(School = gsub('SMU', 'Southern Methodist', School))

head(conf_no_school_abbreviations)

cfb_conf_select <- conf_no_school_abbreviations %>%
  select(Season,
         Conference,
         School,
         Off,
         Def,
         SRS,
         SOS)

####  Wins and Losses Manipulation ####
# Separate the team name from ranks
cfb_games$Winner.Name <- str_replace_all(cfb_games$Winner, "\\(1\\)�|\\(2\\)�|\\(3\\)�|\\(4\\)�|\\(5\\)�|\\(6\\)�|\\(7\\)�|\\(8\\)�|\\(9\\)�|\\(10\\)�|\\(11\\)�|\\(12\\)�|\\(13\\)�|\\(14\\)�|\\(15\\)�|\\(16\\)�|\\(17\\)�|\\(18\\)�|\\(19\\)�|\\(20\\)�|\\(21\\)�|\\(22\\)�|\\(23\\)�|\\(24\\)�|\\(25\\)�", "")
cfb_games$Loser.Name <- str_replace_all(cfb_games$Loser, "\\(1\\)�|\\(2\\)�|\\(3\\)�|\\(4\\)�|\\(5\\)�|\\(6\\)�|\\(7\\)�|\\(8\\)�|\\(9\\)�|\\(10\\)�|\\(11\\)�|\\(12\\)�|\\(13\\)�|\\(14\\)�|\\(15\\)�|\\(16\\)�|\\(17\\)�|\\(18\\)�|\\(19\\)�|\\(20\\)�|\\(21\\)�|\\(22\\)�|\\(23\\)�|\\(24\\)�|\\(25\\)�", "")

cfb_games_distinct <- cfb_games %>%
  distinct(Season, Date, Winner.Name, Loser.Name, Pts, Pts.1)

# Adding losses, too
cfb_loser_games <- cfb_games_distinct %>%
  select(Season, 
         Date, 
         Winner.Name = Loser.Name,
         Loser.Name = Winner.Name,
         Pts = Pts.1,
         Pts.1 = Pts)

all_cfb_games <- rbind(cfb_games_distinct, cfb_loser_games)

# Fixing the game dates
all_cfb_games_dates <- dplyr::mutate(all_cfb_games, Date = mdy(Date))
all_cfb_games_dates_sorted <- dplyr::arrange(all_cfb_games_dates, Date)

cfb_updated <- all_cfb_games_dates_sorted %>%
  select(Season,
         Date,
         Team = Winner.Name,
         Opponent = Loser.Name,
         Team.Pts = Pts,
         Opponent.Pts = Pts.1) %>%
  group_by(Season,
           Team) %>%
  mutate(Game.Number = row_number()) %>%
  ungroup() %>%
  mutate(Wins = ifelse(Team.Pts > Opponent.Pts, 1, 0),
        Loses = ifelse(Team.Pts < Opponent.Pts, 1, 0)) %>%
  group_by(Season,
           Team) %>%
  mutate(Running.Wins = cumsum(Wins),
         Running.Losses = cumsum(Loses)) %>%
  ungroup() %>%
  group_by(Team) %>%
  mutate(Total.Wins = cumsum(Wins),
         Total.Losses = cumsum(Loses)) %>%
  ungroup() %>%
  group_by(Team) %>%
  mutate(Winning.Streak = rowid(rleid(Wins)) * Wins,) %>%
  ungroup() %>%
  group_by(Team,
           Opponent) %>%
  mutate(Opp.Winning.Streak = rowid(rleid(Wins)) * Wins,
         Opp.Losing.Streak = rowid(rleid(Loses)) * Loses,
         Running.Opponent.Series.Wins = cumsum(Wins),
         Running.Opponent.Series.Losses = cumsum(Loses)) %>%
  ungroup()

most_recent_wins <- cfb_updated %>%
  group_by(Team, 
           Opponent) %>%
  slice(which.max(Date)) %>%
  filter(Opp.Winning.Streak > 1) %>%
  select(Team, Date, Opponent, Current.Win.Streak = Opp.Winning.Streak)

most_recent_loses <- cfb_updated %>%
  group_by(Team, 
           Opponent) %>%
  slice(which.max(Date)) %>%
  filter(Opp.Losing.Streak > 1) %>%
  select(Team, Date, Opponent, Current.Lose.Streak = Opp.Losing.Streak)

cfb_winning_streaks <- left_join(cfb_updated, most_recent_wins, by = c('Date' = 'Date',
                                                                       'Team' = 'Team',
                                                                       'Opponent' = 'Opponent'))

cfb_losing_streaks <- left_join(cfb_winning_streaks, most_recent_loses, by = c('Date' = 'Date',
                                                                      'Team' = 'Team',
                                                                      'Opponent' = 'Opponent'))

#### How strong was a team's win? ####
team_win_loss <- cfb_losing_streaks %>%
  group_by(Season,
           Team) %>%
  summarise(Opp.Season.Wins = sum(Wins),
            Opp.Season.Losses = sum(Loses)) %>%
  ungroup() %>%
  as.data.frame()

full.cfb <- left_join(cfb_losing_streaks, team_win_loss, by = c('Season' = 'Season',
                                                                'Opponent' = 'Team'))

full.cfb.team.conf <- inner_join(full.cfb, cfb_conf_select, by = c('Season' = 'Season', 'Team' = 'School'))
full.cfb.opp.conf <- left_join(full.cfb.team.conf, cfb_conf_select, by = c('Season' = 'Season', 'Opponent' = 'School'))

write.csv(full.cfb.opp.conf, 'College Football.csv')










