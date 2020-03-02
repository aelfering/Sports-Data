library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)

seasons <- read.csv('seasons.csv')
passing.fbs <- read.csv('college football passing.csv')

passing.season <- inner_join(passing.fbs, seasons, by = c('Date' = 'Date'))

passing.team <- passing.season %>%
  mutate(Date = mdy(Date)) %>%
  mutate(School = gsub('UTSA', 'Texas-San Antonio', School),
         School = gsub('UTEP', 'Texas-El Paso', School),
         School = gsub('USC', 'Southern California', School),
         School = gsub('UNLV', 'Nevada-Las Vegas', School),
         School = gsub('UCF', 'Central Florida', School),
         School = gsub('UAB', 'Alabama-Birmingham', School),
         School = gsub('SMU', 'Southern Methodist', School),
         School = gsub('Ole Miss', 'Mississippi', School),
         School = gsub('LSU', 'Louisiana State', School)) %>%
  arrange(Date) %>%
  group_by(Season, 
           School) %>%
  mutate(Season.Game = row_number(),
         Wins = ifelse(Result == 'W', 1, 0),
         Loses = ifelse(Result == 'L', 1, 0)) %>%
  mutate(Running.Wins = cumsum(Wins),
         Running.Losses = cumsum(Loses)) %>%
  ungroup() %>%
  select(Season,
         School,
         Date,
         Season.Game,
         Location,
         Opponent,
         Result,
         Running.Wins,
         Running.Losses,
         Cmp,
         Att,
         Yds,
         TD)

passing.opp <- passing.team %>%
  select(Season,
         School,
         Opponent,
         Date,
         Cmp,
         Att,
         Yds,
         TD)

passing.team.opp <- inner_join(passing.team,
                               passing.opp,
                               by = c('Season' = 'Season',
                                      'School' = 'Opponent',
                                      'Opponent' = 'School',
                                      'Date' = 'Date'))

passing.columns.named <- dplyr::select(passing.team.opp,
                                       Season,
                                       School,
                                       Opponent,
                                       Date,
                                       Season.Game,
                                       Result,
                                       Running.Wins,
                                       Running.Losses,
                                       Location,
                                       Team.Cmp = Cmp.x,
                                       Team.Att = Att.x,
                                       Team.Yds = Yds.x,
                                       Team.TDs = TD.x,
                                       Opp.Cmp = Cmp.y,
                                       Opp.Att = Att.y,
                                       Opp.Yds = Yds.y,
                                       Opp.TDs = TD.y)

write.csv(passing.columns.named, 'passing data.csv')

         
         
         
         