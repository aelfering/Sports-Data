# Iowa Basketball Analysis

library(dplyr)
library(tidyr)
library(tidyverse)
library(reshape2)

iowa.team <- read.csv('Iowa Basketball.csv')
iowa.opp <- read.csv('Iowa Opp Basketball.csv')
seasons <- read.csv('iowa seasons.csv')

#### Cleaning the Script ####
iowa.team.opp <- inner_join(iowa.team, 
                            iowa.opp,
                            by = c('Date' = 'Date',
                                   'Location' = 'Location',
                                   'Schl' = 'Schl',
                                   'Opp' = 'Opp',
                                   "Result" = "Result",
                                   'MP' = 'MP'))

iowa.column.rename <- dplyr::select(iowa.team.opp,
                                    Date,
                                    Team = Schl,
                                    Opp,
                                    Location,
                                    Result,
                                    MP,
                                    Team.FG = FG.x,
                                    Team.FGA = FGA.x,
                                    Team.2P = X2P.x,
                                    Team.2PA = X2PA.x,
                                    Team.3P = X3P.x,
                                    Team.3PA = X3PA.x,
                                    Team.FT = FT.x,
                                    Team.FTA = FTA.x,
                                    Team.PTS = PTS.x,
                                    Opp.FG = FG.y,
                                    Opp.FGA = FGA.y,
                                    Opp.2P = X2P.y,
                                    Opp.2PA = X2PA.y,
                                    Opp.3P = X3P.y,
                                    Opp.3PA = X3PA.y,
                                    Opp.FT = FT.y,
                                    Opp.FTA = FTA.y,
                                    Opp.PTS = PTS.y)

iowa.results <- iowa.column.rename %>%
  mutate(Wins = ifelse(grepl('W', Result), 1, 0),
         Loses = ifelse(grepl('L', Result), 1, 0)) %>%
  mutate(Result = gsub('L', '', Result)) %>%
  mutate(Result = gsub('W', '', Result)) %>%
  mutate(Result = gsub(' (OT)', '', Result, fixed = TRUE)) %>%
  mutate(Result = gsub(' (2OT)', '', Result, fixed = TRUE))

iowa.results.split <- separate(iowa.results, Result, into = c('Iowa.Pts', 'Opp.Pts'), sep = '-')

iowa.results.seasons <- inner_join(seasons, iowa.results.split, by = c('Date' = 'Date'))
iowa.results.pts.int <- dplyr::mutate(iowa.results.seasons,
                                      Iowa.Pts = as.numeric(Iowa.Pts),
                                      Opp.Pts = as.numeric(Opp.Pts))

head(iowa.results.pts.int)

#### What percent of points come from 3-point shots? ####
percent.shots <- iowa.results.pts.int %>%
  group_by(Season) %>%
  summarise(Total.Season.Pts = sum(Team.PTS),
            Total.Season.Att = sum(Team.3PA) + sum(Team.2PA) + sum(Team.FTA),
            Total.3P.Pts = sum(Team.3P) * 3,
            Total.3P.Att = sum(Team.3PA),
            Total.2P.Pts = sum(Team.2P) * 2,
            Total.2P.Att = sum(Team.2PA),
            Total.FT.Pts = sum(Team.FT),
            Total.FT.Att = sum(Team.FTA),
            Total.Minutes = max(MP)) %>%
  ungroup() %>%
  mutate(Pct.3P = Total.3P.Pts/Total.Season.Pts,
         Pct.3PA = Total.3P.Att/Total.Season.Att,
         Pct.2P = Total.2P.Pts/Total.Season.Pts,
         Pct.2PA = Total.2P.Att/Total.Season.Att,
         Pct.FT = Total.FT.Pts/Total.Season.Pts,
         Pct.FTA = Total.FT.Att/Total.Season.Att,
         Season = as.factor(Season))

ggplot(percent.shots, aes(x = Season, y = Pct.FTA)) +
  geom_line(aes(group = 1)) +
  geom_point() +
  theme(plot.title = element_text(size = 18, face = 'strong')) +
  labs(title = 'Iowa has Made Three-Pointers a Bigger Focus',
       subtitle = 'Percent of points from three-point shots by season',
       caption = 'Visualization by Alex Elfering\nSource: College Basketball Reference',
       x = '',
       y = 'Percent of Points from Three-Pointers') +
  scale_y_continuous(labels = scales::percent) +
  theme_bw()

# This visualizes that percent of points from three pointers has increased per share

####  Net Pointers?
running.net <- iowa.results.pts.int %>%
  mutate(Net.3P = Team.3P-Opp.3P,
         Net.2P = Team.2P-Opp.2P,
         Net.FT = Team.FT-Opp.FT) %>%
  group_by(Season) %>%
  mutate(Running.Net.3P = cumsum(Net.3P),
         Running.Net.2P = cumsum(Net.2P),
         Running.Net.FT = cumsum(Net.FT),
         Season.Game.No = row_number()) %>%
  ungroup()

ggplot(running.net, 
       aes(x = Season.Game.No)) +
  geom_hline(yintercept = 0, 
             alpha = 0.6) +
  theme_bw() +
  geom_line(mapping = aes(y = Running.Net.FT),
            color = 'red') + 
  geom_line(mapping = aes(y = Running.Net.2P),
            color = 'blue') + 
  geom_line(mapping = aes(y = Running.Net.3P),
            color = 'orange') + 
  facet_wrap(~Season)
  
head(iowa.results.pts.int)  