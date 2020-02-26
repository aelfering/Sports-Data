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
                                    Team.2PA = X2P.x,
                                    Team.3P = X3P.x,
                                    Team.3PA = X3PA.x,
                                    Team.FT = FT.x,
                                    Team.FTA = FTA.x,
                                    Team.PTS = PTS.x,
                                    Opp.FG = FG.y,
                                    Opp.FGA = FGA.y,
                                    Opp.2P = X2P.y,
                                    Opp.2PA = X2P.y,
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








