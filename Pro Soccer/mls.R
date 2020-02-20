library(dplyr)
library(tidyr)
library(tidyverse)
library(reshape2)
library(lubridate)

mls.soccer <- read.csv('mls soccer.csv', header=FALSE, stringsAsFactors=FALSE)

####  Cleaning the initial script  ####
shootouts <- paste('\\(', 0:20, '\\)', sep = '')

mls.header <- as.character(mls.soccer[1,])
mls.no.header <- mls.soccer[-1,]

colnames(mls.no.header) <- mls.header

mls.match <- mls.no.header %>%
  mutate(Score = gsub('\xd0', '-', Score)) %>%
  mutate(Score = ifelse(grepl(paste(shootouts, collapse = "|"), Score), substring(Score, 4), Score)) %>%
  mutate(Score = ifelse(grepl(paste(shootouts, collapse = "|"), Score), str_sub(Score, 1, str_length(Score)-4), Score))

mls.score <- separate(data = mls.match, col = Score, into = c("Team.Points", "Opp.Points"), sep = "\\-")

mls.results <- mls.score %>%
  mutate(Team.Points = gsub('\\s+', '', Team.Points)) %>%
  mutate(Opp.Points = gsub('\\s+', '', Opp.Points)) %>%
  mutate(Result = ifelse(Team.Points > Opp.Points, "W",
                         ifelse(Team.Points < Opp.Points, "L", "D")))

mls.lose <- dplyr::filter(mls.results, Result == 'L')
mls.not.lose <- dplyr::filter(mls.results, Result != 'L')

reorder.loss <- mls.lose %>%
  select(Season,
         Round,
         Day,
         Date, Time,
         Team = Away,
         Team.Points = Opp.Points,
         Opp.Points = Team.Points,
         Opponent = Home,
         Attendance,
         Venue,
         Referee,
         `Match Report`,
         Notes,
         Result)

reorder.not.loss <- mls.not.lose %>%
  select(Season,
         Round,
         Day,
         Date, 
         Time,
         Team = Home,
         Team.Points = Team.Points,
         Opp.Points = Opp.Points,
         Opponent = Away,
         Attendance,
         Venue,
         Referee,
         `Match Report`,
         Notes,
         Result)

full.mls.match <- rbind(reorder.loss, reorder.not.loss)
full.mls.new.result <- dplyr::mutate(full.mls.match, Result = ifelse(Team.Points > Opp.Points, "W",
                                                                     ifelse(Team.Points > Opp.Points, "L", "D")))


####  Full Win and Loss Dataframe  ####