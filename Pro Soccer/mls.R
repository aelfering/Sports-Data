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

head(full.mls.new.result)

losing.games <- full.mls.new.result %>%
  filter(Team.Points != Opp.Points) %>%
  select(Season,
         Round,
         Day,
         Date,
         Time,
         Team = Opponent,
         Team.Points = Opp.Points,
         Opp.Points = Team.Points,
         Opponent = Team,
         Attendance,
         Venue,
         Referee,
         `Match Report`,
         Notes) %>%
  mutate(Result = ifelse(Team.Points < Opp.Points, "L", "Fix"))

win.loss.mls.games <- rbind(full.mls.new.result, losing.games)

win.loss.mls.games.dates <- dplyr::mutate(win.loss.mls.games, Date = mdy(Date))

# Adjusting for name brand changes
team.name.clean <- win.loss.mls.games.dates %>%
  mutate(Team = ifelse(Team %in% c('KC Wiz', 'KC Wizards'), 'Sporting KC', Team),
         Opponent = ifelse(Opponent %in% c('KC Wiz', 'KC Wizards'), 'Sporting KC', Opponent)) %>%
  mutate(Team = ifelse(Team %in% c('MetroStars'), 'NY Red Bulls', Team),
         Opponent = ifelse(Opponent %in% c('MetroStars'), 'NY Red Bulls', Opponent)) 

team.records <- team.name.clean %>%
  arrange(Date) %>%
  # Win, Loss, Draws by Team and Season
  group_by(Season, Team) %>%
  mutate(Team.Wins = ifelse(Result == 'W', 1, 0),
         Team.Loses = ifelse(Result == 'L', 1, 0),
         Team.Draws = ifelse(Result == 'D', 1, 0)) %>%
  mutate(Running.Season.Wins = cumsum(Team.Wins),
         Running.Season.Losses = cumsum(Team.Loses),
         Running.Season.Draws = cumsum(Team.Draws),
         Running.Team.Points = cumsum(Team.Points),
         Running.Opponent.Points = cumsum(Opp.Points),
         Season.Game.Number = row_number()) %>%
  ungroup() %>%
  # Win, Loss, Draws by Team vs. Opponent
  group_by(Team, Opponent) %>%
  mutate(Team.Opp.Wins = ifelse(Result == 'W', 1, 0),
         Team.Opp.Loses = ifelse(Result == 'L', 1, 0),
         Team.Opp.Draws = ifelse(Result == 'D', 1, 0)) %>%
  mutate(Running.Series.Wins = cumsum(Team.Opp.Wins),
         Running.Series.Losses = cumsum(Team.Opp.Loses),
         Running.Series.Draws = cumsum(Team.Opp.Draws)) %>%
  ungroup()  %>%
  # Win, Loss, Draws by Team
  group_by(Team) %>%
  mutate(Total.Game.Num = row_number(),
         Series.Wins = cumsum(Team.Wins),
         Series.Losses = cumsum(Team.Loses),
         Series.Draws = cumsum(Team.Draws)) %>%
  ungroup()

head(team.records)

ggplot(team.records, aes(x = Season.Game.Number, y = Series.Wins)) +
  geom_line() +
  facet_wrap(~Team)
  
write.csv(team.records, 'basic mls.csv')
  
  
  
  
  
  
  
  



