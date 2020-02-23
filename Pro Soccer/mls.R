library(dplyr)
library(tidyr)
library(tidyverse)
library(reshape2)
library(lubridate)
library(data.table)

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
  mutate(Team.Points = gsub('\\(', '', Team.Points)) %>%
  mutate(Opp.Points = gsub('\\(', '', Opp.Points)) %>%
  mutate(Team.Points = gsub('\\)', '', Team.Points)) %>%
  mutate(Opp.Points = gsub('\\)', '', Opp.Points)) %>%
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
  mutate(Round.Group = ifelse(Round == 'Regular Season', Round, 'Post-Season')) %>%
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
  mutate(Series.Winning.Streak = rowid(rleid(Team.Opp.Wins)) * Team.Opp.Wins) %>%
  ungroup()  %>%
  # Win, Loss, Draws by Team
  group_by(Team) %>%
  mutate(Total.Game.Num = row_number(),
         Series.Wins = cumsum(Team.Wins),
         Series.Losses = cumsum(Team.Loses),
         Series.Draws = cumsum(Team.Draws),
         Total.Winning.Streak = rowid(rleid(Team.Wins)) * Team.Wins) %>%
  ungroup() %>%
  as.data.frame()


####  Visualizations ####

first.post.season.games <- team.records %>%
  filter(Round.Group == 'Post-Season') %>%
  group_by(Team, 
           Season) %>%
  slice(which.min(Total.Game.Num)) %>%
  ungroup() %>%
  select(Season,
         Team,
         Total.Game.Num = Total.Game.Num,
         Running.Team.Points,
         Running.Opponent.Points)

last.post.season.games <- team.records %>%
  filter(Round.Group == 'Post-Season') %>%
  group_by(Team, 
           Season) %>%
  slice(which.max(Total.Game.Num)) %>%
  ungroup() %>%
  select(Season,
         Team,
         Last.Game = Total.Game.Num,
         Running.Team.Points,
         Running.Opponent.Points)

first.last.games <- inner_join(first.post.season.games, last.post.season.games, by = c('Team' = 'Team',
                                                                                       'Season' = 'Season'))

first.post <- team.records %>%
  group_by(Team, 
           Season) %>%
  slice(which.min(Total.Game.Num))

post.season <- team.records %>%
  filter(Round.Group == 'Post-Season') %>%
  group_by(Team, 
           Season) %>%
  slice(which.min(Total.Game.Num))

mls.cup.champ <- team.records %>%
  filter(grepl('MLS Cup', Round),
         Result == 'W')

mls.draw.wins <- team.records %>%
  filter(grepl('MLS Cup', Round),
         grepl('won', Notes))

all.mls.cup.wins <- rbind(mls.cup.champ, mls.draw.wins)

team <- 'Sporting KC'
ggplot(subset(team.records, 
              Team == team),
       aes(x = Total.Game.Num + 1)) +
  geom_hline(yintercept = 0, size = 0.2)  +
  # Identifies the beginning of the season
  geom_vline(data = subset(first.post, Team == team), 
             aes(xintercept = Total.Game.Num), 
             color = '#3a3939', 
             alpha = 0.9) +
  # Labels the season
  geom_text(data = subset(first.post, 
                          Team == team), 
            mapping = aes(label = substr(Season, 
                                         start = 3, 
                                         stop = 4), 
                          y = -25), 
            size = 3,
            hjust = 0) +
  # Charts the plus-minus score by season
  geom_line(aes(x = Total.Game.Num, 
                y = Running.Team.Points-Running.Opponent.Points,
                group = Season),
            color = '#0050bf') +
  # Shading identifies the post-season
  geom_rect(subset(first.last.games, 
                   Team == team), 
            mapping = aes(xmin = Total.Game.Num,
                          xmax = Last.Game + 1,
                          ymin = -Inf,
                          ymax = Inf),
            alpha = 0.3) +
  # Pinpoints when a team won the MLS Cup
  geom_point(data = subset(all.mls.cup.wins, Team == team), 
             aes(x = Total.Game.Num, 
                 y = Running.Team.Points-Running.Opponent.Points, 
                 color = '#ff6714',
                 group = Season)) +
  # Chart Theme and Formatting
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = "none",
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(face = 'bold', size = 18)) + 
  scale_x_continuous(limits=c(0,720),
                     breaks = seq(0, 720, by = 20)) +
  # Titles and Captions
  labs(title = 'Plus-Minus Score: The History of Sporting Kansas City',
       subtitle = 'Sporting Kansas City will compete in its 24th consecutive season as a member of Major League Soccer. In 2018 after finishing 19-9-5, scoring a plus-minus score of 26 and making it to the\nSemifinals, KC went 10-16-4 and recorded its third-worst season by plus-minus score in franchise history.',
       x = element_blank(),
       y = 'Plus-Minus Score',
       caption = 'Source: FBRef\nVisualization by Alex Elfering\nInspired by FiveThirtyEight') +
  # Annotations
  geom_label(aes(x = 160, y = 27, label = "Post-Season"), 
             hjust = 0, vjust = 0.5, colour = "#555555", fill = 'white',  
             label.size = NA,  family="Arial",  size = 3) +
  geom_label(aes(x = 160, y = 18, label = "Won MLS Cup"), 
             hjust = 0, vjust = 0.5, colour = "#555555", fill = 'white', 
             label.size = NA,  family="Arial",  size = 3) +
  geom_label(aes(x = 690, y = 18, label = "KC was knocked\nout in semifinals\ndespite strong showing."), 
             hjust = 0, vjust = 0.5, colour = "#555555", fill = 'white',  
             label.size = NA,  family="Arial",  size = 3) +
# Arrows
  geom_curve(aes(x = 160, y = 27, xend = 148, yend = 27),
             colour = "#555555", size=0.2, curvature = -0.2,
             arrow = arrow(length = unit(0.01, "npc"))) +
  geom_curve(aes(x = 160, y = 18, xend = 152, yend = 22),
             colour = "#555555", size=0.2, curvature = 0.2,
             arrow = arrow(length = unit(0.01, "npc"))) +
  geom_curve(aes(x = 690, y = 18, xend = 679, yend = 25),
             colour = "#555555", size=0.2, curvature = 0.2,
             arrow = arrow(length = unit(0.01, "npc")))
  
  
  
  
  
  
  
  



