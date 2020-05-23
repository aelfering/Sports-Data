# Testing a scalable college football plus-minus visualization

library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(lubridate)

cfb <- read.csv('full cfb games.csv')

# Cleaning script
clean.cfb <- cfb %>%
  filter(Season >= 1989) %>%
  filter(!grepl('Cancelled', Notes)) %>%
  # This separates the team's ranks from the team's name
  mutate(Winner.Rank = gsub("[^0-9]", "", Winner)) %>%
  mutate(Winner.Name = gsub("[^A-Z. a-z. -]", "", Winner)) %>%
  mutate(Loser.Rank = gsub("[^0-9]", "", Loser)) %>%
  mutate(Loser.Name = gsub("[^A-Z. a-z. -]", "", Loser)) %>%
  # Date Manipulation
  mutate(Date = mdy(Date)) %>%
  # Fixing names that were butchered
  mutate(Winner.Name = gsub('Miami FL', 'Miami (FL)', Winner.Name),
         Winner.Name = gsub('Miami OH', 'Miami (OH)', Winner.Name),
         Winner.Name = gsub('Texas AM', 'Texas A&M', Winner.Name)) %>%
  mutate(Loser.Name = gsub('Miami FL', 'Miami (FL)', Loser.Name),
         Loser.Name = gsub('Miami OH', 'Miami (OH)', Loser.Name),
         Loser.Name = gsub('Texas AM', 'Texas A&M', Loser.Name)) %>%
  # Selecting all of the elements
  select(Season,
         Date,
         Team = Winner.Name,
         Team.Rank = Winner.Rank,
         Opponent = Loser.Name,
         Opponent.Rank = Loser.Rank,
         Location = X,
         Team.Pts = Pts,
         Opponent.Pts = Pts.1) %>%
  distinct(Season,
         Date,
         Team,
         Team.Rank,
         Opponent,
         Opponent.Rank,
         Location,
         Team.Pts,
         Opponent.Pts)
  

# Opponent side
opponent_side <- dplyr::select(clean.cfb,
              Season,
              Date,
              Team = Opponent,
              Team.Rank = Opponent.Rank,
              Opponent = Team,
              Opponent.Rank = Team.Rank,
              Location,
              Team.Pts = Opponent.Pts,
              Opponent.Pts = Team.Pts)

full_cfb <- bind_rows(clean.cfb, opponent_side)

# Visualization Prep
head(full_cfb)

cfb_records <- full_cfb %>%
  mutate(Wins = ifelse(Team.Pts > Opponent.Pts, 1, 0),
         Loses = ifelse(Opponent.Pts > Team.Pts, 1, 0),
         Ties = ifelse(Opponent.Pts == Team.Pts, 1, 0)) %>%
  group_by(Season,
           Team) %>%
  summarise(Points.For = mean(Team.Pts),
            Points.Against = (mean(Opponent.Pts)*-1),
            Points.Against.Line = mean(Opponent.Pts),
            Games.Played = n_distinct(Date),
            Total.Wins = sum(Wins),
            Total.Losses = sum(Loses),
            Total.Ties = sum(Ties)) %>%
  ungroup() %>%
  mutate(Margin = Points.For + Points.Against) %>%
  filter(Total.Wins + Total.Losses + Total.Ties >= 10)

# Visualization

# Highlights bad seasons
winning.seasons <- cfb_records %>%
  mutate(Losing.Season = ifelse(Total.Losses > Total.Wins, Total.Wins, 0)) %>%
  mutate(Lower = ifelse(Losing.Season > 0 | Total.Wins == 0, Season - 0.5, NA),
         Upper = ifelse(Losing.Season > 0 | Total.Wins == 0, Season + 0.5, NA)) %>%
  filter(Games.Played >= 10)

team.filter <- 'Illinois'

# How many games has team.filter won/lost in the last five seasons?
recent.season <- max(cfb_records$Season)
last.season <- max(cfb_records$Season)-3

five.season.perf <- cfb_records %>%
  filter(Team == team.filter) %>%
  group_by(Team) %>%
  filter(Season >= last.season) %>%
  summarise(Total.Wins = sum(Total.Wins),
            Total.Losses = sum(Total.Losses),
            Avg.Points.Scored = mean(Points.For),
            Avg.Points.Against = mean(Points.Against)) %>%
  ungroup()

total.wins <- as.numeric(five.season.perf$Total.Wins)
total.losses <- as.numeric(five.season.perf$Total.Losses)
avg.points.for <- round(as.numeric(five.season.perf$Avg.Points.Scored), 2)
avg.points.again <- round(as.numeric(five.season.perf$Avg.Points.Against), 2)

title.sentence <- paste('How Many Points ', 
                        team.filter,
                        ' Football Allows and Scores on Median by Season',
                        sep = '')

subtitle.sentence <- paste('Since ', 
                           last.season, 
                           ', ', 
                           team.filter, 
                           ' has gone ',
                           total.wins, 
                           '-', 
                           total.losses,
                           '. They score a median ',
                           avg.points.for,
                           ' points, and give up ',
                           abs(avg.points.again),
                           ' points.',
                           sep = '')

# What is the average points scored and scored against in the last five seasons?

# The visualization
ggplot(data = subset(cfb_records,
                     Team == team.filter), 
       aes(x = Season)) +
  geom_rect(data = subset(winning.seasons,
                          Team == team.filter),
            fill = '#e4e4e2',
            mapping = aes(xmin = Lower,
                          xmax = Upper,
                          ymin = Inf,
                          ymax = -Inf,
                          alpha = 0.9)) +
  geom_hline(yintercept = 0,
             linetype = 'dashed') +
  geom_line(data = subset(cfb_records,
                          Team == team.filter),
            color = '#2e59a8',
            #size = 1,
            aes(y = Points.For)) +
  geom_line(data = subset(cfb_records,
                          Team == team.filter),
            color = '#ff8e56',
            #size = 1,
            aes(y = Points.Against.Line)) +
  geom_point(data = subset(cfb_records,
                          Team == team.filter),
            color = '#2e59a8',
            #size = 1,
            aes(y = Points.For)) +
  geom_point(data = subset(cfb_records,
                          Team == team.filter),
            color = '#ff8e56',
            #size = 1,
            aes(y = Points.Against.Line)) +
  # Points scored average line
  geom_segment(aes(x = last.season, y = avg.points.for, xend = recent.season, yend = avg.points.for), 
               colour = "#8abccf", size=1, linetype=2) +
  # Points scored against average line
  geom_segment(aes(x = last.season, y = abs(avg.points.again), xend = recent.season, yend = abs(avg.points.again)), 
               colour = "#ffd298", size=1, linetype=2) +
  # Labels
  labs(title = title.sentence,
       subtitle = subtitle.sentence,
       x = 'Seasons',
       y = '',
       caption = 'Visualization by Alex Elfering\nSource: College Football Reference') +
  theme(plot.title = element_text(face = 'bold', 
                                  size = 18, 
                                  family = 'Arial'),
        legend.position = 'none',
        plot.subtitle = element_text(size = 15, 
                                     family = 'Arial'),
        plot.caption = element_text(size = 12, 
                                    family = 'Arial'),
        axis.title = element_text(size = 12, 
                                  family = 'Arial'),
        axis.text = element_text(size = 12, 
                                 family = 'Arial'),
        strip.text = ggplot2::element_text(size = 22, 
                                           hjust = 0),
        panel.background = ggplot2::element_blank(),
        axis.line = element_line(colour = "#222222", 
                                 linetype = "solid"),
        panel.grid.major.y = ggplot2::element_line(color = "#cbcbcb"),
        panel.grid.major.x = element_blank()) 