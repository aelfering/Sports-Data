library(tidyverse)
library(stringi)
library(zoo)
library(lubridate)

uno_hockey <- read.csv('UNO Hockey.csv',
                       fileEncoding="UTF-8-BOM")

days.of.week <- paste('\\(', weekdays(x=as.Date(seq(7), origin="1950-01-01")), '\\)', sep = '')
ot.games <- c('OT', '1OT', '2OT', '3OT')

# data cleaning
clean_hockey <- uno_hockey %>%
  select(-Time,
         -City,
         -Tournament) %>%
  # removing games that are yet to happen or have been cancelled
  filter(!Result %in% c('', 'Canceled')) %>%
  # remove all non-score elements in the results column
  mutate(Result = gsub("\\s*\\([^\\)]+\\)","",as.character(Result)),
         Result = stri_trim_both(str_remove_all(Result, paste(ot.games, collapse = "|"))) ) %>%
  # split the results column to create a 'Points For' and 'Points Against' column, respectively
  separate(sep = ', ',
           col = 'Result',
           into = c('Result', 'Score')) %>%
  mutate(Score = str_replace_all(Score, pattern=" ", repl=""),
         Score = gsub('-', ' ', Score)) %>%
  separate(sep = ' ',
           col = 'Score',
           into = c('Team.Points', 'Opp.Points')) %>%
  group_by(Season) %>%
  # identify games won, lost, and tied and WLT records
  # also calculating margin and total goals scored and allowed
  mutate(Team.Points = as.numeric(Team.Points),
         Opp.Points = as.numeric(Opp.Points),
         Wins = ifelse(Team.Points > Opp.Points, 1, 0),
         Loses = ifelse(Team.Points < Opp.Points, 1, 0),
         Ties = ifelse(Team.Points == Opp.Points, 1, 0),
         Margin = Team.Points-Opp.Points,
         Rolling.Wins = cumsum(Wins),
         Rolling.Losses = cumsum(Loses),
         Rolling.Ties = cumsum(Ties),
         Points.Scored = cumsum(Team.Points),
         Points.Allowed = cumsum(Opp.Points),
         Rolling.Margin = cumsum(Margin)) %>%
  ungroup() %>%
  # counting win, lose, and tie streaks
  mutate(Win.Streak = ave(Wins, cumsum(Wins==0), FUN = seq_along) - 1,
         Lose.Streak = ave(Loses, cumsum(Loses==0), FUN = seq_along) - 1,
         Tie.Streak = ave(Ties, cumsum(Ties==0), FUN = seq_along) - 1,
         Streak.Status = case_when(Win.Streak > 0 ~ 'W',
                                   Lose.Streak > 0 ~ 'L',
                                   Tie.Streak > 0 ~ 'T')) %>%
  mutate(Games = row_number()) %>%
  mutate(Date = stri_trim_both(str_remove_all(Date, paste(days.of.week, collapse = "|"))),
         Date = mdy(format(mdy(Date), "%m/%d/%Y")) )

# exploratory analysis
# how did UNO perform at the end of every season?
clean_hockey %>%
  group_by(Season) %>%
  slice(which.max(Date)) %>%
  unite(Final.Record, 
        c('Rolling.Wins',
          'Rolling.Losses',
          'Rolling.Ties'),
        sep = '-') %>%
  select(Season,
         Final.Record,
         Rolling.Margin,
         Points.Scored,
         Points.Allowed) %>%
  arrange(desc(Rolling.Margin))

# create a data frame to label the x-axis correctly
season_lines <- clean_hockey %>%
  group_by(Season) %>%
  summarise(FirstGame = min(Games),
            LastGame = max(Games)) %>%
  ungroup() %>%
  mutate(Season.Label = paste("'", substr(Season, 3, 4), sep = ''))

# identify games to annotate in the final visualization
lines_for_annotation <- clean_hockey %>%
  filter((Season == 2003 & Games >= 260 & Games <= 280) | 
           (Season == 2000 & Games >= 148 & Games <= 157) |
           (Season == 1997 & Games >= 13 & Games <= 22) |
           (Season == 2001 & Games >= 181 & Games <= 188) |
           (Season == 2008 & Games >= 467 & Games <= 488) |
           (Season == 2010 & Games >= 555 & Games <= 571))

# and how did UNO perform in these annotated sections?
lines_for_annotation %>%
  group_by(Season) %>%
  summarise(Wins = sum(Wins),
            Losses = sum(Loses),
            Ties = sum(Ties),
            Points.For = sum(Team.Points),
            Points.Against = sum(Opp.Points)) %>%
  ungroup()

# identify single games where UNO performed well
annotation_geom_point <- clean_hockey %>%
  filter((Season == 2014 & Games == 729) |
           (Season == 1999 & Games == 114) |
           (Season == 2005 & Games == 362) |
           (Season == 2020 & Games == max(Games)))


# the final visualization
ggplot(clean_hockey,
       aes(x = Games,
           y = Rolling.Margin,
           group = Season)) +
  scale_x_continuous(limits=c(min(season_lines$FirstGame),
                              max(season_lines$LastGame)),
                     breaks = season_lines$FirstGame,
                     labels = season_lines$Season.Label) +
  geom_hline(yintercept = 0,
             color = '#636363',
             size = 1,
             alpha = 0.6) +
  geom_line(color = '#ffc67a',
            size = 1.3) +
  geom_line(data = lines_for_annotation,
            mapping = aes(x = Games,
                          y = Rolling.Margin,
                          group = Season),
            size = 1.3,
            color = '#da3f2a') +
  geom_point(data = annotation_geom_point,
             mapping = aes(x = Games,
                           y = Rolling.Margin,
                           group = Season),
             color = '#da3f2a',
             size = 2) +
  geom_vline(data = season_lines,
             aes(xintercept = FirstGame),
             #linetype = 'dashed',
             color = '#636363',
             alpha = 0.4) +
  labs(title = 'The History of Nebraska-Omaha Hockey',
       subtitle = paste('Based on Goal Differential by Season between ' , min(clean_hockey$Season), ' and ', max(clean_hockey$Season), sep = ''),
       caption = paste('As of ', max(clean_hockey$Date), '\nVisualization by Alex Elfering; Data courtesy of University of Nebraska, Omaha', sep = ''),
       x = '\nSeason',
       y = 'Goal Differential\n') +
  theme(plot.title = element_text(face = 'bold', size = 20, family = 'Arial'),
        legend.position = 'none',
        legend.background=element_blank(),
        legend.key=element_blank(),
        plot.subtitle = element_text(size = 16, family = 'helvetica'),
        plot.caption = element_text(size = 14, family = 'helvetica'),
        axis.title = element_text(size = 14, family = 'helvetica'),
        axis.text = element_text(size = 14, family = 'Lucida Console'),
        strip.text = ggplot2::element_text(size = 14, hjust = 0, face = 'bold', color = 'black', family = 'Lucida Console'),
        strip.background = element_rect(fill = NA),
        panel.background = ggplot2::element_blank(),
        axis.line = element_line(colour = "#222222", linetype = "solid"),
        panel.grid.major.y = element_line(colour = "#c1c1c1", linetype = "dashed"),
        panel.grid.major.x = element_blank()) 










