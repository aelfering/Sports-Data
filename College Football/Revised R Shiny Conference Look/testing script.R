library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(zoo)

setwd("~/Documents/GitHub/Sports-Data/College Football/Revised R Shiny Conference Look")

conf_performance <- read.csv('cfb conf.csv')

rolling_var <- 3

tes_var <- 'sum'

team_conf <- conf_performance %>%
  replace(is.na(.), 0) %>%
  arrange(Team, 
          Season) %>%
  select(Team, 
         Season,
         Conf,
         Total.Wins,
         Total.Losses,
         Total.Ties,
         Conf.Wins,
         Conf.Losses,
         Conf.Ties) %>%
  mutate(Total.Games = Total.Wins + Total.Losses + Total.Ties,
         Total.Conf.Games = Conf.Wins + Conf.Losses + Conf.Ties,
         Pct.Win = Total.Wins/Total.Games) %>%
  group_by(Team) %>%
  mutate(Rolling.Wins = rollapplyr(Total.Wins, rolling_var, tes_var, partial = TRUE),
         Rolling.Losses = rollapplyr(Total.Losses, rolling_var, tes_var, partial = TRUE),
         Rolling.Ties = rollapplyr(Total.Ties, rolling_var, tes_var, partial = TRUE),
         Rolling.Total.Games = rollapplyr(Total.Games, rolling_var, tes_var, partial = TRUE)) %>%
  mutate(Rolling.Conf.Wins = rollapplyr(Conf.Wins, rolling_var, tes_var, partial = TRUE),
         Rolling.Conf.Losses = rollapplyr(Conf.Losses, rolling_var, tes_var, partial = TRUE),
         Rolling.Conf.Ties = rollapplyr(Conf.Ties, rolling_var, tes_var, partial = TRUE),
         Rolling.Conf.Total.Games = rollapplyr(Total.Conf.Games, rolling_var, tes_var, partial = TRUE)) %>%
  ungroup() %>%
  mutate(Rolling.Pct.Won = Rolling.Wins/Rolling.Total.Games,
         Rolling.Conf.Pct.Won = Rolling.Conf.Wins/Rolling.Conf.Total.Games) %>%
  filter(Season >= 1936)

season_var <- 2019
conf_var <- 'Big Ten'

top_teams <- team_conf %>%
  filter(Conf == conf_var,
         Season == season_var) %>%
  select(Team,
         Rolling.Wins)

top_sort <- top_teams[with(top_teams, order(Rolling.Wins, decreasing = F)),]
top_sort$rank <- rank(top_sort$Rolling.Wins)

top_teams_sort <- top_sort %>%
  filter(row_number((Rolling.Wins)) <= 5) %>%
  select(Team)

team_conf %>%
  filter(Conf == conf_var) %>%
  ggplot(aes(x = Season,
             y = Rolling.Wins,
             group = Team)) +
  geom_line(alpha = 0.2, 
            color = 'gray',
            size = 1) +
  geom_line(data = subset(team_conf, Team %in% top_teams_sort$Team & Conf == conf_var),
            mapping = aes(x = Season,
                          y = Rolling.Wins,
                          group = Team,
                          color = Team),
            alpha = 0.3,
            size = 1) +
  geom_line(data = subset(team_conf, Team %in% top_teams_sort$Team & Season <= season_var & Conf == conf_var),
            mapping = aes(x = Season,
                          y = Rolling.Wins,
                          group = Team,
                          color = Team),
            size = 1) +
  geom_vline(xintercept = season_var,
             linetype = 'dashed') +
  labs(title = paste('Winningest Teams in the ', conf_var, ' as of ', season_var, sep = ''),
       subtitle = paste('Based on a Rolling ', rolling_var, ' Season Sum', sep = '')) +
  theme(plot.title = element_text(face = 'bold', size = 18, family = 'Arial'),
        legend.position = 'top',
        legend.background=element_blank(),
        legend.key=element_blank(),
        legend.text = element_text(size = 12, family = 'Arial'),
        legend.title = element_text(size = 12, family = 'Arial'),
        plot.subtitle = element_text(size = 15, family = 'Arial'),
        plot.caption = element_text(size = 12, family = 'Arial'),
        axis.title = element_text(size = 12, family = 'Arial'),
        axis.text = element_text(size = 12, family = 'Arial'),
        strip.text = ggplot2::element_text(size = 12, hjust = 0, face = 'bold', color = 'black', family = 'Arial'),
        strip.background = element_rect(fill = NA),
        panel.background = ggplot2::element_blank(),
        axis.line = element_line(colour = "#222222", linetype = "solid"),
        panel.grid.major.y = element_line(colour = "#c1c1c1", linetype = "dashed"),
        panel.grid.major.x = element_blank()) 


