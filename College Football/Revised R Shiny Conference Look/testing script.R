#install.packages('DT')

library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(zoo)
library(DT)

setwd("~/GitHub/Sports-Data/College Football/Revised R Shiny Conference Look")

conf_performance <- read.csv('cfb conf.csv' , fileEncoding="UTF-8-BOM")

rolling_var <- 5

tes_var <- 'mean'

season_var <- 1990
conf_var <- 'MAC'

library(DT)

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
  ungroup() %>%
  mutate(Rolling.Wins = rollapplyr(Total.Wins, rolling_var, tes_var, partial = TRUE),
         Rolling.Losses = rollapplyr(Total.Losses, rolling_var, tes_var, partial = TRUE),
         Rolling.Ties = rollapplyr(Total.Ties, rolling_var, tes_var, partial = TRUE),
         Rolling.Total.Games = rollapplyr(Total.Games, rolling_var, tes_var, partial = TRUE)) %>%
  group_by(Team, Conf) %>%
  mutate(Rolling.Conf.Wins = rollapplyr(Conf.Wins, rolling_var, tes_var, partial = TRUE),
         Rolling.Conf.Losses = rollapplyr(Conf.Losses, rolling_var, tes_var, partial = TRUE),
         Rolling.Conf.Ties = rollapplyr(Conf.Ties, rolling_var, tes_var, partial = TRUE),
         Rolling.Conf.Total.Games = rollapplyr(Total.Conf.Games, rolling_var, tes_var, partial = TRUE)) %>%
  ungroup() %>%
  mutate(Rolling.Pct.Won = Rolling.Wins/Rolling.Total.Games,
         Rolling.Conf.Pct.Won = Rolling.Conf.Wins/Rolling.Conf.Total.Games) %>%
  filter(Season >= 1936)

top_teams <- team_conf %>%
  filter(Conf == conf_var,
         Season == season_var) %>%
  select(Team,
         Rolling.Wins) %>%
  arrange(desc(Rolling.Wins)) %>%
  filter(dense_rank(desc(Rolling.Wins)) <= 5) %>%
  select(Team)


ggplot(team_conf,
       aes(x = Season,
           y = Rolling.Wins,
           group = Team)) +
  geom_line(alpha = 0.4,
            color = 'gray') +
  geom_line(data = subset(team_conf, Team %in% top_teams$Team),
            mapping = aes(x = Season,
                          y = Rolling.Wins,
                          group = Team,
                          color = Team),
            size = 2,
            alpha = 0.5) +
  geom_line(data = subset(team_conf, Team %in% top_teams$Team & Season <= season_var),
            mapping = aes(x = Season,
                          y = Rolling.Wins,
                          group = Team,
                          color = Team),
            size = 2,
            alpha = 0.5) +
  geom_vline(xintercept = season_var) +
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














