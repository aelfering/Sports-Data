#install.packages('DT')

library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(zoo)
library(DT)

setwd("~/GitHub/Sports-Data/College Football/Revised R Shiny Conference Look")

conf_performance <- read.csv('cfb conf.csv' , fileEncoding="UTF-8-BOM")

rolling_var <- 10

tes_var <- 'mean'

season_var <- 2014
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

head(team_conf)

team_conf %>%
  filter(Conf == conf_var,
         Season == season_var) %>%
  select(Team,
         Rolling.Wins) %>%
  arrange(desc(Rolling.Wins)) #%>%
  #filter(dense_rank(desc(Rolling.Wins)) <= input$ranking) %>%

team_conf %>%
  filter(Conf == conf_var) %>%
  group_by(Team) %>%
  complete(Season = seq(min(Season), max(Season), by = 1)) %>%
  ungroup() %>%
  fill(Conf) %>%
  ggplot(aes(x = Season,
             y = Rolling.Wins,
             group = Team)) +
  geom_line() +
  facet_wrap(~Team)



