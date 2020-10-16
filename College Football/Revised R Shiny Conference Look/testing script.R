library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(zoo)

setwd("~/Documents/GitHub/Sports-Data/College Football/Revised R Shiny Conference Look")

conf_performance <- read.csv('cfb conf.csv')

rolling_var <- 5

tes_var <- 'mean'

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
  mutate(Rolling.Wins = round(rollapplyr(Total.Wins, rolling_var, tes_var, partial = TRUE)),
         Rolling.Losses = round(rollapplyr(Total.Losses, rolling_var, tes_var, partial = TRUE)),
         Rolling.Ties = round(rollapplyr(Total.Ties, rolling_var, tes_var, partial = TRUE)),
         Rolling.Total.Games = round(rollapplyr(Total.Games, rolling_var, tes_var, partial = TRUE))) %>%
  ungroup() %>%
  group_by(Team,
           Conf) %>%
  mutate(Rolling.Conf.Wins = round(rollapplyr(Conf.Wins, rolling_var, tes_var, partial = TRUE)),
         Rolling.Conf.Losses = round(rollapplyr(Conf.Losses, rolling_var, tes_var, partial = TRUE)),
         Rolling.Conf.Ties = round(rollapplyr(Conf.Ties, rolling_var, tes_var, partial = TRUE)),
         Rolling.Conf.Total.Games = round(rollapplyr(Total.Conf.Games, rolling_var, tes_var, partial = TRUE))) %>%
  ungroup() %>%
  mutate(Rolling.Pct.Won = Rolling.Wins/Rolling.Total.Games,
         Rolling.Conf.Pct.Won = Rolling.Conf.Wins/Rolling.Conf.Total.Games) %>%
  filter(Season >= 1936)

season_var <- 2008
conf_var <- 'PAC-12'

library(DT)

tbl_test <- team_conf %>%
  filter(Conf == conf_var,
         Season == season_var) %>%
  arrange(desc(Rolling.Pct.Won)) %>%
  unite(Record, c('Rolling.Wins', 'Rolling.Losses', 'Rolling.Ties'), sep = '-', na.rm = TRUE) %>%
  unite(Latest.Record, c('Total.Wins', 'Total.Losses', 'Total.Ties'), sep = '-', na.rm = TRUE) %>%
  unite(Conf.Record, c('Rolling.Conf.Wins', 'Rolling.Conf.Losses', 'Rolling.Conf.Ties'), sep = '-', na.rm = TRUE) %>%
  unite(Latest.Conf.Record, c('Conf.Wins', 'Conf.Losses', 'Conf.Ties'), sep = '-', na.rm = TRUE) %>%
  select(Team,
         Record,
         Latest.Record,
         Conf.Record,
         Latest.Conf.Record)

sketch = htmltools::withTags(table(
  class = 'display',
  thead(
    tr(
      th(rowspan = 2, 'Team'),
      th(colspan = 2, 'Overall'),
      th(colspan = 2, 'Conference Play')
    ),
    tr(
      lapply(rep(c(paste('Rolling ', tes_var, sep = ''), 'Last Season'), 2), th)
    )
  )
))
print(sketch)

datatable(tbl_test, 
          container = sketch, 
          rownames = FALSE,
          caption = paste('Conference performance between ', season_var-rolling_var, ' and ', season_var, sep = ''))














