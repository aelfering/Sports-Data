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

season_var <- 2000
conf_var <- 'Ind'

library(DT)

head(conf_performance)

team_conf <- conf_performance %>%
  replace(is.na(.), 0) %>%
  arrange(Team, 
          Season) %>%
  select(Team, 
         Season,
         Conf,
         Post,
         Total.Wins,
         Total.Losses,
         Total.Ties,
         Conf.Wins,
         Conf.Losses,
         Conf.Ties) %>%
  mutate(Total.Games = Total.Wins + Total.Losses + Total.Ties,
         Total.Conf.Games = Conf.Wins + Conf.Losses + Conf.Ties,
         Pct.Win = Total.Wins/Total.Games,
         Finish.Ranked = ifelse(Post > 0, 1, 0),
         Finished.Top.10 = ifelse(Post > 0 & Post <= 10, 1, 0)) %>%
  group_by(Team) %>%
  mutate(Rolling.Wins = round(rollapplyr(Total.Wins, rolling_var, tes_var, partial = TRUE)),
         Rolling.Losses = round(rollapplyr(Total.Losses, rolling_var, tes_var, partial = TRUE)),
         Rolling.Ties = round(rollapplyr(Total.Ties, rolling_var, tes_var, partial = TRUE)),
         Rolling.Total.Games = rollapplyr(Total.Games, rolling_var, tes_var, partial = TRUE),
         Rolling.Ranked = rollapplyr(Finish.Ranked, rolling_var, sum, partial = TRUE),
         Rolling.Top.10 = rollapplyr(Finished.Top.10, rolling_var, sum, partial = TRUE)) %>%
  ungroup() %>%
  group_by(Team, Conf) %>%
  mutate(Rolling.Conf.Wins = round(rollapplyr(Conf.Wins, rolling_var, tes_var, partial = TRUE)),
         Rolling.Conf.Losses = round(rollapplyr(Conf.Losses, rolling_var, tes_var, partial = TRUE)),
         Rolling.Conf.Ties = round(rollapplyr(Conf.Ties, rolling_var, tes_var, partial = TRUE)),
         Rolling.Conf.Total.Games = rollapplyr(Total.Conf.Games, rolling_var, tes_var, partial = TRUE)) %>%
  ungroup() %>%
  mutate(Rolling.Pct.Won = Rolling.Wins/Rolling.Total.Games,
         Rolling.Conf.Pct.Won = Rolling.Conf.Wins/Rolling.Conf.Total.Games) %>%
  filter(Season >= 1936)

team_conf %>%
  filter(Conf == conf_var) %>%
  group_by(Season) %>%
  summarise(Rolling.Ranked = mean(Rolling.Ranked),
            Rolling.Top.10 = mean(Rolling.Top.10)) %>%
  ungroup() %>%
  ggplot(aes(x = Season,
             y = Rolling.Ranked)) +
  geom_line()

window <- 2
team_conf %>%
  filter(Conf == conf_var) %>%
  rowwise() %>% 
  mutate(Season = list(Season + lubridate::days(0:(window - 1)))) %>% 
  unnest(cols = when) %>%
  group_by(when) %>% 
  summarise(twoDayCount = n_distinct(user))


tbl_test <- team_conf %>%
  filter(Conf == conf_var,
         Season == season_var) %>%
  arrange(desc(Rolling.Pct.Won)) %>%
  mutate(Rolling.Ties = ifelse(Rolling.Ties == 0, NA, Rolling.Ties),
         Rolling.Conf.Ties = ifelse(Rolling.Conf.Ties == 0, NA, Rolling.Conf.Ties)) %>%
  unite(Record, c('Rolling.Wins', 'Rolling.Losses', 'Rolling.Ties'), sep = '-', na.rm = TRUE) %>%
  unite(Conf.Record, c('Rolling.Conf.Wins', 'Rolling.Conf.Losses', 'Rolling.Conf.Ties'), sep = '-', na.rm = TRUE) %>%
  select(Team,
         Record,
         Conf.Record,
         Rolling.Ranked,
         Rolling.Top.10)

tbl_test
