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
conf_var <- 'MAC'

library(DT)

head(conf_performance)

team_conf <- conf_performance %>%
  replace(is.na(.), 0) %>%
  filter(Team == 'Appalachian State') %>%
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
  complete(Season = seq(min(Season), max(Season), by = 1)) %>%
  mutate(Rolling.Wins = round(rollapplyr(Total.Wins, rolling_var, tes_var, partial = TRUE, na.rm = TRUE)),
         Rolling.Losses = round(rollapplyr(Total.Losses, rolling_var, tes_var, partial = TRUE, na.rm = TRUE)),
         Rolling.Ties = round(rollapplyr(Total.Ties, rolling_var, tes_var, partial = TRUE, na.rm = TRUE)),
         Rolling.Total.Games = rollapplyr(Total.Games, rolling_var, tes_var, partial = TRUE),
         Rolling.Ranked = rollapplyr(Finish.Ranked, rolling_var, sum, partial = TRUE),
         Rolling.Top.10 = rollapplyr(Finished.Top.10, rolling_var, sum, partial = TRUE)) %>%
  ungroup() %>%
  group_by(Team, Conf) %>%
  mutate(Rolling.Conf.Wins = round(rollapplyr(Conf.Wins, rolling_var, tes_var, partial = TRUE, na.rm = TRUE)),
         Rolling.Conf.Losses = round(rollapplyr(Conf.Losses, rolling_var, tes_var, partial = TRUE, na.rm = TRUE)),
         Rolling.Conf.Ties = round(rollapplyr(Conf.Ties, rolling_var, tes_var, partial = TRUE, na.rm = TRUE)),
         Rolling.Conf.Total.Games = rollapplyr(Total.Conf.Games, rolling_var, tes_var, partial = TRUE)) %>%
  ungroup() %>%
  mutate(Rolling.Pct.Won = Rolling.Wins/Rolling.Total.Games,
         Rolling.Conf.Pct.Won = Rolling.Conf.Wins/Rolling.Conf.Total.Games) %>%
  filter(Season >= 1936,
         !is.na(Total.Games))

fill_missing_time_series <- team_conf %>%
  filter(Conf == conf_var) %>%
  group_by(Team) %>%
  complete(Season = seq(min(Season), max(Season), by = 1)) %>%
  ungroup() %>%
  fill(Conf)

top_teams <- team_conf %>%
  filter(Conf == conf_var,
         Season == season_var) %>%
  select(Team,
         Rolling.Wins) %>%
  arrange(desc(Rolling.Wins)) %>%
  filter(dense_rank(desc(Rolling.Wins)) <= 3) %>%
  select(Team)

beg_season <- min(fill_missing_time_series$Season)
end_season <- max(fill_missing_time_series$Season)

time_series_var <- 5

fill_missing_time_series %>%
  filter(Conf == conf_var) %>%
  ggplot(aes(x = Season,
             y = Rolling.Wins,
             group = Team)) +
  geom_line(alpha = 0.2, 
            color = 'gray',
            size = 1) +
  geom_line(data = subset(fill_missing_time_series, Team %in% top_teams$Team & Conf == conf_var),
            mapping = aes(x = Season,
                          y = Rolling.Wins,
                          group = Team,
                          color = Team),
            alpha = 0.5,
            size = 2) +
  geom_line(data = subset(fill_missing_time_series, Team %in% top_teams$Team & Season <= season_var & Conf == conf_var),
            mapping = aes(x = Season,
                          y = Rolling.Wins,
                          group = Team,
                          color = Team),
            size = 2) +
  geom_vline(xintercept = season_var,
             linetype = 'dashed') +
  scale_x_continuous(limits=c(beg_season, end_season),
                     breaks = seq(beg_season, end_season, by = time_series_var)) +
  labs(#title = paste('Winningest Teams in the ', input$conference, ' as of ', input$season, sep = ''),
       #subtitle = paste('Based on a Rolling ', input$variable, ' Season ', input$running, '.', sep = ''),
       y = '',
       x = '') +
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

