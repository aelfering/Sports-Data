setwd("~/Documents/GitHub/Sports-Data/College Football/Conference Shiny App/ShinyAppTest")

list.of.packages <- c("ggplot2", 
                      "dplyr", 
                      'tidyverse', 
                      'tidyr', 
                      'lubridate', 
                      'data.table', 
                      'zoo', 
                      'ggrepel', 
                      'directlabels', 
                      'reactable',
                      'shiny',
                      'htmltools',
                      'shinydashboard',
                      'shinythemes')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(ggplot2)
library(dplyr)
library(tidyverse)
library(tidyr)
library(lubridate)
library(data.table)
library(zoo)
library(ggrepel)
library(directlabels)
library(reactable)
library(shiny)
library(htmltools)
library(shinydashboard)
library(shinythemes)

cfb_conf <- read.csv('cfb conf.csv')
conf_records <- read.csv('conf_records.csv')
championships <- read.csv('cfb champions.csv')

# Pull Championship Data
championships$National.Champion <- gsub("\\s*\\([^\\)]+\\)","",as.character(championships$National.Champion))

champ_pivot <- championships %>%
  mutate(National.Champion = ifelse(National.Champion == 'Miami', 'Miami (FL)', National.Champion)) %>%
  separate(National.Champion, into = paste("C", 1:4, sep = '-'), sep = ', ') %>%
  select(Year,
         `C-1`,
         `C-2`,
         `C-3`)

one_champ <- champ_pivot %>%
  mutate(Won.Championship = 1) %>%
  select(Year,
         Champion = `C-1`,
         Won.Championship) %>%
  filter(!is.na(Champion))

two_champ <- champ_pivot %>%
  mutate(Won.Championship = 1) %>%
  select(Year,
         Champion = `C-2`,
         Won.Championship) %>%
  filter(!is.na(Champion))

three_champ <- champ_pivot %>%
  mutate(Won.Championship = 1) %>%
  select(Year,
         Champion = `C-3`,
         Won.Championship) %>%
  filter(!is.na(Champion))

cfb_championships <- bind_rows(one_champ, two_champ, three_champ)

diff <- max(input$Season)-min(input$Season)

# Conference Records
rolling.bowl.wins <- conf_records %>%
  select(Conference,
         Year,
         Bowl.Wins = W.1,
         Bowl.Losses = L.1,
         Bowl.Ties = T.1) %>%
  arrange(Year) %>%
  group_by(Conference) %>%
  mutate(Rolling.Bowl.Wins = rollapplyr(Bowl.Wins, 5, sum, partial = TRUE),
         Rolling.Bowl.Losses = rollapplyr(Bowl.Losses, 5, sum, partial = TRUE),
         Rolling.Bowl.Ties = rollapplyr(Bowl.Ties, 5, sum, partial = TRUE)) %>%
  mutate(Bowl.Record = paste(Rolling.Bowl.Wins,
                             Rolling.Bowl.Losses,
                             Rolling.Bowl.Ties,
                             sep = '-')) %>%
  select(Conference,
         Year,
         Bowl.Record)

# Rolling Wins by Team and by Conference
cfb_team_data <- left_join(cfb_conf, cfb_championships, by = c('Season' = 'Year', 'Team' = 'Champion'))    

#       Find the rolling number of wins, losses, and ties by team and aggregate by conference
team.rolling.wins <- cfb_team_data %>%
  replace(is.na(.), 0) %>%
  mutate(AP_Post_Ranked = ifelse(Post > 0, 1, 0),
         AP_Top_10 = ifelse(Post <= 10 & Post != 0, 1, 0)) %>%
  arrange(Season) %>%
  group_by(Team) %>%
  # this solves for the App States of football who went to FCS in 1981 but rejoined FBS later
  complete(Season = seq(min(Season), 2019, by = 1)) %>%
  mutate(Conf = ifelse(is.na(Conf), 'Did not compete in FBS', Conf),
         Notes = ifelse(is.na(Notes), '', Notes)) %>%
  replace(is.na(.), 0) %>%
  mutate(Rolling.Wins = rollapplyr(Total.Wins, 5, sum, partial = TRUE),
         Rolling.Losses = rollapplyr(Total.Losses, 5, sum, partial = TRUE),
         Rolling.Ties = rollapplyr(Total.Ties, 5, sum, partial = TRUE),
         Rolling.Championships = rollapplyr(Won.Championship, 5, sum, partial = TRUE)) %>%
  ungroup() %>%
  select(Team,
         Season,
         Conf,
         Rolling.Wins,
         Rolling.Losses,
         Rolling.Ties,
         AP_Post_Ranked,
         AP_Top_10,
         Rolling.Championships) %>%
  mutate(Pct.Won = Rolling.Wins/(Rolling.Wins + Rolling.Losses + Rolling.Ties)) %>%
  # Removing teams that did not compete on the FBS level
  filter(!Conf %in% c('Did not compete in FBS')) %>%
  # Here is where we aggregate the team results by conference
  group_by(Season,
           Conf) %>%
  mutate(Total.Wins = sum(Rolling.Wins),
         Total.Losses = sum(Rolling.Losses),
         Total.Ties = sum(Rolling.Ties),
         Total.Pct = Total.Wins/(Total.Wins + Total.Losses + Total.Ties),
         Teams = n_distinct(Team),
         Total.Championships = sum(Rolling.Championships)) %>%
  mutate(Conf_AP_Rank = sum(AP_Post_Ranked),
         Conf_AP_10 = sum(AP_Top_10)) %>%
  mutate(Tm.Rank = dense_rank(desc(Pct.Won))) %>%
  # Return the team who has won the most games by conference overall
  # Some conferences return multiple teams per season...we will solve for that below
  filter(Tm.Rank == 1) %>%
  mutate(Rows = row_number()) %>%
  ungroup()

#     if a conference has more than one leader, this concatenates them
leader.concat <- team.rolling.wins %>%
  distinct(Season,
           Conf,
           Team,
           Rolling.Wins,
           Rolling.Losses,
           Rolling.Ties,
           Rows) %>%
  mutate(Team_Name = paste(Team, ' (', Rolling.Wins, '-', Rolling.Losses, '-', Rolling.Ties, ') ', sep = '')) %>%
  distinct(Season,
           Conf,
           Team_Name,
           Rows) %>%
  group_by(Season,
           Conf) %>%
  mutate(Rows = as.character(Rows)) %>%
  spread(Rows, Team_Name) %>%
  unite(Leaders, -c('Season', 'Conf'), sep = ', ', na.rm = TRUE) %>%
  distinct(Season,
           Conf,
           Leaders)

conf.rolling.records <- team.rolling.wins %>%
  distinct(Season,
           Conf,
           Teams,
           Total.Wins,
           Total.Losses,
           Total.Ties,
           Total.Pct,
           Conf_AP_Rank,
           Conf_AP_10,
           Total.Championships) %>%
  group_by(Conf) %>%
  mutate(Rolling.AP.Rank = rollapplyr(Conf_AP_Rank, 5, sum, partial = TRUE)/5,
         Rolling.AP.10 = rollapplyr(Conf_AP_10, 5, sum, partial = TRUE)/5) %>%
  ungroup()

#       Joining the various data frames together
conf.leaders.records <- inner_join(conf.rolling.records, leader.concat, by = c('Season' = 'Season', 'Conf' = 'Conf'))
conf.leader.bowls <- inner_join(conf.leaders.records, rolling.bowl.wins, by = c('Season' = 'Year', "Conf" = "Conference"))

#       Formatting columns for data table
conf.record.table <- conf.leader.bowls %>%
  mutate(Earlier = (Season-5)+1) %>%
  filter(Season == 2019) %>%
  mutate(Period = paste(Earlier, Season, sep = '-'),
         Conf_Record = paste(Total.Wins, Total.Losses, Total.Ties, sep = '-')
  ) %>%
  arrange(Conf) %>%
  select(Conference = Conf,
         Period,
         `Teams` = Teams,
         `Percent Won` = Total.Pct,
         `Conference Record` = Conf_Record,
         `Bowl Record` = Bowl.Record,
         `Championships` = Total.Championships,
         `Average in AP 25` = Rolling.AP.Rank,
         `Average in AP 10` = Rolling.AP.10,
         `Winningest Team(s)` = Leaders) %>%
  replace(is.na(.), 0)

# Team record
source('Individual Team Records.R')

mark1 <- dplyr::select(cfb_team_data, Season, Team, Conf)

mark2 <- inner_join(overall_names_fixed, mark1, by = c('Team' = 'Team', 'Season' = 'Season'))

mark3 <- full_join(overall_names_fixed, mark1, by = c('Team' = 'Team', 'Season' = 'Season'))