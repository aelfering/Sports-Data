library(ggplot2)
library(dplyr)
library(tidyverse)
library(tidyr)
library(lubridate)
library(data.table)
library(zoo)

setwd("~/Documents/GitHub/Sports-Data/UNO Hockey Analysis")

uno_hockey <- read.csv('full hockey data.csv')

rolling_margins <- uno_hockey %>%
  mutate(Game_No = row_number(),
         Date = ymd(Date),
         Wins = ifelse(UNO.Score > Opp.Score, 1, 0),
         Loses = ifelse(UNO.Score < Opp.Score, 1, 0),
         Ties = ifelse(UNO.Score == Opp.Score, 1, 0),
         Margin = UNO.Score - Opp.Score,
         Rolling.Margin = rollapplyr(Margin, 15, sum, partial = TRUE)) %>%
  group_by() %>%
  group_by(Season) %>%
  mutate(Season_Rolling = cumsum(Margin)) %>%
  ungroup()

w

