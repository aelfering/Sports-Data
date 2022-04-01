#### ELO MODEL 
# generates historical ELO ratings for each CFB team in FBS
# HUGE THANKS TO https://www.sports-reference.com/ for providing the data

library(elo)
library(data.table)
library(tidyverse)
library(ggplot2)
library(rvest)
library(stringi)
library(glue)

# data sources ----
source('~/CFB Parent Variables.R')

FilterSeason <- WinningGames %>%
  #filter(School == 'Kentucky' | Opponent == 'Kentucky') %>%
  filter(Season %in% SeasonVar) %>%
  mutate(Ties = ifelse(Pts == Opp, 1, 0),
         Loses = ifelse(Pts < Opp, 1, 0)) %>%
  mutate(Wk = as.numeric(Wk))

AllSchools <- FilterSeason$School
AllOpponents <- FilterSeason$Opponent

InitRatings <- tibble(School = unique(c(AllOpponents, AllSchools)),
                      Rating = 1500)
UseELODF <- data.frame()
NewRatings <- data.frame()

WeekGames <- list()
SeasonGames <- list()
FinalELORatings <- list()
for(a in SeasonVar){
  
  #a <- 1972
  
  UseSeason <- FilterSeason %>%
    filter(Season == a)
  
  BegWeek <- min(UseSeason$Wk)
  EndWeek <- max(UseSeason$Wk)
  
  for(i in BegWeek:EndWeek){
    
    #i <- 2
    
    if(i == 1 & a == BegSeason){
      UseELODF <- InitRatings
    }else{
      UseELODF <- NewRatings
    }
    
    WeekSeason <- UseSeason %>%
      filter(Wk == i) %>%
      #filter(School == 'Nebraska') %>%
      left_join(UseELODF,
                by = c('School' = 'School')) %>%
      rename(SchoolELO = Rating) %>%
      left_join(UseELODF,
                by = c('Opponent' = 'School')) %>%
      rename(OppELO = Rating) %>%
      mutate(SchoolELOAdj = case_when(Location == '' ~ SchoolELO + Adv,
                                      Location == '@' ~ SchoolELO + DisAdv,
                                      Location == 'N' ~ SchoolELO + NoAdv)) %>%
      mutate(OppELOAdj = case_when(Location == '@' ~ OppELO + Adv,
                                   Location == '' ~ OppELO + DisAdv,
                                   Location == 'N' ~ OppELO + NoAdv)) %>%
      mutate(m = (SchoolELOAdj - OppELOAdj)/400,
             EloDiff = abs(SchoolELOAdj-OppELOAdj),
             PS = round(EloDiff/25, 1),
             p.Opponent = 1/(1+10^m),
             p.Team = 1-p.Opponent,
             MarginABS = abs(Pts-Opp),
             MovMult = log(MarginABS + 1)*(2.2/(EloDiff * 0.001 + 2.2)),
             KAdj = K*log(MarginABS + 1),
             ELOAdj = (K * MovMult) * (1-p.Team),
             TeamELOUpdate = case_when(Wins == 1 ~ SchoolELO + ELOAdj,
                                       Ties == 1 ~ SchoolELO,
                                       Loses == 1 ~ SchoolELO + (ELOAdj*-1) ),
             OpponentELOUpdate = case_when(Loses == 1 ~ OppELO + ELOAdj,
                                           Ties == 1 ~ OppELO,
                                           Wins == 1 ~ OppELO + (ELOAdj * -1) ))
    
    WeekUpdate <- WeekSeason %>%
      select(Season,
             Wk,
             Month,
             Day,
             Year,
             School,
             Opponent,
             Location,
             Pts,
             Opp,
             PS,
             ELOAdj,
             p.Team,
             p.Opponent,
             SchoolELO = TeamELOUpdate,
             OpponentELO = OpponentELOUpdate)
    
    SchoolELO <- WeekUpdate %>%
      select(School,
             Rating = SchoolELO)
    
    OpponentELO <- WeekUpdate %>%
      select(School = Opponent,
             Rating = OpponentELO)
    
    NewRatings <- bind_rows(UseELODF, SchoolELO, OpponentELO) %>%
      group_by(School) %>%
      mutate(Index = row_number()) %>%
      filter(Index == max(Index)) %>%
      ungroup() %>%
      arrange(desc(Rating))
    
    if(i == EndWeek){
      
      #print('Regress happens')
      
      SeasonConf <- Conferences %>%
        filter(Season == a) %>%
        mutate(Conf = trim(Conf)) %>%
        select(Var.2,
               Conf)
      
      UpdateRatings <- NewRatings %>%
        left_join(SeasonConf,
                  by = c('School' = 'Var.2')) %>%
        group_by(Conf) %>%
        mutate(MeanConfELO = mean(Rating)) %>%
        ungroup() %>%
        mutate(RegressRating = case_when(Rating > MeanConfELO ~ Rating-((Rating-MeanConfELO)*RegressVal),
                                         is.na(Conf) ~ 1500,
                                         Rating < MeanConfELO ~ Rating + ((MeanConfELO-Rating)*RegressVal))) %>%
        arrange(desc(Rating))
      
      NewRatings <- UpdateRatings %>%
        select(School,
               Rating = RegressRating) %>%
        arrange(desc(Rating))
      
      ExportRatings <- UpdateRatings %>%
        mutate(Season = a)
      
      FinalELORatings[[a]] <- ExportRatings
      
      print(a)
      print(ExportRatings)
      
    }else{
      
      #print('No Regress happens')
      
      NewRatings <- NewRatings
      
    }
    
    WeekGames[[i]] <- WeekUpdate
    
  }
  
  SeasonDF <- rbindlist(WeekGames)
  
  SeasonGames[[a]] <- SeasonDF
  
}

ELODF <- rbindlist(SeasonGames, fill = TRUE) %>%
  distinct() %>%
  rename(elo.A = SchoolELO,
         elo.B = OpponentELO,
         p.A = p.Team,
         team.A = School,
         team.B = Opponent) %>%
  select(-p.Opponent) %>%
  arrange(Year,
          Month,
          Day)

FullELODF <- ELODF %>%
  distinct() %>%
  select(Season,
         Wk,
         Month,
         Day,
         Year,
         team.A = team.B,
         team.B = team.A,
         Location,
         Pts = Opp,
         Opp = Pts,
         PS,
         ELOAdj,
         p.A,
         elo.A = elo.B,
         elo.B = elo.A) %>%
  mutate(p.A = 1-p.A,
         Location = case_when(Location == '' ~ '@',
                              Location == '@' ~ '',
                              Location == 'N' ~ 'N')) %>%
  bind_rows(ELODF)

write.csv(ELODF, 'C:/Users/alexe/Desktop/ELODF.csv')

