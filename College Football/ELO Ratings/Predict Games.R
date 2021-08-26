options(dplyr.summarise.inform = FALSE)
GamesNoBowls <- FullSchedule %>% 
  #filter(Season == '1996') %>% 
  mutate(Month = as.character(Month), 
         Year = as.character(Year),
         Day = as.character(Day),
         Season = as.character(Season)) %>% 
  anti_join(ConfChamp) #%>% filter(!is.na(Pts))

str(FullSchedule)

# pre-season forecast ----
PreSeasonForecastOutcomesList1 <- list()
for(a in 2021:2021){
  
  varSeason <- a
  varSeasonL <- varSeason-1
  
  HomeAdv <- 55
  AwayAdv <- -55
  
  GamesFilter <- dplyr::filter(GamesNoBowls, Season == a)
  
  PreSeasonELO <- bind_rows(WinningSide,
                            LosingSide) %>%
    arrange(#team.A,
      Year,
      Month,
      Day) %>%
    filter(Season <= varSeasonL) %>%
    group_by(team.A) %>%
    filter(row_number() == max(row_number())) %>%
    ungroup() %>%
    select(team.A,
           elo.A) %>%
    mutate(elo.R = case_when(elo.A >= 1500 ~ elo.A-((elo.A-1500)*0.33),
                             elo.A < 1500 ~ elo.A + ((1500-elo.A)*0.33) )) %>%
    select(team.A,
           -elo.A,
           elo.A = elo.R)
  
  PreSeasonELOGames <- GamesNoBowls %>%
    filter(Season == varSeason) %>%
    select(-Notes,
           -Pts,
           -Opp) %>%
    inner_join(PreSeasonELO,
               by = c('School' = 'team.A')) %>%
    left_join(PreSeasonELO,
              by = c('Opponent' = 'team.A')) %>%
    mutate(elo.A.y = ifelse(is.na(elo.A.y), 1500, as.numeric(elo.A.y)),
           elo.A.x = case_when(Location == '' ~ as.numeric(elo.A.x) + HomeAdv,
                               Location == '@' ~ as.numeric(elo.A.x) + AwayAdv,
                               Location == 'N' ~ as.numeric(elo.A.x)),
           elo.A.y = case_when(Location == '' ~ as.numeric(elo.A.y) + AwayAdv,
                               Location == '@' ~ as.numeric(elo.A.y) + HomeAdv,
                               Location == 'N' ~ as.numeric(elo.A.y)),
           m = (elo.A.y - elo.A.x)/400,
           p.Win = 1/(1+10^m) ) %>%
    distinct() %>%
    rename(OppConf = Conf) %>%
    mutate(Season = as.integer(Season)) %>%
    inner_join(Conferences,
               by = c('Season' = 'Season',
                      'School' = 'Var.2')) %>%
    mutate(Conf = trim(Conf),
           OppConf = trim(ifelse(is.na(OppConf), 'Non-Major', OppConf)))
  
  PreSeasonList <- list()
  for(i in 1:10000){
    
    WinValues <- runif(nrow(PreSeasonELOGames), 0, 1)
    
    PreSeasonOutComes <- PreSeasonELOGames %>%
      mutate(Value = WinValues) %>%
      mutate(Wins = ifelse(Value <= p.Win, 1, 0),
             Loses = ifelse(Value > p.Win, 1, 0),
             ConfWins = ifelse(Value <= p.Win & Conf == OppConf, 1, 0),
             ConfLoses = ifelse(Value > p.Win & Conf == OppConf, 1, 0)) %>%
      #filter(School == 'Missouri') %>%
      group_by(School,
               Conf,
               Div) %>%
      summarise(FWins = sum(Wins),
                FLosses = sum(Loses),
                FConfWins = sum(ConfWins),
                FConfLosses = sum(ConfLoses)) %>%
      ungroup() %>%
      mutate(FConfWins = ifelse(Conf == 'Ind', NA, FConfWins),
             FConfLosses = ifelse(Conf == 'Ind', NA, FConfLosses)) %>%
      mutate(TotalGames = FWins + FLosses,
             Undefeated = ifelse(FWins == TotalGames, 1, 0)) %>%
      group_by(Conf, 
               Div) %>%
      mutate(ConfDivRank = dense_rank(desc(FConfWins))) %>%
      mutate(FirstinConfDiv = ifelse(ConfDivRank == 1, 1, 0),
             TotalFirst = sum(FirstinConfDiv),
             WinConfDivOR = case_when(ConfDivRank ==1 & TotalFirst == 1 ~ 1 ) ) %>%
      ungroup() %>%
      mutate(Win0500 = ifelse((FWins/ (FWins+ FLosses) ) >= 0.5, 1, 0))
    
    PreSeasonList[[i]] <- PreSeasonOutComes
    
  }
  
  PreSeasonForecast <- rbindlist(PreSeasonList) %>%
    group_by(School,
             Conf,
             Div) %>%
    summarise(FMeanWins = round(mean(FWins), 1),
              FMeanLosses = round(mean(FLosses), 1),
              FMeanConfWins = round(mean(FConfWins), 1),
              FMeanConfLosses = round(mean(FConfLosses), 1),
              FUndefeated = (sum(Undefeated)/10000),
              FirstinConfDiv = sum(FirstinConfDiv)/10000,
              Win0500 = sum(Win0500)/10000,
              WinConfDivOR = sum(WinConfDivOR, na.rm = TRUE)/10000) %>%
    arrange(Conf,
            Div,
            desc(WinConfDivOR),
            desc(FMeanWins),
            FMeanLosses) %>%
    mutate(Season = a)
  
  write.csv(PreSeasonForecast, glue::glue('{a} Pre-Season Forecast.csv'))
  
}

# week-by-week forecast ----

SeasonForecastWeeks <- list()
for(b in 2019:2019){
  
  seasonInt <- tibble(Season = 1996:2019) %>%
    mutate(Rows = row_number()) %>%
    filter(Season == b)
  
  varSeason <- b
  varSeasonL <- varSeason-1
  
  HomeAdv <- 55
  AwayAdv <- -55
  
  GamesFilter <- dplyr::filter(GamesNoBowls, Season == b & !is.na(Pts))
  
  InitEloRatings <- bind_rows(WinningSide,
                              LosingSide) %>%
    #filter(team.A == 'Penn State',
    #       Season == varSeason)
    arrange(#team.A,
      Year,
      Month,
      Day) %>%
    filter(Season <= varSeasonL) %>%
    group_by(team.A) %>%
    filter(row_number() == max(row_number())) %>%
    ungroup() %>%
    select(team.A,
           elo.A) %>%
    mutate(elo.R = case_when(elo.A >= 1500 ~ elo.A-((elo.A-1500)*0.33),
                             elo.A < 1500 ~ elo.A + ((1500-elo.A)*0.33) )) %>%
    select(team.A,
           -elo.A,
           elo.A = elo.R) %>%
    mutate(Row = 0)
  
  GameSeasonOutcomes <- GamesFilter %>%
    rename(OppConf = Conf) %>%
    mutate(Season = as.integer(Season)) %>%
    inner_join(Conferences,
               by = c('Season' = 'Season',
                      'School' = 'Var.2')) %>%
    mutate(Conf = trim(Conf),
           OppConf = trim(ifelse(is.na(OppConf), 'Non-Major', OppConf)),
           Pts = as.numeric(Pts),
           Opp = as.numeric(Opp)) %>%
    mutate(Wins = ifelse(Pts > Opp, 1, 0),
           Loses = ifelse(Opp > Pts, 1, 0),
           Ties = ifelse(Pts == Opp, 1, 0),
           ConfWins = ifelse(Pts > Opp & Conf == OppConf, Wins, 0),
           ConfLoses = ifelse(Opp > Pts & Conf == OppConf, Loses, 0),
           ConfTies = ifelse(Pts == Opp & Conf == OppConf, Ties, 0)) %>%
    mutate(Wk = as.numeric(Wk))
  
  MaxSeasnWk <- max(GameSeasonOutcomes$Wk)
  
  SeasonIterList <- list()
  for(c in 1:MaxSeasnWk ){
    
    ResultsSoFar <- GameSeasonOutcomes %>%
      filter(Wk <= c) %>%
      group_by(School,
               Conf,
               Div) %>%
      summarise(Wins = sum(Wins),
                Loses = sum(Loses),
                ConfWins = sum(ConfWins),
                ConfLoses = sum(ConfLoses),
                Wk = max(Wk)) %>%
      ungroup() 
    
    RealRecord <- ResultsSoFar %>%
      select(School,
             Conf,
             Div,
             Wins,
             Loses) %>%
      unite(RealRecord, c('Wins', 'Loses'), sep = '-')
    
    ELOSoFar <- bind_rows(WinningSide,
                          LosingSide) %>%
      arrange(#team.A,
        Year,
        Month,
        Day) %>%
      filter(Season == varSeason) %>%
      inner_join(Weeks) %>%
      mutate(Wk = as.numeric(Wk)) %>%
      filter(Wk <= c) %>%
      group_by(team.A) %>%
      filter(row_number() == max(row_number())) %>%
      ungroup() %>%
      select(team.A,
             elo.A) %>%
      mutate(Row = c)
    
    UpdateTheELO <- InitEloRatings %>%
      bind_rows(ELOSoFar) %>%
      group_by(team.A) %>%
      filter(Row == max(Row)) %>%
      ungroup() %>%
      select(-Row)
    
    NewProbability <- GameSeasonOutcomes %>%
      filter(Wk > c) %>%
      select(Month,
             Day,
             Year,
             Season,
             School,
             Conf,
             Div,
             Opponent,
             OppConf,
             Location,
             Wk) %>%
      inner_join(UpdateTheELO,
                 by = c('School' = 'team.A')) %>%
      left_join(UpdateTheELO,
                by = c('Opponent' = 'team.A'))  %>%
      mutate(elo.A.y = ifelse(is.na(elo.A.y), 1500, as.numeric(elo.A.y)),
             elo.A.x = case_when(Location == '' ~ as.numeric(elo.A.x) + HomeAdv,
                                 Location == '@' ~ as.numeric(elo.A.x) + AwayAdv,
                                 Location == 'N' ~ as.numeric(elo.A.x)),
             elo.A.y = case_when(Location == '' ~ as.numeric(elo.A.y) + AwayAdv,
                                 Location == '@' ~ as.numeric(elo.A.y) + HomeAdv,
                                 Location == 'N' ~ as.numeric(elo.A.y)),
             m = (elo.A.y - elo.A.x)/400,
             p.Win = 1/(1+10^m) )
    
    NextMatch <- NewProbability %>%
      group_by(School) %>%
      slice(which.min(Wk)) %>%
      ungroup() %>%
      select(School,
             Conf,
             Div,
             NextOpponent = Opponent)
    
    ProbabilityIter <- list()
    for(i in 1:10000){
      
      PWinForecast <- runif(nrow(NewProbability), 0, 1)
      
      UpdatedFutureGames <- NewProbability %>%
        mutate(Values = PWinForecast) %>%
        mutate(Wins = ifelse(Values <= p.Win, 1, 0),
               Loses = ifelse(Values >= p.Win, 1, 0),
               ConfWins = ifelse(Values <= p.Win & Conf == OppConf, 1, 0),
               ConfLoses = ifelse(Values >= p.Win & Conf == OppConf, 1, 0)) %>%
        #filter(School %in% c('Iowa State', 'Missouri')) %>%
        group_by(School,
                 Conf,
                 Div) %>%
        summarise(Wins = sum(Wins),
                  Loses = sum(Loses),
                  ConfWins = sum(ConfWins),
                  ConfLoses = sum(ConfLoses)) %>%
        ungroup() %>%
        mutate(FinishesOut = ifelse(Loses == 0, 1, 0))
      
      
      OutLook <- UpdatedFutureGames %>%
        group_by(School,
                 Conf,
                 Div) %>%
        summarise(RWins = mean(Wins),
                  RLoses = mean(Loses),
                  RConfWins = mean(ConfWins),
                  RConfLoses = mean(ConfLoses),
                  FinishesOut = sum(FinishesOut)) %>%
        ungroup()
      
      Outlook1 <- ResultsSoFar %>%
        #filter(School %in% c('Iowa State', 'Missouri')) %>%
        full_join(OutLook) %>%
        mutate(Wins = ifelse(is.na(Wins), 0, Wins),
               Loses = ifelse(is.na(Loses), 0, Loses),
               ConfWins = ifelse(is.na(ConfWins), 0, ConfWins),
               ConfLoses = ifelse(is.na(ConfLoses), 0, ConfLoses)) %>%
        #replace(is.na(.), 0) %>%
        mutate(ForecastedWins = RWins + Wins,
               ForecastedLosses = RLoses + Loses,
               ForecastedConfWins = RConfWins + ConfWins,
               ForecastedConfLosses = RConfLoses + ConfLoses) %>%
        select(School,
               Conf,
               Div,
               FinishesOut,
               ForecastedWins,
               ForecastedLosses,
               ForecastedConfWins,
               ForecastedConfLosses) %>%
        mutate(Iter = c,
               TotalGames = ForecastedWins + ForecastedLosses,
               Win0500 = ifelse((ForecastedWins/TotalGames) >= 0.5, 1, 0),
               WinsOut = ifelse(ForecastedLosses == 0, 1, 0))
      
      Outlook1NO <- Outlook1 %>%
        filter(!is.na(ForecastedWins))
      
      Outlook1NA <- Outlook1 %>%
        filter(is.na(ForecastedWins)) %>%
        left_join(ResultsSoFar) %>%
        select(-ForecastedWins,
               -ForecastedLosses,
               -ForecastedConfWins,
               -ForecastedConfLosses,
               -Wk) %>%
        rename(ForecastedWins = Wins,
               ForecastedLosses = Loses,
               ForecastedConfWins = ConfWins,
               ForecastedConfLosses = ConfLoses) %>%
        mutate(Win0500 = ifelse(ForecastedWins/(ForecastedWins + ForecastedLosses) >= 0.5, 1, 0),
               TotalGames = ForecastedWins + ForecastedLosses)
      
      BindOutlook <- bind_rows(Outlook1NO, Outlook1NA)  %>%
        group_by(Conf, 
                 Div) %>%
        mutate(ConfDivRank = dense_rank(desc(ForecastedConfWins))) %>%
        mutate(FirstinConfDiv = ifelse(ConfDivRank == 1, 1, 0),
               TotalFirst = sum(FirstinConfDiv),
               WinConfDivOR = case_when(ConfDivRank ==1 & TotalFirst == 1 ~ 1 )) %>%
        ungroup() %>%
        left_join(RealRecord) %>%
        mutate(RealRecord = ifelse(is.na(RealRecord), '0-0', RealRecord)) %>%
        left_join(NextMatch)
        
      ProbabilityIter[[i]] <- BindOutlook
      
    }
    
    FutureGames <- rbindlist(ProbabilityIter)
    
    SummariseFuture <- FutureGames %>%
      group_by(School,
               Conf,
               Div,
               RealRecord,
               NextOpponent) %>%
      mutate(WinsOut = ifelse(ForecastedLosses == 0, 1, 0)) %>%
      summarise(ForecastedWins = mean(ForecastedWins),
                ForecastedLosses = mean(ForecastedLosses),
                ForecastedConfWins = mean(ForecastedConfWins),
                ForecastedConfLosses = mean(ForecastedConfLosses),
                FirstinConfDiv = sum(FirstinConfDiv),
                WinConfDivOR = sum(WinConfDivOR, na.rm = TRUE),
                WinsOut = sum(WinsOut, na.rm = TRUE),
                FinishesOut = sum(FinishesOut),
                Win0500 = sum(Win0500)
                ) %>%
      ungroup() %>%
      mutate(Week = c)
    
    SeasonIterList[[c]] <- SummariseFuture
    
  }
  
  Mark1 <- rbindlist(SeasonIterList) %>%
    #filter(School == 'Kansas State') %>%
    mutate(Season = b)
  
  MarkRow <- seasonInt$Rows
  
  SeasonForecastWeeks[[MarkRow]] <- Mark1
  write.csv(Mark1, glue('C:/Users/alexe/Desktop/Logs/{b} Season Forecast.csv'))
  
}

Mark1 <- rbindlist(SeasonForecastWeeks) %>%
  filter(#Conf == 'Big 12',
         School == 'Florida State') %>%
  ggplot() + 
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = 100) +
  geom_step(mapping = aes(x = Week,
                          y = WinConfDivOR  ,
                          color = School),
            size = 1) +
  geom_point(mapping = aes(x = Week,
                          y = WinConfDivOR  ,
                          color = School),
            size = 1.5) +
  facet_wrap(~School)





