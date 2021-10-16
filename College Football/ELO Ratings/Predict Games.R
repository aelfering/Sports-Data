options(dplyr.summarise.inform = FALSE)
GamesNoBowls <- FullSchedule %>% 
  #filter(Season == '1996') %>% 
  mutate(Month = as.character(Month), 
         Year = as.character(Year),
         Day = as.character(Day),
         Season = as.character(Season)) %>% 
  anti_join(ConfChamp) %>%
  group_by(School,
           Opponent,
           Season) %>%
  filter(Day == max(Day)) %>%
  ungroup()

str(FullSchedule)

Sims <- 10000

# pre-season forecast ----
PreSeasonForecastOutcomesList1 <- list()
for(a in 2021:2021){
  
  print(a)
  
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
           OppConf = trim(ifelse(is.na(OppConf), 'Non-Major', OppConf))) %>%
    mutate(Div = ifelse(Season >= 2020 & Conf == 'American', NA, Div))
  
  PreSeasonList <- list()
  for(i in 1:Sims){
    
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
WinForecastWeeks <- list()
ConfFinishForecastWeeks <- list()
for(b in 2021:2021){
  
  print(b)
  
  seasonInt <- tibble(Season = 1996:2021) %>%
    mutate(Rows = row_number()) %>%
    filter(Season == b)
  
  varSeason <- b
  varSeasonL <- varSeason-1
  
  HomeAdv <- 55
  AwayAdv <- -55
  
  GamesFilter <- dplyr::filter(GamesNoBowls, Season == b)
  
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
    mutate(Wk = as.numeric(Wk)) %>%
    mutate(OppConf = case_when(Opponent == 'Notre Dame' & Season != 2020 ~ 'Ind',
                            Opponent == 'Connecticut' & Season > 2020 ~ 'Ind',
                            TRUE ~ as.character(OppConf)))  %>%
    mutate(Div = ifelse(Season >= 2020 & Conf == 'American', NA, Div))
  
  MaxSeasnWk <- max(GameSeasonOutcomes %>% filter(!is.na(Pts)) %>% select(Wk))
  
  SeasonIterList <- list()
  WinForecastList <- list()
  ConfFinishForecastList <- list()
  for(c in 1:MaxSeasnWk ){
    
    ResultsSoFar <- GameSeasonOutcomes %>%
      filter(Wk <= c) %>%
      group_by(School,
               Conf,
               Div) %>%
      summarise(Wins = sum(Wins, na.rm = TRUE),
                Loses = sum(Loses, na.rm = TRUE),
                ConfWins = sum(ConfWins, na.rm = TRUE),
                ConfLoses = sum(ConfLoses, na.rm = TRUE),
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
      inner_join(Weeks,
                 by = c('Month' = 'Month',
                        'Day' = 'Day',
                        'Year' = 'Year')) %>%
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
    GameOutcomeList <- list()
    BowlWinList <- list()
    GamesNeededtoBowl <- list()
    for(i in 1:Sims){
      
      #print(b)
     # print(c)
      #print(i)
      
      PWinForecast <- runif(nrow(NewProbability), 0, 1)
      
      # continuing the forecast
      UpdatedFutureGames <- NewProbability %>%
        mutate(Values = runif(nrow(.), 0, 1)) %>%
        left_join(ResultsSoFar,
                  by = c('School' = 'School',
                         'Conf' = 'Conf',
                         'Div' = 'Div')) %>%
        select(-Wk.y,
               -ConfWins,
               -ConfLoses) %>%
        rename(CWins = Wins,
               CLoses = Loses) %>%
        mutate(Wins = ifelse(Values <= p.Win, 1, 0),
               Loses = ifelse(Values >= p.Win, 1, 0),
               ConfWins = ifelse(Values <= p.Win & Conf == OppConf, 1, 0),
               ConfLoses = ifelse(Values >= p.Win & Conf == OppConf, 1, 0),
               #  in 2021 Wake Forest and North Carolina play a non-conference game despite being in the same conference
               ConfWins = ifelse((School == 'North Carolina' | School == 'Wake Forest') & (Opponent == 'North Carolina' | Opponent == 'Wake Forest'), 0, ConfWins ),
               ConfLoses = ifelse((School == 'North Carolina' | School == 'Wake Forest') & (Opponent == 'North Carolina' | Opponent == 'Wake Forest'), 0, ConfLoses )) %>%
        mutate(iter = i)  %>%
        group_by(School) %>%
        mutate(RollingFWins = cumsum(Wins),
               PotentialWins = CWins + RollingFWins) %>%
        mutate(Games = row_number()) %>%
        ungroup()
      
      TeamGetsBowlWin <- UpdatedFutureGames %>%
        filter(PotentialWins == 6) %>%
        group_by(School) %>%
        filter(Wk.x == min(Wk.x)) %>%
        ungroup() %>%
        select(School,
               Opponent,
               Wk = Wk.x,
               iter) 
      
      GamesNeededToWin <- UpdatedFutureGames %>%
        filter(School %in% TeamGetsBowlWin$School) %>%
        #filter(School == 'Alabama') %>%
        filter(Wins == 1) %>%
        select(School,
               Opponent,
               Wk = Wk.x,
               iter) 
      
        SummariseOutcomes <- UpdatedFutureGames %>%
        group_by(School,
                 Conf,
                 Div) %>%
        summarise(Wins = sum(Wins),
                  Loses = sum(Loses),
                  ConfWins = sum(ConfWins),
                  ConfLoses = sum(ConfLoses)) %>%
        ungroup() %>%
        mutate(FinishesOut = ifelse(Loses == 0, 1, 0))
      
      
      OutLook <- SummariseOutcomes %>%
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
        full_join(OutLook,
                  by = c('School' = 'School', 
                         'Conf' = 'Conf',
                         'Div' = 'Div')) %>%
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
        left_join(ResultsSoFar,
                  by = c('School' = 'School',
                         'Conf' = 'Conf',
                         'Div' = 'Div')) %>%
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
        left_join(RealRecord,
                  by = c('School' = 'School',
                         'Conf' = 'Conf',
                         'Div' = 'Div')) %>%
        mutate(RealRecord = ifelse(is.na(RealRecord), '0-0', RealRecord),
               RealRecord = paste0("'", RealRecord)) %>%
        left_join(NextMatch,
                  by = c('School' = 'School',
                         'Conf' = 'Conf',
                         'Div' = 'Div')) %>%
        mutate(Iter = i)
      
      ProbabilityIter[[i]] <- BindOutlook
      GameOutcomeList[[i]] <- UpdatedFutureGames
      BowlWinList[[i]] <- TeamGetsBowlWin
      GamesNeededtoBowl[[i]] <- GamesNeededToWin
    }
    
    FutureGames <- rbindlist(ProbabilityIter)
    
    Range <- FutureGames %>%
      filter(School == 'Nebraska') %>%
      mutate(zscore = (ForecastedWins - mean(ForecastedWins))/sd(ForecastedWins)) %>%
      arrange(desc(zscore)) %>%
      filter(zscore >= -1.28 & zscore <= 1.28) %>%
      group_by(School) %>%
      summarise(MaxWins = max(ForecastedWins),
                MinWins = min(ForecastedWins)) %>%
      ungroup()
    
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
      mutate(Week = c + 1)
    
    PredictedWins <- FutureGames %>%
      group_by(School,
               Conf,
               Div,
               ForecastedWins) %>%
      summarise(n = n()) %>%
      ungroup() %>%
      mutate(Week = c + 1)
    
    PredictedConfFinish <- FutureGames %>%
      group_by(School,
               Conf,
               Div,
               ConfDivRank) %>%
      summarise(n = n()) %>%
      ungroup() %>%
      mutate(Week = c + 1)
    
    ConfFinishForecastList[[c]] <- PredictedConfFinish
    WinForecastList[[c]] <- PredictedWins
    SeasonIterList[[c]] <- SummariseFuture
    
  }
  
  Mark1 <- rbindlist(SeasonIterList) %>%
    #filter(School == 'North Carolina') %>%
    #filter(Conf == 'Pac-12') %>%
    mutate(Season = b) %>%
    mutate(ForecastedConfWins = ifelse(Conf == 'Ind', NA, ForecastedConfWins),
           ForecastedConfLosses = ifelse(Conf == 'Ind', NA, ForecastedConfLosses),
           FirstinConfDiv = ifelse(Conf == 'Ind', NA, FirstinConfDiv),
           WinConfDivOR = ifelse(Conf == 'Ind', NA, WinConfDivOR))
  
  MarkRow <- seasonInt$Rows
  
  WinForecastWeeks[[MarkRow]] <- rbindlist(WinForecastList) %>% mutate(Season = b)
  ConfFinishForecastWeeks[[MarkRow]] <- rbindlist(ConfFinishForecastList) %>% mutate(Season = b)
  SeasonForecastWeeks[[MarkRow]] <- Mark1
  write.csv(Mark1, glue('C:/Users/alexe/Desktop/Logs/{b} Season Forecast.csv'))
  write.csv(rbindlist(WinForecastList) %>% mutate(Season = b), glue('C:/Users/alexe/Desktop/Logs/{b} Season Forecasted Wins.csv'))
  write.csv(rbindlist(ConfFinishForecastList) %>% mutate(Season = b), glue('C:/Users/alexe/Desktop/Logs/{b} Season Forecasted Conf Div Rank.csv'))
  
}


rbindlist(BowlWinList) %>%
  #filter(School == 'Wisconsin') %>%
  group_by(School,
           Opponent,
           Wk) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  group_by(School) %>%
  mutate(Pct = n/sum(n),
         Sum = sum(n)) %>%
  filter(n == max(n)) %>%
  ungroup() %>%
  arrange(Wk)

rbindlist(GamesNeededtoBowl) %>%
  filter(School == 'Nebraska') %>%
  group_by(School,
           Opponent) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  arrange(desc(n))

rbindlist(GamesNeededtoBowl) %>%
  filter(School == 'Florida State') %>%
  group_by(School,
           iter) %>%
  mutate(Row = row_number()) %>%
  select(School,
         Opponent,
         Row) %>%
  pivot_wider(names_from = Row,
              values_from = Opponent) %>%
  unite(GamesWin, -c(iter, School), sep = ', ', na.rm = TRUE) %>%
  ungroup() %>%
  group_by(School,
           GamesWin) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  arrange(desc(n))
