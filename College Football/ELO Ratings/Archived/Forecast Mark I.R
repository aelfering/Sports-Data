####  COLLEGE FOOTBALL FORECAST
# MARK I

# libraries ----
source('~/CFB Parent Variables.R')

# prepare data  ----
GamesNoBowls <- FullSchedule %>% 
  left_join(UniqueID,
            by = c('School' = 'School',
                   'Season' = 'Season',
                   'Wk' = 'Wk',
                   'Opponent' = 'Opponent')) %>%
  left_join(UniqueID,
            by = c('School' = 'Opponent',
                   'Season' = 'Season',
                   'Wk' = 'Wk',
                   'Opponent' = 'School')) %>%
  unite(ID, c('UniqueGameID.x', 'UniqueGameID.y'), na.rm = TRUE, sep = '') %>%
  arrange(ID) %>%
  group_by(ID) %>%
  mutate(ID1 = row_number()) %>%
  ungroup() %>%
  filter(ID1 == 1) %>%
  select(-ID,
         -ID1) %>%
  mutate(Month = as.character(Month), 
         Year = as.character(Year),
         Day = as.character(Day),
         Season = as.character(Season)) %>% 
  anti_join(BowlGames) %>%
  group_by(School,
           Opponent,
           Season) %>%
  filter(Day == max(Day)) %>%
  ungroup()

# pre-season forecast ----
for(a in SeasonVec:SeasonVec){
  
  # this pre-season loops through each week of games to predict win-loss ratio and likelihood of winning conference division
  
  varSeason <- a
  varSeasonL <- varSeason-1
  
  GamesFilter <- dplyr::filter(GamesNoBowls, Season == a)
  
  PreSeasonELO <- FullELODF %>%
    arrange(
      Year,
      Month,
      Day) %>%
    filter(Season <= varSeasonL) %>%
    group_by(team.A) %>%
    filter(row_number() == max(row_number())) %>%
    ungroup() %>%
    select(team.A,
           elo.A) %>%
    left_join(SeasonConf,
              by = c('team.A' = 'Var.2')) %>%
    group_by(Conf) %>%
    mutate(MeanConfELO = mean(elo.A)) %>%
    ungroup() %>%
    mutate(elo.R = case_when(elo.A >= MeanConfELO ~ elo.A-((elo.A-MeanConfELO)*0.5),
                             elo.A < MeanConfELO ~ elo.A + ((MeanConfELO-elo.A)*0.5),
                             is.na(Conf) ~ 1500),
           elo.R = ifelse(is.na(Conf), 1500, elo.R)) %>%
    arrange(desc(elo.R)) %>%
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
           elo.A.x = case_when(Location == '' ~ as.numeric(elo.A.x) + Adv,
                               Location == '@' ~ as.numeric(elo.A.x) + DisAdv,
                               Location == 'N' ~ as.numeric(elo.A.x)),
           elo.A.y = case_when(Location == '' ~ as.numeric(elo.A.y) + DisAdv,
                               Location == '@' ~ as.numeric(elo.A.y) + Adv,
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
    # AAC removed divisions starting in 2020
    mutate(Div = ifelse(Season >= 2020 & Conf == 'American', NA, Div))
  
  PreSeasonList <- list()
  PreSeasonGamesOutcome <- list()
  BadResultsList <- list()
  for(i in 1:Sims){
    
    print(paste0('Preseason ', varSeason, ' iter ', i))
    
    WinValues <- runif(nrow(PreSeasonELOGames), 0, 1)
    
    PreSeasonResults <- PreSeasonELOGames %>%
      mutate(Value = WinValues) %>%
      mutate(Wins = ifelse(Value <= p.Win, 1, 0),
             Loses = ifelse(Value > p.Win, 1, 0),
             ConfWins = ifelse(Value <= p.Win & Conf == OppConf, 1, 0),
             ConfLoses = ifelse(Value > p.Win & Conf == OppConf, 1, 0),
             iter = i)
    
    PreSeasonOutComes <- PreSeasonResults %>%
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
             Undefeated = ifelse(FWins == TotalGames, 1, 0),
             UndefeatedConf = ifelse(FConfLosses == 0, 1, 0)) %>%
      group_by(Conf, 
               Div) %>%
      mutate(ConfDivRank = dense_rank(desc(FConfWins))) %>%
      mutate(FirstinConfDiv = ifelse(ConfDivRank == 1, 1, 0),
             TotalFirst = sum(FirstinConfDiv),
             Undefeated = sum(Undefeated),
             UndefeatedConf = sum(UndefeatedConf),
             WinConfDivOR = case_when(ConfDivRank ==1 & TotalFirst == 1 ~ 1 ) ) %>%
      ungroup() %>%
      mutate(Win0500 = ifelse((FWins/ (FWins+ FLosses) ) >= 0.5, 1, 0),
             iter = i,
             Season = a)
    
    BadResultsPre <- PreSeasonResults %>%
      #filter((School == 'Kentucky' & Opponent == 'Central Michigan') | (School == 'Central Michigan' & Opponent == 'Kentucky'),
      #       Season == 1983) %>%
      #head(2) %>%
      select(Month,
             Day,
             Year,
             School,
             Season,
             Wk,
             Location,
             Opponent,
             Wins,
             Loses) %>%
      left_join(UniqueID,
                by = c('School' = 'School',
                       'Season' = 'Season',
                       'Wk' = 'Wk',
                       'Opponent' = 'Opponent')) %>%
      left_join(UniqueID,
                by = c('School' = 'Opponent',
                       'Season' = 'Season',
                       'Wk' = 'Wk',
                       'Opponent' = 'School')) %>%
      unite(ID, c('UniqueGameID.x', 'UniqueGameID.y'), na.rm = TRUE, sep = '') %>%
      group_by(ID) %>%
      mutate(FlagWins = sum(Wins),
             FlagLosses = sum(Loses)) %>%
      #filter(sum(Wins) > 1 | sum(Loses) > 1) %>%
      ungroup() %>%
      filter(FlagWins > 1 | FlagLosses > 1) %>%
      distinct(ID) %>%
      summarise(BadRows = n_distinct(ID)) %>%
      as.numeric()
    
    if(BadResultsPre > 0){
      
      print(paste(BadResultsPre, ' bad rows of data!'))
      
    }else{
      
    PreSeasonList[[i]] <- PreSeasonOutComes
    PreSeasonGamesOutcome[[i]] <- PreSeasonResults
    
    }
    
    
  }
  
  PreSeasonForecast <- rbindlist(PreSeasonList) %>%
    group_by(School,
             Conf,
             Div) %>%
    summarise(FMeanWins = round(mean(FWins), 1),
              FMeanLosses = round(mean(FLosses), 1),
              FMeanConfWins = round(mean(FConfWins), 1),
              FMeanConfLosses = round(mean(FConfLosses), 1),
              FUndefeated = (sum(Undefeated)),
              FirstinConfDiv = sum(FirstinConfDiv),
              Win0500 = sum(Win0500),
              WinConfDivOR = sum(WinConfDivOR, na.rm = TRUE)) %>%
    arrange(Conf,
            Div,
            desc(WinConfDivOR),
            desc(FMeanWins),
            FMeanLosses) %>%
    mutate(Season = a) %>%
    group_by(Conf,
             Div) %>%
    mutate(PctWinOR = WinConfDivOR/sum(WinConfDivOR, na.rm = TRUE),
           WinShare = FirstinConfDiv/sum(FirstinConfDiv, na.rm = TRUE) ) %>%
    ungroup() %>%
    select(Season,
           School,
           Conf,
           Div,
           Win0500,
           FMeanWins,
           FMeanLosses,
           FMeanConfWins,
           FMeanConfLosses,
           PctWinOR,
           WinShare) %>%
    mutate(Win0500 = Win0500/Sims)
  
  print(PreSeasonForecast)

  write.csv(PreSeasonForecast, glue::glue('{a} Pre-Season Forecast.csv'))
  
}

# Season forecast ----

SeasonForecastWeeks <- list()
WinForecastWeeks <- list()
ConfFinishForecastWeeks <- list()
FinalBowlList <- list()
for(b in SeasonVec:SeasonVec){
  
  seasonInt <- tibble(Season = 1869:2021) %>%
    mutate(Rows = row_number()) %>%
    filter(Season == b)
  
  varSeason <- b
  varSeasonL <- varSeason-1
  
  GamesFilter <- dplyr::filter(GamesNoBowls, Season == b)
  
  FBSTeams <- Conferences %>%
    filter(Season == b) %>%
    select(team.A = Var.2)
  
  SeasonConf <- Conferences %>%
    filter(Season == b) %>%
    mutate(Conf = trim(Conf)) %>%
    select(Var.2,
           Conf)
  
  InitEloRatings <- FullELODF %>%
    #filter(team.A == 'Iowa State') %>%
    arrange(Wk) %>%
    mutate(SeasonWk = as.numeric(paste0(Season, Wk))) %>%
    arrange(SeasonWk) %>%
    filter(Season <= varSeasonL) %>%
    group_by(team.A) %>%
    filter(row_number() == max(row_number())) %>%
    ungroup() %>%
    select(team.A,
           elo.A) %>%
    left_join(SeasonConf,
               by = c('team.A' = 'Var.2')) %>%
    group_by(Conf) %>%
    mutate(MeanConfELO = mean(elo.A)) %>%
    ungroup() %>%
    mutate(elo.R = case_when(elo.A >= MeanConfELO ~ elo.A-((elo.A-MeanConfELO)*0.5),
                             is.na(Conf) ~ 1500,
                             elo.A < MeanConfELO ~ elo.A + ((MeanConfELO-elo.A)*0.5) )) %>%
    select(team.A,
           -elo.A,
           elo.A = elo.R) %>%
    mutate(Row = 0) %>%
    arrange(desc(elo.A))
  
  GameSeasonOutcomes <- GamesFilter %>%
    rename(OppConf = Conf) %>%
    mutate(Season = as.integer(Season)) %>%
    inner_join(Conferences,
               by = c('Season' = 'Season',
                      'School' = 'Var.2')) %>%
    select(-X) %>%
    mutate(Conf = trim(Conf),
           OppConf = trim(ifelse(is.na(OppConf), 'Non-Major', OppConf)),
           Pts = as.numeric(Pts),
           Opp = as.numeric(Opp)) %>%
    mutate(Wins = ifelse(Pts > Opp, 1, 0),
           Loses = ifelse(Opp > Pts, 1, 0),
           Ties = ifelse(Pts == Opp, 1, 0),
           ConfWins = ifelse(Pts > Opp & Conf == OppConf, Wins, 0),
           ConfLoses = ifelse(Opp > Pts & Conf == OppConf, Loses, 0),
           ConfTies = ifelse(Pts == Opp & Conf == OppConf, Ties, 0),
           #  Wake Forest and UNC are playing a non-conf just because
           ConfWins = ifelse(((School == 'North Carolina' | School == 'Wake Forest') & Season == 2021) & ((School == 'North Carolina' | School == 'Wake Forest') & Season == 2021), 0, ConfWins ),
           ConfLoses = ifelse(((School == 'North Carolina' | School == 'Wake Forest') & Season == 2021) & ((School == 'North Carolina' | School == 'Wake Forest') & Season == 2021), 0, ConfLoses )
    ) %>%
    mutate(Wk = as.numeric(Wk)) %>%
    # ensuring independents remain independent
    mutate(OppConf = case_when(Opponent == 'Notre Dame' & Season != 2020 ~ 'Ind',
                               Opponent == 'Connecticut' & Season > 2020 ~ 'Ind',
                               TRUE ~ as.character(OppConf)))  %>%
    # AAC not doing divisions since 2020
    mutate(Div = ifelse(Season >= 2020 & Conf == 'American', NA, Div))
  
  GameSeasonOutcomes %>%
    filter(School == 'Nebraska')
  
  GameSeasonOutcomes %>%
    filter(Opponent == 'Nebraska') %>%
    mutate(Location = case_when(Location == "" ~ "@",
                                Location == "@" ~ "",
                                TRUE ~ Location)) %>%
    rename(Opponent = School,
           School = Opponent,
           Opp = Pts,
           Pts = Opp,
           Wins = Loses,
           Loses = Wins,
           ConfWins = ConfLoses,
           ConfLoses = ConfWins)
  
  
  
  
  
  # latest week
  MaxSeasnWk <- max(GameSeasonOutcomes %>% filter(!is.na(Pts)) %>% select(Wk))
  
  # forecasting wins and losses each week
  SeasonIterList <- list()
  WinForecastList <- list()
  ConfFinishForecastList <- list()
  BowlList <- list()
  for(c in 1:MaxSeasnWk ){
    
    ResultsSoFar <- GameSeasonOutcomes %>%
      filter(Wk <= c) %>%
      group_by(School,
               Conf,
               Div) %>%
      summarise(Wins = sum(Wins, na.rm = TRUE),
                Loses = sum(Loses, na.rm = TRUE),
                Ties = sum(Ties, na.rm = TRUE),
                ConfWins = sum(ConfWins, na.rm = TRUE),
                ConfLoses = sum(ConfLoses, na.rm = TRUE),
                ConfTies = sum(ConfTies, na.rm = TRUE),
                Wk = max(Wk)) %>%
      ungroup() %>%
      mutate(Ties = ifelse(Ties == 0, NA, Ties),
             ConfTies = ifelse(ConfTies == 0, NA, ConfTies))
    
    RealRecord <- ResultsSoFar %>%
      select(School,
             Conf,
             Div,
             Wins,
             Loses,
             Ties,
             ConfWins,
             ConfLoses,
             ConfTies) %>%
      unite(RealRecord, c('Wins', 'Loses', 'Ties'), sep = '-', na.rm = TRUE) %>%
      unite(RealConfRecord, c('ConfWins', 'ConfLoses', 'ConfTies'), sep = '-', na.rm = TRUE)
    
    ELOSoFar <- FullELODF %>%
      arrange(#team.A,
        Year,
        Month,
        Day) %>%
      filter(Season == varSeason) %>%
      mutate(Wk = as.numeric(Wk)) %>%
      filter(Wk <= c) %>%
      select(Wk,
             team.A,
             elo.A)
    
    UpdateTheELO <- InitEloRatings %>%
      #filter(team.A == 'Kansas State') %>%
      bind_rows(ELOSoFar) %>%
      group_by(team.A) %>%
      mutate(Row = row_number()) %>%
      filter(Row == max(Row)) %>%
      arrange(desc(elo.A)) %>%
      ungroup() %>%
      select(-Row) %>%
      select(-Wk)
    
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
      rename(elo.A = elo.A.x,
             elo.B = elo.A.y) %>%
      mutate(elo.B = ifelse(is.na(elo.B), 1500, as.numeric(elo.B)),
             elo.A.x = case_when(Location == '' ~ as.numeric(elo.A) + Adv,
                                 Location == '@' ~ as.numeric(elo.A) + DisAdv,
                                 Location == 'N' ~ as.numeric(elo.A)),
             elo.A.y = case_when(Location == '' ~ as.numeric(elo.B) + DisAdv,
                                 Location == '@' ~ as.numeric(elo.B) + Adv,
                                 Location == 'N' ~ as.numeric(elo.B)),
             m = (elo.B - elo.A)/400,
             p.Win = 1/(1+10^m) ) %>%
      select(-m)
    
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
      
       print(paste0(b, ' week ', c,' iter ', i))

      PWinForecast <- runif(nrow(NewProbability), 0, 1)
      
      # continuing the forecast
      UpdatedFutureGames <- NewProbability %>%
        mutate(Values = runif(nrow(.), 0, 1)) %>%
        #filter(School == 'Nebraska') %>%
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
               ConfWins = ifelse(((School == 'North Carolina' | School == 'Wake Forest') & Season == 2021) & ((School == 'North Carolina' | School == 'Wake Forest') & Season == 2021), 0, ConfWins ),
               ConfLoses = ifelse(((School == 'North Carolina' | School == 'Wake Forest') & Season == 2021) & ((School == 'North Carolina' | School == 'Wake Forest') & Season == 2021), 0, ConfLoses )
        ) %>%
        mutate(iter = i)  %>%
        group_by(School) %>%
        mutate(CWins = ifelse(is.na(CWins), 0, CWins),
               RollingFWins = cumsum(Wins),
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
               iter) %>%
        mutate(wk = c)
      
      GamesNeededToWin <- UpdatedFutureGames %>%
        filter(School %in% TeamGetsBowlWin$School) %>%
        #filter(School == 'Alabama') %>%
        filter(Wins == 1) %>%
        select(School,
               Opponent,
               Wk = Wk.x,
               iter) 
      
      MostLikelyBowlWin <- TeamGetsBowlWin %>%
        select(School,
               Opponent,
               Wk) %>%
        mutate(iter = i)
      
      SummariseOutcomes <- UpdatedFutureGames %>%
        group_by(School,
                 Conf,
                 Div) %>%
        summarise(Wins = sum(Wins),
                  Loses = sum(Loses),
                  ConfWins = sum(ConfWins),
                  ConfLoses = sum(ConfLoses)) %>%
        ungroup() %>%
        mutate(FinishesOut = ifelse(Loses == 0, 1, 0)) %>%
        arrange(desc(Wins))
      
      #print(SummariseOutcomes)
      
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
               Ties = ifelse(is.na(Ties), 0, Ties),
               ConfWins = ifelse(is.na(ConfWins), 0, ConfWins),
               ConfLoses = ifelse(is.na(ConfLoses), 0, ConfLoses),
               ConfTies = ifelse(is.na(ConfTies), 0, ConfTies)) %>%
        #replace(is.na(.), 0) %>%
        mutate(ForecastedWins = RWins + Wins,
               ForecastedLosses = RLoses + Loses,
               ForecastedTies = Ties,
               ForecastedConfWins = RConfWins + ConfWins,
               ForecastedConfLosses = RConfLoses + ConfLoses,
               ForecastedConfTies = ConfTies) %>%
        select(School,
               Conf,
               Div,
               FinishesOut,
               ForecastedWins,
               ForecastedLosses,
               ForecastedTies,
               ForecastedConfWins,
               ForecastedConfLosses,
               ForecastedConfTies) %>%
        mutate(Iter = c,
               TotalGames = ForecastedWins + ForecastedLosses + ForecastedTies,
               Win0500 = ifelse((ForecastedWins/TotalGames) >= 0.5, 1, 0),
               WinsOut = ifelse(ForecastedLosses == 0, 1, 0))
      
      Outlook1NO <- Outlook1 %>%
        filter(!is.na(ForecastedWins))
      
      BindOutlook <- bind_rows(Outlook1NO)  %>%
        group_by(Conf, 
                 Div) %>%
        mutate(ConfDivRank = dense_rank(desc(ForecastedConfWins))) %>%
        mutate(FirstinConfDiv = ifelse(ConfDivRank == 1, 1, 0),
               TotalFirst = sum(FirstinConfDiv),
               WinConfDivOR = case_when(ConfDivRank == 1 & TotalFirst == 1 ~ 1 )) %>%
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
        mutate(Iter = i,
               WinOutWinDiv = ifelse(FinishesOut == 1 & (WinConfDivOR == 1 | FirstinConfDiv == 1), 1, 0 )) %>%
        arrange(desc(ForecastedWins))
      
      PreviewSelect <- BindOutlook %>%
        arrange(desc(WinOutWinDiv),
                desc(ForecastedWins)) %>%
        unite(ProjRecord, c('ForecastedWins', 'ForecastedLosses'), sep = '-') %>%
        unite(ProjConfRecord, c('ForecastedConfWins', 'ForecastedConfLosses'), sep = '-') %>%
        select(School,
               Conf,
               Div,
               RealRecord ,
               ProjRecord,
               ProjConfRecord,
               WinOutWinDiv,
               FirstinConfDiv,
               WinConfDivOR )
      
      #print(PreviewSelect)
      
      ProbabilityIter[[i]] <- BindOutlook
      GameOutcomeList[[i]] <- UpdatedFutureGames
      BowlWinList[[i]] <- TeamGetsBowlWin
      GamesNeededtoBowl[[i]] <- GamesNeededToWin
    }
    
    FutureGames <- rbindlist(ProbabilityIter)
    
    Range <- FutureGames %>%
      #filter(School == 'Nebraska') %>%
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
      summarise(WinOutWinDiv = sum(WinOutWinDiv, na.rm = TRUE),
                ForecastedWins = mean(ForecastedWins),
                ForecastedLosses = mean(ForecastedLosses),
                ForecastedTies = mean(ForecastedTies),
                ForecastedConfWins = mean(ForecastedConfWins),
                ForecastedConfLosses = mean(ForecastedConfLosses),
                ForecastedConfTies = mean(ForecastedConfTies),
                FirstinConfDiv = sum(FirstinConfDiv),
                WinConfDivOR = sum(WinConfDivOR, na.rm = TRUE),
                WinsOut = sum(WinsOut, na.rm = TRUE),
                FinishesOut = sum(FinishesOut),
                Win0500 = sum(Win0500)
      ) %>%
      ungroup() %>% 
      mutate(WinsShareWinningOut = WinOutWinDiv/FinishesOut) %>%
      mutate(Week = c + 1) %>%
      left_join(Range) %>%
      group_by(Conf,
               Div) %>%
      mutate(PctWinOR = WinConfDivOR/sum(WinConfDivOR),
             PctWin = FirstinConfDiv/sum(FirstinConfDiv)) %>%
      ungroup()
    
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
    
    BowlWinDF <- rbindlist(BowlWinList) %>%
      group_by(Wk,
               School,
               Opponent,
               wk) %>%
      summarise(n = n()) %>%
      ungroup() %>%
      arrange(School,
              Wk)
    
    BowlList[[c]] <- BowlWinDF
    ConfFinishForecastList[[c]] <- PredictedConfFinish
    WinForecastList[[c]] <- PredictedWins
    SeasonIterList[[c]] <- SummariseFuture
    
  }
  
  Mark1 <- rbindlist(SeasonIterList) %>%
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
