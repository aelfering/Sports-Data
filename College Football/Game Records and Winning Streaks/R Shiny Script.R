  # College Football Season Progress Tracker
  # Script & Dashboard by Alex Elfering
  
  # Last Updated: 22 November 2020
  
  # Load packages
  list.of.packages <- c("ggplot2", 
                        "shiny", 
                        'htmltools', 
                        'shinydashboard', 
                        'shinythemes', 
                        'dplyr', 
                        'tidyr', 
                        'tidyverse', 
                        'scales', 
                        'zoo',
                        'ggrepel',
                        'rsconnect',
                        'DT',
                        'stringr',
                        'tidylog',
                        'elo')
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  
  library(ggplot2)  
  library(shiny)
  library(htmltools)
  library(shinydashboard)
  library(shinythemes) 
  library(dplyr) 
  library(tidyr)
  library(tidyverse)
  library(scales)
  library(zoo)
  library(ggrepel)
  library(rsconnect)
  library(DT)
  library(stringi)
  library(tidylog)
  library(elo)
  
#setwd("~/GitHub/Sports-Data/College Football/Game Records and Winning Streaks")
  
  cfb_games <- read.csv('Games teams CFB.csv', fileEncoding="UTF-8-BOM")
  cfb_conferences <- read.csv('cfb conf.csv', fileEncoding="UTF-8-BOM")
  
  
  #### Data cleaning  ####
  colnames(cfb_games) <- c('Season', 'Rk', 'Wk', 'Date', 'Day', 'Team', 'Team.Pts', 'Location', 'Opponent', 'Opp.Pts', 'Notes')
  
  rank_patterns <- paste('\\(', 1:25, '\\)', sep = '')
  
  cfb_games <- cfb_games %>%
    mutate(Team_Rank = gsub("[^0-9.-]", "", Team),
           Opponent_Rank = gsub("[^0-9.-]", "", Opponent),
           Team_Rank = gsub('-', '', Team_Rank),
           Opponent_Rank = gsub('-', '', Opponent_Rank)) %>%
    mutate(Team = stri_trim_both(str_remove_all(Team, paste(rank_patterns, collapse = "|"))),
           Opponent = stri_trim_both(str_remove_all(Opponent, paste(rank_patterns, collapse = "|"))))
  
  # find the fbs teams
  cfb_select <- dplyr::select(cfb_conferences, Season, Conf, Team) %>%
    mutate(Team = case_when(Team == 'UTEP' ~ 'Texas-El Paso',
                            Team == 'UAB' ~ 'Alabama-Birmingham',
                            Team == 'BYU' ~ "Brigham Young",
                            Team == 'UCF' ~ "Central Florida",
                            Team == "LSU" ~ "Louisiana State",
                            Team == "Ole Miss" ~ "Mississippi",
                            Team == "Pitt" ~ "Pittsburgh",
                            Team == "USC" ~ 'Southern California',
                            Team == 'SMU' ~ "Southern Methodist",
                            Team == 'UTSA' ~ 'Texas-San Antonio',
                            TRUE ~ Team))
  
  # expand games
  opp_select <- cfb_games %>%
    select(Season,
           Rk,
           Wk,
           Date,
           Day,
           Team = Opponent,
           Team_Rank = Opponent_Rank,
           Team.Pts = Opp.Pts,
           Location,
           Opponent = Team,
           Opponent_Rank = Team_Rank,
           Opp.Pts = Team.Pts,
           Notes)
  
  bind_all_games <- bind_rows(cfb_games, 
                              opp_select)
  
  distinct_bind <- dplyr::distinct(bind_all_games, 
                                   Season, 
                                   Wk, 
                                   Date, 
                                   Day, 
                                   Team, 
                                   Team_Rank, 
                                   Team.Pts, 
                                   Location, 
                                   Opponent, 
                                   Opponent_Rank, 
                                   Opp.Pts) %>%
    inner_join(cfb_select) %>%
    # Iowa is the only team to hold dual-conference membership, and this groups Iowa into the Big Ten/Western Conference
    mutate(Team_Conf = paste(Team, Conf, sep = '-')) %>%
    filter(Team_Conf != 'Iowa-Big 8')
  
  
  #### R Shiny App  ####
  
  max_season_name <- max(distinct_bind$Season)
  
  get_max_week <- distinct_bind %>%
    filter(!is.na(Team.Pts),
           Season == max(max_season_name)) %>%
    summarise(MAX_WK = max(Wk))
  
  max_week_name <- as.numeric(get_max_week$MAX_WK)
    
  
  # building the r shiny dashboard
  ui <- fluidPage(
    
    titlePanel("College Football Team Performance"),    
    sidebarLayout(position = "left",
                  sidebarPanel(
                    conditionalPanel(condition = "input.tabs1==1",
                                     h4("Ongoing winning streaks and streaks that ended each season.\n"),
                                     print(" "),
                                     sliderInput("range", "Select a Season:",
                                                 min = 1936, 
                                                 max = max(distinct_bind$Season),
                                                 value = max(distinct_bind$Season))),
                    
                    conditionalPanel(condition = "input.tabs1==2",
                                     h4("Average points scored and allowed and the plus-minus score for all FBS teams.\n"),
                                     print(" "),
                                     sliderInput("season", "Select a Season:",
                                                 min = 1936, 
                                                 max = max(distinct_bind$Season),
                                                 value = max(distinct_bind$Season)),
                                     selectInput('Tab2Conference',
                                                 'Select a Conference:',
                                                 sort(unique(distinct_bind$Conf)),
                                                 selected = c('Big Ten', 'ACC', 'Big 12', 'SEC', 'PAC-12', 'AAC', 'MWC', 'Ind', 'Sun Belt', 'MAC', 'CUSA'),
                                                 multiple = TRUE),
                                     selectInput("percent",
                                                 "Percent of Games Won:",
                                                 c('0-20%', '20-40%', '40-60%', '60-80%', '80-100%'),
                                                 selected = c('0-20%', '20-40%', '40-60%', '60-80%', '80-100%'),
                                                 multiple = TRUE)),
                    
                    conditionalPanel(condition = "input.tabs1==3",
                                     h4("The current standing among FBS teams. Includes average strength of win, and the last time a team held a specific record."),
                                     print(" "),
                                     sliderInput("Tab3Range", 
                                                 "Select a Season:",
                                                 min = 1936, 
                                                 max = max(distinct_bind$Season),
                                                 value = max(distinct_bind$Season)),
                                     selectInput('Tab3Conference',
                                                 'Select a Conference:',
                                                 sort(unique(cfb_conferences$Conf)),
                                                 selected = c('Big Ten', 'ACC', 'Big 12', 'SEC', 'PAC-12', 'AAC', 'MWC', 'Ind', 'Sun Belt', 'MAC', 'CUSA'),
                                                 multiple = TRUE)),
                    
                    conditionalPanel(condition = "input.tabs1==4",
                                     h4("The median winning and losing margin by season among FBS teams."),
                                     print(" "),
                                     selectInput('Tab4Team',
                                                 'Select a Team:',
                                                 sort(unique(distinct_bind$Team)),
                                                 selected = 'Iowa'),
                                     sliderInput("Tab4Range", 
                                                 "Select a Season:",
                                                 min = 1936, 
                                                 max = max(distinct_bind$Season),
                                                 value = c(2000,2020)),
                                     sliderInput("Tab4Ranks", 
                                                 "Annotate the Graph! Highlight the Best and Worst Games:",
                                                 min = 1, 
                                                 max = 5,
                                                 value = 3)),
                    
                    width = 2),
                  mainPanel(
                    print(paste('Code & Design by Alex Elfering | Data Source: College Football Reference | ', max_season_name, ' Season through Week ', max_week_name, sep = '' )),
                    tabsetPanel(id="tabs1",
                                tabPanel('Current Team Season Records',
                                         value = 3,
                                         br(),
                                         DT::dataTableOutput('first')),
                                tabPanel("Series Winning Streaks", 
                                         value = 1,
                                         br(),
                                         print('Active Winning Streaks in the Last 5 Seasons'),
                                         DT::dataTableOutput('active'),
                                         br(),
                                         br(),
                                         br(),
                                         print('Winning Streaks that Ended This Season'),
                                         DT::dataTableOutput('streaks')),
                                tabPanel("Season Point Differential",  
                                         value = 2,
                                         br(),
                                         plotOutput("Plot", width = "100%"), 
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         DT::dataTableOutput('scoring')),
                                tabPanel("Team Median Winning and Losing Margins",  
                                         value = 4,
                                         br(),
                                         plotOutput("TeamTimeline", width = "100%"),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         DT::dataTableOutput('TeamTimelineTable'))
                    )
                  )
    )) 
  
  server <- function(input, output, session){
    # Teams holding a record for the first time in team history
    output$first <- DT::renderDataTable({
      
      fbs_teams <- distinct_bind %>%
        filter(Season == input$Tab3Range) %>%
        distinct(Conf, 
                 Team)
      
      # calculate the running games won and lost by team per season
      running_games <- distinct_bind %>%
        # played teams
        filter(!is.na(Team.Pts)) %>%
        arrange(Season,
                Wk) %>%
        mutate(Wins = ifelse(Team.Pts > Opp.Pts, 1, 0),
               Ties = ifelse(Team.Pts == Opp.Pts, 1, 0),
               Loses = ifelse(Team.Pts < Opp.Pts, 1, 0)) %>%
        # cumulative sum of games won and lost
        group_by(Season,
                 Team) %>%
        mutate(Rolling.Wins = cumsum(Wins),
               Rolling.Losses = cumsum(Loses),
               Rolling.Ties = cumsum(Ties),
               Rolling.Total.Games = Rolling.Wins + Rolling.Losses + Rolling.Ties) %>%
        ungroup() %>%
        group_by(Team) %>%
        # cauclating winning and losing streaks ongoing 
        mutate(Win.Streak = ave(Wins, cumsum(Wins==0), FUN = seq_along) - 1,
               Lose.Streak = ave(Loses, cumsum(Loses==0), FUN = seq_along) - 1,
               Streak.Status = ifelse(Win.Streak > 0, 
                                      paste('W', Win.Streak, sep = ''), 
                                      paste('L', Lose.Streak, sep = ''))) %>%
        ungroup()
      
      # return the latest records of each team in the latest season 
      latest_season_records <- running_games %>%
        filter(Season == input$Tab3Range) %>%
        mutate(Result = ifelse(Team.Pts > Opp.Pts, 'W', 'L'),
               Latest_Game = paste(Result, ' ', Team.Pts, '-', Opp.Pts, ' vs ', Opponent, sep = '')) %>%
        group_by(Team) %>%
        slice(which.max(Wk)) %>%
        select(Team, 
               Season,
               Streak.Status,
               Latest_Game,
               Rolling.Wins,
               Rolling.Losses,
               Rolling.Ties) %>%
        arrange(Rolling.Losses,
                desc(Rolling.Wins),
                Rolling.Ties) %>%
        unite(Record, c('Rolling.Wins', 'Rolling.Losses', 'Rolling.Ties'), sep = '-', na.rm = TRUE)
      
      current_team_wins_losses <- running_games %>%
        filter(Season == input$Tab3Range) %>%
        mutate(Result = ifelse(Team.Pts > Opp.Pts, 'W', 'L'),
               Latest_Game = paste(Result, ' ', Team.Pts, '-', Opp.Pts, ' vs ', Opponent, sep = '')) %>%
        group_by(Team) %>%
        slice(which.max(Wk)) %>%
        select(Team, 
               Season,
               Rolling.Wins,
               Rolling.Losses,
               Rolling.Ties,
               Rolling.Total.Games)
      
      win_strength <- running_games %>%
        filter(Season == input$Tab3Range) %>%
        filter(Team.Pts > Opp.Pts) %>%
        select(Season,
               Team,
               Opponent) %>%
        inner_join(current_team_wins_losses,
                   by = c('Opponent' = 'Team', 'Season' = 'Season')) %>%
        group_by(Season,
                 Team) %>%
        summarise(Opponent_Wins = round(sum(Rolling.Wins)),
                  Opponent_Losses = round(sum(Rolling.Losses)),
                  Opponent_Ties = round(sum(Rolling.Ties))) %>%
        ungroup() %>%
        unite(Opponent_FBS_Record, c('Opponent_Wins', 'Opponent_Losses', 'Opponent_Ties'), sep = '-', na.rm = TRUE)
      
      # what is each team's next game?
      next_game <- distinct_bind %>%
        filter(is.na(Team.Pts),
               Season == input$Tab3Range) %>%
        group_by(Team) %>%
        slice(which.min(Wk)) %>%
        ungroup() %>%
        left_join(latest_season_records, by = c('Season' = 'Season', 'Opponent' = 'Team')) %>%
        mutate(Record = ifelse(!Opponent %in% fbs_teams$Team, '**', Record),
               Record = ifelse(is.na(Record), ' (0-0)', paste(' (', Record, ')', sep = '')),
               Record = ifelse(grepl('\\*', Record), gsub("[()]", "", Record),  Record)) %>%
        mutate(Next_Game = paste('vs ', Opponent, Record, sep = '')) %>%
        select(Team, 
               Next_Game)
      
      # what is every fbs team's record of all time?
      all_records <- running_games %>%
        filter(Season < input$Tab3Range) %>%
        select(Season,
               Team, 
               Rolling.Wins,
               Rolling.Losses,
               Rolling.Ties) %>%
        arrange(Rolling.Losses) %>%
        unite(Record, c('Rolling.Wins', 'Rolling.Losses', 'Rolling.Ties'), sep = '-', na.rm = TRUE) %>%
        group_by(Team,
                 Record) %>%
        summarise(Last_Season = max(Season),
                  Seasons = n_distinct(Season))
      
      # when was the last time that a team had a specific record?
      # if a team is 4-0 for the first time since 1960, they will show up here
      team_performance <- latest_season_records %>%
        left_join(all_records) %>%
        left_join(next_game, by = c('Team' = 'Team')) %>%
        left_join(win_strength) %>%
        inner_join(fbs_teams) %>%
        filter(Team %in% unique(fbs_teams$Team),
               Conf %in% input$Tab3Conference) %>%
        mutate(Next_Game = ifelse(is.na(Next_Game), '', Next_Game),
               Team = ifelse(is.na(Last_Season), paste(Team, '*', sep = ''), Team)) %>%
        select(Season,
               Team,
               Conf,
               Record,
               Opponent_FBS_Record,
               Last_Season,
               Streak.Status,
               Latest_Game,
               Next_Game)
      
      datatable(team_performance,
                extensions = 'Buttons', 
                colnames = c('Season',
                             'Team',
                             'Conference',
                             'Record',
                             'Strength of FBS Win',
                             'Last Season',
                             'Streak',
                             'Last Game',
                             'Next Game'),
                fillContainer = TRUE,
                caption = htmltools::tags$caption(
                  style = 'caption-side: bottom; text-align: center;',
                  htmltools::em('* Indicates Team Holding a Historic Record | ** Indicates an FCS Team')),
                options = list(paging = FALSE,
                               dom = 'Bfrtip',
                               scroller = TRUE,
                               scrollY = "500px",
                               scrollX = TRUE,
                               fixedColumns = TRUE,
                               buttons = c('copy', 'csv', 'excel')))
      
      
      
    })
    
    # Table of active streaks
    output$active <- DT::renderDataTable({
      
      end_season <- input$range-5
      
      game_streaks <- distinct_bind %>%
        # played teams
        filter(!is.na(Team.Pts)) %>%
        arrange(Season,
                Wk) %>%
        mutate(Wins = ifelse(Team.Pts > Opp.Pts, 1, 0),
               Ties = ifelse(Team.Pts == Opp.Pts, 1, 0),
               Loses = ifelse(Team.Pts < Opp.Pts, 1, 0)) %>%
        group_by(Team,
                 Opponent) %>%
        # cauclating winning and losing streaks ongoing 
        mutate(Win.Streak = ave(Wins, cumsum(Wins==0), FUN = seq_along)-1,
               Lose.Streak = ave(Loses, cumsum(Loses==0), FUN = seq_along)-1) %>%
        ungroup()
      
      game_series <- game_streaks %>%
        group_by(Team, 
                 Opponent) %>%
        mutate(Total_Wins = cumsum(Wins),
               Total_Losses = cumsum(Loses),
               Total_Ties = cumsum(Ties)) %>%
        ungroup() %>%
        unite(Record, c('Total_Wins', 'Total_Losses', 'Total_Ties'), sep = '-', na.rm = TRUE) %>%
        select(Season,
               Team,
               Opponent,
               Wk,
               Record)
      
      active_streaks <- game_streaks %>%
        filter(Season <= input$range) %>%
        group_by(Team,
                 Opponent) %>%
        filter(Season == max(Season),
               Wins > 0,
               Win.Streak > 1,
               Season >= end_season) %>%
        arrange(desc(Win.Streak)) %>%
        left_join(game_series) %>%
        select(Season,
               Wk,
               Team,
               Opponent,
               Win.Streak,
               Record) 
      
      filter_streaks <- active_streaks %>%
        group_by(Team,
                 Opponent) %>%
        filter(Season == max(Season),
               Wk == max(Wk)) %>%
        select(-Wk)
      
      datatable(filter_streaks,
                colnames = c('Last Met',
                             'Team',
                             'Opponent',
                             'Winning Streak',
                             'Record'),
                fillContainer = TRUE,
                options = list(paging = FALSE,
                               dom = 'Bfrtip',
                               scroller = TRUE,
                               scrollY = "500px",
                               buttons = c('copy', 'csv', 'excel')))
      
    })
    
    # Table of winning streaks
    output$streaks <- DT::renderDataTable({
      
      game_streaks <- distinct_bind %>%
        # played teams
        filter(!is.na(Team.Pts)) %>%
        arrange(Season,
                Wk) %>%
        mutate(Wins = ifelse(Team.Pts > Opp.Pts, 1, 0),
               Ties = ifelse(Team.Pts == Opp.Pts, 1, 0),
               Loses = ifelse(Team.Pts < Opp.Pts, 1, 0)) %>%
        group_by(Team,
                 Opponent) %>%
        # cauclating winning and losing streaks ongoing 
        mutate(Win.Streak = ave(Wins, cumsum(Wins==0), FUN = seq_along)-1,
               Lose.Streak = ave(Loses, cumsum(Loses==0), FUN = seq_along)-1) %>%
        ungroup()
      
      flag_streak_broken <- game_streaks %>%
        group_by(Team,
                 Opponent) %>%
        mutate(Streak.Broken = lag(Lose.Streak),
               Series.Number = row_number(),
               Last.Win = (Series.Number-Streak.Broken)-1,
               Major.Streak.Broken = ifelse(Streak.Broken > 2 & Win.Streak > 0, 1, 0)) %>%
        ungroup() 
      
      series_numbers <- flag_streak_broken %>%
        distinct(Team,
                 Opponent,
                 Series.Number,
                 Season)
      
      current_streaks_broken <- flag_streak_broken %>%
        filter(Season == input$range,
               Major.Streak.Broken == 1) %>%
        select(Season,
               Team,
               Opponent,
               Team.Pts,
               Opp.Pts,
               Streak.Broken,
               Last.Win) %>%
        inner_join(series_numbers, by = c('Last.Win' = 'Series.Number', 'Team' = 'Team', 'Opponent' = 'Opponent')) %>%
        unite(Final_Score, c('Team.Pts', 'Opp.Pts'), sep = '-') %>%
        select(Season = Season.x,
               Team,
               Opponent,
               Final_Score,
               Streak.Broken,
               Last_Win = Season.y) %>%
        arrange(desc(Streak.Broken))
      
      mark1 <- distinct_bind %>%
        filter(!is.na(Team.Pts)) %>%
        mutate(Wins = ifelse(Team.Pts > Opp.Pts, 1, 0),
               Loses = ifelse(Team.Pts < Opp.Pts, 1, 0),
               Ties = ifelse(Team.Pts == Opp.Pts, 1, 0)) %>%
        arrange(Season,
                Wk) %>%
        group_by(Team, 
                 Opponent) %>%
        mutate(Total.Wins = cumsum(Wins),
               Total.Losses = cumsum(Loses),
               Total.Ties = cumsum(Ties),
               First.Win = ifelse(Total.Wins == 1, 'First Win Ever', NA)) %>%
        ungroup() %>%
        select(Season,
               Team,
               Opponent,
               Total.Wins,
               Total.Losses,
               Total.Ties,
               First.Win) %>%
        unite(Record, c('Total.Wins', 'Total.Losses', 'Total.Ties'), sep = '-', na.rm = TRUE)
      
      join_streaks <- current_streaks_broken %>%
        inner_join(mark1) %>%
        mutate(Last_Win = ifelse(!is.na(First.Win), First.Win, Last_Win)) %>%
        select(Season,
               Team,
               Opponent,
               Final_Score,
               Streak.Broken,
               Last_Win,
               Record)
      
      datatable(join_streaks,
                extensions = 'Buttons', 
                colnames = c('Season',
                             'Team',
                             'Opponent',
                             'Final Score',
                             'Streak Broken',
                             'Last Season Won',
                             'Current Series'),
                fillContainer = TRUE,
                options = list(paging = FALSE,
                               dom = 'Bfrtip',
                               scrollY = "500px",
                               buttons = c('copy', 'csv', 'excel')),
                caption = htmltools::tags$caption(
                  style = 'caption-side: bottom; text-align: center;',
                  htmltools::em('Winning Streaks have at least three consecutive wins')))
      
    })
    
    # Visualization of team point differential
    output$TeamPlusMinus <- renderPlot({
      
      incomplete_seasons <- distinct_bind %>%
        filter(Team == input$Tab4Team,
               is.na(Team.Pts)) %>%
        distinct(Season)
      
      # highlight variables
      
      season_var <- input$Tab4Range
      years_back <- 10
      
      end_season_var <- season_var-years_back
      
      begin_season <- season_var + 0.5
      end_season <- end_season_var-0.5
      
      plus_minus <- distinct_bind %>%
        filter(Team == input$Tab4Team,
               !is.na(Team.Pts)) %>%
        arrange(Season,
                Wk) %>%
        mutate(Margin = Team.Pts-Opp.Pts) %>%
        group_by(Season) %>%
        summarise(Team.Pts = sum(Team.Pts),
                  Opp.Pts = sum(Opp.Pts),
                  Plus_Minus = sum(Margin),
                  Games = n_distinct(Date)) %>%
        ungroup() %>%
        mutate(TBA_Season = ifelse(Season == incomplete_seasons$Season & Plus_Minus < 0, 'Negative TBA', NA),
               TBA_Season = ifelse(Season == incomplete_seasons$Season & Plus_Minus > 0, 'Positive TBA', TBA_Season),
               TBA_Season = ifelse(Season != incomplete_seasons$Season & Plus_Minus > 0, 'Positive', TBA_Season),
               TBA_Season = ifelse(Season != incomplete_seasons$Season & Plus_Minus < 0, 'Negative', TBA_Season),
               Plus_Minus_Avg = Plus_Minus/Games)
      
      
      ggplot(plus_minus,
             aes(x = Season,
                 y = Plus_Minus)) +
        geom_rect(aes(xmin=begin_season, 
                      xmax=end_season, 
                      ymin=-Inf, 
                      ymax=Inf),
                  alpha = 0.05,
                  fill = '#d9d9d9') +
        geom_bar(stat = 'identity',
                 position = 'identity',
                 aes(fill = TBA_Season,
                     color = TBA_Season)) +
        geom_text(data = subset(plus_minus, TBA_Season %in% c('Negative TBA', 'Positive TBA')),
                  mapping = aes(x = Season,
                                y = Plus_Minus),
                  label = 'Season\nOngoing',
                  hjust = -0.15) +
        scale_fill_manual(values = c('Negative TBA' = '#fdd49e', 
                                     'Positive TBA' = '#a6bddb',
                                     'Positive' = '#3690c0',
                                     'Negative' = '#ef6548')) +
        scale_color_manual(values = c('Negative TBA' = 'black', 
                                      'Positive TBA' = 'black',
                                      'Positive' = NA,
                                      'Negative' = NA)) +
        geom_hline(yintercept = 0) +
        labs(title = paste(team_variable, ' Football Plus-Minus Scores by Season', sep = ''),
             y = 'Plus-Minus Average',
             x = 'Season') +
        theme(plot.title = element_text(face = 'bold', size = 18, family = 'Arial'),
              legend.position = 'none',
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
      
    }, height = 600)
    
    # Visualization of team scatter plot
    output$TeamScatter <- renderPlot({
      
      incomplete_seasons <- distinct_bind %>%
        filter(Team == input$Tab4Team,
               is.na(Team.Pts)) %>%
        distinct(Season)
      
      # highlight variables
      
      season_var <- input$Tab4Range
      years_back <- 10
      
      end_season_var <- season_var-years_back
      
      begin_season <- season_var + 0.5
      end_season <- end_season_var-0.5
      
      distinct_bind %>%
        filter(Team == input$Tab4Team,
               !is.na(Team.Pts)) %>%
        arrange(Season,
                Wk) %>%
        mutate(Margin = Team.Pts-Opp.Pts) %>%
        group_by(Season) %>%
        summarise(Team.Pts = sum(Team.Pts),
                  Opp.Pts = sum(Opp.Pts),
                  Margin = sum(Margin),
                  Games = n_distinct(Date)) %>%
        ungroup() %>%
        mutate(Points_Game = Team.Pts/Games,
               Points_Allowed = Opp.Pts/Games) %>%
        filter(Season >= end_season,
               Season <= begin_season) %>%
        ggplot(aes(x = Points_Game,
                   y = Points_Allowed)) +
        geom_point() +
        geom_segment(aes(
          xend=c(tail(Points_Game, n=-1), NA), 
          yend=c(tail(Points_Allowed, n=-1), NA))) +
        geom_label_repel(aes(label = Season)) +
        labs(title = 'Average Points Allowed and Scored Over Time') +
        theme(plot.title = element_text(face = 'bold', size = 18, family = 'Arial'),
              legend.position = 'none',
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
      
    }, height = 600)
    
    # Visualization of point differential
    output$Plot <- renderPlot({
      
      # return fbs teams that played that season
      fbs_teams <- distinct_bind %>%
        filter(Season == input$season) %>%
        distinct(Conf, 
                 Team)
      
      # calculate points scored and allowed, the average points scored and allowed, and the point differential
      mark1 <- distinct_bind %>%
        group_by(Season, 
                 Team,
                 Conf) %>%
        filter(!is.na(Team.Pts),
               Season == input$season,
               Team %in% fbs_teams$Team) %>%
        mutate(Wins = ifelse(Team.Pts > Opp.Pts, 1, 0),
               Loses = ifelse(Team.Pts < Opp.Pts, 1, 0),
               Ties = ifelse(Team.Pts == Opp.Pts, 1, 0)) %>%
        summarise(Total_Games = n_distinct(Date),
                  Points_For = sum(Team.Pts),
                  Points_Against = sum(Opp.Pts),
                  Total_Wins = sum(Wins),
                  Total_Losses = sum(Loses),
                  Total_Ties = sum(Ties)) %>%
        ungroup() %>%
        mutate(Points_Per_Game = round(Points_For/Total_Games, 1),
               Points_Allowed_Per_Game = round(Points_Against/Total_Games, 1),
               Pct_Won = Total_Wins/(Total_Wins+Total_Losses+Total_Ties),
               Diff = round(Points_Per_Game-Points_Allowed_Per_Game, 1),
               Avg_PG = mean(Points_Per_Game),
               Avg_PGA = mean(Points_Allowed_Per_Game),
               Team = ifelse(Total_Games == 1, paste(Team, '*', sep = ''), Team),
               Pct_Group = cut(Pct_Won, c(0, 0.2, 0.4, 0.6, 0.8, 1), labels = c('0-20%', '20-40%', '40-60%', '60-80%', '80-100%'), include.lowest=TRUE)) %>%
        filter(Pct_Group %in% input$percent,
               Conf %in% input$Tab2Conference) %>%
        mutate(Diff_Rank = dense_rank(desc(Diff)),
               Reverse_Rank = dense_rank(Diff))
      
      # the visualization
      ggplot(mark1, 
             aes(x = Points_Per_Game, 
                 y = Points_Allowed_Per_Game,
                 group = Team, 
                 color = Pct_Group)) + 
        # season averages
        geom_vline(aes(xintercept = Avg_PG),
                   linetype = 'dashed',
                   alpha = 0.6,
                   size = 1) +
        geom_hline(aes(yintercept = Avg_PGA),
                   linetype = 'dashed',
                   alpha = 0.6,
                   size = 1) +
        # scatterplot points
        geom_point(size = 5,
                   alpha = 0.4) +
        geom_point(data = subset(mark1, Diff_Rank <= 5),
                   shape = 1,
                   size = 5,
                   colour = "black") +
        geom_point(data = subset(mark1, Reverse_Rank <= 5),
                   shape = 1,
                   size = 5,
                   colour = "black") +
        scale_color_manual(values = c('0-20%' = '#e41a1c', 
                                      '20-40%' = '#377eb8', 
                                      '40-60%' = '#4daf4a', 
                                      '60-80%' = '#984ea3', 
                                      '80-100%' = '#ff7f00')) +
        geom_label_repel(data = subset(mark1, Diff_Rank <= 5),
                         mapping = aes(x = Points_Per_Game, 
                                       y = Points_Allowed_Per_Game,
                                       label = Team),
                         box.padding = 1,
                         color = 'black') +
        geom_label_repel(data = subset(mark1, Reverse_Rank <= 5),
                         mapping = aes(x = Points_Per_Game, 
                                       y = Points_Allowed_Per_Game,
                                       label = Team),
                         box.padding = 1,
                         color = 'black') +
        labs(title = paste(input$season, ' Season: Which CFB Teams Had the Best and Worst Point Differential?', sep = ''),
             subtitle = 'Teams by Average Points Scored and Points Allowed, and Percent of Games Won',
             color = 'Percent of Games Won',
             x = 'Points Scored per Game',
             y = 'Points Allowed per Game',
             caption = '*Team has only played one game') +
        theme(plot.title = element_text(face = 'bold', size = 20, family = 'Arial'),
              legend.position = 'top',
              legend.background=element_blank(),
              legend.key=element_blank(),
              legend.text = element_text(size = 14, family = 'Arial'),
              legend.title = element_text(size = 14, family = 'Arial'),
              plot.subtitle = element_text(size = 15, family = 'Arial'),
              plot.caption = element_text(size = 14, family = 'Arial'),
              axis.title = element_text(size = 14, family = 'Arial'),
              axis.text = element_text(size = 14, family = 'Arial'),
              strip.text = ggplot2::element_text(size = 14, hjust = 0, face = 'bold', color = 'black', family = 'Arial'),
              strip.background = element_rect(fill = NA),
              panel.background = ggplot2::element_blank(),
              axis.line = element_line(colour = "#222222", linetype = "solid"),
              panel.grid.major.y = element_line(colour = "#c1c1c1", linetype = "dashed"),
              panel.grid.major.x = element_line(colour = "#c1c1c1", linetype = "dashed")) 
      
    }, height = 600)
    
    # Table for point differential
    output$scoring <- DT::renderDataTable({
      
      fbs_teams <- distinct_bind %>%
        filter(Season == input$season) %>%
        distinct(Conf, 
                 Team)
      
      mark1 <- distinct_bind %>%
        group_by(Season, 
                 Team,
                 Conf) %>%
        filter(!is.na(Team.Pts),
               Season == input$season,
               Team %in% fbs_teams$Team) %>%
        mutate(Wins = ifelse(Team.Pts > Opp.Pts, 1, 0),
               Loses = ifelse(Team.Pts < Opp.Pts, 1, 0),
               Ties = ifelse(Team.Pts == Opp.Pts, 1, 0)) %>%
        summarise(Total_Games = n_distinct(Date),
                  Points_For = sum(Team.Pts),
                  Points_Against = sum(Opp.Pts),
                  Total_Wins = sum(Wins),
                  Total_Losses = sum(Loses),
                  Total_Ties = sum(Ties)) %>%
        ungroup() %>%
        mutate(Points_Per_Game = round(Points_For/Total_Games, 1),
               Points_Allowed_Per_Game = round(Points_Against/Total_Games, 1),
               Pct_Won = Total_Wins/(Total_Wins+Total_Losses+Total_Ties),
               Total_Ties = ifelse(Total_Ties == 0, NA, Total_Ties),
               Diff = round(Points_Per_Game-Points_Allowed_Per_Game, 1),
               Avg_PG = mean(Points_Per_Game),
               Avg_PGA = mean(Points_Allowed_Per_Game),
               Team = ifelse(Total_Games == 1, paste(Team, '*', sep = ''), Team),
               Pct_Group = cut(Pct_Won, c(0, 0.2, 0.4, 0.6, 0.8, 1), labels = c('0-20%', '20-40%', '40-60%', '60-80%', '80-100%'), include.lowest=TRUE)) %>%
        unite(Record, c('Total_Wins', 'Total_Losses', 'Total_Ties'), sep = '-', na.rm = TRUE) %>%
        filter(Pct_Group %in% input$percent,
               Conf %in% input$Tab2Conference) %>%
        select(Season,
               Team,
               Conf,
               Total_Games,
               Pct_Group,
               Record,
               Points_For,
               Points_Against,
               Points_Per_Game,
               Points_Allowed_Per_Game,
               Diff) %>%
        arrange(desc(Diff))
      
      datatable(mark1,
                extensions = 'Buttons',
                colnames = c('Season',
                             'Team',
                             'Conference',
                             'Total Games',
                             'Percent Won',
                             'Record',
                             'Points For',
                             'Points Against',
                             'Points per Game',
                             'Points Allowed Per Game',
                             'Point Differential'),
                options = list(paging = FALSE,
                               dom = 'Bfrtip',
                               buttons = c('copy', 'csv', 'excel')))
      
      
      
    })
    
    # Visualization for Team Output
    output$TeamTimeline <- renderPlot({
      margins <- distinct_bind %>%
        filter(Team == input$Tab4Team,
               !is.na(Team.Pts),
               Season >= 1936) %>%
        arrange(Season,
                Wk) %>%
        mutate(Margin = Team.Pts-Opp.Pts,
               Winning_Margin = ifelse(Margin > 0, Margin, NA),
               Losing_Margin = ifelse(Margin < 0, Margin, NA),
               Wins = ifelse(Team.Pts > Opp.Pts, 1, 0),
               Loses = ifelse(Team.Pts < Opp.Pts, 1, 0),
               Ties = ifelse(Team.Pts == Opp.Pts, 1, 0))
      
      record_summary <- margins %>%
        filter(Season >= min(input$Tab4Range),
               Season <= max(input$Tab4Range)) %>%
        summarise(Wins = sum(Wins),
                  Losses = sum(Loses),
                  Ties = sum(Ties),
                  min_Season = min(Season),
                  max_Season = max(Season)) %>%
        mutate(Ties = ifelse(Ties == 0, NA, Ties)) %>%
        unite(Overall_Record, c('Wins', 'Losses', 'Ties'), sep = '-', na.rm = TRUE)
      
      winning_margins <- dplyr::filter(margins, !is.na(Winning_Margin))
      losing_margins <- dplyr::filter(margins, is.na(Winning_Margin))
      
      winning_summary <- winning_margins %>%
        filter(!is.na(Winning_Margin)) %>%
        group_by(Season) %>%
        summarise(Winning_Margin = median(Winning_Margin)) %>%
        ungroup()
      
      losing_summary <- losing_margins %>%
        filter(!is.na(Losing_Margin)) %>%
        group_by(Season) %>%
        summarise(Losing_Margin = median(Losing_Margin)) %>%
        ungroup()
      
      
      best_margin <- margins %>%
        filter(Season >= min(input$Tab4Range),
               Season <= max(input$Tab4Range)) %>%
        filter(dense_rank(desc(Margin)) <= input$Tab4Ranks)
      
      worst_margin <- margins %>%
        filter(Season >= min(input$Tab4Range),
               Season <= max(input$Tab4Range)) %>%
        filter(dense_rank((Margin)) <= input$Tab4Ranks)
      
      
      ggplot(winning_summary,
             aes(x = Season,
                 y = Winning_Margin)) +
        geom_bar(stat = 'identity',
                 position = 'identity',
                 alpha = 0.2,
                 fill = 'steelblue') +
        geom_bar(data = losing_summary,
                 mapping = aes(x = Season,
                               y = Losing_Margin),
                 stat = 'identity',
                 position = 'identity',
                 alpha = 0.2,
                 fill = 'Orange') +
        geom_point(data = margins,
                   mapping = aes(x = Season,
                                 y = Margin,
                                 color = Margin > 0),
                   alpha = 0.4) +
        scale_color_manual(values = c('orange', 'steelblue')) +
        geom_bar(data = subset(winning_summary, Season >= min(input$Tab4Range) & Season <= max(input$Tab4Range)),
                 mapping = aes(x = Season,
                               y = Winning_Margin),
                 alpha = 0.5,
                 fill = 'steelblue',
                 stat = 'identity',
                 position = 'identity') +
        geom_bar(data = subset(losing_summary, Season >= min(input$Tab4Range) & Season <= max(input$Tab4Range)),
                 mapping = aes(x = Season,
                               y = Losing_Margin),
                 alpha = 0.5,
                 fill = 'orange',
                 stat = 'identity',
                 position = 'identity') +
        geom_point(data = subset(margins, Season >= min(input$Tab4Range) & Season <= max(input$Tab4Range)),
                   mapping = aes(x = Season,
                                 y = Margin,
                                 color = Margin > 0),
                   size = 3) +
        geom_label_repel(data = best_margin,
                         mapping = aes(x = Season,
                                       y = Margin,
                                       label = paste(Season, Opponent, sep = '-')),
                         arrow = arrow(length = unit(0.02, "npc")),
                         box.padding = 1,
                         size = 5,
                         color = 'black') +
        geom_label_repel(data = worst_margin,
                         mapping = aes(x = Season,
                                       y = Margin,
                                       label = paste(Season, Opponent, sep = '-')),
                         arrow = arrow(length = unit(0.02, "npc")),
                         box.padding = 1,
                         size = 5,
                         color = 'black') +
        geom_hline(yintercept = 0,
                   size = 1) +
        geom_label_repel(data = subset(losing_summary, Season == min(Season)),
                         mapping = aes(x = Season,
                                       y = Losing_Margin),
                         box.padding = 2,
                         size = 5,
                         arrow = arrow(length = unit(0.02, "npc")),
                         color = 'orange',
                         label = 'Median Losing Margin') +
        geom_label_repel(data = subset(winning_summary, Season == min(Season)),
                         mapping = aes(x = Season,
                                       y = Winning_Margin),
                         box.padding = 2,
                         size = 5,
                         arrow = arrow(length = unit(0.02, "npc")),
                         color = 'steelblue',
                         label = 'Median Winning Margin') +
        labs(title = paste('Median Winning and Losing Margins between ', min(input$Tab4Range), ' and ', max(input$Tab4Range), sep= ''),
             subtitle = paste(input$Tab4Team, ' finished with a record of ', record_summary$Overall_Record, sep = ''),
             x = '',
             y = '') +
        theme(plot.title = element_text(face = 'bold', size = 18, family = 'Arial'),
              legend.position = 'none',
              legend.background=element_blank(),
              legend.key=element_blank(),
              legend.text = element_text(size = 14, family = 'Arial'),
              legend.title = element_text(size = 14, family = 'Arial'),
              plot.subtitle = element_text(size = 15, family = 'Arial'),
              plot.caption = element_text(size = 14, family = 'Arial'),
              axis.title = element_text(size = 14, family = 'Arial'),
              axis.text = element_text(size = 14, family = 'Arial'),
              strip.text = ggplot2::element_text(size = 14, hjust = 0, face = 'bold', color = 'black', family = 'Arial'),
              strip.background = element_rect(fill = NA),
              panel.background = ggplot2::element_blank(),
              axis.line = element_line(colour = "#222222", linetype = "solid"),
              panel.grid.major.x = element_line(colour = "#c1c1c1", linetype = "dashed"),
              panel.grid.major.y = element_blank()) 
      
    }, height = 600)
    
    # Table for team timeline
    output$TeamTimelineTable <- DT::renderDataTable({
      
      margins <- distinct_bind %>%
        filter(Season >= min(input$Tab4Range),
               Season <= max(input$Tab4Range),
               Team == input$Tab4Team,
               !is.na(Team.Pts)) %>%
        arrange(Season,
                Wk) %>%
        mutate(Margin = Team.Pts-Opp.Pts,
               Winning_Margin = ifelse(Margin > 0, Margin, NA),
               Losing_Margin = ifelse(Margin < 0, Margin, NA),
               Wins = ifelse(Team.Pts > Opp.Pts, 1, 0),
               Loses = ifelse(Team.Pts < Opp.Pts, 1, 0),
               Ties = ifelse(Team.Pts == Opp.Pts, 1, 0))
      
      season_record <- margins %>%
        select(Season,
               Wins,
               Loses,
               Ties) %>%
        group_by(Season) %>%
        summarise_if(is.numeric, sum) %>%
        ungroup() %>%
        mutate(Ties = ifelse(Ties == 0, NA, Ties)) %>%
        unite(Record, c('Wins', 'Loses', 'Ties'), sep = '-', na.rm = TRUE)
      
      winning_margins <- margins %>%
        filter(!is.na(Winning_Margin)) %>%
        select(Season,
               Winning_Margin) %>%
        group_by(Season) %>%
        summarise(Median_Margin = median(Winning_Margin),
                  Largest_Winning_Margin = max(Winning_Margin)) %>%
        ungroup()
      
      losing_margns <- margins %>%
        filter(!is.na(Losing_Margin)) %>%
        select(Season,
               Losing_Margin) %>%
        group_by(Season) %>%
        summarise(Median_Margin = median(Losing_Margin),
                  Largest_Losing_Margin = min(Losing_Margin)) %>%
        ungroup()
      
      best_games <- margins %>%
        filter(Margin > 0) %>%
        group_by(Season) %>%
        mutate(Margin_Rank = dense_rank(desc(Margin))) %>%
        ungroup() %>%
        filter(Margin_Rank == 1) %>%
        mutate(Biggest_Wins = paste(Opponent, ' (', Team.Pts, '-', Opp.Pts, ')', sep = '')) %>%
        select(Season,
               Biggest_Wins) %>%
        group_by(Season) %>%
        mutate(Rows = row_number()) %>%
        spread(Rows, Biggest_Wins) %>%
        unite(Biggest_Wins, -c('Season'), sep = ', ', na.rm = TRUE)
      
      worst_games <- margins %>%
        filter(Margin < 0) %>%
        group_by(Season) %>%
        mutate(Margin_Rank = dense_rank((Margin))) %>%
        ungroup() %>%
        filter(Margin_Rank == 1)  %>%
        mutate(Biggest_Losses = paste(Opponent, ' (', Team.Pts, '-', Opp.Pts, ')', sep = '')) %>%
        select(Season,
               Biggest_Losses)  %>%
        group_by(Season) %>%
        mutate(Rows = row_number()) %>%
        spread(Rows, Biggest_Losses) %>%
        unite(Biggest_Losses, -c('Season'), sep = ', ', na.rm = TRUE)
      
      final_tble_stats <- season_record %>%
        full_join(winning_margins, by = c('Season' = 'Season')) %>%
        full_join(losing_margns, by = c('Season' = 'Season')) %>%
        full_join(best_games, by = c('Season' = 'Season')) %>%
        full_join(worst_games, by = c('Season' = 'Season')) %>%
        select(Season,
               Record,
               Median_Winning_Margin = Median_Margin.x,
               Largest_Winning_Margin,
               Biggest_Wins,
               Median_Losing_Margin = Median_Margin.y,
               Largest_Losing_Margin,
               Biggest_Losses)
      
      datatable(final_tble_stats,
                extensions = 'Buttons',
                colnames = c('Season',
                             'Record',
                             'Median Winning Margin',
                             'Largest Margin',
                             'Biggest Wins',
                             'Median Losing Margin',
                             'Largest Margin',
                             'Biggest Losses'),
                options = list(paging = FALSE,
                               dom = 'Bfrtip',
                               buttons = c('copy', 'csv', 'excel')))
      
      
      
    })
    
  }
  
  shinyApp(ui, server)