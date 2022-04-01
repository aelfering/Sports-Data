setwd("~/Team ELO")
source('~/GitHub/Sports-Data/College Football/ELO Ratings/ELO MODEL.R')

windowsFonts(sans="Fira Code")
loadfonts(device="win")
loadfonts(device="postscript")

options(dplyr.summarise.inform = FALSE)

library(ggrepel)
library(glue)

# how has elo changed in the last 50 years? ----

ELODF1 <- FullELODF %>%
  filter(Season >= 1971)

distinctTeam <- unique(ELODF1$team.A)

FBSRecent <- Conferences %>%
  filter(Season >= 1971) %>%
  select(Season,
         team.A = Var.2)

teamDF <- data.frame(team = distinctTeam) %>%
  arrange(team) %>%
  mutate(Index = row_number())

for(i in 1:max(teamDF$Index)){
#for(i in 1:10){
  
  DataFrameTeam <- data.frame()
  
  #i <- 4
  
  filterTeam <- teamDF %>%
    filter(Index == i)
  
  TeamName <- filterTeam$team
  
  TeamChart <- ELODF1 %>%
    filter(team.A == filterTeam$team) %>%
    inner_join(FBSRecent,
               by = c('Season' = 'Season',
                      'team.A' = 'team.A')) %>%
    arrange(Season,
            Wk) %>%
    mutate(GameNo = row_number(),
           SeasonWk = as.numeric(paste0(Season, Wk)))
  
  SeasonWk <- ELODF1 %>%
    filter(Season == 2007) %>%
    select(Wk,
           Season) %>%
    dist
  
  ELODF1 %>%
    select(team.A,
           team.B,
           elo.A,
           Season,
           Wk) %>%
    filter(Season == 2007,
           team.A == 'Nebraska') %>%
    arrange(Wk)
  
  ELODF1 %>%
    mutate(#GameNo = row_number(),
           SeasonWk = as.numeric(paste0(Season, Wk)))
  
  
  if(nrow(TeamChart) == 0){
    
    #print('Nothing to print')
    
  }else{
    
    MaxELO <- TeamChart %>%
      filter(elo.A == max(elo.A)) %>%
      filter(GameNo == min(GameNo)) %>%
      mutate(SeasonWk = as.numeric(as.numeric(paste0(Season, Wk)))) %>%
      select(team.B,
             Season,
             GameNo,
             SeasonWk,
             Pts,
             Opp,
             elo.A) %>%
      mutate(elo.A = round(elo.A,1),
             Result = case_when(Pts > Opp ~ 'W',
                                Pts < Opp ~ 'L',
                                TRUE ~ 'T'))
    
    LabelGame <- paste(MaxELO$Season, ' | ', MaxELO$Result, ' ', MaxELO$Pts, '-', MaxELO$Opp, ' vs ', MaxELO$team.B, ' ', sep = '')
    
    gg <- TeamChart %>%
      ggplot() + 
      geom_line(mapping = aes(x = GameNo,
                              y = elo.A,
                              group = Season),
                color = 'steelblue',
                size = 1) +
      geom_point(MaxELO,
                 mapping = aes(x = GameNo,
                               y = elo.A),
                 size = 2,
                 color = 'darkorange') +
      geom_point(MaxELO,
                 mapping = aes(x = GameNo,
                               y = elo.A),
                 size = 2,
                 shape = 1,
                 color = 'black') +
      geom_text_repel(data = MaxELO,
                mapping = aes(x = GameNo,
                              y = elo.A),
                fontface = 'bold',
                force=1, 
                point.padding=unit(1,'lines'),
                direction = 'both',
                nudge_x = 0.1,
                nudge_y = 0.3,
                segment.size=0.2,
                box.padding = 1,
                label = LabelGame) +
      labs(title = filterTeam$team) +
      expand_limits(y =  max(c(TeamChart$elo.A))*1.05 ) +
      theme(plot.title = element_text(face = 'bold', size = 14),
            plot.subtitle = element_text(face = 'bold', size = 12),
            legend.position = 'top',
            legend.background=element_blank(),
            legend.key=element_blank(),
            legend.text = element_text(size = 12),
            plot.title.position = "plot",
            plot.caption.position =  "plot",
            plot.caption = element_text(size = 12),
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 12, color = '#969696'),
            axis.text.x.bottom = element_text(size = 12, color = 'black'),
            axis.line.y = element_blank(),
            axis.ticks.y = element_blank(),
            strip.text = ggplot2::element_text(size = 12, hjust = 0, face = 'bold', color = 'black'),
            strip.background = element_rect(fill = NA),
            panel.background = ggplot2::element_blank(),
            axis.line = element_line(colour = "#222222", linetype = "solid"),
            panel.grid.major.y = element_line(colour = "#c1c1c1", linetype = "dashed"),
            panel.grid.major.x = element_blank()) 
    
    ggsave(gg,
           file = glue('{TeamName} ELO Ratings.png'), 
           width = 15, 
           height = 4, 
           units = 'in')
    
    print(filterTeam$team)
    print(gg)
    
  }
  
}

# which team saw the largest increase in the ELO in the latest season?  ----

CurrentRecord <- FullELODF %>%
  filter(Season == 1990 ) %>%
  #mutate(Season = as.character(Season)) %>%
  mutate(Wins = ifelse(Pts > Opp, 1, 0),
         Loses = ifelse(Pts < Opp, 1, 0),
         Ties = ifelse(Pts == Opp, 1, 0)) %>%
  group_by(team.A,
           Season) %>%
  summarise_if(is.numeric, sum) %>%
  ungroup() %>%
  select(Season,
         team.A,
         Wins,
         Loses,
         Ties,
         Pts,
         Opp) %>%
  mutate(TotalGames = Wins + Loses + Ties,
         WinExpect = ((Pts)^2/((Pts)^2 + (Opp)^2)),
         WinGamesExpect = round(TotalGames*WinExpect),
         LoseGamesExpect = round(TotalGames*(1-WinExpect)),
         WinDiff = Wins-WinGamesExpect,
         Ties = ifelse(Ties == 0, NA, Ties)) %>%
  unite(Record, c('Wins', 'Loses', 'Ties'), sep = '-', na.rm = TRUE ) %>%
  unite(EstRecord, c('WinGamesExpect', 'LoseGamesExpect'), sep = '-') %>%
  inner_join(Conferences,
             by = c('Season' = 'Season',
                    'team.A' = 'Var.2')) %>%
  select(Season,
         team.A,
         Record,
         EstRecord,
         WinDiff,
         Pts, 
         Opp)
  

FullELODF %>%
  filter(Season == 1990) %>%
  #filter(team.A == 'Iowa') %>%
  group_by(team.A) %>%
  filter(Wk == max(Wk) | Wk == min(Wk)) %>%
  arrange(Wk) %>%
  select(Season,
         team.A,
         elo.A) %>%
  inner_join(Conferences,
             by = c('Season' = 'Season',
                    'team.A' = 'Var.2')) %>%
  select(Season,
         team.A,
         Conf,
         elo.A) %>%
  mutate(Conf = trim(Conf)) %>%
  mutate(Index = row_number()) %>%
  pivot_wider(names_from = Index,
              values_from = elo.A) %>%
  ungroup() %>%
  rename(Beg = 4,
         End = 5) %>%
  mutate(Change = End-Beg,
         Pct = Change/Beg,
         RankBefore = dense_rank(desc(Beg)),
         RankEnd = dense_rank(desc(End))) %>%
  mutate(Beg = round(Beg, 1),
         End = round(End, 1),
         Change = round(Change, 1),
         Pct = round(Pct, 2)) %>%
  inner_join(CurrentRecord) %>%
  arrange((WinDiff)) %>%
  mutate(Diff = Pts-Opp) %>%
  as.data.frame() #%>%
  #filter(Conf == 'Pac-12')








