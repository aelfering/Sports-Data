# create time periods
season <- 2000
season_beg <- 2000-4

season_b4 <- 2000-5
season_beg_b4 <- season_b4-4

# find matches won, lost, and tied
win_loss_tie_outcomes <- distinct_bind %>%
  filter(!is.na(Team.Pts)) %>%
  select(-Team_Conf) %>%
  arrange(Season,
          Wk) %>%
  mutate(Wins = ifelse(Team.Pts > Opp.Pts, 1, 0),
         Loses = ifelse(Team.Pts < Opp.Pts, 1, 0),
         Ties = ifelse(Team.Pts == Opp.Pts, 1, 0))

mark1 <- win_loss_tie_outcomes %>%
  filter(Season >= season_beg_b4,
         Season <= season_b4) %>%
  select(Team,
         Season,
         Wins,
         Loses,
         Ties) %>%
  group_by(Team) %>%
  summarise(Seasons = n_distinct(Season),
            Wins = sum(Wins),
            Loses = sum(Loses),
            Ties = sum(Ties)) %>%
  ungroup() %>%
  mutate(B4_PCT = Wins/(Wins + Loses + Ties)) %>%
  unite(B4_Rec, c('Wins', 'Loses', 'Ties'), sep = '-', na.rm = TRUE) %>%
  select(Team,
         #Seasons,
         B4_Rec,
         B4_PCT)

mark2 <- win_loss_tie_outcomes %>%
  filter(Season >= season_beg,
         Season <= season) %>%
  select(Team,
         Season,
         Wins,
         Loses,
         Ties) %>%
  group_by(Team) %>%
  summarise(Seasons = n_distinct(Season),
            Wins = sum(Wins),
            Loses = sum(Loses),
            Ties = sum(Ties)) %>%
  ungroup()  %>%
  mutate(PCT = Wins/(Wins + Loses + Ties),
         Wins = round(Wins),
         Loses = round(Loses),
         Ties = round(Ties)) %>%
  unite(Rec, c('Wins', 'Loses', 'Ties'), sep = '-', na.rm = TRUE) %>%
  select(Team,
         #Seasons,
         Rec,
         PCT)

# find teams that had losing records before, but winning records now
team_names <- mark2 %>%
  inner_join(mark1) %>%
  mutate(Change = PCT-B4_PCT) %>%
  filter(B4_PCT < 0.5,
         PCT > 0.5) %>%
  arrange(desc(B4_PCT)) %>%
  filter(dense_rank(desc(Change)) <= 10)

season_summary <- win_loss_tie_outcomes %>%
  filter(Season >= season_beg_b4,
         Season <= season) %>%
  select(Team,
         Season,
         Wins,
         Loses,
         Ties) %>%
  group_by(Team,
           Season) %>%
  summarise_if(is.numeric, sum) %>%
  ungroup() %>%
  filter(Team %in% team_names$Team) %>%
  mutate(Grouping = case_when(Season >= season_beg_b4 & Season <= season_b4 ~ '0',
                              Season >= season_beg & Season <= season ~ '1'  ))

season_sum <- season_summary %>%
  group_by(Team,
           Grouping) %>%
  #mutate(Pct = sum(Wins)/(sum(Wins) + sum(Loses) + sum(Ties))) %>%
  summarise(FirstSeason = min(Season),
            LastSeason = max(Season),
            Wins = sum(Wins),
            Loses = sum(Loses),
            Ties = sum(Ties)) %>%
  ungroup() %>%
  mutate(Pct = Wins/(Wins + Loses + Ties),
         MedianYear = (FirstSeason + LastSeason)/2 ) %>%
  unite(Record, c('Wins', 'Loses', 'Ties'), sep = '-', na.rm = TRUE)

ggplot(season_summary,
       aes(x = Season,
           y = Wins/(Wins + Loses + Ties),
           group = Grouping)) +
  geom_hline(yintercept = 0.5,
             color = 'gray',
             #linetype = 'dashed',
             size = 1) +
  geom_line(color = 'orange',
            size = 2) + 
  geom_segment(data = subset(season_sum, Grouping == 0),
               mapping = aes(x = FirstSeason,
                             xend = LastSeason,
                             y = Pct,
                             yend = Pct),
               #linetype = 'dashed',
               size = 1.5) +
  geom_segment(data = subset(season_sum, Grouping == 1),
               mapping = aes(x = FirstSeason,
                             xend = LastSeason,
                             y = Pct,
                             yend = Pct),
               #linetype = 'dashed',
               size = 1.5) +
  #geom_label_repel(data = subset(season_sum, Grouping == 1),
  #                 mapping = aes(x = FirstSeason,
  #                               y = Pct,
  #                               label = Record),
  #                 hjust = 0,
  #                 vjust = 0.5) +
  #geom_label_repel(data = subset(season_sum, Grouping == 0),
  #                 mapping = aes(x = FirstSeason,
  #                               y = Pct,
  #                               label = Record),
  #                 hjust = 0,
  #                 vjust = 0.5) +
  facet_wrap(~Team) +
  scale_x_continuous(breaks = seq(season_beg_b4, season, by = 3),
                     labels = paste("'", substr(seq(season_beg_b4, season, by = 3), 3, 4), sep = '') ) +
  scale_y_continuous(labels = scales::percent,
                     limits=c(0,1),
                     breaks = seq(0, 1, by = 0.25)) + 
  labs(title = paste(season_beg, '-', season, ':', 'Which Teams Improved Their Win-Loss Record from ', season_beg_b4, '-', season_b4, '?\n', sep = ''),
       x = '\nSeason',
       y = '% Games Won\n') +
  theme(plot.title = element_text(face = 'bold', 
                                  size = 18, 
                                  color = 'black',
                                  family = 'Arial'),
        legend.position = 'top',
        legend.background=element_blank(),
        legend.key=element_blank(),
        legend.text = element_text(size = 16,
                                   color = 'black',
                                   family = 'Arial'),
        legend.title = element_text(size = 16, 
                                    family = 'Arial'),
        plot.subtitle = element_text(size = 17, 
                                     family = 'Arial', 
                                     color = 'black'),
        plot.caption = element_text(size = 16, 
                                    family = 'Arial', 
                                    color = 'black'),
        axis.title = element_text(size = 16,
                                  family = 'Arial', 
                                  color = 'black'),
        axis.text = element_text(size = 16, 
                                 family = 'Arial', 
                                 color = 'black'),
        strip.text = ggplot2::element_text(size = 16, 
                                           hjust = 0, 
                                           face = 'bold', 
                                           color = 'black', 
                                           family = 'Arial'),
        strip.background = element_rect(fill = NA),
        panel.background = ggplot2::element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.line = element_line(colour = "#222222", 
                                 linetype = "solid"),
        panel.grid.major.y = element_line(colour = "#c1c1c1", 
                                          linetype = "dashed"),
        panel.grid.major.x = ggplot2::element_blank()) 


ggplot(team_names,
       aes(x = Team,
           y = PCT)) +
  coord_flip() +
  geom_point(color = 'orange',
             size = 4) +
  geom_point(mapping = aes(x = Team,
                           y = B4_PCT),
             color = 'gray',
             size = 4) +
  geom_text_repel(data = team_names,
                   mapping = aes(x = Team,
                                 y = PCT,
                                 label = Rec),
                  size = 6,
                   hjust = 1,
                   vjust = 0.5) +
  geom_text_repel(data = team_names,
                   mapping = aes(x = Team,
                                 y = B4_PCT,
                                 label = B4_Rec),
                  size = 6,
                   hjust = 1,
                   vjust = 0.5)
  

