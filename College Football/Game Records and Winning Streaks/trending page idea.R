head(distinct_bind)

n_var <- 30
compare_var <- 12

season_var <- 2002

rolling_pcts <- distinct_bind %>%
  filter(!is.na(Team.Pts)) %>%
  arrange(Team, 
          Season,
          Wk) %>%
  mutate(Wins = ifelse(Team.Pts > Opp.Pts, 1, 0),
         Loses = ifelse(Team.Pts < Opp.Pts, 1, 0),
         Ties = ifelse(Team.Pts == Opp.Pts, 1, 0)) %>%
  ungroup() %>%
  group_by(Team) %>%
  mutate(Rolling_Long_Term_Wins = rollapplyr(Wins, n_var, sum, partial = TRUE),
         Rolling_Long_Term_Losses = rollapplyr(Loses, n_var, sum, partial = TRUE),
         Rolling_Long_Term_Ties = rollapplyr(Ties, n_var, sum, partial = TRUE),
         Long_Term_Total = Rolling_Long_Term_Wins + Rolling_Long_Term_Losses + Rolling_Long_Term_Ties,
         Games = row_number()) %>%
  mutate(Rolling_Short_Term_Wins = rollapplyr(Wins, compare_var, sum, partial = TRUE),
         Rolling_Short_Term_Losses = rollapplyr(Loses, compare_var, sum, partial = TRUE),
         Rolling_Short_Term_Ties = rollapplyr(Ties, compare_var, sum, partial = TRUE),
         Short_Term_Total = Rolling_Short_Term_Wins + Rolling_Short_Term_Losses + Rolling_Short_Term_Ties) %>%
  mutate(Long_Term_Pct = Rolling_Long_Term_Wins/Long_Term_Total,
         Short_Term_Pct = Rolling_Short_Term_Wins/Short_Term_Total) %>%
  ungroup() 

under_performing <- rolling_pcts %>%
  filter(Season == season_var) %>%
  group_by(Team) %>%
  filter(Wk == max(Wk)) %>%
  unite(Long_Term_Record, c('Rolling_Long_Term_Wins', 'Rolling_Long_Term_Losses', 'Rolling_Long_Term_Ties'), sep = '-', na.rm = TRUE) %>%
  unite(Short_Term_Record, c('Rolling_Short_Term_Wins', 'Rolling_Short_Term_Losses', 'Rolling_Short_Term_Ties'), sep = '-', na.rm = TRUE) %>%
  select(Season,
         Team,
         Long_Term_Record,
         Short_Term_Record,
         Long_Term_Pct,
         Short_Term_Pct) %>%
  filter(Short_Term_Pct < Long_Term_Pct) %>%
  mutate(Gap = Short_Term_Pct - Long_Term_Pct) %>%
  arrange(Gap)

ggplot(head(under_performing, 15),
       aes(x = reorder(Team, -Gap),
           y = Long_Term_Pct)) +
  #geom_hline(yintercept = 0.5,
  #           linetype = 'dashed') +
  geom_point(size = 2) +
  geom_point(mapping = aes(x = reorder(Team, Gap),
                           y = Short_Term_Pct),
             size = 2) +
  geom_segment(aes(x = Team,
                   y = Long_Term_Pct-0.01,
                   xend = Team,
                   yend = Short_Term_Pct + 0.01),
               arrow = arrow(length = unit(0.01, "npc")),
               size = 1,
               color = '#969696') +
  geom_text_repel(mapping = aes(x = reorder(Team, -Gap),
                                y = Long_Term_Pct,
                                label = Long_Term_Record),
                  vjust = -1) +
  geom_text_repel(mapping = aes(x = reorder(Team, -Gap),
                                y = Short_Term_Pct,
                                label = Short_Term_Record),
                  vjust = -1) +
  labs(title =paste(season_var, ' Season: ', 'Which College Football Teams are Performing Worse?', sep = ''),
       subtitle = paste('Comparing the last ', compare_var, ' games against the last ', n_var , ' games played', sep = ''),
       x = '',
       y = 'Percent of Games Won') +
  scale_y_continuous(labels = percent) +
  coord_flip() +
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
        panel.grid.major.x = element_line(colour = "#c1c1c1", linetype = "dashed")) 


over_performing <- rolling_pcts %>%
  filter(Season == season_var) %>%
  group_by(Team) %>%
  filter(Wk == max(Wk)) %>%
  unite(Long_Term_Record, c('Rolling_Long_Term_Wins', 'Rolling_Long_Term_Losses', 'Rolling_Long_Term_Ties'), sep = '-', na.rm = TRUE) %>%
  unite(Short_Term_Record, c('Rolling_Short_Term_Wins', 'Rolling_Short_Term_Losses', 'Rolling_Short_Term_Ties'), sep = '-', na.rm = TRUE) %>%
  select(Season,
         Team,
         Long_Term_Record,
         Short_Term_Record,
         Long_Term_Pct,
         Short_Term_Pct) %>%
  filter(Short_Term_Pct > Long_Term_Pct) %>%
  mutate(Gap = Short_Term_Pct - Long_Term_Pct) %>%
  arrange(desc(Gap))

ggplot(head(over_performing, 20),
       aes(x = reorder(Team, Gap),
           y = Long_Term_Pct)) +
  geom_hline(yintercept = 0.5,
             linetype = 'dashed',
             size = 1,
             alpha = 0.3) +
  geom_point(size = 2) +
  geom_point(mapping = aes(x = reorder(Team, Gap),
                           y = Short_Term_Pct),
             size = 2) +
  geom_segment(aes(x = Team,
                   y = Long_Term_Pct + 0.01,
                   xend = Team,
                   yend = Short_Term_Pct - 0.01),
               arrow = arrow(length = unit(0.01, "npc")),
               size = 1,
               color = '#969696') +
  geom_text_repel(mapping = aes(x = reorder(Team, -Gap),
                                y = Long_Term_Pct,
                                label = Long_Term_Record),
                  vjust = -1) +
  geom_text_repel(mapping = aes(x = reorder(Team, -Gap),
                                y = Short_Term_Pct,
                                label = Short_Term_Record),
                  vjust = -1) +
  coord_flip() +
  labs(title =paste(season_var, ' Season: ', 'Which College Football Teams are Performing Worse?', sep = ''),
       subtitle = paste('Comparing the last ', compare_var, ' games against the last ', n_var , ' games played\n', sep = ''),
       x = '',
       y = 'Percent of Games Won') +
  scale_y_continuous(labels = percent) +
  coord_flip() +
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
        panel.grid.major.x = element_line(colour = "#c1c1c1", linetype = "dashed")) 

head(rolling_pcts)

rolling_pcts %>%
  filter(Team == 'Michigan',
         Season >= 2010) %>%
  ggplot(aes(x = Games,
             y = Short_Term_Pct,
             group = Team)) +
  geom_line() +
  geom_line(mapping = aes(x = Games,
                          y = Long_Term_Pct))











