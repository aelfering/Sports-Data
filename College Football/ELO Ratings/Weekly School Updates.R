library(extrafont)
library(extrafontdb)
library(ggpubr)
library(glue)
library(ggrepel)

extrafont::loadfonts(device="win")
windowsFonts(Times=windowsFont("Menlo"))

Week1Matches <- GameSeasonOutcomes %>%
  #filter(Conf == 'ACC') %>%
  group_by(School) %>%
  filter(Wk == min(Wk)) %>%
  ungroup() %>%
  select(School,
         Opponent)

Mark1 <- (rbindlist(SeasonForecastWeeks)) %>%
  #filter(Conf == 'ACC') %>%
  select(-WinsOut) %>%
  mutate(FirstinConfDiv = FirstinConfDiv/10000,
         Win0500 = Win0500/10000,
         FinishesOut = FinishesOut/10000,
         WinConfDivOR = WinConfDivOR/10000)


FullWeekForecast <- PreSeasonForecast %>%
  rename(ForecastedWins = FMeanWins,
         ForecastedLosses = FMeanLosses,
         ForecastedConfWins = FMeanConfWins,
         ForecastedConfLosses = FMeanConfLosses,
         FinishesOut = FUndefeated) %>%
  mutate(RealRecord = "'0-0") %>%
  mutate(Week = 1) %>%
  #filter(Conf == 'ACC') %>%
  as.data.frame() %>%
  bind_rows(Mark1) %>%
  filter(!grepl('NA', RealRecord)) %>%
  left_join(Week1Matches) %>%
  mutate(Opponent = ifelse(!is.na(NextOpponent), NA, Opponent)) %>%
  unite(NextOpponent, c('NextOpponent', 'Opponent'), na.rm = TRUE) %>%
  mutate(FirstinConfDiv = ifelse(Conf == 'Ind', NA, (FirstinConfDiv)),
         WinConfDivOR = ifelse(Conf == 'Ind', NA, as.numeric(WinConfDivOR))) %>%
  select(School,
         Conf,
         Div,
         Week,
         #RealRecord
         #ForecastedWins,
         Win0500,
         FirstinConfDiv,
         WinConfDivOR)

WeekPivot <- FullWeekForecast %>%
  pivot_longer(cols = -c('School',
                         'Week',
                         'Conf',
                         'Div')) %>%
  mutate(name = case_when(name == 'Win0500' ~ 'Finishes 0.500 in Games',
                          name == 'FirstinConfDiv' ~ 'Wins a Share of the Division',
                          name == 'WinConfDivOR' ~ 'Wins Division Outright'),
         Div = case_when(Conf == 'Big 12' ~ 'Regular Season',
                         TRUE ~ Div)) %>%
  mutate(name = factor(name, levels = c('Finishes 0.500 in Games', 'Wins a Share of the Division', 'Wins Division Outright')))

Schools <- FullWeekForecast$School

for(a in Schools){
  
  #a <- 'Oklahoma'
  
  PivotSchool <- dplyr::filter(WeekPivot, School == a)
  
  ConfLabel <- (rbindlist(SeasonForecastWeeks)) %>%
    filter(School == a) %>%
    distinct(Conf, Div) %>%
    mutate(Div = case_when(Conf == 'Big 12' ~ 'Regular Season',
                           TRUE ~ Div)) %>%
    unite(ConfDiv, c('Conf', 'Div'), sep = '-', na.rm = TRUE)
  
  ConfLabelName <- ConfLabel$ConfDiv
  
  SchoolPivotConfLabel <- PivotSchool %>%
    mutate(name = case_when(name == 'Wins a Share of the Division' ~ glue('Wins a Share of the\n{ConfLabelName}'  ),
                            name == 'Wins Division Outright' ~ glue('Wins the\n{ConfLabelName} Outright'),
                            TRUE ~ as.character(name))) %>%
    filter(!is.na(value))
  
  MaxWeek <- max(SchoolPivotConfLabel$Week)-1
  
  SchoolProgress <- ggplot() + 
    geom_hline(yintercept = 0,
               color = 'black') +
    geom_hline(yintercept = 1,
               color = 'black') +
    geom_vline(xintercept = 16,
               color = NA) +
    geom_step(SchoolPivotConfLabel,
              mapping = aes(x = Week,
                            y = value,
                            color = name,
                            group = 1),
              #color = 'dark orange',
              size = 2) +
    geom_point(SchoolPivotConfLabel,
               mapping = aes(x = Week,
                             y = value,
                             group = 1),
               color = 'white',
               size = 3,
    )  +
    geom_point(SchoolPivotConfLabel,
               mapping = aes(x = Week,
                             y = value,
                             color = name),
               #color = 'dark orange',
               size = 3,
               shape = 1) +
    geom_text_repel(subset(SchoolPivotConfLabel, Week == max(Week) ),
                    mapping = aes(x = Week,
                                  y = value,
                                  label = paste0(round(value*100, 1), '%' ),
                                  color = name),
                    #color = 'dark orange',
                    family = 'Century Gotic',
                    vjust = 0.5,
                    hjust = -0.7,
                    box.padding = 1,
                    fontface = 'bold') +
    facet_wrap(~name) +
    scale_color_manual(values = c('steelblue',
                                  'dark orange',
                                  'dark green')) +
    scale_y_continuous(labels = scales::percent,
                       expand = c(0, 0)) +
    labs(title = glue('The Likelihood that {a}...'),
         caption = glue('Predictions as of 2021 season after Week {MaxWeek}\nSource: College Football Reference'),
         x = '',
         y = '') +
    expand_limits(x = 0, y = 0) +
    #scale_x_continuous(expand = c(0, 0)) + 
    #scale_y_continuous(expand = c(0, 0)) +
    theme(plot.title = element_text(face = 'bold', size = 18, family = 'Arial'),
          plot.subtitle = element_text(size = 12, family = 'Arial'),
          legend.position = 'none',
          legend.background=element_blank(),
          legend.key=element_blank(),
          legend.text = element_text(size = 12, family = 'Arial'),
          plot.title.position = "plot",
          plot.caption.position =  "plot",
          plot.caption = element_text(size = 10, family = 'Arial', color = '#969696'),
          axis.title = element_text(size = 12, family = 'Arial'),
          axis.text = element_text(size = 12, family = 'Arial', color = '#969696'),
          axis.text.x.bottom = element_text(size = 12, family = 'Arial', color = 'black'),
          axis.line.y = element_blank(),
          axis.ticks.y = element_blank(),
          strip.text = ggplot2::element_text(size = 12, hjust = 0, face = 'bold', color = 'black', family = 'Arial'),
          strip.background = element_rect(fill = NA),
          panel.background = ggplot2::element_blank(),
          axis.line = element_line(colour = "#222222", linetype = "solid"),
          panel.grid.major.y = element_line(colour = "#d9d9d9", linetype = "dashed"),
          panel.grid.major.x = element_blank()) 
  
  ggsave(SchoolProgress,
         file = glue('~/College Football Team Forecasts/{a} Forecast.png'),
         width = 10, height = 4, units = 'in')
  
  print(a)
  
  #ggsave(SchoolProgress, file = 'Athens Percent Domestic International.png', width = 10, height = 4, units = 'in')
  
  
}

# which schools increased their odds of winning the most from the previous week?  ----

WeekChanges <- WeekPivot %>%
  group_by(School,
           #RealRecord,
           name) %>%
  mutate(Change = value-lag(value)) %>%
  ungroup() %>%
  filter(Week == max(Week)) %>%
  arrange((Change))

# who are the biggest winners and losers of winning 0.500 in games?
Games0500Winners <- WeekChanges %>%
  filter(name == 'Finishes 0.500 in Games',
         dense_rank(desc(Change)) <= 10) %>%
  arrange(desc(Change)) %>%
  mutate(Prev = value-Change)

Games0500Losers <- WeekChanges %>%
  filter(name == 'Finishes 0.500 in Games',
         dense_rank((Change)) <= 10) %>%
  arrange((Change)) %>%
  mutate(Prev = value-Change)

# who are the biggest winners and losers of winning a share of their division?
WinsShareWinners <- WeekChanges %>%
  filter(name == 'Wins a Share of the Division') %>%
  arrange(desc(Change)) %>%
  filter(dense_rank(desc(Change)) <= 10) %>%
  mutate(Prev = value+Change)

WinsShareLosers <- WeekChanges %>%
  filter(name == 'Wins a Share of the Division') %>%
  arrange((Change)) %>%
  filter(dense_rank((Change)) <= 10) %>%
  mutate(Prev = value-Change)

# who are the biggest winners and loser of winning their division outright?
WinsOutRightWinner <- WeekChanges %>%
  filter(name == 'Wins Division Outright') %>%
  arrange(desc(Change)) %>%
  filter(dense_rank(desc(Change)) <= 10) %>%
  mutate(Prev = value+Change)

WinsOutrightLosers <- WeekChanges %>%
  filter(name == 'Wins Division Outright') %>%
  arrange((Change)) %>%
  filter((desc(Change)) <= 10,
         row_number() <= 10) %>%
  mutate(Prev = value-Change)







