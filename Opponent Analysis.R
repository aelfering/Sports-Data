# UNO Hockey Opponent Analysis

hockey <- read.csv('UNO Hockey.csv')

hockey <- hockey %>%
  mutate(Win = ifelse(Result == 'W', 1, 0),
         Loss = ifelse(Result == 'L', 1, 0),
         Tie = ifelse(Result == 'T', 1, 0)) %>%
  ungroup() %>%
  group_by(Season) %>%
  mutate(Game.Number = row_number()) %>%
  ungroup() %>%
  mutate(Game.Number.overall = row_number())

