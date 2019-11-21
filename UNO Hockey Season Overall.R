library(dplyr)

hockey <- read.csv('UNO Hockey.csv')

hockey <- hockey %>%
  mutate(Win = ifelse(Result == 'W', 1, 0),
         Loss = ifelse(Result == 'L', 1, 0),
         Tie = ifelse(Result == 'T', 1, 0)) %>%
  ungroup() %>%
  group_by(Season) %>%
  mutate(Game.Number = row_number()) %>%
  ungroup()  %>%
  group_by(Oppponent) %>%
  mutate(Game.Number.Series = row_number()) %>%
  mutate(running.wins = cumsum(Win),
         running.loses = cumsum(Loss),
         running.ties = cumsum(Tie)) %>%
  ungroup()

hockey$win.streak = rowid(rleid(hockey$Win)) * hockey$Win
hockey$lose.streak = rowid(rleid(hockey$Loss)) * hockey$Loss
hockey$tie.streak = rowid(rleid(hockey$Tie)) * hockey$Tie

write.csv(hockey, file = 'hockey_games_uno.csv')