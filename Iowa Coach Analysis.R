# Iowa Football Analysis

library(dplyr)

iowa <- read.csv('iowa_coaches.csv')

#### Separates ranks from the team name ####
iowa_no_ranks <- remove_ranks(iowa)
iowa_with_ranks <- keep_ranks(iowa_no_ranks)
iowa_with_ranks$Team.Name <- trim.leading(iowa_with_ranks$Team.Name)
iowa_with_ranks$Opponent.Name <- trim.leading(iowa_with_ranks$Opponent.Name)
ia_games <- remove_parentheses(iowa_with_ranks)

ia_games <- ia_games %>%
  mutate(Team.Rank = as.integer(Team.Rank),
         Opp.Rank = as.integer(Opp.Rank)) %>%
  mutate(Team.Rank = ifelse(is.na(Team.Rank), 0, Team.Rank),
         Opp.Rank = ifelse(is.na(Opp.Rank), 0, Opp.Rank)) %>%
  select(Coach, Conference, Season, G, Date, School = Team.Name, 
         Team.Rank, Location, Opponent = Opponent.Name, Opp.Rank,
         Conf, Result, Pts, Opp) %>%
  mutate(Win.Int = ifelse(Result == 'W', 1, 0),
         Lose.Int = ifelse(Result == 'L', 1, 0),
         Tie.Int = ifelse(Result == 'T', 1, 0))

coach_years <- ia_games %>%
  distinct(Coach, Season) %>%
  select(Coach, Season) %>%
  group_by(Coach) %>%
  mutate(Year.No = row_number()) %>%
  ungroup()

####  Assign Conf Game Records per Coach ####
# Iowa was a member of the Western and MVIAA conferences between 1907 and 1910
special_seasons <- c(1907, 1908, 1909, 1910)

mviaa_western <- subset(ia_games, (ia_games$Season %in% special_seasons))
mviaa_western <- mviaa_western %>%
  mutate(Conf.Game = ifelse(Conf %in% c('Western', 'MVIAA'), 1, 0))

western <- subset(ia_games, !(ia_games$Season %in% special_seasons))
western <- western %>%
  mutate(Conf.Game = ifelse(Conf %in% c('Western', 'Big Ten'), 1, 0))

ia_game_data <- bind_rows(mviaa_western, western)
ia_game_data <- ia_game_data %>%
  arrange(Season, G)

####  Coach Analysis ####

coaches_game_no <- ia_game_data %>%
  group_by(Coach) %>%
  mutate(Game.No = row_number(),
         Running.Wins = cumsum(Win.Int),
         Running.Loses = cumsum(Lose.Int),
         Running.Ties = cumsum(Tie.Int)) %>%
  ungroup()

####  What is the coaching record adjusted for conference? ####
conf.performance <- ia_game_data %>%
  filter(Conf.Game == 1, G != 13) %>%
  group_by(Coach) %>%
  mutate(Conf.Number = row_number(),
         Conf.Winning.Streak = rowid(rleid(Win.Int)) * Win.Int,
         Running.Conf.Wins = cumsum(Win.Int),
         Running.Conf.Loses = cumsum(Lose.Int),
         Running.Conf.Ties = cumsum(Tie.Int)) %>%
  ungroup() 

  code.check <- conf.performance %>%
    group_by(Coach) %>%
    summarise(Wins = sum(Win.Int),
              Loses = sum(Lose.Int),
              Ties = sum(Tie.Int))


####  How is Iowa against Top 25 and Top 10 Teams? ####
top_10 <- ia_game_data %>%
  filter(Opp.Rank <= 10 & Opp.Rank != 0) %>%
  group_by(Coach) %>%
  mutate(top_10_number = row_number(),
         top_10_wins = cumsum(Win.Int),
         top_10_loses = cumsum(Lose.Int),
         top_10_ties = cumsum(Tie.Int)) %>%
  ungroup()
  
  ia_game_data %>%
    filter(Opp.Rank <= 25 & Opp.Rank != 0) %>%
    group_by(Coach) %>%
    summarise(Wins = sum(Win.Int),
              Loses = sum(Lose.Int),
              Ties = sum(Tie.Int))

top_25 <- ia_game_data %>%
  filter(Opp.Rank <= 25 & Opp.Rank != 0) %>%
  group_by(Coach) %>%
  mutate(top_25_number = row_number(),
         top_25_wins = cumsum(Win.Int),
         top_25_loses = cumsum(Lose.Int),
         top_25_ties = cumsum(Tie.Int)) %>%
  ungroup()
  
####  Export Data ####

coaches_game_no <- left_join(coaches_game_no, conf.performance, by = c('Date' = 'Date'))
coaches_game_no <- inner_join(coaches_game_no, coach_years, by = c('Coach' = 'Coach', 'Season' = 'Season'))

write.csv(coaches_game_no, file = 'iowa_winning_record.csv')


