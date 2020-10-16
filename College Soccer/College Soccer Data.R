# College Soccer Script
# Examining D-1 Men's Soccer

soccer <- read.csv('College Soccer.csv')

soccer_clean <- soccer %>%
  filter(Score != '-') %>%
  select(Month,
         Date,
         Season,
         Team, 
         Opponent,
         Score) %>%
  mutate(Full.Game.Date = as.Date( paste( Month, Date, Season, sep = "." )  , format = "%m.%d.%Y" ),
         Location = substr(Opponent, start = 1, stop = 2),
         Opponent.Name = stri_sub(Opponent, 4),
         Result = stri_sub(Score, 5),
         Score.Goals = substr(Score, start = 1, stop = 3)) %>%
  mutate(Opponent.Name = gsub("\\*", "", Opponent.Name),
         Team.Score = as.numeric(substr(Score.Goals, start = 1, stop = 1)),
         Opponent.Score = as.numeric(stri_sub(Score.Goals, 3))) %>%
  select(Season,
         Full.Game.Date,
         Team,
         Opponent.Name,
         Location,
         Result,
         Team.Score,
         Opponent.Score) 

soccer_clean %>%
  group_by(Team) %>%
  summarise(Goals.For = sum(Team.Score),
            Goals.Against = sum(Opponent.Score),
            Total.Points = sum(Team.Score) + sum(Opponent.Score),
            Net.Goals = sum(Team.Score) - sum(Opponent.Score)) %>%
  arrange(desc(Net.Goals)) %>%
  as.data.frame()