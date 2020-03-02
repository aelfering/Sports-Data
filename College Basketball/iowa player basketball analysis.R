# Iowa Player Analysis

roster <- read.csv('iowa basketball roster.csv')
player.data.index <- read.csv('iowa player index.csv')
seasons <- read.csv('iowa seasons.csv')

roster.select <- dplyr::select(roster,
                               Season,
                               Player,
                               Class,
                               Pos)

player.index.sep <- separate(player.data.index, col = Player, into = c('Player', 'Player.ID'), sep = '\\\\')

index.seasons <- inner_join(player.index.sep, 
                            seasons, 
                            by = c('Date' = 'Date'))

roster.index.join <- right_join(roster.select,
                               index.seasons,
                               by = c('Season' = 'Season',
                                      'Player' = 'Player'))

head(roster.index.join)

####  Share of Points from FT, 2P, and 3P ####
points.agg <- roster.index.join %>%
  group_by(Season) %>%
  mutate(Total.Season.Points = sum(PTS)) %>%
  ungroup() %>%
  group_by(Season,
           Player) %>%
  mutate(Total.Player.Points = sum(PTS),
         Total.Player.Attempts = sum(X2PA) + sum(X3PA) + sum(FTA)) %>%
  summarise(Player.2P = sum(X2P) * 2,
            Player.2PA = sum(X2PA),
            Player.3P = sum(X3P) * 3,
            Player.3PA = sum(X3PA),
            Player.FT = sum(FT),
            Player.FTA = sum(FTA),
            Total.Player.Points = max(Total.Player.Points),
            Total.Player.Attempts = max(Total.Player.Attempts),
            Total.Season.Points = max(Total.Season.Points),
            MP = sum(MP)) %>%
  ungroup() %>%
  filter(MP >= 90) %>%
  mutate(Pct.3P = Player.3P/Total.Player.Points,
         Pct.3PA = Player.3PA/Total.Player.Attempts)

ggplot(points.agg, aes(x = Season, y = Pct.3PA, group = Player)) +
  geom_line() +
  geom_point()


####  Stats per 90 Minutes ####
# Who played the most by minutes?
roster.index.join %>%
  group_by(Season,
           Player,
           Class) %>%
  summarise(MP = sum(MP),
            Games = n_distinct(Date)) %>%
  ungroup() %>%
  mutate(MP.per.Game = MP/Games) %>%
  arrange(desc(MP.per.Game))
  

# Who scored the most points overall per 90 minutes?
points.90 <- roster.index.join %>%
  group_by(Season,
           Player,
           Class) %>%
  summarise(TOT.PTS = sum(PTS),
            TOT.MP = sum(MP)) %>%
  ungroup() %>%
  filter(TOT.MP >= 40) %>%
  mutate(Pts.per.40 = (TOT.PTS * 40)/TOT.MP) %>%
  arrange(desc(Pts.per.40)) %>%
  as.data.frame()

points.2014 <- subset(points.90, Season == 2014)
points.2015 <- subset(points.90, Season == 2015)

# Who scored the most three-point shots per 90 minutes?
three.pointers.90 <- roster.index.join %>%
  group_by(Season,
           Player,
           Class) %>%
  summarise(X3P = sum(X3P),
            MP = sum(MP),
            X3PA = sum(X3PA)) %>%
  ungroup() %>%
  filter(MP >= 40) %>%
  mutate(Pct.Made = X3P/X3PA) %>%
  mutate(X3P.90M = (X3P * 40)/MP,
         X3PA.90M = (X3PA * 40)/MP) %>%
  mutate(Pct.90M = X3P.90M/X3PA.90M) %>%
  arrange(desc(X3P.90M))

three.point13 <- subset(three.pointers.90, 
                        Season %in% c(2012,
                                      2013, 
                                      2014, 
                                      2015,
                                      2016,
                                      2017))

ggplot(three.pointers.90, aes(x = Season, 
                          y = X3PA.90M,
                          group = Player)) +
  geom_line() +
  geom_point()

# Who scored the most two-point shoes per 90 minutes?
two.pointers.90 <- roster.index.join %>%
  group_by(Season,
           Player,
           Class) %>%
  summarise(X2P = sum(X2P),
            MP = sum(MP),
            X2PA = sum(X2PA)) %>%
  ungroup() %>%
  filter(MP >= 40) %>%
  mutate(Pct.Made = X2P/X2PA) %>%
  mutate(X2P.90M = (X2P * 40)/MP,
         X2PA.90M = (X2PA * 40)/MP) %>%
  mutate(Pct.90M = X2P.90M/X2PA.90M) %>%
  arrange(desc(X2PA.90M))

# Who had the most free throw opportunities per 90 minutes?
roster.index.join %>%
  group_by(Season,
           Player,
           Class) %>%
  summarise(FT = sum(FT),
            MP = sum(MP),
            FTA = sum(FTA)) %>%
  ungroup() %>%
  filter(MP >= 90) %>%
  mutate(Pct.Made = FT/FTA) %>%
  mutate(FT.90M = (FT * 90)/MP,
         FTA.90M = (FTA * 90)/MP) %>%
  mutate(Pct.90M = FT.90M/FTA.90M) %>%
  arrange(desc(FTA.90M))

  

  
  