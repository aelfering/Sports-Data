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