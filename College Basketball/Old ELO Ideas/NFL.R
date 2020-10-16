library(elo)
library(data.table, warn.conflicts = FALSE)
K <- 20.0                                                                           # The rate at which ELO ratings change.
REVERT <- 1/3                                                                       # Between seasons, a team retains 0.66 of the previous season ratings.
NFL <- read.csv("NFL1.csv", header = TRUE, stringsAsFactors = FALSE) # Wrangle the .csv file
Team.A.Score <- NFL$Team.A.Score                                                       # Pull Team.A score
Team.B.Score <- NFL$Team.B.Score                                                    # Pull Team.B score
A.Adv <- NFL$A.Adv                                                 # Team A Homefield Advantage
B.Adv <- NFL$B.Adv                                                # Team B Homefield Advantage

# Run the initial ELO scores
elo <-  elo.run(score(Team.A.Score, Team.B.Score)~adjust(Team.A, A.Adv) + adjust(Team.B, B.Adv) + regress(Season, 1505, REVERT), k=K*log(abs(Team.A.Score - Team.B.Score) + 1), data=NFL)

# Parse data from the ELO results and College FB data
elodf <- as.data.frame(elo)                                                         # Transform the "elo" variable into a data frame
Location <- NFL$Location                                                      # Create a location column
Season <- NFL$Season                                                          # Create a season column
Week <- NFL$Week
Month <- NFL$Month
Day <- NFL$Day
WDay <- NFL$WDay

# Find the ELO ratings before the change
elo.A <- elodf$elo.A                                                                # ELO score of Team.A
elo.B <- elodf$elo.B                                                                # ELO score of Team.B
elo.delta <- elodf$update                                                           # ELO change
elo.A.before <- elo.A - elo.delta                                                   # Team.A ELO before the update
elo.B.before <- elo.B + elo.delta                                                   # Team.B ELO before the update

# Add a point differential multiplier to factor autocorrelation
PD <- Team.A.Score - Team.B.Score                                                   # Point differential
multiplier <- log(PD+1)*(2.2/((abs(elo.A.before-elo.B.before)*0.001+2.2)))

# FINAL ELO EQUATION!
elo.final <- elo.run(score(Team.A.Score, Team.B.Score)~adjust(Team.A, A.Adv) + adjust(Team.B, B.Adv) + regress(Season, 1505, REVERT), k=K*multiplier, data=NFL)
elodf1 <- as.data.frame(elo.final)

# There is a way to calculate the point spread of how a game should go based on the ELO scores. Take the difference of the ELO scores and divide by 25
Points.Spread <- (elo.A.before-elo.B.before)/25

#  Add Date, Netural, Location, Team.A and Team.B scores, Season, and Points.Spread to elodf1
elodf1$Location <- Location
elodf1$Team.A.Score <- Team.A.Score
elodf1$Team.B.Score <- Team.B.Score
elodf1$Season <- Season
elodf1$Points.Spread <- Points.Spread
elodf1$Week <- Week
elodf1$Month <- Month
elodf1$Day <- Day
elodf1$WDay <- WDay

# Data Export
write.csv(elodf1, file = "NFLELO.csv") # Create the .csv file
save(elodf1, file='NFL')
