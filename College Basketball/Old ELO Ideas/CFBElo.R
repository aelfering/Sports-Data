library(elo)
library(data.table, warn.conflicts = FALSE)
K <- 20.0                                                                           
REVERT <- 1/3                                                                       
CollegeFB <- read.csv("CFBTest.csv", header = TRUE, stringsAsFactors = FALSE) 
CollegeFB <- unique(setDT(CollegeFB), by = c("Date", "Team.A", "Team.B"))           
Team.A.Score <- CollegeFB$Pts                                                       
Team.B.Score <- CollegeFB$Pts_1                                                     
A.Adv <- CollegeFB$A.Adv                                                 
B.Adv <- CollegeFB$B.Adv                                                
Wk <- CollegeFB$Wk                                                                  
Rk <- CollegeFB$A.Rank
Rk1 <- CollegeFB$B.Rank

elo <-  elo.run(score(Team.A.Score, Team.B.Score)~adjust(Team.A, A.Adv) + 
                  adjust(Team.B, B.Adv) + regress(Season, 1505, REVERT), 
                k=K*log(abs(Team.A.Score - Team.B.Score) + 1), data=CollegeFB)

elodf <- as.data.frame(elo)                                                         
Game.Date <- CollegeFB$Date                                                         
Location <- CollegeFB$Location                                                      
Season <- CollegeFB$Season                                                          

elo.A <- elodf$elo.A                                                                
elo.B <- elodf$elo.B                                                               
elo.delta <- elodf$update                                                           
elo.A.before <- elo.A - elo.delta                                                   
elo.B.before <- elo.B + elo.delta                                                   

PD <- Team.A.Score - Team.B.Score                                                   
multiplier <- log(PD+1)*(2.2/((abs(elo.A.before-elo.B.before)*0.001+2.2)))

elo.final <- elo.run(score(Team.A.Score, Team.B.Score)~
                       adjust(Team.A, A.Adv) + adjust(Team.B, B.Adv) + 
                       regress(Season, 1505, REVERT), k=K*multiplier, data=CollegeFB)

elodf1 <- as.data.frame(elo.final)

Points.Spread <- (elo.A.before-elo.B.before)/25

elodf1$Day.of.Game <- Game.Date
elodf1$Neutral <- Neutral
elodf1$Location <- Location
elodf1$Team.A.Score <- Team.A.Score
elodf1$Team.B.Score <- Team.B.Score
elodf1$Season <- Season
elodf1$Points.Spread <- Points.Spread
elodf1$Wk <- Wk
elodf1$Rank.A <- as.integer(Rk)
elodf1$Rank.B <- as.integer(Rk1)

# Data Export
write.csv(elodf1, file = "College Football ELO.csv") # Create the .csv file
save(elodf1, file='CFBELO')
