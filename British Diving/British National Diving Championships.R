#   BRITISH DIVING SCRIPT
#   Code by Alex Elfering

diving <- read.csv('british diving.csv')
head(diving)

dive_adj <- diving %>%
  mutate(Date = dmy(Date)) %>%
  # Fixing typos in the script
  mutate(Stage = gsub("Fina", "Final", Stage)) %>%
  mutate(Stage = gsub("Finall", "Final", Stage)) %>%
  # Additional Calculations
  mutate(Aprox.Age = year(Date) - DOB,
         J1 = as.numeric(gsub("_", ".5", J1)),
         J2 = as.numeric(gsub("_", ".5", J2)),
         J3 = as.numeric(gsub("_", ".5", J3)),
         J4 = as.numeric(gsub("_", ".5", J4)),
         J5 = as.numeric(gsub("_", ".5", J5)),
         J6 = as.numeric(gsub("_", ".5", J6)),
         J7 = as.numeric(gsub("_", ".5", J7)),
         Full.Dive.No = paste(as.character(Dive), X., sep = '')) %>%
  mutate(Total = J1 + J2 + J3 + J4 + J5 + J6 + J7) %>%   # This is not quite there yet and needs some scrunity
  group_by(Diver, 
           Event, 
           Stage) %>%
  mutate(DD.Rank = dense_rank(desc(DD))) %>% #  How tough is this dive? Ranking
  ungroup()

write.csv(dive_adj, "Diving Performance.csv")

#### Decoding the Dive ####
# 1: The first digit indicates the dive’s group: 1 = forward, 2 = back, 3 = reverse, 4 = inward, 5 = twisting, 6 = armstand.
# 2: In front, back, reverse, and inward dives, a ‘1’ as the second digit indicates a flying action. A ‘0’ indicates none. In twisting and armstand dives, the second digit indicates the dive’s group (forward, back, reverse).
# 3: The third digit indicates the number of half somersaults.
# 4: The fourth digit, if applicable, indicates the number of half twists.
# 5: The letter indicates body position: A = straight, B = pike, C = tuck, D = free.

dive_select <- dplyr::select(dive_adj, Meet, Stage, Event, Date, Diver, Full.Dive.No)

four_charac_dive <- dive_select %>%
  filter(nchar(Full.Dive.No) == 4) %>%
  mutate(Group.Dive.Length = nchar(Full.Dive.No)) %>%
  # The First Digit
  mutate(First = ifelse(substring(Full.Dive.No, 1, 1) == 1, 'Forward',
                        ifelse(substring(Full.Dive.No, 1, 1) == 2, 'Back',
                               ifelse(substring(Full.Dive.No, 1, 1) == 3, 'Reverse',
                                      ifelse(substring(Full.Dive.No, 1, 1) == 4, 'Inward',
                                             ifelse(substring(Full.Dive.No, 1, 1) == 5, 'Twisting',
                                                    ifelse(substring(Full.Dive.No, 1, 1) == 6, 'Armstand',''))))))) %>%
  # The Second Digit
  mutate(Second = ifelse(First %in% c("Twisting", "Armstand") & substring(Full.Dive.No, 2, 2) == 1, 'Forward',
                         ifelse(First %in% c("Twisting", "Armstand") & substring(Full.Dive.No, 2, 2) == 2, 'Back',
                                ifelse(First %in% c("Twisting", "Armstand") & substring(Full.Dive.No, 2, 2) == 3, 'Reverse',
                                       ifelse(First %in% c("Twisting", "Armstand") & substring(Full.Dive.No, 2, 2) == 4, 'Inward',
                                              ifelse(!First %in% c("Twisting", "Armstand") & substring(Full.Dive.No, 2, 2) == 1, 'Flying', '')))))) %>%
  # The Third Digit
  mutate(Third = ifelse(substring(Full.Dive.No, 3, 3) == 3, "1 and 1/2 Somersaults",
                        ifelse(substring(Full.Dive.No, 3, 3) == 5, "2 and 1/2 Somersaults",
                               ifelse(substring(Full.Dive.No, 3, 3) == 7, "3 and 1/2 Somersaults",
                                      ifelse(substring(Full.Dive.No, 3, 3) == 9, "4 and 1/2 Somersaults", 
                                             ifelse(substring(Full.Dive.No, 3, 3) == 4, "Double Somersaults", 
                                                    ifelse(substring(Full.Dive.No, 3, 3) == 6, "Triple Somersaults", 
                                                           ifelse(substring(Full.Dive.No, 3, 3) == 1, "1/2 Somersault", 
                                                                  ifelse(substring(Full.Dive.No, 3, 3) == 2, "1 Somersault",""))))))))) %>%
  # The Fourth Character
  mutate(Fourth = ifelse(substring(Full.Dive.No, 4, 4) == "A", "Straight",
                         ifelse(substring(Full.Dive.No, 4, 4) == "B", "Pike",
                                ifelse(substring(Full.Dive.No, 4, 4) == "C", "Tuck",
                                       ifelse(substring(Full.Dive.No, 4, 4) == "D", "Free", ""))))) %>%
  # The irrelvant Fifth Character
  mutate(Fifth = "") %>%
  # Put it together
  mutate(Dive.Name = paste(Second, " ", First, " - ", Third, " - ", Fourth, sep = ""))
                         
                               
five_charac_dive <- dive_select %>%
  filter(nchar(Full.Dive.No) == 5) %>%
  mutate(Group.Dive.Length = nchar(Full.Dive.No)) %>%
  # The First Character
  mutate(First = ifelse(substring(Full.Dive.No, 1, 1) == 6, "Armstand", "")) %>%
  # The Second Character
  mutate(Second = ifelse(substring(Full.Dive.No, 2, 2) == 1, 'Forward',
                        ifelse(substring(Full.Dive.No, 2, 2) == 2, 'Backward',
                               ifelse(substring(Full.Dive.No, 2, 2) == 3, 'Reverse',
                                      ifelse(substring(Full.Dive.No, 2, 2) == 4, 'Inward', '' ))))) %>%
  # The Third Characteer
  mutate(Third = ifelse(substring(Full.Dive.No, 3, 3) == 1, "1/2 Somersault",
                        ifelse(substring(Full.Dive.No, 3, 3) == 2, "1 Somersault",
                               ifelse(substring(Full.Dive.No, 3, 3) == 3, "1 and 1/2 Somersaults",
                                      ifelse(substring(Full.Dive.No, 3, 3) == 4, "Double Somersaults",
                                             ifelse(substring(Full.Dive.No, 3, 3) == 5, "2 and 1/2 Somersaults",
                                                    ifelse(substring(Full.Dive.No, 3, 3) == 6, "Triple Somersaults",
                                                           ifelse(substring(Full.Dive.No, 3, 3) == 7, "3 and 1/2 Somersaults",
                                                                  ifelse(substring(Full.Dive.No, 3, 3) == 9, "4 and 1/2 Somersaults", ""))))))))) %>%
  # The Fourth Character
  mutate(Fourth = ifelse(substring(Full.Dive.No, 4, 4) == 1, "1/2 Twist",
                        ifelse(substring(Full.Dive.No, 4, 4) == 2, "1 Twist",
                               ifelse(substring(Full.Dive.No, 4, 4) == 3, "1 and 1/2 Twists",
                                      ifelse(substring(Full.Dive.No, 4, 4) == 4, "Double Twists",
                                             ifelse(substring(Full.Dive.No, 4, 4) == 5, "2 and 1/2 Twists",
                                                    ifelse(substring(Full.Dive.No, 4, 4) == 6, "Triple Twists",
                                                           ifelse(substring(Full.Dive.No, 4, 4) == 7, "3 and 1/2 Twists",
                                                                  ifelse(substring(Full.Dive.No, 4, 4) == 9, "4 and 1/2 Twists", ""))))))))) %>%  
  # The Fifth Character
  mutate(Fifth = ifelse(substring(Full.Dive.No, 5, 5) == "A", "Straight",
                         ifelse(substring(Full.Dive.No, 5, 5) == "B", "Pike",
                                ifelse(substring(Full.Dive.No, 5, 5) == "C", "Tuck",
                                       ifelse(substring(Full.Dive.No, 5, 5) == "D", "Free", ""))))) %>%
  # Putting it together
  mutate(Dive.Name = paste(Second, " ", First, " - ", Third, " - ", Fourth, " - ", Fifth, sep = ""))
  
full.diving.names <- bind_rows(four_charac_dive, five_charac_dive) 

####  Bringing all the dataframes together ####

write.csv(test, file = 'diving british.csv')
  
  
  
  
  


                               
                               
                               
