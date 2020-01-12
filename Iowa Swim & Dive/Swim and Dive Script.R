# College Swimming and Diving Analysis
# By Alex Elfering
# Script Completed:

library(dplyr)
library(tidyverse)
library(tidyr)

swim_dive <- read.csv('iowa swimming.csv')
roster <- read.csv('Iowa Roster.csv')

####  Roster Manipulation ####
# Remove the initials and order the name column as "First, Last"
roster$Name <- gsub("(.*)\\s+[A-Z]\\.?$", "\\1", roster$Name)
roster_separate_names <- separate(data = roster, col = Name, into = c("Last.Name", "First.Name"), sep = "\\,")
roster_full_name <- dplyr::mutate(roster_separate_names, Full.Name = paste(First.Name, Last.Name, sep = " "))
roster_full_roster <- dplyr::select(roster_full_name, Season, Full.Name, Last.Name, Hometown, Class, Gender)

roster_full_name$Name.Key <- tolower(roster_full_roster$Full.Name)

roster_full_name$Name.Key <- gsub(" ", "", roster_full_name$Name.Key, fixed = TRUE)

####  Swim Time Manipulation ####
swim_no_DiveRelays <- swim_dive %>%
  filter(!grepl("Relay|Diving", Event)) %>%
  mutate(Time = as.character(Time),
         Time_Count = nchar(Time),
         Name = gsub("Jack Allen", "Jackson Allen", Name)) %>% #  Jackson Allen goes by Jack Allen and I need to standardize
  
# The time column is a mix of two, four, five, or more character column and I needed 
# to manipulate the columns differently to get them into the same format.
two_count <- swim_no_DiveRelays %>%
  filter(Time_Count <= 2) %>%
  mutate(Time = paste("0:", Time, ".0", sep = ""))

four_five_count <- swim_no_DiveRelays %>%
  filter(Time_Count %in% c(4, 5)) %>%
  mutate(Time = paste("0:", Time, sep = ""))

other_count <- swim_no_DiveRelays %>%
  filter(Time_Count > 5)

swim_times <- bind_rows(two_count, four_five_count, other_count)

# Time conversion to minutes
swim_times$Minutes <- sapply(strsplit(swim_times$Time, ":"),
       function(x){
         x <- as.numeric(x)
         x[1] + x[2]/60
       }
)

# Time conversion to seconds
swim_times$Seconds <- period_to_seconds(hms(paste("0:", swim_times$Time, sep = "")))

swim_times_data <- swim_times %>%
  select(Name,
         Event,
         Round,
         Place,
         Time,
         Flag, 
         Pts, 
         Event.Name,
         Date, 
         Course,
         Gender,
         Season,
         Minutes,
         Seconds) %>%
  mutate(Date = dmy(Date),
         Name.Key = tolower(Name)) %>%
  mutate(Name.Key = gsub(" ", "", Name.Key, fixed = TRUE))

swim_roster_join <- inner_join(swim_times_data, roster_full_name, by = c('Name.Key' = 'Name.Key', 'Gender' = 'Gender'))

swim_with_ranks <- swim_roster_join %>%
  mutate(Event.Category = 'Swimming') %>%
  select(Event.Category,
         Name, 
         Last.Name,
         Name.Key,
         Gender, 
         Class, # Class starts with 2015-2016 roster
         Season = Season.x, 
         Event, 
         Round, 
         Place, 
         Time, 
         Minutes,
         Seconds,
         Flag, 
         Pts, 
         Event.Name, 
         Date, 
         Course,
         Hometown) %>%
  # PR Rank
  group_by(Name, Event) %>%
  mutate(PR.Rank = dense_rank(Seconds)) %>%
  ungroup() %>%
  # Best Swimmer per Event
  group_by(Event) %>%
  mutate(Event.Rank = dense_rank(Seconds)) %>%
  ungroup()

####  Diving Manipulation ####
# Let's 'dive' into the diver section
# Sorry, I had to...

diving <- swim_dive %>%
  filter(grepl("Diving", Event)) %>%
  filter(!Name %in% c('Iowa 1', 'Iowa Women 2', 'Uic 1', 'Iowa Women 1')) %>%
  # Fixing some name misspellings
  mutate(Name = gsub('Will Brenner', 'William Brenner', Name),
         Name = gsub('Sam Tamborski', 'Samantha Tamborski', Name),
         Name = gsub('Matt Mauser', 'Matthew Mauser', Name),
         Name = gsub('Mohamed Noaman', 'Mohamed Neuman', Name),
         Name = gsub('Mohamad Neuman', 'Mohamed Neuman', Name),
         Name = gsub('Anton Hoerz', 'Anton Hoherz', Name)) %>%
  mutate(Name.Key = gsub(" ", "", tolower(Name)))

dive_set <- inner_join(diving, roster_full_name, by = c('Name.Key' = 'Name.Key'))
  
write.csv(swim_with_ranks, file = 'Swimming and Dive.csv')  
  
  






