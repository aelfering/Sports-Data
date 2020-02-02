# College Swimming and Diving Analysis
# By Alex Elfering
# Script Completed:

library(dplyr)
library(tidyverse)
library(tidyr)
library(tibble)
library(lubridate)

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

roster_name_list <- roster_full_name %>%
  filter(grepl(year(Sys.Date()), roster_full_name$Season)) %>%
  distinct(Name.Key, First.Name, Last.Name, Hometown, Gender, Class)

####  Swim Time Manipulation ####
swim_no_DiveRelays <- swim_dive %>%
  filter(!grepl("Relay|Diving", Event)) %>%
  filter(!grepl("Iowa Black & Gold Intrasquad", Event.Name)) %>%
  mutate(Time = as.character(Time),
         Time_Count = nchar(Time),
         Name = gsub("Jack Allen", "Jackson Allen", Name))
  
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

swim_roster_join <- inner_join(swim_times_data, roster_name_list, by = c('Name.Key' = 'Name.Key', 'Gender' = 'Gender'))

swim_clean <- swim_roster_join %>%
  mutate(Event.Category = 'Swimming') %>%
  select(Event.Category,
         Name, 
         First.Name,
         Last.Name,
         Name.Key,
         Gender, 
         Class, # Class starts with 2015-2016 roster
         Season, 
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
         Hometown)

swimmer_pr_times <- swim_clean %>%
  group_by(Name, Event) %>%
  mutate(Swimmer.Pr.Rank = dense_rank(Seconds)) %>%
  as.data.frame() %>%
  select(Name, Last.Name, Name.Key, Gender, Class, Season, Event, Event.Name, Round, Swimmer.Pr.Rank)

event_pr_times <- swim_clean %>%
  group_by(Event, Gender) %>%
  mutate(Event.PR.Rank = dense_rank(Seconds)) %>%
  as.data.frame()%>%
  select(Name, Last.Name, Name.Key, Gender, Class, Season, Event, Event.Name, Round, Event.PR.Rank)


swim_clean_swimmer_pr <- left_join(swim_clean, swimmer_pr_times, by = c('Name' = 'Name',
                                                                        'Last.Name' = 'Last.Name',
                                                                        'Name.Key' = 'Name.Key',
                                                                        'Gender' = 'Gender',
                                                                        'Class' = 'Class',
                                                                        'Season' = 'Season',
                                                                        'Event' = 'Event',
                                                                        'Event.Name' = "Event.Name",
                                                                        'Round' = 'Round'))

final_swimmer_dataset <- left_join(swim_clean_swimmer_pr, event_pr_times, by = c('Name' = 'Name',
                                                                        'Last.Name' = 'Last.Name',
                                                                        'Name.Key' = 'Name.Key',
                                                                        'Gender' = 'Gender',
                                                                        'Class' = 'Class',
                                                                        'Season' = 'Season',
                                                                        'Event' = 'Event',
                                                                        'Event.Name' = "Event.Name",
                                                                        'Round' = 'Round'))


####  Diving Manipulation ####

diving <- swim_dive %>%
  filter(grepl("Diving", Event)) %>%
  filter(!grepl("Iowa Black & Gold Intrasquad", Event.Name)) %>%
  filter(!Name %in% c('Iowa 1', 'Iowa Women 2', 'Uic 1', 'Iowa Women 1')) %>%
  # Fixing some name misspellings
  mutate(Name = gsub('Will Brenner', 'William Brenner', Name),
         Name = gsub('Sam Tamborski', 'Samantha Tamborski', Name),
         Name = gsub('Matt Mauser', 'Matthew Mauser', Name),
         Name = gsub('Mohamed Noaman', 'Mohamed Neuman', Name),
         Name = gsub('Mohamad Neuman', 'Mohamed Neuman', Name),
         Name = gsub('Anton Hoerz', 'Anton Hoherz', Name)) %>%
  # Standardizing the event names
  mutate(Event = gsub(' (6 dives)', '', Event, fixed = TRUE)) %>%
  mutate(Name.Key = gsub(" ", "", tolower(Name)),
         Date = dmy(Date))

dive_set <- inner_join(diving, roster_name_list, by = c('Name.Key' = 'Name.Key', 'Gender' = 'Gender'))

dive_clean <- dive_set %>%
  mutate(Event.Category = 'Diving',
         Seconds = as.numeric(as.character(Time))) %>%
  mutate(Minutes = 0,
         Time = '') %>%
  select(Event.Category,
         Name,
         First.Name,
         Last.Name,
         Name.Key,
         Gender,
         Class,
         Season,
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
         Hometown)

diver_pr <- dive_clean %>%
  group_by(Name, Event) %>%
  mutate(Swimmer.Pr.Rank = dense_rank(Seconds)) %>%
  ungroup() %>%
  select(Name, Last.Name, Name.Key, Gender, Class, Season, Event, Event.Name, Round, Swimmer.Pr.Rank) %>%
  as.data.frame()

event_pr <- dive_clean %>%
  group_by(Event, Gender) %>%
  mutate(Event.PR.Rank = dense_rank(Seconds)) %>%
  ungroup() %>%
  as.data.frame()%>%
  select(Name, Last.Name, Name.Key, Gender, Class, Season, Event, Event.Name, Round, Event.PR.Rank)

dive_clean_diver_pr <- left_join(dive_clean, diver_pr, by = c('Name' = 'Name',
                                                                        'Last.Name' = 'Last.Name',
                                                                        'Name.Key' = 'Name.Key',
                                                                        'Gender' = 'Gender',
                                                                        'Class' = 'Class',
                                                                        'Season' = 'Season',
                                                                        'Event' = 'Event',
                                                                        'Event.Name' = "Event.Name",
                                                                        'Round' = 'Round'))

final_diver_dataset <- left_join(dive_clean_diver_pr, event_pr, by = c('Name' = 'Name',
                                                                                 'Last.Name' = 'Last.Name',
                                                                                 'Name.Key' = 'Name.Key',
                                                                                 'Gender' = 'Gender',
                                                                                 'Class' = 'Class',
                                                                                 'Season' = 'Season',
                                                                                 'Event' = 'Event',
                                                                                 'Event.Name' = "Event.Name",
                                                                                 'Round' = 'Round'))


####  Exporting Data ####
iowa_swim_dive_data <- bind_rows(final_diver_dataset, final_swimmer_dataset)
  
write.csv(iowa_swim_dive_data, file = 'Swimming and Dive.csv')
  


####  Best times per date by swimmer ####
columns_needed <- iowa_swim_dive_data %>%
  select(Name, 
         Name.Key,
         Gender,
         Class,
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
         Season,
         Course)

# How many times has a swimmer competed in an event?
times_competed <- columns_needed %>%
  distinct(Name, Event.Name, Event) %>%
  arrange(Name, Event) %>%
  select(Name, Event) %>%
  group_by(Name, Event) %>%
  summarise(Times.Competed = n()) %>%
  ungroup() %>%
  filter(Times.Competed >= 2) %>%
  as.data.frame()

swimmer_best_times_meet <- columns_needed %>%
  group_by(Season, Name, Event.Name, Event) %>%
  slice(which.min(Seconds)) %>%
  ungroup() %>%
  group_by(Name, Event) %>%
  mutate(Number = row_number()) %>%
  ungroup() %>%
  as.data.frame()

frequent_events <- inner_join(swimmer_best_times_meet, times_competed, by = c('Name' = 'Name',
                                                                              'Event' = 'Event'))

first_event_swim_time <- frequent_events %>%
  filter(Number == 1) %>%
  select(Name,
         First_Time = Seconds)

write.csv(frequent_events, file = 'frequent_swimmer_events.csv')
