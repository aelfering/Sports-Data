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

####  Swim & Dive Time Manipulation ####
swim_no_DiveRelays <- swim_dive %>%
  filter(!grepl("Relay|Diving", Event)) %>%
  mutate(Time = as.character(Time),
         Time_Count = nchar(Time),
         Name = gsub("Jack Allen", "Jackson Allen", Name)) #  Jackson Allen goes by Jack Allen and I need to standardize

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
swim_times$swim_decimal <- sapply(strsplit(swim_times$Time, ":"),
       function(x){
         x <- as.numeric(x)
         x[1] + x[2]/60
       }
)

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
         swim_decimal) %>%
  mutate(Date = dmy(Date),
         Name.Key = tolower(Name)) %>%
  mutate(Name.Key = gsub(" ", "", Name.Key, fixed = TRUE))



test <- inner_join(swim_times_data, roster_full_name, by = c('Name.Key' = 'Name.Key', 'Gender' = 'Gender'))







