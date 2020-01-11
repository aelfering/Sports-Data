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

####  Swim & Dive Time Manipulation ####
swim_no_relays <- dplyr::filter(swim_dive, !grepl("Relay|Diving",Event)) #  I want to focus solely on swimming and non-relay for now
swim_no_relays$Time <- as.character(swim_no_relays$Time)
swim_no_relays$Time_Count <- nchar(swim_no_relays$Time)

# The time column is a mix of two, four, five, or more character column and I needed 
# to manipulate the columns differently to get them into the same format.
two_count <- swim_no_relays %>%
  filter(Time_Count <= 2) %>%
  mutate(Time = paste("0:", Time, ".0", sep = ""))

four_five_count <- swim_no_relays %>%
  filter(Time_Count %in% c(4, 5)) %>%
  mutate(Time = paste("0:", Time, sep = ""))

other_count <- swim_no_relays %>%
  filter(Time_Count > 5)

swim_times <- bind_rows(two_count, four_five_count, other_count)

# Time conversion to minutes
swim_times$swim_decimal <- sapply(strsplit(swim_times$Time, ":"),
       function(x){
         x <- as.numeric(x)
         x[1] + x[2]/60
       }
)