# bowl and conference championship games to exclude

library(glue)
library(tidyverse)
library(rvest)
library(data.table)
library(lubridate)

rank_patterns <- paste('\\(', 1:25, '\\)', sep = '')

seasonsbowl <- 1960:year(Sys.Date())-1

BowlListFinal <- list()
for(i in seasonsbowl){

    Bowls <- read_html(glue('https://www.sports-reference.com/cfb/years/{i}-bowls.html'))
    
    BowlsList <- Bowls %>% 
      html_table(fill = TRUE) %>%
      as.data.frame()
    
    
    BowlsDF <- select(BowlsList, -contains("Pts")) %>% mutate(Season = i)
    
    BowlListFinal[[i]] <- BowlsDF
  
}

BowlDataFrame <- rbindlist(BowlListFinal, fill = TRUE) %>%
  unite(Var.3, c(Var.3, Var.4), sep = '', na.rm = TRUE) %>%
  unite(Var.5, c(Var.5, Var.6), sep = '', na.rm = TRUE)

BowlsCleaned <- BowlDataFrame %>%
  mutate(Var.3 = trim(stri_trim_both(str_remove_all(Var.3, paste(rank_patterns, collapse = "|")))),
         Var.5 = trim(stri_trim_both(str_remove_all(Var.5, paste(rank_patterns, collapse = "|"))))) %>%
  select(Season,
         Date,
         Var.3,
         Var.5) %>%
  mutate(Date = ymd(Date),
         Year = year(Date),
         Month = month(Date),
         Day = day(Date))

LosingBowls <- BowlsCleaned %>%
  select(Year,
         Month,
         Day,
         Season,
         Var.3 = Var.5,
         Var.5 = Var.3)

CompleteBowls <- bind_rows(BowlsCleaned, LosingBowls) %>%
  rename(School = Var.3,
         Opponent = Var.5) #%>%
  #mutate(Year = as.character(Year))

ConfChamp <- FullSchedule %>%
  filter(grepl('championship', Notes, ignore.case = TRUE)) %>%
  select(Month,
         Day,
         Year,
         Season,
         School,
         Opponent) %>%
  mutate(Year = as.integer(Year)) %>%
  bind_rows(CompleteBowls) %>%
  select(-Date) %>%
  mutate(Month = (trim(Month)),
         Day = (trim(Day)),
         Year = trim(Year),
         Season = (trim(Season)),
         School = trim(School),
         Opponent = trim(Opponent)) 

write.csv(ConfChamp, 'C:/Users/alexe/Desktop/Bowl Games.csv')




