# The History of Minnesota Football by Coach
# Small Multiples Visualization
# Script By Alex Elfering

library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)

# Load the data set
msp <- read.csv('minnesota fb history.csv')

# Identify every season each coach led, even if they were interim
coach_yrs <- msp %>%
  distinct(Coach,
           Season) %>%
  mutate(Coach = as.character(Coach)) %>%
  mutate(Coach = ifelse(Coach == 'Bernie Bierman' & Season <= 1941, 'Bernie Bierman (1st)',
                        ifelse(Coach == 'Bernie Bierman' & Season > 1942, 'Bernie Bierman (2nd)', Coach))) %>%
  group_by(Coach) %>%
  mutate(Year_No = row_number(),
         First_Season = (min(Season)),
         Latest_Season = (max(Season))) %>%
  ungroup() %>%
  # Bernie Bierman was Minnesota's coach from 1932-1941 and 1945-1950
  # Need to identify those breaks and separate each tenure
  mutate(First_Season = factor(First_Season),
         Latest_Season = factor(Latest_Season),
         Coach = as.factor(Coach))

# Calculate the rolling margin for every 11 games (modern teams play between 11-12 games each season)
coach_roll <- msp %>%
  select(Conference,
         Coach,
         Season,
         Date,
         Opponent,
         Opp.Conference,
         Result,
         Points,
         Opp.Points) %>%
  mutate(Coach = as.character(Coach)) %>%
  mutate(Coach = ifelse(Coach == 'Bernie Bierman' & Season <= 1941, 'Bernie Bierman (1st)',
                        ifelse(Coach == 'Bernie Bierman' & Season > 1942, 'Bernie Bierman (2nd)', Coach))) %>%
  mutate(Coach = as.factor(Coach)) %>%
  group_by(Coach) %>%
  mutate(Coach_Game_No = row_number(),
         Games_Coached = n_distinct(Date),
         Running.Margin = cumsum(Points - Opp.Points)) %>%
  mutate(Wins = ifelse(Points > Opp.Points, 1, 0),
         Loses = ifelse(Points < Opp.Points, 1, 0),
         Running.Win.Ct = cumsum(Wins),
         Percent.Won = Running.Win.Ct/Games_Coached) %>%
  filter(Games_Coached > 6) %>%
  ungroup() %>%
  mutate(Margin = Points - Opp.Points,
         Rolling.Margin = rollapplyr(Margin, 11, sum, partial = TRUE),
         Rollings.Wins = rollapplyr(Wins, 11, mean, partial = TRUE),
         Coach_Name = Coach) 

# Joining the dataframes together
coach_df <- inner_join(coach_roll, coach_yrs, by = c('Coach' = 'Coach', 'Season' = 'Season'))
coach_df <- dplyr::mutate(coach_df, Coach = paste(Coach, " (", First_Season, '-', Latest_Season, ")", sep = ''))
coach_df <- dplyr::mutate(coach_df, Coach_Name = Coach)

# Who are the winningest coaches of all time (for Minnesota)?
winningest_coaches <- coach_roll %>%
  group_by(Coach) %>%
  slice(which.max(Coach_Game_No)) %>%
  ungroup() %>%
  mutate(Percent.Wins = Rollings.Wins/Coach_Game_No) %>%
  select(Coach,
         Percent.Wins) %>%
  filter(row_number() <= 3)

winningest_coaches_names <- as.character(winningest_coaches$Coach)

top_coaches <- dplyr::filter(coach_df, Coach %in% winningest_coaches_names)

# Order the facets by the coach's first season
coach_df$Coach <- factor(coach_df$Coach, levels = unique(coach_df$Coach[order(coach_df$First_Season)]))

# The visualization!
# Using geom_line() and transform() to create a small multiples graph that highlights each coach by facet
ggplot(coach_df, 
       aes(x = Coach_Game_No, 
           y = Rolling.Margin,
           group = Coach)) + 
  geom_line(data = transform(coach_df,
                             Coach = NULL), 
            aes(group = Coach_Name), 
            size = 1,
            color = 'gray',
            alpha = 0.6) +   
  geom_hline(yintercept = 0,
             color = '#898989',
             linetype = 'dashed') +
  geom_line(aes(group = Coach), 
            color = 'brown',
            size = 1) + 
  scale_colour_identity() + 
  facet_wrap(~Coach) +
  #~factor(df$id, levels = unique(df$id))
  labs(title = 'The History of Minnesota Football by Coach',
       x = 'Total Games Coached',
       y = 'Rolling Margin',
       subtitle = 'Rolling Plus-Minus Every 11 Games For Coaches Who Led More Than 6 Games',
       caption = 'Since Minnesota joined the Western/Big Ten Conference.\nSource: College Football Reference\nVisualization by Alex Elfering') + 
  theme(plot.title = element_text(face = 'bold', size = 18, family = 'Arial'),
        plot.subtitle = element_text(size = 15, family = 'Arial'),
        plot.caption = element_text(size = 12, family = 'Arial'),
        axis.title = element_text(size = 12, family = 'Arial'),
        axis.text = element_text(size = 12, family = 'Arial'),
        strip.text = ggplot2::element_text(size = 12, hjust = 0, face = 'bold', color = 'brown', family = 'Arial'),
        strip.background = element_rect(fill = NA),
        panel.background = ggplot2::element_blank(),
        axis.line = element_line(colour = "#222222", linetype = "solid"),
        panel.grid.major.y = ggplot2::element_line(color = "#dedede", linetype = 'dashed'),
        panel.grid.major.x = element_blank()) 



