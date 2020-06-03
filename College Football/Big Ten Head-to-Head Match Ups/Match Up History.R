# The History of the Big Ten Conference

# This script is designed to calculate the percent of games won in head-to-head match ups between Big Ten teams
# End Product is a Heat Map
# Source of Data: College Football Reference
# By: Alex Elfering
# Initially Published: 3 June 2020

library(RColorBrewer)
library(grid)
library(ggplot2)
library(tidyr)
library(dplyr)
library(tidyverse)

big_ten <- read.csv('history big ten.csv')

# Function to trim the leading and trailing white spaces
trim.leading <- function (x)  sub("^\\s+", "", x)

# Ranks to identify and remove from Team and Opponent names
ap_ranks <- c("\\(1\\)|\\(2\\)|\\(3\\)|\\(4\\)|\\(5\\)|\\(6\\)|\\(7\\)|\\(8\\)|\\(9\\)|\\(10\\)|\\(11\\)|\\(12\\)|\\(13\\)|\\(14\\)|\\(15\\)|\\(16\\)|\\(17\\)|\\(18\\)|\\(19\\)|\\(20\\)|\\(21\\)|\\(22\\)|\\(23\\)|\\(24\\)|\\(25\\)")

# Clean up the data frame and calculate wins, losses, and ties between head-to-head opponents
all_time_record <- big_ten %>%
  # Data cleaning: Removing the Rank from the Team and Opponent Name
  mutate(Conference = as.character(Conference),
         Conf = as.character(Conf),
         School = str_replace_all(School, ap_ranks, ""),
         Opponent = str_replace_all(Opponent, ap_ranks, "")) %>%
  mutate(School = trim.leading(School),
         Opponent = trim.leading(Opponent)) %>%
  # Filtering for Conference Match Ups and Most Recent ~20 Years
  filter(Conference == Conf) %>%
  # Summing Wins, Losses, and Ties Between Teams in Head-to-Head Matchups
  select(Season,
         G,
         Date,
         School,
         Location = X,
         Opponent,
         Conf,
         Result = X.1,
         Pts,
         Opp) %>%
  mutate(Margin = Pts-Opp,
         Wins = ifelse(Result == 'W', 1, 0),
         Loses = ifelse(Result == 'L', 1, 0),
         Ties = ifelse(Result == 'T', 1, 0)) %>%
  group_by(School,
           Opponent) %>%
  summarise(Total_Wins = sum(Wins),
            Total_Losses = sum(Loses),
            Total_Ties = sum(Ties)) %>%
  ungroup() %>%
  # The final calculation
  mutate(Percent_Won = (Total_Wins /(Total_Losses + Total_Ties + Total_Wins)),
         Percent_Buckets = ifelse(Percent_Won <= 0.25, '0-25%',
                                  ifelse(Percent_Won <= 0.50, '25-50%',
                                         ifelse(Percent_Won <= 0.75, '50-75%', '75-100%'))))

# The Visualization
perc_wins_losses <- ggplot(all_time_record, 
       aes(Opponent, 
           School, 
           fill = Percent_Buckets)) + 
  # Establish Tiles
  geom_tile(color = 'white',
            size = 1,
            aes(width = 0.95, 
                height = 0.95)) +
  # Set color for buckets
  scale_fill_manual(values = c('#c6c6c6', '#4bacc5', '#1985a1', '#005f79')) +
  # Titles and labels
  labs(x = '\n...Against Each Conference Opponent.',
       y = '\nHow the Team Performs...',
       title = 'Percent of Games Won Between Big Ten Teams',
       subtitle = 'Since the Conference was Renamed Big Nine/Ten',
       caption = 'Visualization by Alex Elfering\nSource: College Football Reference') +
  # Theme adjustments
  theme(plot.title = element_text(face = 'bold', 
                                  size = 18, 
                                  family = 'Arial'),
        legend.position = 'top',
        axis.text.x = element_text(angle = 270, 
                                   vjust = 0.5, 
                                   hjust = 0, 
                                   size = 10),
        axis.title.y = element_text(angle = 270, 
                                    hjust = 0.5, 
                                    vjust = 0.5, 
                                    size = 12),
        axis.title.x = element_text(hjust = -1, 
                                    vjust = 0.5, 
                                    size = 12),
        plot.subtitle = element_text(size = 15, 
                                     family = 'Arial'),
        plot.caption = element_text(size = 10, 
                                    family = 'Arial'),
        axis.title = element_text(size = 10, 
                                  family = 'Arial'),
        axis.text = element_text(size = 10, 
                                 family = 'Arial'),
        strip.text = ggplot2::element_text(size = 10, 
                                           hjust = 0, 
                                           face = 'bold', 
                                           color = 'brown', 
                                           family = 'Arial'),
        strip.background = element_rect(fill = NA),
        panel.background = ggplot2::element_blank(),
        axis.line = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank()) 

perc_wins_losses

#print(mark1, vp = grid::viewport(width = 0.5, height=0.5, angle = -45))

