library(reactable)
library(htmltools)
library(tidyverse)
library(glue)

#source("~/New CFB.R")
#source("~/GitHub/Sports-Data/College Football/ELO Ratings/Bowl Games.R")
#source("~/GitHub/Sports-Data/College Football/ELO Ratings/Predict Games.R")

# dataset ----
SeasonDFTable <- rbindlist(SeasonForecastWeeks)

Season <- max(SeasonDFTable$Season)
WeekNumber <- max(SeasonDFTable$Week)

PreSeason <- SeasonDFTable %>%
  filter(!grepl("NA", RealRecord)) %>%
  group_by(School) %>%
  filter(Week == 12) %>%
  ungroup() %>%
  #select(-Season) %>%
  mutate(ForecastedWins = round(ForecastedWins),
         Div = ifelse(Div == '', NA, Div),
         ForecastedLosses = round(ForecastedLosses),
         ForecastedTies = round(ForecastedTies),
         ForecastedConfWins = round(ForecastedConfWins),
         ForecastedConfLosses = round(ForecastedConfLosses),
         ForecastedConfTies = round(ForecastedConfTies),
         FirstinConfDiv = round(FirstinConfDiv/Sims, 4),
         WinConfDivOR = round(WinConfDivOR/Sims, 4),
         FinishesOut = round(FinishesOut/Sims, 4),
         Win0500 = round(Win0500/Sims, 4)) %>%
  group_by(Conf,
           Div) %>%
  mutate(DivisionRank = dense_rank(desc(ForecastedConfWins))) %>%
  ungroup() %>%
  mutate(ForecastedTies = ifelse(ForecastedTies == 0, NA, ForecastedTies),
         ForecastedConfTies = ifelse(ForecastedConfTies == 0, NA, ForecastedConfTies)) %>%
  unite(SimulatedRecord, c('ForecastedWins', 'ForecastedLosses', 'ForecastedTies'), sep = '-', na.rm = TRUE) %>%
  unite(SimulatedConference, c('ForecastedConfWins', 'ForecastedConfLosses', 'ForecastedConfTies'), sep = '-', na.rm = TRUE) %>%
  mutate(FirstinConfDiv = ifelse(is.na(FirstinConfDiv), 0, FirstinConfDiv),
         RealRecord = gsub("\\'", '', RealRecord)) %>%
  #filter(Conf == 'Big Ten') %>%
  unite(ConferenceDivision, c('Conf', 'Div'), sep = ' - ', na.rm = TRUE) %>%
  dplyr::select(                School,
                                RealRecord,
                                ConferenceDivision,
                                SimulatedRecord,
                                SimulatedConference,
                                Win0500,
                                FinishesOut,
                                FirstinConfDiv,
                                WinConfDivOR,
                                NextOpponent) %>%
  mutate(WinConfDivOR= ifelse(is.na(WinConfDivOR), 0, WinConfDivOR),
         FinishesOut = ifelse(is.na(FinishesOut), 0, FinishesOut),
         FirstinConfDiv = ifelse(is.na(FirstinConfDiv), 0, FirstinConfDiv))

# table formatting  ----
knockout_cols <- c("FirstinConfDiv", "WinConfDivOR", "Win0500", "FinishesOut")
simulationCols <- c("SimulatedRecord", "SimulatedConference")

format_pct <- function(value) {
  
  # formats percents for each probability column
  
  if (value == 0) "  \u2013 "    # en dash for 0%
  else if (value == 1) "\u2713"  # checkmark for 100%
  else if (value < 0.001) " <0.1%"
  else if (value > 0.99) ">99%"
  else formatC(paste0(round(value * 100, 1), "%"), width = 4)
  
}
format_winsout <- function(value){
  
  # formats the wins out column
  
  if (value == 0) "  \u2013 "    # en dash for 0%
  else if (value == 1) "\u2713"  # checkmark for 100%
  else if (value < 0.01) " <1%"
  else if (value > 0.99) ">99%"
  else formatC(paste0(round(value * 100), "%"), width = 4)
  
}
make_color_pal <- function(colors, bias = 1) {
  get_color <- colorRamp(colors, bias = bias)
  function(x) rgb(get_color(x), maxColorValue = 255)
}

off_rating_color <- make_color_pal(c("#ff2700", "#f8fcf8", "#44ab43"), bias = 1.3)
def_rating_color <- make_color_pal(c("#ff2700", "#f8fcf8", "#44ab43"), bias = 0.6)
probability_colors <- make_color_pal(c("#fef0d9", "#fdcc8a", "#fc8d59", "#e34a33", "#b30000"), bias = 2)

winsout_columns <- function(class = NULL, ...){
  colDef(
    cell = format_pct,
    maxWidth = 150,
    class = paste("cell number", class),
    style = function(value) {
      # Lighter color for <1%
      if (value > 0.00) {
        list(color = "#aaa")
      } else {
        list(color = "#aaa")
      }
    },
    ...
  )
}
knockout_column <- function(maxWidth = 310, class = NULL, ...) {
  colDef(
    cell = format_pct,
    maxWidth = maxWidth,
    class = paste("cell number", class),
    style = function(value) {
      # Lighter color for <1%
      if (value < 0.001) {
        list(color = "#aaa")
      } else {
        list(color = "#111", background = probability_colors(value))
      }
    },
    ...
  )
}

# table ----
SeasonReacTable <- reactable(PreSeason,
                             pagination = FALSE,
                             outlined = TRUE,
                             highlight = TRUE,
                             wrap = TRUE,
                             defaultSorted = list(ConferenceDivision = 'asc', FirstinConfDiv = 'desc'),
                             defaultColDef = colDef(headerClass = "header", 
                                                    align = "left"),
                             style = list(fontFamily = 'Franklin Gothic Book', 
                                          fontSize = '18px'),
                             columnGroups = list(
                               colGroup(name = "Odds that the team...", columns = knockout_cols),
                               colGroup(name = 'Simulated...', columns = simulationCols)
                             ),
                             theme = reactableTheme(
                               headerStyle = list(
                                 "&:hover[aria-sort]" = list(background = "hsl(0, 0%, 96%)"),
                                 "&[aria-sort='ascending'], &[aria-sort='descending']" = list(background = "hsl(0, 0%, 96%)"),
                                 borderColor = "#555"
                               )),
                             columns = list(
                               ConferenceDivision = colDef(
                                 name = 'Conference',
                                 width = 210,
                                 style = JS("function(rowInfo, colInfo, state) {
        var firstSorted = state.sorted[0]
        // Merge cells if unsorted or sorting by school
        if (!firstSorted || firstSorted.id === 'ConferenceDivision') {
          var prevRow = state.pageRows[rowInfo.viewIndex - 1]
          if (prevRow && rowInfo.row['ConferenceDivision'] === prevRow['ConferenceDivision']) {
            return { visibility: 'hidden' }
          }
        }
      }")
                               ),
                               #ConferenceDivision = colDef(name = 'Conference'),
                               WinConfDivOR = knockout_column(name = 'Wins the Division Outright'),
                               School = colDef(width = 210),
                               DivisionRank = colDef(width = 110, name = 'Division Rank'),
                               NextOpponent = colDef(width = 210, name = 'Next Game'),
                               Win0500 = knockout_column(name = 'Wins >=50% of games'),
                               FinishesOut = winsout_columns(name = 'Wins Out'),
                               FirstinConfDiv = knockout_column(name = 'Wins a share of the Division'),
                               SimulatedRecord = colDef(name = 'Regular Season', width = 135),
                               RealRecord = colDef(name = 'Current Record', width = 100),
                               SimulatedConference = colDef(name = 'Conference', width = 135) 
                             )
)

withtitle <- htmlwidgets::prependContent(SeasonReacTable, 
                                         h1(class = "title", 
                                            style = "font-family: Franklin Gothic Book",
                                            glue("{Season} College Football Predictions after Week {WeekNumber}") ))

withtitle

library(htmlwidgets)
library(webshot)

saveWidget(widget = withtitle, file = "~/College Football Team Forecasts/table.png", selfcontained = TRUE)
saveWidget(withtitle, html)
html <- "table.html"
webshot(html, glue('~/College Football Team Forecasts/{Season} Conference Forecast after Week {WeekNumber}.png'))

