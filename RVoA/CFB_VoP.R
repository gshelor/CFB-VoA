##### Vortex of Projection version 0.1
### This script will take the most recent csv from the VoA
### It will take the VoA Ratings from that csv and use them to project scoring margins for upcoming FBS games
## loading packages
library(pacman)
pacman::p_load(tidyverse, matrixStats, grid, gridExtra, gt, viridis, 
               webshot, writexl, rvest, cfbfastR, espnscrapeR, openxlsx, 
               here, ggsci, RColorBrewer, ggpubr, gtExtras, tidymodels, ranger)
## Inputting upcoming week number
upcoming <- readline(prompt = "What week is upcoming? ")

## reading in appropriate csv based on upcoming week
if (as.numeric(upcoming) == 1) {
  PrevWeek_VoA <- read_csv(here("Data", "VoA2022", "2022Week0_VoA.csv"))
} else if (as.numeric(upcoming) == 2) {
  PrevWeek_VoA <- read_csv(here("Data", "VoA2022", "2022Week1_VoA.csv"))
} else if (as.numeric(upcoming) == 3) {
  PrevWeek_VoA <- read_csv(here("Data", "VoA2022", "2022Week2_VoA.csv"))
} else if (as.numeric(upcoming) == 4) {
  PrevWeek_VoA <- read_csv(here("Data", "VoA2022", "2022Week3_VoA.csv"))
} else if (as.numeric(upcoming) == 5) {
  PrevWeek_VoA <- read_csv(here("Data", "VoA2022", "2022Week4_VoA.csv"))
} else if (as.numeric(upcoming) == 6) {
  PrevWeek_VoA <- read_csv(here("Data", "VoA2022", "2022Week5_VoA.csv"))
} else if (as.numeric(upcoming) == 7) {
  PrevWeek_VoA <- read_csv(here("Data", "VoA2022", "2022Week6_VoA.csv"))
} else if (as.numeric(upcoming) == 8) {
  PrevWeek_VoA <- read_csv(here("Data", "VoA2022", "2022Week7_VoA.csv"))
} else if (as.numeric(upcoming) == 9) {
  PrevWeek_VoA <- read_csv(here("Data", "VoA2022", "2022Week8_VoA.csv"))
} else if (as.numeric(upcoming) == 10) {
  PrevWeek_VoA <- read_csv(here("Data", "VoA2022", "2022Week9_VoA.csv"))
} else if (as.numeric(upcoming) == 11) {
  PrevWeek_VoA <- read_csv(here("Data", "VoA2022", "2022Week10_VoA.csv"))
} else if (as.numeric(upcoming) == 12) {
  PrevWeek_VoA <- read_csv(here("Data", "VoA2022", "2022Week11_VoA.csv"))
} else if (as.numeric(upcoming) == 13) {
  PrevWeek_VoA <- read_csv(here("Data", "VoA2022", "2022Week12_VoA.csv"))
} else if (as.numeric(upcoming) == 14) {
  PrevWeek_VoA <- read_csv(here("Data", "VoA2022", "2022Week13_VoA.csv"))
} else {
  PrevWeek_VoA <- read_csv(here("Data", "VoA2022", "2022Week14_VoA.csv"))
}

## simple function to take VoA Ratings as inputs
margin_projection <- function(away, home, neutral) {
  margin_proj = PrevWeek_VoA$VoA_Rating[PrevWeek_VoA$team == away] -  PrevWeek_VoA$VoA_Rating[PrevWeek_VoA$team == home]
  if (neutral == FALSE) {
    margin_proj = margin_proj - 2.5
  }
  return(margin_proj)
}

upcoming_games_df <- cfbd_game_info(2022, week = 1) %>%
  filter(home_team %in% PrevWeek_VoA$team | away_team %in% PrevWeek_VoA$team)
