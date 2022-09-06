##### Vortex of Projection version 0.1
### This script will take the most recent csv from the VoA
### It will take the VoA Ratings from that csv and use them to project scoring margins for upcoming FBS games
## loading packages
library(pacman)
pacman::p_load(tidyverse, gt, cfbfastR, here, gtExtras)
## testing taking packages out to see which ones I'm actually using in this script
# openxlsx, writexl, rvest, webshot, RColorBrewer, tidymodels, ranger, grid, gridExtra,
# matrixStats, viridis,ggsci, ggpubr, 
## Inputting year
year <- readline(prompt = "What Year is it? ")
## Inputting upcoming week number
upcoming <- readline(prompt = "What week is upcoming? ")

## reading in appropriate csv based on upcoming week
if (as.numeric(upcoming) == 1) {
  PrevWeek_VoA <- read_csv(here("Data", "VoA2022", "2022Week0_VoA.csv")) %>%
    select(team, VoA_Rating)
} else if (as.numeric(upcoming) == 2) {
  PrevWeek_VoA <- read_csv(here("Data", "VoA2022", "2022Week1_VoA.csv")) %>%
    select(team, VoA_Rating)
} else if (as.numeric(upcoming) == 3) {
  PrevWeek_VoA <- read_csv(here("Data", "VoA2022", "2022Week2_VoA.csv")) %>%
    select(team, VoA_Rating)
} else if (as.numeric(upcoming) == 4) {
  PrevWeek_VoA <- read_csv(here("Data", "VoA2022", "2022Week3_VoA.csv")) %>%
    select(team, VoA_Rating)
} else if (as.numeric(upcoming) == 5) {
  PrevWeek_VoA <- read_csv(here("Data", "VoA2022", "2022Week4_VoA.csv")) %>%
    select(team, VoA_Rating)
} else if (as.numeric(upcoming) == 6) {
  PrevWeek_VoA <- read_csv(here("Data", "VoA2022", "2022Week5_VoA.csv")) %>%
    select(team, VoA_Rating)
} else if (as.numeric(upcoming) == 7) {
  PrevWeek_VoA <- read_csv(here("Data", "VoA2022", "2022Week6_VoA.csv")) %>%
    select(team, VoA_Rating)
} else if (as.numeric(upcoming) == 8) {
  PrevWeek_VoA <- read_csv(here("Data", "VoA2022", "2022Week7_VoA.csv")) %>%
    select(team, VoA_Rating)
} else if (as.numeric(upcoming) == 9) {
  PrevWeek_VoA <- read_csv(here("Data", "VoA2022", "2022Week8_VoA.csv")) %>%
    select(team, VoA_Rating)
} else if (as.numeric(upcoming) == 10) {
  PrevWeek_VoA <- read_csv(here("Data", "VoA2022", "2022Week9_VoA.csv")) %>%
    select(team, VoA_Rating)
} else if (as.numeric(upcoming) == 11) {
  PrevWeek_VoA <- read_csv(here("Data", "VoA2022", "2022Week10_VoA.csv")) %>%
    select(team, VoA_Rating)
} else if (as.numeric(upcoming) == 12) {
  PrevWeek_VoA <- read_csv(here("Data", "VoA2022", "2022Week11_VoA.csv")) %>%
    select(team, VoA_Rating)
} else if (as.numeric(upcoming) == 13) {
  PrevWeek_VoA <- read_csv(here("Data", "VoA2022", "2022Week12_VoA.csv")) %>%
    select(team, VoA_Rating)
} else if (as.numeric(upcoming) == 14) {
  PrevWeek_VoA <- read_csv(here("Data", "VoA2022", "2022Week13_VoA.csv")) %>%
    select(team, VoA_Rating)
} else {
  PrevWeek_VoA <- read_csv(here("Data", "VoA2022", "2022Week14_VoA.csv")) %>%
    select(team, VoA_Rating)
}

## creating random distribution to create artificial VoA rating to be used for FCS teams
set.seed(69)
FCS_random <- rnorm(130, mean = mean(min(PrevWeek_VoA$VoA_Rating) + 10, median(PrevWeek_VoA$VoA_Rating) + 10, mean(PrevWeek_VoA$VoA_Rating) + 10), sd = sd(PrevWeek_VoA$VoA_Rating))
## reading in upcoming games to create df of games and VoA projected margins
upcoming_games_df <- cfbd_game_info(2022, week = as.numeric(upcoming)) %>%
  filter(home_team %in% PrevWeek_VoA$team | away_team %in% PrevWeek_VoA$team) %>%
  select(game_id, season, week, neutral_site, home_team, away_team)

## making separate home and away team dfs to do anti_joins to find teams with no VoA_Rating
# should be entirely FCS teams if VoA is running properly
upcoming_games_home_teams <- upcoming_games_df
colnames(upcoming_games_home_teams) <- c("game_id", "season", "week", "neutral_site", "team", "away_team")
upcoming_games_away_teams <- upcoming_games_df
colnames(upcoming_games_away_teams) <- c("game_id", "season", "week", "neutral_site", "ignore_team", "team")
## doing an anti_join to find FCS teams
missing_home_ratings <- anti_join(upcoming_games_home_teams, PrevWeek_VoA, by = "team")
missing_away_ratings <- anti_join(upcoming_games_away_teams, PrevWeek_VoA, by = "team")
## trying to use FCS_random to supply FCS VoA Ratings since VoA doesn't actually rank FCS teams
## need to store randomly sampled in a vector
## might be another way to do this without all these steps but I, a doofus, do not know them
## unfortunately this means if multiple FCS teams play FBS teams in a given week, all the FCS teams will have the same randomly assigned VoA rating
## I don't like that but I'll have to come up with a better way to do it later

## assigning random VoA_Ratings to missing home teams who are not part of VoA
if (nrow(missing_home_ratings) > 0) {
  missing_home_ratings <- missing_home_ratings %>%
    mutate(VoA_Rating = sample(FCS_random, nrow(missing_home_ratings), replace = TRUE)) %>%
    select(team, VoA_Rating)
} else {
  print("No FCS teams play FBS teams at home this week.")
}
## assigning random VoA_Ratings to missing away teams who are not part of VoA
if (nrow(missing_away_ratings) > 0) {
  missing_away_ratings <- missing_away_ratings %>%
    mutate(VoA_Rating = sample(FCS_random, nrow(missing_away_ratings), replace = TRUE)) %>%
    select(team, VoA_Rating)
} else {
  print("No FCS teams play FBS teams on the road this week.")
}

## binding random FCS ratings to PrevWeek_VoA, assigning it to same df name
if (nrow(missing_home_ratings) > 0) {
  PrevWeek_VoA <- rbind(PrevWeek_VoA, missing_home_ratings)
} else {
  print("No FCS teams play FBS teams at home this week.") 
}
if (nrow(missing_away_ratings) > 0) {
  PrevWeek_VoA <- rbind(PrevWeek_VoA, missing_away_ratings)
} else {
  print("No FCS teams play FBS teams on the road this week.")
}

## filtering out home teams
home_VoA_Ratings <- PrevWeek_VoA %>%
  filter(team %in% upcoming_games_df$home_team)
## might be duplicates
home_duplicates <- PrevWeek_VoA %>%
  filter(team %in% upcoming_games_df$home_team[duplicated(upcoming_games_df$home_team)])
home_VoA_Ratings <- rbind(home_VoA_Ratings, home_duplicates)

## changing col names before doing full_join
colnames(home_VoA_Ratings) <- c("home_team", "home_VoA_Rating")
## filtering out away teams
away_VoA_Ratings <- PrevWeek_VoA %>%
  filter(team %in% upcoming_games_df$away_team)
## might be duplicates
away_duplicates <- PrevWeek_VoA %>%
  filter(team %in% upcoming_games_df$away_team[duplicated(upcoming_games_df$away_team)])
away_VoA_Ratings <- rbind(away_VoA_Ratings, away_duplicates)
## changing col names before doing full_join
colnames(away_VoA_Ratings) <- c("away_team", "away_VoA_Rating")

## joining home_VoA_Ratings, then away
home_list <- list(upcoming_games_df, home_VoA_Ratings)
upcoming_games_df <- home_list %>%
  reduce(full_join, by = "home_team")
## removing duplicates
upcoming_games_df <- upcoming_games_df[!duplicated(upcoming_games_df),]
away_list <- list(upcoming_games_df, away_VoA_Ratings)
upcoming_games_df <- away_list %>%
  reduce(full_join, by = "away_team")
## removing duplicates
upcoming_games_df <- upcoming_games_df[!duplicated(upcoming_games_df),]

## for some reason the above part led to duplicates
# duplicate_games <- upcoming_games_df %>%
#   filter(game_id %in% upcoming_games_df$game_id[duplicated(upcoming_games_df$game_id)]) %>%
#   distinct(.keep_all = TRUE)
# upcoming_games_df <- upcoming_games_df %>%
#   filter(game_id != duplicate_games$game_id)


## Creating Vortex of Projection Spread column
upcoming_games_df <- upcoming_games_df %>%
  mutate(predicted = case_when(neutral_site = TRUE ~ away_VoA_Rating - home_VoA_Rating,
                               TRUE ~ away_VoA_Rating - home_VoA_Rating - 2)) %>%
  select(game_id, home_team, away_team, predicted)
colnames(upcoming_games_df) <- c("id", "home", "away", "predicted")

write_csv(upcoming_games_df, here("Data", paste("VoA", year, sep = ""), paste(year, "VoP", "Week", upcoming, "Games", ".csv", sep = "")))

## simple function to take VoA Ratings and field neutrality as inputs
## commenting out function so I can still use it here in case above code fails
# margin_projection <- function(away, home, neutral) {
#   margin_proj = PrevWeek_VoA$VoA_Rating[PrevWeek_VoA$team == away] -  PrevWeek_VoA$VoA_Rating[PrevWeek_VoA$team == home]
#   if (neutral == FALSE) {
#     margin_proj = margin_proj - 2
#   }
#   return(margin_proj)
# }
