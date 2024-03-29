##### Vortex of Projection version 1.0.1
### This script will take the most recent csv from the VoA
### It will take the VoA Ratings from that csv and use them to project scoring margins for upcoming FBS games
## loading packages
library(pacman)
pacman::p_load(tidyverse, gt, cfbfastR, here, gtExtras, RColorBrewer, cfbplotR, webshot2)
## Inputting year
year <- readline(prompt = "What Year is it? ")
## Inputting upcoming week number
upcoming <- readline(prompt = "What week is upcoming? ")

## Text Strings for gt table of game projections at the end
week_text <- "Week"
gameprojections_png <- "GameProjections.png"
gameprojections_filename <- paste(year, week_text, upcoming, gameprojections_png, sep = "")

## reading in appropriate csv based on upcoming week
if (as.numeric(upcoming) == 1) {
  PrevWeek_VoA <- read_csv(here("Data", "VoA2023", "2023Week0_VoA.csv")) |>
    select(team, VoA_Rating)
} else if (as.numeric(upcoming) == 2) {
  PrevWeek_VoA <- read_csv(here("Data", "VoA2023", "2023Week1_VoA.csv")) |>
    select(team, VoA_Rating)
} else if (as.numeric(upcoming) == 3) {
  PrevWeek_VoA <- read_csv(here("Data", "VoA2023", "2023Week2_VoA.csv")) |>
    select(team, VoA_Rating)
} else if (as.numeric(upcoming) == 4) {
  PrevWeek_VoA <- read_csv(here("Data", "VoA2023", "2023Week3_VoA.csv")) |>
    select(team, VoA_Rating)
} else if (as.numeric(upcoming) == 5) {
  PrevWeek_VoA <- read_csv(here("Data", "VoA2023", "2023Week4_VoA.csv")) |>
    select(team, VoA_Rating)
} else if (as.numeric(upcoming) == 6) {
  PrevWeek_VoA <- read_csv(here("Data", "VoA2023", "2023Week5_VoA.csv")) |>
    select(team, VoA_Rating)
} else if (as.numeric(upcoming) == 7) {
  PrevWeek_VoA <- read_csv(here("Data", "VoA2023", "2023Week6_VoA.csv")) |>
    select(team, VoA_Rating)
} else if (as.numeric(upcoming) == 8) {
  PrevWeek_VoA <- read_csv(here("Data", "VoA2023", "2023Week7_VoA.csv")) |>
    select(team, VoA_Rating)
} else if (as.numeric(upcoming) == 9) {
  PrevWeek_VoA <- read_csv(here("Data", "VoA2023", "2023Week8_VoA.csv")) |>
    select(team, VoA_Rating)
} else if (as.numeric(upcoming) == 10) {
  PrevWeek_VoA <- read_csv(here("Data", "VoA2023", "2023Week9_VoA.csv")) |>
    select(team, VoA_Rating)
} else if (as.numeric(upcoming) == 11) {
  PrevWeek_VoA <- read_csv(here("Data", "VoA2023", "2023Week10_VoA.csv")) |>
    select(team, VoA_Rating)
} else if (as.numeric(upcoming) == 12) {
  PrevWeek_VoA <- read_csv(here("Data", "VoA2023", "2023Week11_VoA.csv")) |>
    select(team, VoA_Rating)
} else if (as.numeric(upcoming) == 13) {
  PrevWeek_VoA <- read_csv(here("Data", "VoA2023", "2023Week12_VoA.csv")) |>
    select(team, VoA_Rating)
} else if (as.numeric(upcoming) == 14) {
  PrevWeek_VoA <- read_csv(here("Data", "VoA2023", "2023Week13_VoA.csv")) |>
    select(team, VoA_Rating)
} else if (as.numeric(upcoming) == 15) {
  PrevWeek_VoA <- read_csv(here("Data", "VoA2023", "2023Week14_VoA.csv")) |>
    select(team, VoA_Rating)
} else {
  PrevWeek_VoA <- read_csv(here("Data", "VoA2023", "2023Week15_VoA.csv")) |>
    select(team, VoA_Rating)
}

## pulling out SRS ratings for just FCS teams since I don't have VoA ratings for them
# using last year's until SRS ratings are available for current season
# expectation is that this will be sometime between weeks 5 and 7, based on 2022 season
set.seed(69)
if (as.numeric(upcoming) < 5) {
  FCS_ratings <- cfbd_ratings_srs(as.numeric(year) - 1) |>
    filter(conference != "ACC" & conference != "American Athletic" & conference != "Big 12" & conference != "Big Ten" & conference != "Conference USA" & conference != "FBS Independents" & conference != "Mid-American" & conference != "Mountain West" & conference != "Pac-12" & conference != "SEC" & conference != "Sun Belt") |>
    filter(team != "Jacksonville State" & team != "Sam Houston State" & team != "James Madison") |>
    select(team, rating)
  colnames(FCS_ratings) <- c("team", "VoA_Rating")
} else {
  FCS_ratings <- cfbd_ratings_srs(as.numeric(year)) |>
    filter(conference != "ACC" & conference != "American Athletic" & conference != "Big 12" & conference != "Big Ten" & conference != "Conference USA" & conference != "FBS Independents" & conference != "Mid-American" & conference != "Mountain West" & conference != "Pac-12" & conference != "SEC" & conference != "Sun Belt") |>
    filter(team != "Jacksonville State" & team != "Sam Houston State" & team != "James Madison") |>
    select(team, rating)
  colnames(FCS_ratings) <- c("team", "VoA_Rating")
}

## Binding most recent VoA (PrevWeek_VoA) and FCS_ratings as if rating systems are the same
PrevWeek_VoA <- rbind(PrevWeek_VoA, FCS_ratings)

## reading in upcoming games to create df of games and VoA projected margins
if (as.numeric(upcoming) == 16) {
  upcoming_games_df <- cfbd_game_info(2023, season_type = "postseason") |>
    filter(home_team %in% PrevWeek_VoA$team | away_team %in% PrevWeek_VoA$team) |>
    select(game_id, season, week, neutral_site, home_team, away_team)
} else {
  upcoming_games_df <- cfbd_game_info(2023, week = as.numeric(upcoming)) |>
    filter(home_team %in% PrevWeek_VoA$team | away_team %in% PrevWeek_VoA$team) |>
    select(game_id, season, week, neutral_site, home_team, away_team)
}

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
# if (nrow(missing_home_ratings) > 0) {
#   missing_home_ratings <- missing_home_ratings |>
#     mutate(VoA_Rating = sample(FCS_random, nrow(missing_home_ratings), replace = TRUE)) |>
#     select(team, VoA_Rating)
# } else {
#   print("No FCS teams play FBS teams at home this week.")
# }
## assigning random VoA_Ratings to missing away teams who are not part of VoA
# if (nrow(missing_away_ratings) > 0) {
#   missing_away_ratings <- missing_away_ratings |>
#     mutate(VoA_Rating = sample(FCS_random, nrow(missing_away_ratings), replace = TRUE)) |>
#     select(team, VoA_Rating)
# } else {
#   print("No FCS teams play FBS teams on the road this week.")
# }

## binding random FCS ratings to PrevWeek_VoA, assigning it to same df name
# if (nrow(missing_home_ratings) > 0) {
#   PrevWeek_VoA <- rbind(PrevWeek_VoA, missing_home_ratings)
# } else {
#   print("No FCS teams play FBS teams at home this week.") 
# }
# if (nrow(missing_away_ratings) > 0) {
#   PrevWeek_VoA <- rbind(PrevWeek_VoA, missing_away_ratings)
# } else {
#   print("No FCS teams play FBS teams on the road this week.")
# }

## filtering out home teams
home_VoA_Ratings <- PrevWeek_VoA |>
  filter(team %in% upcoming_games_df$home_team)
## might be duplicates
home_duplicates <- PrevWeek_VoA |>
  filter(team %in% upcoming_games_df$home_team[duplicated(upcoming_games_df$home_team)])
home_VoA_Ratings <- rbind(home_VoA_Ratings, home_duplicates)

## changing col names before doing full_join
colnames(home_VoA_Ratings) <- c("home_team", "home_VoA_Rating")
## filtering out away teams
away_VoA_Ratings <- PrevWeek_VoA |>
  filter(team %in% upcoming_games_df$away_team)
## might be duplicates
away_duplicates <- PrevWeek_VoA |>
  filter(team %in% upcoming_games_df$away_team[duplicated(upcoming_games_df$away_team)])
away_VoA_Ratings <- rbind(away_VoA_Ratings, away_duplicates)
## changing col names before doing full_join
colnames(away_VoA_Ratings) <- c("away_team", "away_VoA_Rating")

## joining home_VoA_Ratings, then away
home_list <- list(upcoming_games_df, home_VoA_Ratings)
upcoming_games_df <- home_list |>
  reduce(full_join, by = "home_team")
## removing duplicates
upcoming_games_df <- upcoming_games_df[!duplicated(upcoming_games_df),]
away_list <- list(upcoming_games_df, away_VoA_Ratings)
upcoming_games_df <- away_list |>
  reduce(full_join, by = "away_team")
## removing duplicates
upcoming_games_df <- upcoming_games_df[!duplicated(upcoming_games_df),]

## for some reason the above part led to duplicates
# duplicate_games <- upcoming_games_df |>
#   filter(game_id %in% upcoming_games_df$game_id[duplicated(upcoming_games_df$game_id)]) |>
#   distinct(.keep_all = TRUE)
# upcoming_games_df <- upcoming_games_df |>
#   filter(game_id != duplicate_games$game_id)


## Creating Vortex of Projection Spread column
cfbdata_contest_df <- upcoming_games_df |>
  mutate(predicted = case_when(neutral_site == FALSE ~ away_VoA_Rating - (home_VoA_Rating + 2),
                               TRUE ~ away_VoA_Rating - home_VoA_Rating)) |>
  select(game_id, home_team, away_team, predicted)
colnames(cfbdata_contest_df) <- c("id", "home", "away", "predicted")

write_csv(cfbdata_contest_df, here("Data", paste("VoA", year, sep = ""), "Projections", paste(year, "VoP", "Week", upcoming, "Games", ".csv", sep = "")))

## simple function to take VoA Ratings and field neutrality as inputs
margin_projection <- function(away, home, neutral) {
  margin_proj = PrevWeek_VoA$VoA_Rating[PrevWeek_VoA$team == away] -  PrevWeek_VoA$VoA_Rating[PrevWeek_VoA$team == home]
  if (neutral == FALSE) {
    margin_proj = margin_proj - 2
  }
  return(margin_proj)
}
## FCS version of above function
# srs <- cfbd_ratings_srs(year = as.numeric(year))
## above line produces an error message "Error: The API returned an error"
## below line is exactly the same, but works for some reason
# srs <- cfbd_ratings_srs(as.numeric(year))
# fcs_margin_projection <- function(away, home, neutral) {
#   margin_proj = srs$rating[srs$team == away] -  srs$rating[srs$team == home]
#   if (neutral == FALSE) {
#     margin_proj = margin_proj - 2
#   }
#   return(margin_proj)
# }

## making gt table of upcoming games df to display games with close spreads
upcoming_games_df <- upcoming_games_df |>
  mutate(Proj_Winner = case_when((neutral_site == FALSE & (home_VoA_Rating + 2) > away_VoA_Rating) ~ home_team,
                                 (neutral_site == FALSE & away_VoA_Rating > (home_VoA_Rating + 2)) ~ away_team,
                                 (neutral_site == TRUE & home_VoA_Rating > away_VoA_Rating) ~ home_team,
                                 (neutral_site == TRUE & away_VoA_Rating > home_VoA_Rating) ~ away_team,
                                 TRUE ~ "TIE"),
         Proj_Margin = case_when(neutral_site == FALSE ~ abs(away_VoA_Rating - (home_VoA_Rating + 2)),
                                 TRUE ~ abs(away_VoA_Rating - home_VoA_Rating)),
         Initial_Win_Prob = 50.37036489 + (2.38892221 * Proj_Margin) + (-0.02809534 * (Proj_Margin^2))) |>
  select(game_id, season, week, neutral_site, home_team, home_VoA_Rating, away_team, away_VoA_Rating, Proj_Winner, Proj_Margin, Initial_Win_Prob) ## |>
  ## arrange(desc(Proj_Margin))
## making sure no game has win probability for projected winner lower than 50 or higher than 100
upcoming_games_df <- upcoming_games_df |>
  mutate(Win_Prob = case_when((Initial_Win_Prob < 50) ~ 50.01,
                              Initial_Win_Prob > 100 ~ 100,
                              Proj_Margin > 45 ~ 100,
                              TRUE ~ Initial_Win_Prob)) |>
  select(game_id, season, week, neutral_site, home_team, home_VoA_Rating, away_team, away_VoA_Rating, Proj_Winner, Proj_Margin, Win_Prob)

## bowl projection table made slightly differently
# not saved, just created for personal use
if (as.numeric(upcoming) == 16) {
  upcoming_games_df <- upcoming_games_df |>
    arrange(Proj_Margin)
  
  ## Creating gt table
  # adding title and subtitle
  upcoming_games_gt <- upcoming_games_df |>
    gt() |> # use 'gt' to make an awesome table...
    gt_theme_espn() |>
    tab_header(
      title = paste(year, week_text, upcoming, "Vortex of Projection Game Projections"), # ...with this title
      subtitle = "The Unquestionably Puzzling Yet Impeccibly Perceptive Vortex of Projection")  |>  # and this subtitle
    fmt_number( # A column (numeric data)
      columns = c(Proj_Margin), # What column variable? FinalVoATop25$VoA_Rating
      decimals = 5 # With four decimal places
    ) |> 
    fmt_number( # Another column (also numeric data)
      columns = c(home_VoA_Rating), # What column variable? FinalVoATop25$VoA_Ranking
      decimals = 5 # I want this column to have 5 decimal places
    ) |>
    fmt_number( # Another numeric column
      columns = c(away_VoA_Rating),
      decimals = 5
    ) |>
    fmt_number( # Another numeric column
      columns = c(away_VoA_Rating),
      decimals = 2
    ) |>  
    data_color( # Update cell colors, testing different color palettes
      columns = c(Proj_Margin), # ...for dose column
      fn = scales::col_numeric( # <- bc it's numeric
        palette = brewer.pal(9, "RdBu"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    data_color( # Update cell colors, testing different color palettes
      columns = c(Win_Prob), # ...for dose column
      fn = scales::col_numeric( # <- bc it's numeric
        palette = brewer.pal(9, "RdYlGn"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    cols_label(home_team = "Home", away_team = "Away", home_VoA_Rating = "Home VoA Rating", away_VoA_Rating = "Away VoA Rating", Proj_Winner = "Projected Winner", Proj_Margin = "Projected Margin", Win_Prob = "Win Probability") |> # Update labels
    cols_move_to_end(columns = "Win_Prob") |>
    cols_hide(c(game_id, season, week, neutral_site)) |>
    tab_footnote(
      footnote = "Data from CFB Data API, ESPN.com, and ESPN's Bill Connelly via cfbfastR, FCS data mostly from stats.ncaa.org,
    VoA Ratings for FCS teams are actually SRS ratings taken from CFB Data API"
    )
} else {
  ## Creating gt table
  # adding title and subtitle
  upcoming_games_gt <- upcoming_games_df |>
    gt() |> # use 'gt' to make an awesome table...
    gt_theme_espn() |>
    tab_header(
      title = paste(year, week_text, upcoming, "Vortex of Projection Game Projections"), # ...with this title
      subtitle = "The Unquestionably Puzzling Yet Impeccibly Perceptive Vortex of Projection")  |>  # and this subtitle
    fmt_number( # A column (numeric data)
      columns = c(Proj_Margin), # What column variable? FinalVoATop25$VoA_Rating
      decimals = 5 # With four decimal places
    ) |> 
    fmt_number( # Another column (also numeric data)
      columns = c(home_VoA_Rating), # What column variable? FinalVoATop25$VoA_Ranking
      decimals = 5 # I want this column to have 5 decimal places
    ) |>
    fmt_number( # Another numeric column
      columns = c(away_VoA_Rating),
      decimals = 5
    ) |>
    fmt_number( # Another numeric column
      columns = c(away_VoA_Rating),
      decimals = 2
    ) |>  
    data_color( # Update cell colors, testing different color palettes
      columns = c(Proj_Margin), # ...for dose column
      fn = scales::col_numeric( # <- bc it's numeric
        palette = brewer.pal(9, "RdBu"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    data_color( # Update cell colors, testing different color palettes
      columns = c(Win_Prob), # ...for dose column
      fn = scales::col_numeric( # <- bc it's numeric
        palette = brewer.pal(9, "RdYlGn"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    cols_label(home_team = "Home", away_team = "Away", home_VoA_Rating = "Home VoA Rating", away_VoA_Rating = "Away VoA Rating", Proj_Winner = "Projected Winner", Proj_Margin = "Projected Margin", Win_Prob = "Win Probability") |> # Update labels
    cols_move_to_end(columns = "Win_Prob") |>
    cols_hide(c(game_id, season, week, neutral_site)) |>
    tab_footnote(
      footnote = "Data from CFB Data API, ESPN.com, and ESPN's Bill Connelly via cfbfastR, FCS data mostly from stats.ncaa.org,
    VoA Ratings for FCS teams are actually SRS ratings taken from CFB Data API"
    )
}
upcoming_games_gt
upcoming_games_gt |>
  gtsave(
    gameprojections_filename, expand = 5,
    path = here("RVoA", "Outputs", "VoP")
  )
