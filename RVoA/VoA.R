## The Vortex of Accuracy, Version 4.1.1
## Supremely Excellent Yet Salaciously Godlike And Infallibly Magnificent Vortex of Accuracy
## Created by Griffin Shelor
## Initially intended to use SP+ ratings but due to paywall issues they are unlikely to be
# accessible via collegefootballdata.com's API and thus they will be inaccessible via
# cfbfastR, so sections involving current season SP+ will no longer be included in
# Vortex of Accuracy. Those sections will be commented out in case this accessibility
# issue ever changes.
## installing packages
# install.packages(c("devtools", "tidyverse", "matrixStats", "gt", "viridis", "webshot", "rvest", "cfbfastR", "here", "ggsci", "RColorBrewer", "ggpubr", "remotes", "pacman", "gtExtras", cfbplotR))
## Load Packages for Ranking Variables
start_time <- Sys.time()
library(pacman)
pacman::p_load(tidyverse, gt, cfbfastR, here, RColorBrewer, gtExtras, cfbplotR, ggpubr, webshot2)
## testing to see if script runs without certain packages I don't think are being used
# writexl, rvest, openxlsx, tidymodels, ranger, grid, gridExtra, matrixStats, viridis, webshot, ggsci,

## Creating Week and Year String for Top 25 Table Title, eventually could be used as part of reading in cfbfastR/cfbdata API data
year <- readline(prompt = "What year is it? ")
week <- readline(prompt = "What week is it? ")

##### setting strings for table titles, file pathways, unintelligible charts #####
output_dir <- here("RVoA", "Outputs")
data_dir <- here("Data", paste("VoA", year, sep = ""))
preseason_text <- "Preseason"
resume_text <- "Resume"
VoAString <- "VoA.csv"
week_text <- "Week"
VoA_Top25_text <- "Vortex of Accuracy Top 25"
top25_png <- "VoATop25.png"
fulltable_png <- "VoAFullTable.png"
VoA_text <- "Vortex of Accuracy"
Postseason_text <- "Postseason"
AAC_text <- "AAC"
ACC_text <- "ACC"
Big12_text <- "Big12"
Big10_text <- "Big10"
CUSA_text <- "CUSA"
Indy_text <- "Independents"
MAC_text <- "MAC"
MWC_text <- "MWC"
Pac12_text <- "Pac12"
SEC_text <- "SEC"
SunBelt_text <- "SunBelt"
FBS_text <- "FBS"
Power_Five_text <- "Power 5"
Group_Five_text <- "Group of 5"
Rating_text <- "_Ratings_Chart.png"
Ranking_text <- "_Rankings_Chart.png"
Histogram_text <- "_RatingHist.png"
Output_Rating_Plot_text <- "VoA Outputs vs VoA Ratings"
Output_Rating_Plot_png <- "Output_Rating.png"

FBS_hist_title <- paste(year, week_text, week, FBS_text, VoA_text, "Ratings")
Power5_hist_title <- paste(year, week_text, week, Power_Five_text, VoA_text, "Ratings")
Group5_hist_title <- paste(year, week_text, week, Group_Five_text, VoA_text, "Ratings")
Output_Rating_Plot_title <- paste(year, week_text, week, Output_Rating_Plot_text)
top25_file_pathway <- paste(year,week_text,week,"_",top25_png, sep = "")
resumetop25_file_pathway <- paste(year,week_text,week,resume_text,"_",top25_png, sep = "")
fulltable_file_pathway <- paste(year,week_text,week,"_",fulltable_png, sep = "")
resumefulltable_file_pathway <- paste(year,week_text,week,resume_text,"_",fulltable_png, sep = "")
AAC_Output_filename <- paste(year,week_text, week, AAC_text, Rating_text, sep = "")
AAC_Ranking_filename <- paste(year,week_text, week, AAC_text, Ranking_text, sep = "")
ACC_Output_filename <- paste(year,week_text, week, ACC_text, Rating_text, sep = "")
ACC_Ranking_filename <- paste(year,week_text, week, ACC_text, Ranking_text, sep = "")
Big12_Output_filename <- paste(year,week_text, week, Big12_text, Rating_text, sep = "")
Big12_Ranking_filename <- paste(year,week_text, week, Big12_text, Ranking_text, sep = "")
Big10_Output_filename <- paste(year,week_text, week, Big10_text, Rating_text, sep = "")
Big10_Ranking_filename <- paste(year,week_text, week, Big10_text, Ranking_text, sep = "")
CUSA_Output_filename <- paste(year,week_text, week, CUSA_text, Rating_text, sep = "")
CUSA_Ranking_filename <- paste(year,week_text, week, CUSA_text, Ranking_text, sep = "")
Indy_Output_filename <- paste(year,week_text, week, Indy_text, Rating_text, sep = "")
Indy_Ranking_filename <- paste(year,week_text, week, Indy_text, Ranking_text, sep = "")
MAC_Output_filename <- paste(year,week_text, week, MAC_text, Rating_text, sep = "")
MAC_Ranking_filename <- paste(year,week_text, week, MAC_text, Ranking_text, sep = "")
MWC_Output_filename <- paste(year,week_text, week, MWC_text, Rating_text, sep = "")
MWC_Ranking_filename <- paste(year,week_text, week, MWC_text, Ranking_text, sep = "")
Pac12_Output_filename <- paste(year,week_text, week, Pac12_text, Rating_text, sep = "")
Pac12_Ranking_filename <- paste(year,week_text, week, Pac12_text, Ranking_text, sep = "")
SEC_Output_filename <- paste(year,week_text, week, SEC_text, Rating_text, sep = "")
SEC_Ranking_filename <- paste(year,week_text, week, SEC_text, Ranking_text, sep = "")
SunBelt_Output_filename <- paste(year,week_text, week, SunBelt_text, Rating_text, sep = "")
SunBelt_Ranking_filename <- paste(year,week_text, week, SunBelt_text, Ranking_text, sep = "")
FBS_hist_filename <- paste(year, week_text, week, "_", FBS_text, Histogram_text, sep = "")
Power5_hist_filename <- paste(year, week_text, week, "_", Power_Five_text, Histogram_text, sep = "")
Group5_hist_filename <- paste(year, week_text, week, "_", Group_Five_text, Histogram_text, sep = "")
Output_Rating_Plot_filename <- paste(year, week_text, week, "_", Output_Rating_Plot_png, sep = "")
## creating string for excel spreadsheet pathway
file_pathway <- paste(data_dir, "/", year, week_text, week,"_", VoAString, sep = "")

##### Reading in Data #####

##### TEMPORARY COVID OPT OUTS FIX #####
## This will no longer be necessary after 2020 season is no longer included in VoA
if (as.numeric(week) <= 1) {
  Stats_covid <- cfbd_stats_season_team(2019, season_type = "both") |>
    filter(team == "Connecticut" | team == "New Mexico State" | team == "Old Dominion") |>
    mutate(total_yds_pg = total_yds/games,
           pass_yds_pg = net_pass_yds/games,
           rush_yds_pg = rush_yds/games,
           first_downs_pg = first_downs/games,
           def_interceptions_pg = passes_intercepted/games,
           pass_ypa = net_pass_yds / pass_atts,
           off_ypp = total_yds / (rush_atts + pass_atts),
           completion_pct = pass_comps / pass_atts,
           pass_ypr = net_pass_yds / pass_comps,
           int_pct = interceptions / pass_atts,
           rush_ypc = rush_yds / rush_atts,
           turnovers_pg = turnovers / games,
           third_conv_rate = third_down_convs / third_downs,
           fourth_conv_rate = fourth_down_convs / fourth_downs,
           penalty_yds_pg = penalty_yds / games,
           yards_per_penalty = penalty_yds / penalties,
           kick_return_avg = kick_return_yds / kick_returns,
           punt_return_avg = punt_return_yds / punt_returns)
  ## removing NAs
  Stats_covid[is.na(Stats_covid)] = 0
  
  ## PY3 advanced stats
  Adv_Stats_covid <- cfbd_stats_season_advanced(year = 2019, excl_garbage_time = FALSE, start_week = 1, end_week = 15) |>
    filter(team == "Connecticut" | team == "New Mexico State" | team == "Old Dominion") |>
    select(team, off_ppa, off_success_rate, off_explosiveness, off_power_success,
           off_stuff_rate, off_line_yds, off_second_lvl_yds, off_open_field_yds,
           off_pts_per_opp, off_field_pos_avg_predicted_points, off_havoc_total, 
           off_havoc_front_seven, off_havoc_db, off_standard_downs_ppa,
           off_standard_downs_success_rate, off_standard_downs_explosiveness,
           off_passing_downs_ppa, off_passing_downs_success_rate,
           off_passing_downs_explosiveness, off_rushing_plays_ppa,
           off_rushing_plays_success_rate, off_rushing_plays_explosiveness,
           off_passing_plays_ppa, off_passing_plays_success_rate,
           off_passing_plays_explosiveness, def_ppa, def_success_rate,
           def_explosiveness, def_power_success, def_stuff_rate, def_line_yds,
           def_second_lvl_yds, def_open_field_yds, def_pts_per_opp, 
           def_field_pos_avg_predicted_points, def_havoc_total, def_havoc_front_seven,
           def_havoc_db, def_standard_downs_ppa, def_standard_downs_success_rate,
           def_standard_downs_explosiveness , def_passing_downs_ppa,
           def_passing_downs_success_rate, def_passing_downs_explosiveness,
           def_rushing_plays_ppa, def_rushing_plays_success_rate,
           def_rushing_plays_explosiveness, def_passing_plays_ppa,
           def_passing_plays_success_rate, def_passing_plays_explosiveness)
  
  ## SP+ data
  SP_Rankings_covid <- cfbd_ratings_sp(year = 2019) |>
    filter(team == "Connecticut" | team == "New Mexico State" | team == "Old Dominion") |>
    filter(team != "nationalAverages") |>
    select(team, rating, offense_rating, defense_rating, special_teams_rating)
  colnames(SP_Rankings_covid) <- c("team", "sp_rating_PY3", "sp_offense_rating_PY3", "sp_defense_rating_PY3", "sp_special_teams_rating_PY3")
  ## Eliminating NAs
  SP_Rankings_covid[is.na(SP_Rankings_covid)] = 0
  
  ## FPI data
  ## filtering out COVID opt-outs from 2019 data, will be merged into 2020 data
  FPI_covid <- espn_ratings_fpi(year = 2019) |>
    mutate(team = case_when(team_name == 'UConn' ~ 'Connecticut',
                            team_name == 'New Mexico St' ~ 'New Mexico State',
                            TRUE ~ team_name)) |>
    filter(team == "Connecticut" | team == "New Mexico State" | team == "Old Dominion") |>
    select(team, fpi, w, l)
  colnames(FPI_covid) <- c("team", "FPI_PY3", "Wins_PY3", "Losses_PY3")
  FPI_covid[,2:ncol(FPI_covid)] <- FPI_covid[,2:ncol(FPI_covid)] |> mutate_if(is.character, as.numeric)
  
  ## pulling in recruiting rankings
  recruit_covid <- cfbd_recruiting_team(year = 2019) |>
    filter(team == "Connecticut" | team == "New Mexico State" | team == "Old Dominion") |>
    select(team, points)
  colnames(recruit_covid) <- c("team", "recruit_pts_PY3")
  
  ## pulling in talent rankings
  talent_df_covid <- cfbd_team_talent(year = 2019) |>
    filter(school == "Connecticut" | school == "New Mexico State" | school == "Old Dominion") |>
    select(school, talent)
  colnames(talent_df_covid) <- c("team", "talent_PY3")
  
  ## pulling in SRS rankings
  SRS_covid <- cfbd_ratings_srs(year = 2019) |>
    filter(team == "Connecticut" | team == "New Mexico State" | team == "Old Dominion") |>
    select(team, rating)
  colnames(SRS_covid) <- c("team", "SRS_rating_PY3")
  
  ## merging stats and ADV stats together first to rename columns
  COVID_stats_adv_stats_list <- list(Stats_covid, Adv_Stats_covid)
  COVID_stats_adv_stats_merge <- COVID_stats_adv_stats_list |>
    reduce(full_join, by = "team") |>
    select(season, team, conference, games, completion_pct, 
           pass_ypa, pass_ypr, int_pct, rush_ypc, turnovers_pg, 
           third_conv_rate, fourth_conv_rate, penalty_yds_pg, 
           yards_per_penalty, kick_return_avg, punt_return_avg, total_yds_pg, 
           pass_yds_pg, rush_yds_pg, first_downs_pg, off_ypp, def_interceptions_pg, off_ppa, 
           off_success_rate, off_explosiveness, off_power_success, off_stuff_rate, 
           off_line_yds, off_second_lvl_yds, off_open_field_yds, off_pts_per_opp, 
           off_field_pos_avg_predicted_points, off_havoc_total, off_havoc_front_seven, 
           off_havoc_db, off_standard_downs_ppa, off_standard_downs_success_rate, 
           off_standard_downs_explosiveness, off_passing_downs_ppa, 
           off_passing_downs_success_rate, off_passing_downs_explosiveness, 
           off_rushing_plays_ppa, off_rushing_plays_success_rate, 
           off_rushing_plays_explosiveness, off_passing_plays_ppa, 
           off_passing_plays_success_rate, off_passing_plays_explosiveness, def_ppa, 
           def_success_rate, def_explosiveness, def_power_success, def_stuff_rate, 
           def_line_yds, def_second_lvl_yds, def_open_field_yds, def_pts_per_opp, 
           def_field_pos_avg_predicted_points, def_havoc_total, def_havoc_front_seven, 
           def_havoc_db, def_standard_downs_ppa, def_standard_downs_success_rate, 
           def_standard_downs_explosiveness, def_passing_downs_ppa, 
           def_passing_downs_success_rate, def_passing_downs_explosiveness, 
           def_rushing_plays_ppa, def_rushing_plays_success_rate, 
           def_rushing_plays_explosiveness, def_passing_plays_ppa, 
           def_passing_plays_success_rate, def_passing_plays_explosiveness)
  colnames(COVID_stats_adv_stats_merge) <- c("season", "team", "conference", "games_PY3", "completion_pct_PY3",
                                           "pass_ypa_PY3", "pass_ypr_PY3", "int_pct_PY3", "rush_ypc_PY3", "turnovers_pg_PY3",
                                           "third_conv_rate_PY3", "fourth_conv_rate_PY3", "penalty_yds_pg_PY3",
                                           "yards_per_penalty_PY3", "kick_return_avg_PY3", "punt_return_avg_PY3", "total_yds_pg_PY3",
                                           "pass_yds_pg_PY3", "rush_yds_pg_PY3", "first_downs_pg_PY3", "off_ypp_PY3", "def_interceptions_pg_PY3", "off_ppa_PY3",
                                           "off_success_rate_PY3", "off_explosiveness_PY3", "off_power_success_PY3", "off_stuff_rate_PY3",
                                           "off_line_yds_PY3", "off_second_lvl_yds_PY3", "off_open_field_yds_PY3", "off_pts_per_opp_PY3",
                                           "off_field_pos_avg_predicted_points_PY3", "off_havoc_total_PY3", "off_havoc_front_seven_PY3",
                                           "off_havoc_db_PY3", "off_standard_downs_ppa_PY3", "off_standard_downs_success_rate_PY3",
                                           "off_standard_downs_explosiveness_PY3", "off_passing_downs_ppa_PY3",
                                           "off_passing_downs_success_rate_PY3", "off_passing_downs_explosiveness_PY3",
                                           "off_rushing_plays_ppa_PY3", "off_rushing_plays_success_rate_PY3",
                                           "off_rushing_plays_explosiveness_PY3", "off_passing_plays_ppa_PY3",
                                           "off_passing_plays_success_rate_PY3", "off_passing_plays_explosiveness_PY3", "def_ppa_PY3",
                                           "def_success_rate_PY3", "def_explosiveness_PY3", "def_power_success_PY3", "def_stuff_rate_PY3",
                                           "def_line_yds_PY3", "def_second_lvl_yds_PY3", "def_open_field_yds_PY3", "def_pts_per_opp_PY3",
                                           "def_field_pos_avg_predicted_points_PY3", "def_havoc_total_PY3", "def_havoc_front_seven_PY3",
                                           "def_havoc_db_PY3", "def_standard_downs_ppa_PY3", "def_standard_downs_success_rate_PY3",
                                           "def_standard_downs_explosiveness_PY3", "def_passing_downs_ppa_PY3",
                                           "def_passing_downs_success_rate_PY3", "def_passing_downs_explosiveness_PY3",
                                           "def_rushing_plays_ppa_PY3", "def_rushing_plays_success_rate_PY3",
                                           "def_rushing_plays_explosiveness_PY3", "def_passing_plays_ppa_PY3",
                                           "def_passing_plays_success_rate_PY3", "def_passing_plays_explosiveness_PY3")
  
  ## Merging COVID opt out data frames together
  covid_df_list <- list(COVID_stats_adv_stats_merge, SP_Rankings_covid, FPI_covid, recruit_covid, talent_df_covid, SRS_covid)
  COVID_Optouts_df <- covid_df_list |>
    reduce(full_join, by = "team")
} else {
  print("COVID-year data no longer included in Vortex of Accuracy")
}
##### regular data pull #####
## pulling in data based on week of the season
if (as.numeric(week) == 0) {
  ##### WEEK 0 Data Pull #####
  ## reading in data for 3 previous years
  FCS_PY3 <- read_csv(here("Data", "VoA2023", "FCSPrevYears", "FCS_PY3.csv"))
  FCS_PY2 <- read_csv(here("Data", "VoA2023", "FCSPrevYears", "FCS_PY2.csv"))
  FCS_PY1 <- read_csv(here("Data", "VoA2023", "FCSPrevYears", "FCS_PY1.csv"))
  Stats_PY1 <- cfbd_stats_season_team(year = as.integer(year) - 1, season_type = "both") |>
    filter(team != "Sam Houston State" & team != "Jacksonville State") |>
    mutate(total_yds_pg = total_yds/games,
           pass_yds_pg = net_pass_yds/games,
           rush_yds_pg = rush_yds/games,
           first_downs_pg = first_downs/games,
           def_interceptions_pg = passes_intercepted/games,
           pass_ypa = net_pass_yds / pass_atts,
           off_ypp = total_yds / (rush_atts + pass_atts),
           completion_pct = pass_comps / pass_atts,
           pass_ypr = net_pass_yds / pass_comps,
           int_pct = interceptions / pass_atts,
           rush_ypc = rush_yds / rush_atts,
           turnovers_pg = turnovers / games,
           third_conv_rate = third_down_convs / third_downs,
           fourth_conv_rate = fourth_down_convs / fourth_downs,
           penalty_yds_pg = penalty_yds / games,
           yards_per_penalty = penalty_yds / penalties,
           kick_return_avg = kick_return_yds / kick_returns,
           punt_return_avg = punt_return_yds / punt_returns)
  ## removing NAs
  Stats_PY1[is.na(Stats_PY1)] = 0
  ## PY2 stats
  Stats_PY2 <- cfbd_stats_season_team(year = as.integer(year) - 2, season_type = "both") |>
    filter(team != "Sam Houston State" & team != "Jacksonville State" & team != "James Madison") |>
    mutate(total_yds_pg = total_yds/games,
           pass_yds_pg = net_pass_yds/games,
           rush_yds_pg = rush_yds/games,
           first_downs_pg = first_downs/games,
           def_interceptions_pg = passes_intercepted/games,
           pass_ypa = net_pass_yds / pass_atts,
           off_ypp = total_yds / (rush_atts + pass_atts),
           completion_pct = pass_comps / pass_atts,
           pass_ypr = net_pass_yds / pass_comps,
           int_pct = interceptions / pass_atts,
           rush_ypc = rush_yds / rush_atts,
           turnovers_pg = turnovers / games,
           third_conv_rate = third_down_convs / third_downs,
           fourth_conv_rate = fourth_down_convs / fourth_downs,
           penalty_yds_pg = penalty_yds / games,
           yards_per_penalty = penalty_yds / penalties,
           kick_return_avg = kick_return_yds / kick_returns,
           punt_return_avg = punt_return_yds / punt_returns)
  ## removing NAs
  Stats_PY2[is.na(Stats_PY2)] = 0
  ## PY3 stats
  Stats_PY3 <- cfbd_stats_season_team(year = as.integer(year) - 3, season_type = "both") |>
    filter(team != "James Madison" & team != "Sam Houston State" & team != "Jacksonville State" & team != "Connecticut" & team != "New Mexico State" & team != "Old Dominion") |>
    mutate(total_yds_pg = total_yds/games,
           pass_yds_pg = net_pass_yds/games,
           rush_yds_pg = rush_yds/games,
           first_downs_pg = first_downs/games,
           def_interceptions_pg = passes_intercepted/games,
           pass_ypa = net_pass_yds / pass_atts,
           off_ypp = total_yds / (rush_atts + pass_atts),
           completion_pct = pass_comps / pass_atts,
           pass_ypr = net_pass_yds / pass_comps,
           int_pct = interceptions / pass_atts,
           rush_ypc = rush_yds / rush_atts,
           turnovers_pg = turnovers / games,
           third_conv_rate = third_down_convs / third_downs,
           fourth_conv_rate = fourth_down_convs / fourth_downs,
           penalty_yds_pg = penalty_yds / games,
           yards_per_penalty = penalty_yds / penalties,
           kick_return_avg = kick_return_yds / kick_returns,
           punt_return_avg = punt_return_yds / punt_returns)
  ## removing NAs
  Stats_PY3[is.na(Stats_PY3)] = 0
  ## filtering out COVID opt-outs from 2019 (PY3) data, will be merged into 2020 (PY2) data
  # COVID_Optouts <- Stats_PY3 |>
  #   filter(team == "New Mexico State" | team == "Connecticut" | team == "Old Dominion")
  # Stats_PY2 <- rbind(Stats_PY2, COVID_Optouts)
  
  ## advanced stats data
  Adv_Stats_PY1 <- cfbd_stats_season_advanced(year = as.integer(year) - 1, excl_garbage_time = FALSE, start_week = 1, end_week = 15) |>
    filter(team != "Sam Houston State" & team != "Jacksonville State") |>
    select(team, off_ppa, off_success_rate, off_explosiveness, off_power_success,
           off_stuff_rate, off_line_yds, off_second_lvl_yds, off_open_field_yds,
           off_pts_per_opp, off_field_pos_avg_predicted_points, off_havoc_total, 
           off_havoc_front_seven, off_havoc_db, off_standard_downs_ppa,
           off_standard_downs_success_rate, off_standard_downs_explosiveness,
           off_passing_downs_ppa, off_passing_downs_success_rate,
           off_passing_downs_explosiveness, off_rushing_plays_ppa,
           off_rushing_plays_success_rate, off_rushing_plays_explosiveness,
           off_passing_plays_ppa, off_passing_plays_success_rate,
           off_passing_plays_explosiveness, def_ppa, def_success_rate,
           def_explosiveness, def_power_success, def_stuff_rate, def_line_yds,
           def_second_lvl_yds, def_open_field_yds, def_pts_per_opp, 
           def_field_pos_avg_predicted_points, def_havoc_total, def_havoc_front_seven,
           def_havoc_db, def_standard_downs_ppa, def_standard_downs_success_rate,
           def_standard_downs_explosiveness , def_passing_downs_ppa,
           def_passing_downs_success_rate, def_passing_downs_explosiveness,
           def_rushing_plays_ppa, def_rushing_plays_success_rate,
           def_rushing_plays_explosiveness, def_passing_plays_ppa,
           def_passing_plays_success_rate, def_passing_plays_explosiveness)
  ## removing NAs
  Adv_Stats_PY1[is.na(Adv_Stats_PY1)] = 0
  ## PY2 advanced stats
  Adv_Stats_PY2 <- cfbd_stats_season_advanced(year = as.integer(year) - 2, excl_garbage_time = FALSE, start_week = 1, end_week = 15) |>
    filter(team != "James Madison" & team != "Sam Houston State" & team != "Jacksonville State") |>
    select(team, off_ppa, off_success_rate, off_explosiveness, off_power_success,
           off_stuff_rate, off_line_yds, off_second_lvl_yds, off_open_field_yds,
           off_pts_per_opp, off_field_pos_avg_predicted_points, off_havoc_total, 
           off_havoc_front_seven, off_havoc_db, off_standard_downs_ppa,
           off_standard_downs_success_rate, off_standard_downs_explosiveness,
           off_passing_downs_ppa, off_passing_downs_success_rate,
           off_passing_downs_explosiveness, off_rushing_plays_ppa,
           off_rushing_plays_success_rate, off_rushing_plays_explosiveness,
           off_passing_plays_ppa, off_passing_plays_success_rate,
           off_passing_plays_explosiveness, def_ppa, def_success_rate,
           def_explosiveness, def_power_success, def_stuff_rate, def_line_yds,
           def_second_lvl_yds, def_open_field_yds, def_pts_per_opp, 
           def_field_pos_avg_predicted_points, def_havoc_total, def_havoc_front_seven,
           def_havoc_db, def_standard_downs_ppa, def_standard_downs_success_rate,
           def_standard_downs_explosiveness , def_passing_downs_ppa,
           def_passing_downs_success_rate, def_passing_downs_explosiveness,
           def_rushing_plays_ppa, def_rushing_plays_success_rate,
           def_rushing_plays_explosiveness, def_passing_plays_ppa,
           def_passing_plays_success_rate, def_passing_plays_explosiveness)
  ## removing NAs
  Adv_Stats_PY2[is.na(Adv_Stats_PY2)] = 0
  ## PY3 advanced stats
  Adv_Stats_PY3 <- cfbd_stats_season_advanced(year = as.integer(year) - 3, excl_garbage_time = FALSE, start_week = 1, end_week = 15) |>
    filter(team != "James Madison" & team != "Sam Houston State" & team != "Jacksonville State" & team != "Connecticut" & team != "New Mexico State" & team != "Old Dominion") |>
    select(team, off_ppa, off_success_rate, off_explosiveness, off_power_success,
           off_stuff_rate, off_line_yds, off_second_lvl_yds, off_open_field_yds,
           off_pts_per_opp, off_field_pos_avg_predicted_points, off_havoc_total, 
           off_havoc_front_seven, off_havoc_db, off_standard_downs_ppa,
           off_standard_downs_success_rate, off_standard_downs_explosiveness,
           off_passing_downs_ppa, off_passing_downs_success_rate,
           off_passing_downs_explosiveness, off_rushing_plays_ppa,
           off_rushing_plays_success_rate, off_rushing_plays_explosiveness,
           off_passing_plays_ppa, off_passing_plays_success_rate,
           off_passing_plays_explosiveness, def_ppa, def_success_rate,
           def_explosiveness, def_power_success, def_stuff_rate, def_line_yds,
           def_second_lvl_yds, def_open_field_yds, def_pts_per_opp, 
           def_field_pos_avg_predicted_points, def_havoc_total, def_havoc_front_seven,
           def_havoc_db, def_standard_downs_ppa, def_standard_downs_success_rate,
           def_standard_downs_explosiveness , def_passing_downs_ppa,
           def_passing_downs_success_rate, def_passing_downs_explosiveness,
           def_rushing_plays_ppa, def_rushing_plays_success_rate,
           def_rushing_plays_explosiveness, def_passing_plays_ppa,
           def_passing_plays_success_rate, def_passing_plays_explosiveness)
  ## removing NAs
  Adv_Stats_PY3[is.na(Adv_Stats_PY3)] = 0
  
  ## pulling in SP+ data
  ## PY1 SP Ratings
  SP_Rankings_PY1 <-cfbd_ratings_sp(year = as.integer(year) - 1) |>
    filter(team != "Jacksonville State" & team != "Sam Houston State" & team != "nationalAverages" & team != "Sam Houston") |>
    select(team, rating, offense_rating, defense_rating, special_teams_rating)
  colnames(SP_Rankings_PY1) <- c("team", "sp_rating_PY1", "sp_offense_rating_PY1", "sp_defense_rating_PY1", "sp_special_teams_rating_PY1")
  ## Eliminating NAs
  SP_Rankings_PY1[is.na(SP_Rankings_PY1)] = 0
  ## PY2 SP Ratings
  SP_Rankings_PY2 <-cfbd_ratings_sp(year = as.integer(year) - 2) |>
    filter(team != "James Madison" & team != "Jacksonville State" & team != "Sam Houston State" & team != "nationalAverages" & team != "Sam Houston") |>
    select(team, rating, offense_rating, defense_rating, special_teams_rating)
  colnames(SP_Rankings_PY2) <- c("team", "sp_rating_PY2", "sp_offense_rating_PY2", "sp_defense_rating_PY2", "sp_special_teams_rating_PY2")
  ## Eliminating NAs
  SP_Rankings_PY2[is.na(SP_Rankings_PY2)] = 0
  ## PY3 SP Ratings
  SP_Rankings_PY3 <-cfbd_ratings_sp(year = as.integer(year) - 3) |>
    filter(team != "James Madison" & team != "Jacksonville State" & team != "Sam Houston State" & team != "nationalAverages" & team != "Sam Houston" & team != "Connecticut" & team != "New Mexico State" & team != "Old Dominion") |>
    select(team, rating, offense_rating, defense_rating, special_teams_rating)
  colnames(SP_Rankings_PY3) <- c("team", "sp_rating_PY3", "sp_offense_rating_PY3", "sp_defense_rating_PY3", "sp_special_teams_rating_PY3")
  ## Eliminating NAs
  SP_Rankings_PY3[is.na(SP_Rankings_PY3)] = 0
  
  ## pulling FPI data
  FPI_df_PY1 <- espn_ratings_fpi(year = as.integer(year) - 1) |>
    filter(team_name != "Sam Houston State" & team_name != "Jacksonville State" & team_name != "Sam Houston") |>
    select(team_name, fpi, w, l)
  ## reading in PY2 FPI data
  FPI_df_PY2 <- espn_ratings_fpi(year = as.integer(year) - 2) |>
    filter(team_name != "James Madison" & team_name != "Jacksonville State" & team_name != "Sam Houston State" & team_name != "Sam Houston") |>
    select(team_name, fpi, w, l)
  ## reading in PY3 FPI data
  FPI_df_PY3 <- espn_ratings_fpi(year = as.integer(year) - 3) |>
    filter(team_name != "James Madison" & team_name != "Jacksonville State" & team_name != "Sam Houston State" & team_name != "Sam Houston" & team_name != "Connecticut" & team_name != "New Mexico State" & team_name != "Old Dominion") |>
    select(team_name, fpi, w, l)
  
  ## converting character columns to numeric
  FPI_df_PY1[,2:ncol(FPI_df_PY1)] <- FPI_df_PY1[,2:ncol(FPI_df_PY1)] |> mutate_if(is.character,as.numeric)
  FPI_df_PY2[,2:ncol(FPI_df_PY2)] <- FPI_df_PY2[,2:ncol(FPI_df_PY2)] |> mutate_if(is.character,as.numeric)
  FPI_df_PY3[,2:ncol(FPI_df_PY3)] <- FPI_df_PY3[,2:ncol(FPI_df_PY3)] |> mutate_if(is.character,as.numeric)
  ## removing NAs
  FPI_df_PY1[is.na(FPI_df_PY1)] = 0
  ## removing NAs
  FPI_df_PY2[is.na(FPI_df_PY2)] = 0
  ## removing NAs
  FPI_df_PY3[is.na(FPI_df_PY3)] = 0
  
  ## Changing FPI team names to match up with outputs of cfbdata functions
  ## Changing team names in FPI df to match what appears in cfbfastR stats function
  FPI_df_PY1 <- FPI_df_PY1 |>
    mutate(team = case_when(team_name == 'Appalachian St' ~ 'Appalachian State',
                            team_name == 'C Michigan' ~ 'Central Michigan',
                            team_name == 'Coast Carolina' ~ 'Coastal Carolina',
                            team_name == 'Coastal Car' ~ 'Coastal Carolina',
                            team_name == 'UConn' ~ 'Connecticut',
                            team_name == 'E Michigan' ~ 'Eastern Michigan',
                            team_name == 'FAU' ~ 'Florida Atlantic',
                            team_name == 'Florida Intl' ~ 'Florida International',
                            team_name == 'FIU' ~ 'Florida International',
                            team_name == 'Georgia So' ~ 'Georgia Southern',
                            team_name == 'UL Monroe' ~ 'Louisiana Monroe',
                            team_name == 'LA Tech' ~ 'Louisiana Tech',
                            team_name == 'MTSU' ~ 'Middle Tennessee',
                            team_name == 'Mississippi St' ~ 'Mississippi State',
                            team_name == 'New Mexico St' ~ 'New Mexico State',
                            team_name == 'N Illinois' ~ 'Northern Illinois',
                            team_name == 'Oklahoma St' ~ 'Oklahoma State',
                            team_name == 'Oregon St' ~ 'Oregon State',
                            team_name == 'San Jose State' ~ 'San José State',
                            team_name == 'Southern Miss' ~ 'Southern Mississippi',
                            team_name == 'UTSA' ~ 'UT San Antonio',
                            team_name == 'Washington St' ~ 'Washington State',
                            team_name == 'Western KY' ~ 'Western Kentucky',
                            team_name == 'W Michigan' ~ 'Western Michigan',
                            team_name == 'Arizona St' ~ 'Arizona State',
                            team_name == 'Arkansas St' ~ 'Arkansas State',
                            team_name == 'Boise St' ~ 'Boise State',
                            team_name == 'Colorado St' ~ 'Colorado State',
                            team_name == 'Florida St' ~ 'Florida State',
                            team_name == 'Fresno St' ~ 'Fresno State',
                            team_name == 'Georgia St' ~ 'Georgia State',
                            team_name == 'Kansas St' ~ 'Kansas State',
                            team_name == 'Miami OH' ~ 'Miami (OH)',
                            team_name == 'Michigan St' ~ 'Michigan State',
                            team_name == 'Pitt' ~ 'Pittsburgh',
                            team_name == 'San Diego St' ~ 'San Diego State',
                            team_name == 'San José St' ~ 'San José State',
                            team_name == 'Texas St' ~ 'Texas State',
                            team_name == 'Sam Houston' ~ 'Sam Houston State',
                            team_name == 'GA Southern' ~ 'Georgia Southern',
                            team_name == 'Massachusetts' ~ 'UMass',
                            team_name == "J'Ville St" ~ 'Jacksonville State',
                            TRUE ~ team_name)) |>
    select(team, fpi, w, l)
  FPI_df_PY2 <- FPI_df_PY2 |>
    mutate(team = case_when(team_name == 'Appalachian St' ~ 'Appalachian State',
                            team_name == 'C Michigan' ~ 'Central Michigan',
                            team_name == 'Coast Carolina' ~ 'Coastal Carolina',
                            team_name == 'Coastal Car' ~ 'Coastal Carolina',
                            team_name == 'UConn' ~ 'Connecticut',
                            team_name == 'E Michigan' ~ 'Eastern Michigan',
                            team_name == 'FAU' ~ 'Florida Atlantic',
                            team_name == 'Florida Intl' ~ 'Florida International',
                            team_name == 'FIU' ~ 'Florida International',
                            team_name == 'Georgia So' ~ 'Georgia Southern',
                            team_name == 'UL Monroe' ~ 'Louisiana Monroe',
                            team_name == 'LA Tech' ~ 'Louisiana Tech',
                            team_name == 'MTSU' ~ 'Middle Tennessee',
                            team_name == 'Mississippi St' ~ 'Mississippi State',
                            team_name == 'New Mexico St' ~ 'New Mexico State',
                            team_name == 'N Illinois' ~ 'Northern Illinois',
                            team_name == 'Oklahoma St' ~ 'Oklahoma State',
                            team_name == 'Oregon St' ~ 'Oregon State',
                            team_name == 'San Jose State' ~ 'San José State',
                            team_name == 'Southern Miss' ~ 'Southern Mississippi',
                            team_name == 'UTSA' ~ 'UT San Antonio',
                            team_name == 'Washington St' ~ 'Washington State',
                            team_name == 'Western KY' ~ 'Western Kentucky',
                            team_name == 'W Michigan' ~ 'Western Michigan',
                            team_name == 'Arizona St' ~ 'Arizona State',
                            team_name == 'Arkansas St' ~ 'Arkansas State',
                            team_name == 'Boise St' ~ 'Boise State',
                            team_name == 'Colorado St' ~ 'Colorado State',
                            team_name == 'Florida St' ~ 'Florida State',
                            team_name == 'Fresno St' ~ 'Fresno State',
                            team_name == 'Georgia St' ~ 'Georgia State',
                            team_name == 'Kansas St' ~ 'Kansas State',
                            team_name == 'Miami OH' ~ 'Miami (OH)',
                            team_name == 'Michigan St' ~ 'Michigan State',
                            team_name == 'Pitt' ~ 'Pittsburgh',
                            team_name == 'San Diego St' ~ 'San Diego State',
                            team_name == 'San José St' ~ 'San José State',
                            team_name == 'Texas St' ~ 'Texas State',
                            team_name == 'Sam Houston' ~ 'Sam Houston State',
                            team_name == 'GA Southern' ~ 'Georgia Southern',
                            team_name == 'Massachusetts' ~ 'UMass',
                            team_name == "J'Ville St" ~ 'Jacksonville State',
                            TRUE ~ team_name)) |>
    select(team, fpi, w, l)
  FPI_df_PY3 <- FPI_df_PY3 |>
    mutate(team = case_when(team_name == 'Appalachian St' ~ 'Appalachian State',
                            team_name == 'C Michigan' ~ 'Central Michigan',
                            team_name == 'Coast Carolina' ~ 'Coastal Carolina',
                            team_name == 'Coastal Car' ~ 'Coastal Carolina',
                            team_name == 'UConn' ~ 'Connecticut',
                            team_name == 'E Michigan' ~ 'Eastern Michigan',
                            team_name == 'FAU' ~ 'Florida Atlantic',
                            team_name == 'Florida Intl' ~ 'Florida International',
                            team_name == 'FIU' ~ 'Florida International',
                            team_name == 'Georgia So' ~ 'Georgia Southern',
                            team_name == 'UL Monroe' ~ 'Louisiana Monroe',
                            team_name == 'LA Tech' ~ 'Louisiana Tech',
                            team_name == 'MTSU' ~ 'Middle Tennessee',
                            team_name == 'Mississippi St' ~ 'Mississippi State',
                            team_name == 'New Mexico St' ~ 'New Mexico State',
                            team_name == 'N Illinois' ~ 'Northern Illinois',
                            team_name == 'Oklahoma St' ~ 'Oklahoma State',
                            team_name == 'Oregon St' ~ 'Oregon State',
                            team_name == 'San Jose State' ~ 'San José State',
                            team_name == 'Southern Miss' ~ 'Southern Mississippi',
                            team_name == 'UTSA' ~ 'UT San Antonio',
                            team_name == 'Washington St' ~ 'Washington State',
                            team_name == 'Western KY' ~ 'Western Kentucky',
                            team_name == 'W Michigan' ~ 'Western Michigan',
                            team_name == 'Arizona St' ~ 'Arizona State',
                            team_name == 'Arkansas St' ~ 'Arkansas State',
                            team_name == 'Boise St' ~ 'Boise State',
                            team_name == 'Colorado St' ~ 'Colorado State',
                            team_name == 'Florida St' ~ 'Florida State',
                            team_name == 'Fresno St' ~ 'Fresno State',
                            team_name == 'Georgia St' ~ 'Georgia State',
                            team_name == 'Kansas St' ~ 'Kansas State',
                            team_name == 'Miami OH' ~ 'Miami (OH)',
                            team_name == 'Michigan St' ~ 'Michigan State',
                            team_name == 'Pitt' ~ 'Pittsburgh',
                            team_name == 'San Diego St' ~ 'San Diego State',
                            team_name == 'San José St' ~ 'San José State',
                            team_name == 'Texas St' ~ 'Texas State',
                            team_name == 'Sam Houston' ~ 'Sam Houston State',
                            team_name == 'GA Southern' ~ 'Georgia Southern',
                            team_name == 'Massachusetts' ~ 'UMass',
                            team_name == "J'Ville St" ~ 'Jacksonville State',
                            TRUE ~ team_name)) |>
    select(team, fpi, w, l)
  
  ## changing column names here since all of the columns used in the VoA are extracted in the first step
  FPI_PY1_colnames <- c("team", "FPI_PY1", "Wins_PY1", "Losses_PY1")
  FPI_PY2_colnames <- c("team", "FPI_PY2", "Wins_PY2", "Losses_PY2")
  FPI_PY3_colnames <- c("team", "FPI_PY3", "Wins_PY3", "Losses_PY3")
  colnames(FPI_df_PY1) <- FPI_PY1_colnames
  colnames(FPI_df_PY2) <- FPI_PY2_colnames
  colnames(FPI_df_PY3) <- FPI_PY3_colnames
  
  ## pulling in recruiting rankings
  recruit_PY1 <- cfbd_recruiting_team(year = as.numeric(year) - 1) |>
    filter(team != "Sam Houston State" & team != "Jacksonville State") |>
    filter(team %in% Stats_PY1$team) |>
    select(team, points)
  colnames(recruit_PY1) <- c("team", "recruit_pts_PY1")
  
  ## pulling in talent rankings
  talent_df_PY1 <- cfbd_team_talent(year = as.numeric(year) - 1) |>
    filter(school != "Sam Houston State" & school != "Jacksonville State") |>
    filter(school %in% Stats_PY1$team) |>
    select(school, talent)
  colnames(talent_df_PY1) <- c("team", "talent_PY1")
  
  ## pulling in recruiting rankings
  recruit_PY2 <- cfbd_recruiting_team(year = as.numeric(year) - 2) |>
    filter(team != "James Madison" & team != "Sam Houston State" & team != "Jacksonville State") |>
    filter(team %in% Stats_PY2$team) |>
    select(team, points)
  colnames(recruit_PY2) <- c("team", "recruit_pts_PY2")
  
  ## pulling in talent rankings
  talent_df_PY2 <- cfbd_team_talent(year = as.numeric(year) - 2) |>
    filter(school != "James Madison" & school != "Sam Houston State" & school != "Jacksonville State") |>
    filter(school %in% Stats_PY2$team) |>
    select(school, talent)
  colnames(talent_df_PY2) <- c("team", "talent_PY2")
  
  ## pulling in recruiting rankings
  recruit_PY3 <- cfbd_recruiting_team(year = as.numeric(year) - 3) |>
    filter(team != "James Madison" & team != "Sam Houston State" & team != "Jacksonville State" & team != "Connecticut" & team != "New Mexico State" & team != "Old Dominion") |>
    filter(team %in% Stats_PY3$team) |>
    select(team, points)
  colnames(recruit_PY3) <- c("team", "recruit_pts_PY3")
  
  ## pulling in talent rankings
  talent_df_PY3 <- cfbd_team_talent(year = as.numeric(year) - 3) |>
    filter(school != "James Madison" & school != "Sam Houston State" & school != "Jacksonville State" & school != "Connecticut" & school != "New Mexico State" & school != "Old Dominion") |>
    filter(school %in% Stats_PY3$team) |>
    select(school, talent)
  colnames(talent_df_PY3) <- c("team", "talent_PY3")
  
  ## incoming recruiting class rankings
  recruit <- cfbd_recruiting_team(year = as.numeric(year)) |>
    select(team, points)
  recruit <- recruit |>
    mutate(school = case_when(team == "Florida Intl" ~ "Florida International",
                              TRUE ~ team)) |>
    filter(school %in% Stats_PY1$team) |>
    select(school, points)
  recruit[,2] <- recruit[,2] |> mutate_if(is.character, as.numeric)
  colnames(recruit) <- c("team", "recruit_pts")
  
  ## Pulling Simple Ranking System (SRS) data
  SRS_PY3 <- cfbd_ratings_srs(year = as.numeric(year) - 3) |>
    filter(team != "Connecticut" & team != "New Mexico State" & team != "Old Dominion") |>
    select(team, rating)
  colnames(SRS_PY3) <- c("team", "SRS_rating_PY3")
  ## IFF SRS data for PY3 is missing due to whatever issue
  missing_SRSPY3_teams <- anti_join(Stats_PY1, SRS_PY3) |>
    filter(team != "James Madison")
  if (nrow(missing_SRSPY3_teams) > 0) {
    SampleSRS_PY3 <- cfbd_ratings_srs(year = as.numeric(year) - 3) |>
      select(team, conference, rating)
    missing_SRSPY3teams <- missing_SRSPY3_teams |>
      select(team) |>
      mutate(SRS_rating_PY3 = mean(SampleSRS_PY3$rating))
    SRS_PY3 <- rbind(SRS_PY3, missing_SRSPY3teams) |>
      filter(team != "Connecticut" & team != "New Mexico State" & team != "Old Dominion")
  } else {
    print("no teams missing from SRS_PY3 data frame")
  }
  
  ## SRS PY2
  SRS_PY2 <- cfbd_ratings_srs(year = as.numeric(year) - 2) |>
    select(team, rating)
  colnames(SRS_PY2) <- c("team", "SRS_rating_PY2")
  ## IFF SRS data for PY2 is missing due to whatever issue
  missing_SRSPY2_teams <- anti_join(Stats_PY1, SRS_PY2) |>
    filter(team != "James Madison")
  if (nrow(missing_SRSPY2_teams) > 0) {
    SampleSRS_PY2 <- cfbd_ratings_srs(year = as.numeric(year) - 2) |>
      select(team, conference, rating)
    missing_SRSPY2teams <- missing_SRSPY2_teams |>
      select(team) |>
      mutate(SRS_rating_PY2 = mean(SampleSRS_PY2$rating))
    SRS_PY2 <- rbind(SRS_PY2, missing_SRSPY2teams)
  } else {
    print("no teams missing from SRS_PY2 data frame")
  }
  
  ## SRS PY1
  SRS_PY1 <- cfbd_ratings_srs(year = as.numeric(year) - 1) |>
    select(team, rating) |>
    filter(team %in% Stats_PY1$team)
  colnames(SRS_PY1) <- c("team", "SRS_rating_PY1")
  ## IFF SRS data for PY1 is missing due to whatever issue
  missing_SRSPY1_teams <- anti_join(Stats_PY1, SRS_PY1)
  if (nrow(missing_SRSPY1_teams) > 0) {
    SampleSRS_PY1 <- cfbd_ratings_srs(year = as.numeric(year) - 1) |>
      select(team, conference, rating)
    missing_SRSPY1teams <- missing_SRSPY1_teams |>
      select(team) |>
      mutate(SRS_rating_PY1 = mean(SampleSRS_PY1$rating))
    SRS_PY1 <- rbind(SRS_PY1, missing_SRSPY1teams)
  } else {
    print("no teams missing from SRS_PY1 data frame")
  }
} else if (as.numeric(week) == 1) {
  ##### WEEK 1 DATA PULL #####
  ## reading in data for 3 previous years
  FCS_PY3 <- read_csv(here("Data", "VoA2023", "FCSPrevYears", "FCS_PY3.csv"))
  FCS_PY2 <- read_csv(here("Data", "VoA2023", "FCSPrevYears", "FCS_PY2.csv"))
  FCS_PY1 <- read_csv(here("Data", "VoA2023", "FCSPrevYears", "FCS_PY1.csv"))
  ### CURRENT SEASON STATS
  Stats <- cfbd_stats_season_team(year = as.integer(year), start_week = 1, end_week = as.numeric(week)) |>
    mutate(total_yds_pg = total_yds/games,
           pass_yds_pg = net_pass_yds/games,
           rush_yds_pg = rush_yds/games,
           first_downs_pg = first_downs/games,
           def_interceptions_pg = passes_intercepted/games,
           pass_ypa = net_pass_yds / pass_atts,
           off_ypp = total_yds / (rush_atts + pass_atts),
           completion_pct = pass_comps / pass_atts,
           pass_ypr = net_pass_yds / pass_comps,
           int_pct = interceptions / pass_atts,
           rush_ypc = rush_yds / rush_atts,
           turnovers_pg = turnovers / games,
           third_conv_rate = third_down_convs / third_downs,
           fourth_conv_rate = fourth_down_convs / fourth_downs,
           penalty_yds_pg = penalty_yds / games,
           yards_per_penalty = penalty_yds / penalties,
           kick_return_avg = kick_return_yds / kick_returns,
           punt_return_avg = punt_return_yds / punt_returns)
  Stats[is.na(Stats)] = 0
  
  ## advanced stats data
  Adv_Stats <- cfbd_stats_season_advanced(year = as.integer(year), excl_garbage_time = FALSE, start_week = 1, end_week = as.numeric(week)) |>
    select(team, off_ppa, off_success_rate, off_explosiveness, off_power_success,
           off_stuff_rate, off_line_yds, off_second_lvl_yds, off_open_field_yds,
           off_pts_per_opp, off_field_pos_avg_predicted_points, off_havoc_total, 
           off_havoc_front_seven, off_havoc_db, off_standard_downs_ppa,
           off_standard_downs_success_rate, off_standard_downs_explosiveness,
           off_passing_downs_ppa, off_passing_downs_success_rate,
           off_passing_downs_explosiveness, off_rushing_plays_ppa,
           off_rushing_plays_success_rate, off_rushing_plays_explosiveness,
           off_passing_plays_ppa, off_passing_plays_success_rate,
           off_passing_plays_explosiveness, def_ppa, def_success_rate,
           def_explosiveness, def_power_success, def_stuff_rate, def_line_yds,
           def_second_lvl_yds, def_open_field_yds, def_pts_per_opp, 
           def_field_pos_avg_predicted_points, def_havoc_total, def_havoc_front_seven,
           def_havoc_db, def_standard_downs_ppa, def_standard_downs_success_rate,
           def_standard_downs_explosiveness , def_passing_downs_ppa,
           def_passing_downs_success_rate, def_passing_downs_explosiveness,
           def_rushing_plays_ppa, def_rushing_plays_success_rate,
           def_rushing_plays_explosiveness, def_passing_plays_ppa,
           def_passing_plays_success_rate, def_passing_plays_explosiveness)
  Adv_Stats[is.na(Adv_Stats)] = 0
  
  ## current FPI data as of this week
  ## pulling FPI data
  FPI_df <- espn_ratings_fpi(year = as.integer(year)) |>
    select(team_name, fpi, w, l)
  ## converting character columns to numeric
  FPI_df[,2:ncol(FPI_df)] <- FPI_df[,2:ncol(FPI_df)] |> mutate_if(is.character,as.numeric)
  
  ## Changing FPI team names to match up with outputs of cfbdata functions
  ## Changing team names in FPI df to match what appears in cfbfastR stats function
  FPI_df <- FPI_df |>
    mutate(team = case_when(team_name == 'Appalachian St' ~ 'Appalachian State',
                            team_name == 'C Michigan' ~ 'Central Michigan',
                            team_name == 'Coast Carolina' ~ 'Coastal Carolina',
                            team_name == 'Coastal Car' ~ 'Coastal Carolina',
                            team_name == 'UConn' ~ 'Connecticut',
                            team_name == 'E Michigan' ~ 'Eastern Michigan',
                            team_name == 'FAU' ~ 'Florida Atlantic',
                            team_name == 'Florida Intl' ~ 'Florida International',
                            team_name == 'FIU' ~ 'Florida International',
                            team_name == 'Georgia So' ~ 'Georgia Southern',
                            team_name == 'UL Monroe' ~ 'Louisiana Monroe',
                            team_name == 'LA Tech' ~ 'Louisiana Tech',
                            team_name == 'MTSU' ~ 'Middle Tennessee',
                            team_name == 'Mississippi St' ~ 'Mississippi State',
                            team_name == 'New Mexico St' ~ 'New Mexico State',
                            team_name == 'N Illinois' ~ 'Northern Illinois',
                            team_name == 'Oklahoma St' ~ 'Oklahoma State',
                            team_name == 'Oregon St' ~ 'Oregon State',
                            team_name == 'San Jose State' ~ 'San José State',
                            team_name == 'Southern Miss' ~ 'Southern Mississippi',
                            team_name == 'UTSA' ~ 'UT San Antonio',
                            team_name == 'Washington St' ~ 'Washington State',
                            team_name == 'Western KY' ~ 'Western Kentucky',
                            team_name == 'W Michigan' ~ 'Western Michigan',
                            team_name == 'Arizona St' ~ 'Arizona State',
                            team_name == 'Arkansas St' ~ 'Arkansas State',
                            team_name == 'Boise St' ~ 'Boise State',
                            team_name == 'Colorado St' ~ 'Colorado State',
                            team_name == 'Florida St' ~ 'Florida State',
                            team_name == 'Fresno St' ~ 'Fresno State',
                            team_name == 'Georgia St' ~ 'Georgia State',
                            team_name == 'Kansas St' ~ 'Kansas State',
                            team_name == 'Miami OH' ~ 'Miami (OH)',
                            team_name == 'Michigan St' ~ 'Michigan State',
                            team_name == 'Pitt' ~ 'Pittsburgh',
                            team_name == 'San Diego St' ~ 'San Diego State',
                            team_name == 'San José St' ~ 'San José State',
                            team_name == 'Texas St' ~ 'Texas State',
                            team_name == 'Sam Houston' ~ 'Sam Houston State',
                            team_name == 'GA Southern' ~ 'Georgia Southern',
                            team_name == 'Massachusetts' ~ 'UMass',
                            team_name == "J'Ville St" ~ 'Jacksonville State',
                            TRUE ~ team_name)) |>
    select(team, fpi, w, l)
  ## changing column names here since all of the columns used in the VoA are extracted in the first step
  FPI_colnames <- c("team", "FPI", "Wins", "Losses")
  colnames(FPI_df) <- FPI_colnames
  
  ## Current SP+ data
  # SP_Rankings <-cfbd_ratings_sp(year = as.integer(year)) |>
  #   filter(team != "nationalAverages") |>
  #   select(team, rating, offense_rating, defense_rating, special_teams_rating)
  # colnames(SP_Rankings) <- c("team", "sp_rating", "sp_offense_rating", "sp_defense_rating", "sp_special_teams_rating")
  # ## Eliminating NAs
  # SP_Rankings[is.na(SP_Rankings)] = 0
  
  ### Previous Years
  Stats_PY1 <- cfbd_stats_season_team(year = as.integer(year) - 1, start_week = 1, end_week = 15) |>
    filter(team != "Sam Houston State" & team != "Jacksonville State") |>
    mutate(total_yds_pg = total_yds/games,
           pass_yds_pg = net_pass_yds/games,
           rush_yds_pg = rush_yds/games,
           first_downs_pg = first_downs/games,
           def_interceptions_pg = passes_intercepted/games,
           pass_ypa = net_pass_yds / pass_atts,
           off_ypp = total_yds / (rush_atts + pass_atts),
           completion_pct = pass_comps / pass_atts,
           pass_ypr = net_pass_yds / pass_comps,
           int_pct = interceptions / pass_atts,
           rush_ypc = rush_yds / rush_atts,
           turnovers_pg = turnovers / games,
           third_conv_rate = third_down_convs / third_downs,
           fourth_conv_rate = fourth_down_convs / fourth_downs,
           penalty_yds_pg = penalty_yds / games,
           yards_per_penalty = penalty_yds / penalties,
           kick_return_avg = kick_return_yds / kick_returns,
           punt_return_avg = punt_return_yds / punt_returns)
  ## removing NAs
  Stats_PY1[is.na(Stats_PY1)] = 0
  ## PY2 stats
  Stats_PY2 <- cfbd_stats_season_team(year = as.integer(year) - 2, start_week = 1, end_week = 15) |>
    filter(team != "James Madison" & team != "Sam Houston State" & team!= "Jacksonville State") |>
    mutate(total_yds_pg = total_yds/games,
           pass_yds_pg = net_pass_yds/games,
           rush_yds_pg = rush_yds/games,
           first_downs_pg = first_downs/games,
           def_interceptions_pg = passes_intercepted/games,
           pass_ypa = net_pass_yds / pass_atts,
           off_ypp = total_yds / (rush_atts + pass_atts),
           completion_pct = pass_comps / pass_atts,
           pass_ypr = net_pass_yds / pass_comps,
           int_pct = interceptions / pass_atts,
           rush_ypc = rush_yds / rush_atts,
           turnovers_pg = turnovers / games,
           third_conv_rate = third_down_convs / third_downs,
           fourth_conv_rate = fourth_down_convs / fourth_downs,
           penalty_yds_pg = penalty_yds / games,
           yards_per_penalty = penalty_yds / penalties,
           kick_return_avg = kick_return_yds / kick_returns,
           punt_return_avg = punt_return_yds / punt_returns)
  ## removing NAs
  Stats_PY2[is.na(Stats_PY2)] = 0
  ## PY3 Stats
  Stats_PY3 <- cfbd_stats_season_team(year = as.integer(year) - 3, start_week = 1, end_week = 15) |>
    filter(team != "James Madison" & team != "Sam Houston State" & team != "Jacksonville State" & team != "New Mexico State" & team != "Connecticut" & team != "Old Dominion") |>
    mutate(total_yds_pg = total_yds/games,
           pass_yds_pg = net_pass_yds/games,
           rush_yds_pg = rush_yds/games,
           first_downs_pg = first_downs/games,
           def_interceptions_pg = passes_intercepted/games,
           pass_ypa = net_pass_yds / pass_atts,
           off_ypp = total_yds / (rush_atts + pass_atts),
           completion_pct = pass_comps / pass_atts,
           pass_ypr = net_pass_yds / pass_comps,
           int_pct = interceptions / pass_atts,
           rush_ypc = rush_yds / rush_atts,
           turnovers_pg = turnovers / games,
           third_conv_rate = third_down_convs / third_downs,
           fourth_conv_rate = fourth_down_convs / fourth_downs,
           penalty_yds_pg = penalty_yds / games,
           yards_per_penalty = penalty_yds / penalties,
           kick_return_avg = kick_return_yds / kick_returns,
           punt_return_avg = punt_return_yds / punt_returns)
  ## removing NAs
  Stats_PY3[is.na(Stats_PY3)] = 0
  
  ## advanced stats data
  Adv_Stats_PY1 <- cfbd_stats_season_advanced(year = as.integer(year) - 1, excl_garbage_time = FALSE, start_week = 1, end_week = 15) |>
    filter(team != "Jacksonville State" & team != "Sam Houston State") |>
    select(team, off_ppa, off_success_rate, off_explosiveness, off_power_success,
           off_stuff_rate, off_line_yds, off_second_lvl_yds, off_open_field_yds,
           off_pts_per_opp, off_field_pos_avg_predicted_points, off_havoc_total, 
           off_havoc_front_seven, off_havoc_db, off_standard_downs_ppa,
           off_standard_downs_success_rate, off_standard_downs_explosiveness,
           off_passing_downs_ppa, off_passing_downs_success_rate,
           off_passing_downs_explosiveness, off_rushing_plays_ppa,
           off_rushing_plays_success_rate, off_rushing_plays_explosiveness,
           off_passing_plays_ppa, off_passing_plays_success_rate,
           off_passing_plays_explosiveness, def_ppa, def_success_rate,
           def_explosiveness, def_power_success, def_stuff_rate, def_line_yds,
           def_second_lvl_yds, def_open_field_yds, def_pts_per_opp, 
           def_field_pos_avg_predicted_points, def_havoc_total, def_havoc_front_seven,
           def_havoc_db, def_standard_downs_ppa, def_standard_downs_success_rate,
           def_standard_downs_explosiveness , def_passing_downs_ppa,
           def_passing_downs_success_rate, def_passing_downs_explosiveness,
           def_rushing_plays_ppa, def_rushing_plays_success_rate,
           def_rushing_plays_explosiveness, def_passing_plays_ppa,
           def_passing_plays_success_rate, def_passing_plays_explosiveness)
  ## removing NAs
  Adv_Stats_PY1[is.na(Adv_Stats_PY1)] = 0
  
  ## IFF advanced stats data for current year is missing due to data issues
  missing_adv_teams <- anti_join(Stats, Adv_Stats)
  if (nrow(missing_adv_teams) > 0) {
    missing_adv_teams_adv_stats <- Adv_Stats_PY1 |>
      filter(team %in% missing_adv_teams$team) 
    Adv_Stats <- rbind(Adv_Stats, missing_adv_teams_adv_stats)
  } else {
    print("no teams missing from advanced stats data frame")
  }
  
  ## pulling in PY2 advanced stats
  Adv_Stats_PY2 <- cfbd_stats_season_advanced(year = as.integer(year) - 2, excl_garbage_time = FALSE, start_week = 1, end_week = 15) |>
    filter(team != "James Madison" & team != "Jacksonville State" & team != "Sam Houston State") |>
    select(team, off_ppa, off_success_rate, off_explosiveness, off_power_success,
           off_stuff_rate, off_line_yds, off_second_lvl_yds, off_open_field_yds,
           off_pts_per_opp, off_field_pos_avg_predicted_points, off_havoc_total, 
           off_havoc_front_seven, off_havoc_db, off_standard_downs_ppa,
           off_standard_downs_success_rate, off_standard_downs_explosiveness,
           off_passing_downs_ppa, off_passing_downs_success_rate,
           off_passing_downs_explosiveness, off_rushing_plays_ppa,
           off_rushing_plays_success_rate, off_rushing_plays_explosiveness,
           off_passing_plays_ppa, off_passing_plays_success_rate,
           off_passing_plays_explosiveness, def_ppa, def_success_rate,
           def_explosiveness, def_power_success, def_stuff_rate, def_line_yds,
           def_second_lvl_yds, def_open_field_yds, def_pts_per_opp, 
           def_field_pos_avg_predicted_points, def_havoc_total, def_havoc_front_seven,
           def_havoc_db, def_standard_downs_ppa, def_standard_downs_success_rate,
           def_standard_downs_explosiveness , def_passing_downs_ppa,
           def_passing_downs_success_rate, def_passing_downs_explosiveness,
           def_rushing_plays_ppa, def_rushing_plays_success_rate,
           def_rushing_plays_explosiveness, def_passing_plays_ppa,
           def_passing_plays_success_rate, def_passing_plays_explosiveness)
  ## removing NAs
  Adv_Stats_PY2[is.na(Adv_Stats_PY2)] = 0
  ## pulling in PY3 advanced stats
  Adv_Stats_PY3 <- cfbd_stats_season_advanced(year = as.integer(year) - 3, excl_garbage_time = FALSE, start_week = 1, end_week = 15) |>
    filter(team != "James Madison" & team != "Sam Houston State" & team != "Jacksonville State" & team != "New Mexico State" & team != "Connecticut" & team != "Old Dominion") |>
    select(team, off_ppa, off_success_rate, off_explosiveness, off_power_success,
           off_stuff_rate, off_line_yds, off_second_lvl_yds, off_open_field_yds,
           off_pts_per_opp, off_field_pos_avg_predicted_points, off_havoc_total, 
           off_havoc_front_seven, off_havoc_db, off_standard_downs_ppa,
           off_standard_downs_success_rate, off_standard_downs_explosiveness,
           off_passing_downs_ppa, off_passing_downs_success_rate,
           off_passing_downs_explosiveness, off_rushing_plays_ppa,
           off_rushing_plays_success_rate, off_rushing_plays_explosiveness,
           off_passing_plays_ppa, off_passing_plays_success_rate,
           off_passing_plays_explosiveness, def_ppa, def_success_rate,
           def_explosiveness, def_power_success, def_stuff_rate, def_line_yds,
           def_second_lvl_yds, def_open_field_yds, def_pts_per_opp, 
           def_field_pos_avg_predicted_points, def_havoc_total, def_havoc_front_seven,
           def_havoc_db, def_standard_downs_ppa, def_standard_downs_success_rate,
           def_standard_downs_explosiveness , def_passing_downs_ppa,
           def_passing_downs_success_rate, def_passing_downs_explosiveness,
           def_rushing_plays_ppa, def_rushing_plays_success_rate,
           def_rushing_plays_explosiveness, def_passing_plays_ppa,
           def_passing_plays_success_rate, def_passing_plays_explosiveness)
  ## removing NAs
  Adv_Stats_PY3[is.na(Adv_Stats_PY3)] = 0
  
  ## pulling in SP+ data
  SP_Rankings_PY1 <-cfbd_ratings_sp(year = as.integer(year) - 1) |>
    filter(team != "Sam Houston" & team != "Sam Houston State" & team != "Jacksonville State" & team != "nationalAverages") |>
    select(team, rating, offense_rating, defense_rating, special_teams_rating)
  colnames(SP_Rankings_PY1) <- c("team", "sp_rating_PY1", "sp_offense_rating_PY1", "sp_defense_rating_PY1", "sp_special_teams_rating_PY1")
  ## Eliminating NAs
  SP_Rankings_PY1[is.na(SP_Rankings_PY1)] = 0
  ## PY2 SP Ratings
  SP_Rankings_PY2 <-cfbd_ratings_sp(year = as.integer(year) - 2) |>
    filter(team != "James Madison" & team != "Sam Houston" & team != "Sam Houston State" & team != "Jacksonville State" & team != "nationalAverages") |>
    select(team, rating, offense_rating, defense_rating, special_teams_rating)
  colnames(SP_Rankings_PY2) <- c("team", "sp_rating_PY2", "sp_offense_rating_PY2", "sp_defense_rating_PY2", "sp_special_teams_rating_PY2")
  ## Eliminating NAs
  SP_Rankings_PY2[is.na(SP_Rankings_PY2)] = 0
  ## PY3 SP Ratings
  SP_Rankings_PY3 <-cfbd_ratings_sp(year = as.integer(year) - 3) |>
    filter(team != "James Madison" & team != "Sam Houston" & team != "Sam Houston State" & team != "Jacksonville State" & team != "nationalAverages" & team != "Connecticut" & team != "New Mexico State" & team != "Old Dominion") |>
    select(team, rating, offense_rating, defense_rating, special_teams_rating)
  colnames(SP_Rankings_PY3) <- c("team", "sp_rating_PY3", "sp_offense_rating_PY3", "sp_defense_rating_PY3", "sp_special_teams_rating_PY3")
  ## Eliminating NAs
  SP_Rankings_PY3[is.na(SP_Rankings_PY3)] = 0
  
  ## pulling FPI data
  FPI_df_PY1 <- espn_ratings_fpi(year = as.integer(year) - 1) |>
    filter(team_name != "Sam Houston" & team_name != "Sam Houston State" & team_name != "Jacksonville State") |>
    select(team_name, fpi, w, l)
  ## pulling in PY2 FPI data
  FPI_df_PY2 <- espn_ratings_fpi(year = as.integer(year) - 2) |>
    filter(team_name != "James Madison" & team_name != "Sam Houston" & team_name != "Sam Houston State" & team_name != "Jacksonville State") |>
    select(team_name, fpi, w, l)
  ## pulling in PY3 FPI data
  FPI_df_PY3 <- espn_ratings_fpi(year = as.integer(year) - 3) |>
    filter(team_name != "James Madison" & team_name != "Sam Houston" & team_name != "Sam Houston State" & team_name != "Jacksonville State" & team_name != "Connecticut" & team_name != "New Mexico State" & team_name != "Old Dominion") |>
    select(team_name, fpi, w, l)
  ## converting character columns to numeric
  FPI_df_PY1[,2:ncol(FPI_df_PY1)] <- FPI_df_PY1[,2:ncol(FPI_df_PY1)] |> mutate_if(is.character,as.numeric)
  FPI_df_PY2[,2:ncol(FPI_df_PY2)] <- FPI_df_PY2[,2:ncol(FPI_df_PY2)] |> mutate_if(is.character,as.numeric)
  FPI_df_PY3[,2:ncol(FPI_df_PY3)] <- FPI_df_PY3[,2:ncol(FPI_df_PY3)] |> mutate_if(is.character,as.numeric)
  ## removing NAs
  FPI_df_PY1[is.na(FPI_df_PY1)] = 0
  ## removing NAs
  FPI_df_PY2[is.na(FPI_df_PY2)] = 0
  ## removing NAs
  FPI_df_PY3[is.na(FPI_df_PY3)] = 0
  
  ## Changing FPI team names to match up with outputs of cfbdata functions
  ## Changing team names in FPI df to match what appears in cfbfastR stats function
  FPI_df_PY1 <- FPI_df_PY1 |>
    mutate(team = case_when(team_name == 'Appalachian St' ~ 'Appalachian State',
                            team_name == 'C Michigan' ~ 'Central Michigan',
                            team_name == 'Coast Carolina' ~ 'Coastal Carolina',
                            team_name == 'Coastal Car' ~ 'Coastal Carolina',
                            team_name == 'UConn' ~ 'Connecticut',
                            team_name == 'E Michigan' ~ 'Eastern Michigan',
                            team_name == 'FAU' ~ 'Florida Atlantic',
                            team_name == 'Florida Intl' ~ 'Florida International',
                            team_name == 'FIU' ~ 'Florida International',
                            team_name == 'Georgia So' ~ 'Georgia Southern',
                            team_name == 'UL Monroe' ~ 'Louisiana Monroe',
                            team_name == 'LA Tech' ~ 'Louisiana Tech',
                            team_name == 'MTSU' ~ 'Middle Tennessee',
                            team_name == 'Mississippi St' ~ 'Mississippi State',
                            team_name == 'New Mexico St' ~ 'New Mexico State',
                            team_name == 'N Illinois' ~ 'Northern Illinois',
                            team_name == 'Oklahoma St' ~ 'Oklahoma State',
                            team_name == 'Oregon St' ~ 'Oregon State',
                            team_name == 'San Jose State' ~ 'San José State',
                            team_name == 'Southern Miss' ~ 'Southern Mississippi',
                            team_name == 'UTSA' ~ 'UT San Antonio',
                            team_name == 'Washington St' ~ 'Washington State',
                            team_name == 'Western KY' ~ 'Western Kentucky',
                            team_name == 'W Michigan' ~ 'Western Michigan',
                            team_name == 'Arizona St' ~ 'Arizona State',
                            team_name == 'Arkansas St' ~ 'Arkansas State',
                            team_name == 'Boise St' ~ 'Boise State',
                            team_name == 'Colorado St' ~ 'Colorado State',
                            team_name == 'Florida St' ~ 'Florida State',
                            team_name == 'Fresno St' ~ 'Fresno State',
                            team_name == 'Georgia St' ~ 'Georgia State',
                            team_name == 'Kansas St' ~ 'Kansas State',
                            team_name == 'Miami OH' ~ 'Miami (OH)',
                            team_name == 'Michigan St' ~ 'Michigan State',
                            team_name == 'Pitt' ~ 'Pittsburgh',
                            team_name == 'San Diego St' ~ 'San Diego State',
                            team_name == 'San José St' ~ 'San José State',
                            team_name == 'Texas St' ~ 'Texas State',
                            team_name == 'Sam Houston' ~ 'Sam Houston State',
                            team_name == 'GA Southern' ~ 'Georgia Southern',
                            team_name == 'Massachusetts' ~ 'UMass',
                            team_name == "J'Ville St" ~ 'Jacksonville State',
                            TRUE ~ team_name)) |>
    select(team, fpi, w, l)
  FPI_df_PY2 <- FPI_df_PY2 |>
    mutate(team = case_when(team_name == 'Appalachian St' ~ 'Appalachian State',
                            team_name == 'C Michigan' ~ 'Central Michigan',
                            team_name == 'Coast Carolina' ~ 'Coastal Carolina',
                            team_name == 'Coastal Car' ~ 'Coastal Carolina',
                            team_name == 'UConn' ~ 'Connecticut',
                            team_name == 'E Michigan' ~ 'Eastern Michigan',
                            team_name == 'FAU' ~ 'Florida Atlantic',
                            team_name == 'Florida Intl' ~ 'Florida International',
                            team_name == 'FIU' ~ 'Florida International',
                            team_name == 'Georgia So' ~ 'Georgia Southern',
                            team_name == 'UL Monroe' ~ 'Louisiana Monroe',
                            team_name == 'LA Tech' ~ 'Louisiana Tech',
                            team_name == 'MTSU' ~ 'Middle Tennessee',
                            team_name == 'Mississippi St' ~ 'Mississippi State',
                            team_name == 'New Mexico St' ~ 'New Mexico State',
                            team_name == 'N Illinois' ~ 'Northern Illinois',
                            team_name == 'Oklahoma St' ~ 'Oklahoma State',
                            team_name == 'Oregon St' ~ 'Oregon State',
                            team_name == 'San Jose State' ~ 'San José State',
                            team_name == 'Southern Miss' ~ 'Southern Mississippi',
                            team_name == 'UTSA' ~ 'UT San Antonio',
                            team_name == 'Washington St' ~ 'Washington State',
                            team_name == 'Western KY' ~ 'Western Kentucky',
                            team_name == 'W Michigan' ~ 'Western Michigan',
                            team_name == 'Arizona St' ~ 'Arizona State',
                            team_name == 'Arkansas St' ~ 'Arkansas State',
                            team_name == 'Boise St' ~ 'Boise State',
                            team_name == 'Colorado St' ~ 'Colorado State',
                            team_name == 'Florida St' ~ 'Florida State',
                            team_name == 'Fresno St' ~ 'Fresno State',
                            team_name == 'Georgia St' ~ 'Georgia State',
                            team_name == 'Kansas St' ~ 'Kansas State',
                            team_name == 'Miami OH' ~ 'Miami (OH)',
                            team_name == 'Michigan St' ~ 'Michigan State',
                            team_name == 'Pitt' ~ 'Pittsburgh',
                            team_name == 'San Diego St' ~ 'San Diego State',
                            team_name == 'San José St' ~ 'San José State',
                            team_name == 'Texas St' ~ 'Texas State',
                            team_name == 'Sam Houston' ~ 'Sam Houston State',
                            team_name == 'GA Southern' ~ 'Georgia Southern',
                            team_name == 'Massachusetts' ~ 'UMass',
                            team_name == "J'Ville St" ~ 'Jacksonville State',
                            TRUE ~ team_name)) |>
    select(team, fpi, w, l)
  FPI_df_PY3 <- FPI_df_PY3 |>
    mutate(team = case_when(team_name == 'Appalachian St' ~ 'Appalachian State',
                            team_name == 'C Michigan' ~ 'Central Michigan',
                            team_name == 'Coast Carolina' ~ 'Coastal Carolina',
                            team_name == 'Coastal Car' ~ 'Coastal Carolina',
                            team_name == 'UConn' ~ 'Connecticut',
                            team_name == 'E Michigan' ~ 'Eastern Michigan',
                            team_name == 'FAU' ~ 'Florida Atlantic',
                            team_name == 'Florida Intl' ~ 'Florida International',
                            team_name == 'FIU' ~ 'Florida International',
                            team_name == 'Georgia So' ~ 'Georgia Southern',
                            team_name == 'UL Monroe' ~ 'Louisiana Monroe',
                            team_name == 'LA Tech' ~ 'Louisiana Tech',
                            team_name == 'MTSU' ~ 'Middle Tennessee',
                            team_name == 'Mississippi St' ~ 'Mississippi State',
                            team_name == 'New Mexico St' ~ 'New Mexico State',
                            team_name == 'N Illinois' ~ 'Northern Illinois',
                            team_name == 'Oklahoma St' ~ 'Oklahoma State',
                            team_name == 'Oregon St' ~ 'Oregon State',
                            team_name == 'San Jose State' ~ 'San José State',
                            team_name == 'Southern Miss' ~ 'Southern Mississippi',
                            team_name == 'UTSA' ~ 'UT San Antonio',
                            team_name == 'Washington St' ~ 'Washington State',
                            team_name == 'Western KY' ~ 'Western Kentucky',
                            team_name == 'W Michigan' ~ 'Western Michigan',
                            team_name == 'Arizona St' ~ 'Arizona State',
                            team_name == 'Arkansas St' ~ 'Arkansas State',
                            team_name == 'Boise St' ~ 'Boise State',
                            team_name == 'Colorado St' ~ 'Colorado State',
                            team_name == 'Florida St' ~ 'Florida State',
                            team_name == 'Fresno St' ~ 'Fresno State',
                            team_name == 'Georgia St' ~ 'Georgia State',
                            team_name == 'Kansas St' ~ 'Kansas State',
                            team_name == 'Miami OH' ~ 'Miami (OH)',
                            team_name == 'Michigan St' ~ 'Michigan State',
                            team_name == 'Pitt' ~ 'Pittsburgh',
                            team_name == 'San Diego St' ~ 'San Diego State',
                            team_name == 'San José St' ~ 'San José State',
                            team_name == 'Texas St' ~ 'Texas State',
                            team_name == 'Sam Houston' ~ 'Sam Houston State',
                            team_name == 'GA Southern' ~ 'Georgia Southern',
                            team_name == 'Massachusetts' ~ 'UMass',
                            team_name == "J'Ville St" ~ 'Jacksonville State',
                            TRUE ~ team_name)) |>
    select(team, fpi, w, l)
  ## changing column names here since all of the columns used in the VoA are extracted in the first step
  FPI_PY1_colnames <- c("team", "FPI_PY1", "Wins_PY1", "Losses_PY1")
  FPI_PY2_colnames <- c("team", "FPI_PY2", "Wins_PY2", "Losses_PY2")
  FPI_PY3_colnames <- c("team", "FPI_PY3", "Wins_PY3", "Losses_PY3")
  colnames(FPI_df_PY1) <- FPI_PY1_colnames
  colnames(FPI_df_PY2) <- FPI_PY2_colnames
  colnames(FPI_df_PY3) <- FPI_PY3_colnames
  
  ## pulling in recruiting rankings
  recruit_PY1 <- cfbd_recruiting_team(year = as.numeric(year) - 1) |>
    filter(team != "Sam Houston State" & team != "Jacksonville State") |>
    filter(team %in% Stats_PY1$team) |>
    select(team, points)
  recruit_PY1[,2] <- recruit_PY1[,2] |> mutate_if(is.character, as.numeric)
  colnames(recruit_PY1) <- c("team", "recruit_pts_PY1")
  
  ## pulling in talent rankings
  talent_df_PY1 <- cfbd_team_talent(year = as.numeric(year) - 1) |>
    filter(school != "Jacksonville State" & school != "Sam Houston State") |>
    filter(school %in% Stats_PY1$team) |>
    select(school, talent)
  colnames(talent_df_PY1) <- c("team", "talent_PY1")
  
  ## pulling in recruiting rankings
  recruit_PY2 <- cfbd_recruiting_team(year = as.numeric(year) - 2) |>
    filter(team != "Jacksonville State" & team != "Sam Houston State" & team != "James Madison") |>
    filter(team %in% Stats_PY2$team) |>
    select(team, points)
  recruit_PY2[,2] <- recruit_PY2[,2] |> mutate_if(is.character, as.numeric)
  colnames(recruit_PY2) <- c("team", "recruit_pts_PY2")
  
  ## pulling in talent rankings
  talent_df_PY2 <- cfbd_team_talent(year = as.numeric(year) - 2) |>
    filter(school != "Jacksonville State" & school != "Sam Houston State" & school != "James Madison") |>
    filter(school %in% Stats_PY2$team) |>
    select(school, talent)
  colnames(talent_df_PY2) <- c("team", "talent_PY2")
  
  ## pulling in recruiting rankings
  recruit_PY3 <- cfbd_recruiting_team(year = as.numeric(year) - 3) |>
    filter(team != "James Madison" & team != "Jacksonville State" & team != "Sam Houston State") |>
    filter(team %in% Stats_PY3$team) |>
    select(team, points)
  recruit_PY3[,2] <- recruit_PY3[,2] |> mutate_if(is.character, as.numeric)
  colnames(recruit_PY3) <- c("team", "recruit_pts_PY3")
  
  ## pulling in talent rankings
  talent_df_PY3 <- cfbd_team_talent(year = as.numeric(year) - 3) |>
    filter(school != "James Madison" & school != "Jacksonville State" & school != "Sam Houston State") |>
    filter(school %in% Stats_PY3$team) |>
    select(school, talent)
  colnames(talent_df_PY3) <- c("team", "talent_PY3")
  
  ## incoming recruiting class rankings
  recruit <- cfbd_recruiting_team(year = as.numeric(year)) |>
    select(team, points) |>
    mutate(school = case_when(team == "Florida Intl" ~ "Florida International",
                              TRUE ~ team)) |>
    filter(school %in% Stats$team) |>
    select(school, points)
  recruit[,2] <- recruit[,2] |> mutate_if(is.character, as.numeric)
  colnames(recruit) <- c("team", "recruit_pts")
  
  ## Pulling Simple Ranking System (SRS) data
  SRS_PY3 <- cfbd_ratings_srs(year = as.numeric(year) - 3) |>
    select(team, rating)
  colnames(SRS_PY3) <- c("team", "SRS_rating_PY3")
  ## IFF SRS data for PY3 is missing due to whatever issue
  missing_SRSPY3_teams <- anti_join(Stats_PY1, SRS_PY3) |>
    filter(team != "James Madison")
  if (nrow(missing_SRSPY3_teams) > 0) {
    SampleSRS_PY3 <- cfbd_ratings_srs(year = as.numeric(year) - 3) |>
      select(team, conference, rating)
    missing_SRSPY3teams <- missing_SRSPY3_teams |>
      select(team) |>
      mutate(SRS_rating_PY3 = mean(SampleSRS_PY3$rating))
    SRS_PY3 <- rbind(SRS_PY3, missing_SRSPY3teams) |>
      filter(team != "Connecticut" & team != "New Mexico State" & team != "Old Dominion")
  } else {
    print("no teams missing from SRS_PY3 data frame")
  }
  
  ## SRS PY2
  SRS_PY2 <- cfbd_ratings_srs(year = as.numeric(year) - 2) |>
    select(team, rating)
  colnames(SRS_PY2) <- c("team", "SRS_rating_PY2")
  ## IFF SRS data for PY2 is missing due to whatever issue
  missing_SRSPY2_teams <- anti_join(Stats_PY1, SRS_PY2) |>
    filter(team != "James Madison")
  if (nrow(missing_SRSPY2_teams) > 0) {
    SampleSRS_PY2 <- cfbd_ratings_srs(year = as.numeric(year) - 2) |>
      select(team, conference, rating)
    missing_SRSPY2teams <- missing_SRSPY2_teams |>
      select(team) |>
      mutate(SRS_rating_PY2 = mean(SampleSRS_PY2$rating))
    SRS_PY2 <- rbind(SRS_PY2, missing_SRSPY2teams)
  } else {
    print("no teams missing from SRS_PY2 data frame")
  }
  
  ## SRS PY1
  SRS_PY1 <- cfbd_ratings_srs(year = as.numeric(year) - 1) |>
    select(team, rating) |>
    filter(team %in% Stats_PY1$team)
  colnames(SRS_PY1) <- c("team", "SRS_rating_PY1")
  ## IFF SRS data for PY1 is missing due to whatever issue
  missing_SRSPY1_teams <- anti_join(Stats_PY1, SRS_PY1)
  if (nrow(missing_SRSPY1_teams) > 0) {
    SampleSRS_PY1 <- cfbd_ratings_srs(year = as.numeric(year) - 1) |>
      select(team, conference, rating)
    missing_SRSPY1teams <- missing_SRSPY1_teams |>
      select(team) |>
      mutate(SRS_rating_PY1 = mean(SampleSRS_PY1$rating))
    SRS_PY1 <- rbind(SRS_PY1, missing_SRSPY1teams)
  } else {
    print("no teams missing from SRS_PY1 data frame")
  }
  
  # ## Current SRS
  # SRS <- cfbd_ratings_srs(year = as.numeric(year)) |>
  #   select(team, rating) |>
  #   filter(team %in% Stats$team)
  # colnames(SRS) <- c("team", "SRS_rating")
  # ## IFF SRS data for PY1 is missing due to whatever issue
  # missing_SRS_teams <- anti_join(Stats, SRS)
  # if (nrow(missing_SRS_teams) > 0) {
  #   SampleSRS <- cfbd_ratings_srs(year = as.numeric(year)) |>
  #     select(team, conference, rating)
  #   missing_SRSteams <- missing_SRS_teams |>
  #     select(team) |>
  #     mutate(SRS_rating_PY1 = mean(SampleSRS$rating))
  #   SRS <- rbind(SRS, missing_SRSteams)
  # } else {
  #   print("no teams missing from SRS data frame")
  # }
} else if (as.numeric(week) <= 4) {
  ##### WEEKS 2-4 DATA PULL #####
  ## CURRENT SEASON STATS
  Stats <- cfbd_stats_season_team(year = as.integer(year), start_week = 1, end_week = as.numeric(week)) |>
    mutate(total_yds_pg = total_yds/games,
           pass_yds_pg = net_pass_yds/games,
           rush_yds_pg = rush_yds/games,
           first_downs_pg = first_downs/games,
           def_interceptions_pg = passes_intercepted/games,
           pass_ypa = net_pass_yds / pass_atts,
           off_ypp = total_yds / (rush_atts + pass_atts),
           completion_pct = pass_comps / pass_atts,
           pass_ypr = net_pass_yds / pass_comps,
           int_pct = interceptions / pass_atts,
           rush_ypc = rush_yds / rush_atts,
           turnovers_pg = turnovers / games,
           third_conv_rate = third_down_convs / third_downs,
           fourth_conv_rate = fourth_down_convs / fourth_downs,
           penalty_yds_pg = penalty_yds / games,
           yards_per_penalty = penalty_yds / penalties,
           kick_return_avg = kick_return_yds / kick_returns,
           punt_return_avg = punt_return_yds / punt_returns)
  Stats[is.na(Stats)] = 0
  
  ## advanced stats data
  Adv_Stats <- cfbd_stats_season_advanced(year = as.integer(year), excl_garbage_time = FALSE, start_week = 1, end_week = as.numeric(week)) |>
    select(team, off_ppa, off_success_rate, off_explosiveness, off_power_success,
           off_stuff_rate, off_line_yds, off_second_lvl_yds, off_open_field_yds,
           off_pts_per_opp, off_field_pos_avg_predicted_points, off_havoc_total, 
           off_havoc_front_seven, off_havoc_db, off_standard_downs_ppa,
           off_standard_downs_success_rate, off_standard_downs_explosiveness,
           off_passing_downs_ppa, off_passing_downs_success_rate,
           off_passing_downs_explosiveness, off_rushing_plays_ppa,
           off_rushing_plays_success_rate, off_rushing_plays_explosiveness,
           off_passing_plays_ppa, off_passing_plays_success_rate,
           off_passing_plays_explosiveness, def_ppa, def_success_rate,
           def_explosiveness, def_power_success, def_stuff_rate, def_line_yds,
           def_second_lvl_yds, def_open_field_yds, def_pts_per_opp, 
           def_field_pos_avg_predicted_points, def_havoc_total, def_havoc_front_seven,
           def_havoc_db, def_standard_downs_ppa, def_standard_downs_success_rate,
           def_standard_downs_explosiveness , def_passing_downs_ppa,
           def_passing_downs_success_rate, def_passing_downs_explosiveness,
           def_rushing_plays_ppa, def_rushing_plays_success_rate,
           def_rushing_plays_explosiveness, def_passing_plays_ppa,
           def_passing_plays_success_rate, def_passing_plays_explosiveness)
  Adv_Stats[is.na(Adv_Stats)] = 0
  ## current FPI data as of this week
  ## pulling FPI data
  FPI_df <- espn_ratings_fpi(year = as.integer(year)) |>
    select(team_name, fpi, w, l)
  ## converting character columns to numeric
  FPI_df[,2:ncol(FPI_df)] <- FPI_df[,2:ncol(FPI_df)] |> mutate_if(is.character,as.numeric)
  
  ## Changing FPI team names to match up with outputs of cfbdata functions
  ## Changing team names in FPI df to match what appears in cfbfastR stats function
  FPI_df <- FPI_df |>
    mutate(team = case_when(team_name == 'Appalachian St' ~ 'Appalachian State',
                            team_name == 'C Michigan' ~ 'Central Michigan',
                            team_name == 'Coast Carolina' ~ 'Coastal Carolina',
                            team_name == 'Coastal Car' ~ 'Coastal Carolina',
                            team_name == 'UConn' ~ 'Connecticut',
                            team_name == 'E Michigan' ~ 'Eastern Michigan',
                            team_name == 'FAU' ~ 'Florida Atlantic',
                            team_name == 'Florida Intl' ~ 'Florida International',
                            team_name == 'FIU' ~ 'Florida International',
                            team_name == 'Georgia So' ~ 'Georgia Southern',
                            team_name == 'UL Monroe' ~ 'Louisiana Monroe',
                            team_name == 'LA Tech' ~ 'Louisiana Tech',
                            team_name == 'MTSU' ~ 'Middle Tennessee',
                            team_name == 'Mississippi St' ~ 'Mississippi State',
                            team_name == 'New Mexico St' ~ 'New Mexico State',
                            team_name == 'N Illinois' ~ 'Northern Illinois',
                            team_name == 'Oklahoma St' ~ 'Oklahoma State',
                            team_name == 'Oregon St' ~ 'Oregon State',
                            team_name == 'San Jose State' ~ 'San José State',
                            team_name == 'Southern Miss' ~ 'Southern Mississippi',
                            team_name == 'UTSA' ~ 'UT San Antonio',
                            team_name == 'Washington St' ~ 'Washington State',
                            team_name == 'Western KY' ~ 'Western Kentucky',
                            team_name == 'W Michigan' ~ 'Western Michigan',
                            team_name == 'Arizona St' ~ 'Arizona State',
                            team_name == 'Arkansas St' ~ 'Arkansas State',
                            team_name == 'Boise St' ~ 'Boise State',
                            team_name == 'Colorado St' ~ 'Colorado State',
                            team_name == 'Florida St' ~ 'Florida State',
                            team_name == 'Fresno St' ~ 'Fresno State',
                            team_name == 'Georgia St' ~ 'Georgia State',
                            team_name == 'Kansas St' ~ 'Kansas State',
                            team_name == 'Miami OH' ~ 'Miami (OH)',
                            team_name == 'Michigan St' ~ 'Michigan State',
                            team_name == 'Pitt' ~ 'Pittsburgh',
                            team_name == 'San Diego St' ~ 'San Diego State',
                            team_name == 'San José St' ~ 'San José State',
                            team_name == 'Texas St' ~ 'Texas State',
                            team_name == 'Sam Houston' ~ 'Sam Houston State',
                            team_name == 'GA Southern' ~ 'Georgia Southern',
                            team_name == 'Massachusetts' ~ 'UMass',
                            team_name == "J'Ville St" ~ 'Jacksonville State',
                            TRUE ~ team_name)) |>
    select(team, fpi, w, l)
  ## changing column names here since all of the columns used in the VoA are extracted in the first step
  FPI_colnames <- c("team", "FPI", "Wins", "Losses")
  colnames(FPI_df) <- FPI_colnames
  
  ## Current SP+ data
  SP_Rankings <-cfbd_ratings_sp(year = as.integer(year)) |>
    filter(team != "nationalAverages") |>
    select(team, rating, offense_rating, defense_rating, special_teams_rating)
  colnames(SP_Rankings) <- c("team", "sp_rating", "sp_offense_rating", "sp_defense_rating", "sp_special_teams_rating")
  ## Eliminating NAs
  SP_Rankings[is.na(SP_Rankings)] = 0
  
  
  ## reading in data for 2 previous years
  FCS_PY2 <- read_csv(here("Data", "VoA2023", "FCSPrevYears", "FCS_PY2.csv"))
  FCS_PY1 <- read_csv(here("Data", "VoA2023", "FCSPrevYears", "FCS_PY1.csv"))
  ### Previous Years
  Stats_PY1 <- cfbd_stats_season_team(year = as.integer(year) - 1, start_week = 1, end_week = 15) |>
    filter(team != "Sam Houston State" & team != "Jacksonville State") |>
    mutate(total_yds_pg = total_yds/games,
           pass_yds_pg = net_pass_yds/games,
           rush_yds_pg = rush_yds/games,
           first_downs_pg = first_downs/games,
           def_interceptions_pg = passes_intercepted/games,
           pass_ypa = net_pass_yds / pass_atts,
           off_ypp = total_yds / (rush_atts + pass_atts),
           completion_pct = pass_comps / pass_atts,
           pass_ypr = net_pass_yds / pass_comps,
           int_pct = interceptions / pass_atts,
           rush_ypc = rush_yds / rush_atts,
           turnovers_pg = turnovers / games,
           third_conv_rate = third_down_convs / third_downs,
           fourth_conv_rate = fourth_down_convs / fourth_downs,
           penalty_yds_pg = penalty_yds / games,
           yards_per_penalty = penalty_yds / penalties,
           kick_return_avg = kick_return_yds / kick_returns,
           punt_return_avg = punt_return_yds / punt_returns)
  ## removing NAs
  Stats_PY1[is.na(Stats_PY1)] = 0
  ## PY2 stats
  Stats_PY2 <- cfbd_stats_season_team(year = as.integer(year) - 2, start_week = 1, end_week = 15) |>
    filter(team != "James Madison" & team != "Sam Houston State" & team!= "Jacksonville State") |>
    mutate(total_yds_pg = total_yds/games,
           pass_yds_pg = net_pass_yds/games,
           rush_yds_pg = rush_yds/games,
           first_downs_pg = first_downs/games,
           def_interceptions_pg = passes_intercepted/games,
           pass_ypa = net_pass_yds / pass_atts,
           off_ypp = total_yds / (rush_atts + pass_atts),
           completion_pct = pass_comps / pass_atts,
           pass_ypr = net_pass_yds / pass_comps,
           int_pct = interceptions / pass_atts,
           rush_ypc = rush_yds / rush_atts,
           turnovers_pg = turnovers / games,
           third_conv_rate = third_down_convs / third_downs,
           fourth_conv_rate = fourth_down_convs / fourth_downs,
           penalty_yds_pg = penalty_yds / games,
           yards_per_penalty = penalty_yds / penalties,
           kick_return_avg = kick_return_yds / kick_returns,
           punt_return_avg = punt_return_yds / punt_returns)
  ## removing NAs
  Stats_PY2[is.na(Stats_PY2)] = 0
  
  ## advanced stats data
  Adv_Stats_PY1 <- cfbd_stats_season_advanced(year = as.integer(year) - 1, excl_garbage_time = FALSE, start_week = 1, end_week = 15) |>
    filter(team != "Jacksonville State" & team != "Sam Houston State") |>
    select(team, off_ppa, off_success_rate, off_explosiveness, off_power_success,
           off_stuff_rate, off_line_yds, off_second_lvl_yds, off_open_field_yds,
           off_pts_per_opp, off_field_pos_avg_predicted_points, off_havoc_total, 
           off_havoc_front_seven, off_havoc_db, off_standard_downs_ppa,
           off_standard_downs_success_rate, off_standard_downs_explosiveness,
           off_passing_downs_ppa, off_passing_downs_success_rate,
           off_passing_downs_explosiveness, off_rushing_plays_ppa,
           off_rushing_plays_success_rate, off_rushing_plays_explosiveness,
           off_passing_plays_ppa, off_passing_plays_success_rate,
           off_passing_plays_explosiveness, def_ppa, def_success_rate,
           def_explosiveness, def_power_success, def_stuff_rate, def_line_yds,
           def_second_lvl_yds, def_open_field_yds, def_pts_per_opp, 
           def_field_pos_avg_predicted_points, def_havoc_total, def_havoc_front_seven,
           def_havoc_db, def_standard_downs_ppa, def_standard_downs_success_rate,
           def_standard_downs_explosiveness , def_passing_downs_ppa,
           def_passing_downs_success_rate, def_passing_downs_explosiveness,
           def_rushing_plays_ppa, def_rushing_plays_success_rate,
           def_rushing_plays_explosiveness, def_passing_plays_ppa,
           def_passing_plays_success_rate, def_passing_plays_explosiveness)
  ## removing NAs
  Adv_Stats_PY1[is.na(Adv_Stats_PY1)] = 0
  
  ## IFF advanced stats data for current year is missing due to data issues
  missing_adv_teams <- anti_join(Stats, Adv_Stats)
  if (nrow(missing_adv_teams) > 0) {
    missing_adv_teams_adv_stats <- Adv_Stats_PY1 |>
      filter(team %in% missing_adv_teams$team) 
    Adv_Stats <- rbind(Adv_Stats, missing_adv_teams_adv_stats)
  } else {
    print("no teams missing from advanced stats data frame")
  }
  
  ## pulling in PY2 advanced stats
  Adv_Stats_PY2 <- cfbd_stats_season_advanced(year = as.integer(year) - 2, excl_garbage_time = FALSE, start_week = 1, end_week = 15) |>
    filter(team != "James Madison" & team != "Jacksonville State" & team != "Sam Houston State") |>
    select(team, off_ppa, off_success_rate, off_explosiveness, off_power_success,
           off_stuff_rate, off_line_yds, off_second_lvl_yds, off_open_field_yds,
           off_pts_per_opp, off_field_pos_avg_predicted_points, off_havoc_total, 
           off_havoc_front_seven, off_havoc_db, off_standard_downs_ppa,
           off_standard_downs_success_rate, off_standard_downs_explosiveness,
           off_passing_downs_ppa, off_passing_downs_success_rate,
           off_passing_downs_explosiveness, off_rushing_plays_ppa,
           off_rushing_plays_success_rate, off_rushing_plays_explosiveness,
           off_passing_plays_ppa, off_passing_plays_success_rate,
           off_passing_plays_explosiveness, def_ppa, def_success_rate,
           def_explosiveness, def_power_success, def_stuff_rate, def_line_yds,
           def_second_lvl_yds, def_open_field_yds, def_pts_per_opp, 
           def_field_pos_avg_predicted_points, def_havoc_total, def_havoc_front_seven,
           def_havoc_db, def_standard_downs_ppa, def_standard_downs_success_rate,
           def_standard_downs_explosiveness , def_passing_downs_ppa,
           def_passing_downs_success_rate, def_passing_downs_explosiveness,
           def_rushing_plays_ppa, def_rushing_plays_success_rate,
           def_rushing_plays_explosiveness, def_passing_plays_ppa,
           def_passing_plays_success_rate, def_passing_plays_explosiveness)
  ## removing NAs
  Adv_Stats_PY2[is.na(Adv_Stats_PY2)] = 0
  
  ## pulling in SP+ data
  SP_Rankings_PY1 <-cfbd_ratings_sp(year = as.integer(year) - 1) |>
    filter(team != "Sam Houston" & team != "Sam Houston State" & team != "Jacksonville State" & team != "nationalAverages") |>
    select(team, rating, offense_rating, defense_rating, special_teams_rating)
  colnames(SP_Rankings_PY1) <- c("team", "sp_rating_PY1", "sp_offense_rating_PY1", "sp_defense_rating_PY1", "sp_special_teams_rating_PY1")
  ## Eliminating NAs
  SP_Rankings_PY1[is.na(SP_Rankings_PY1)] = 0
  ## PY2 SP Ratings
  SP_Rankings_PY2 <-cfbd_ratings_sp(year = as.integer(year) - 2) |>
    filter(team != "James Madison" & team != "Sam Houston" & team != "Sam Houston State" & team != "Jacksonville State" & team != "nationalAverages") |>
    select(team, rating, offense_rating, defense_rating, special_teams_rating)
  colnames(SP_Rankings_PY2) <- c("team", "sp_rating_PY2", "sp_offense_rating_PY2", "sp_defense_rating_PY2", "sp_special_teams_rating_PY2")
  ## Eliminating NAs
  SP_Rankings_PY2[is.na(SP_Rankings_PY2)] = 0
  
  ## pulling FPI data
  FPI_df_PY1 <- espn_ratings_fpi(year = as.integer(year) - 1) |>
    filter(team_name != "Sam Houston" & team_name != "Sam Houston State" & team_name != "Jacksonville State") |>
    select(team_name, fpi, w, l)
  ## pulling in PY2 FPI data
  FPI_df_PY2 <- espn_ratings_fpi(year = as.integer(year) - 2) |>
    filter(team_name != "James Madison" & team_name != "Sam Houston" & team_name != "Sam Houston State" & team_name != "Jacksonville State") |>
    select(team_name, fpi, w, l)
  
  ## converting character columns to numeric
  FPI_df_PY1[,2:ncol(FPI_df_PY1)] <- FPI_df_PY1[,2:ncol(FPI_df_PY1)] |> mutate_if(is.character,as.numeric)
  FPI_df_PY2[,2:ncol(FPI_df_PY2)] <- FPI_df_PY2[,2:ncol(FPI_df_PY2)] |> mutate_if(is.character,as.numeric)
  ## removing NAs
  FPI_df_PY1[is.na(FPI_df_PY1)] = 0
  ## removing NAs
  FPI_df_PY2[is.na(FPI_df_PY2)] = 0
  
  ## Changing FPI team names to match up with outputs of cfbdata functions
  ## Changing team names in FPI df to match what appears in cfbfastR stats function
  FPI_df_PY1 <- FPI_df_PY1 |>
    mutate(team = case_when(team_name == 'Appalachian St' ~ 'Appalachian State',
                            team_name == 'C Michigan' ~ 'Central Michigan',
                            team_name == 'Coast Carolina' ~ 'Coastal Carolina',
                            team_name == 'Coastal Car' ~ 'Coastal Carolina',
                            team_name == 'UConn' ~ 'Connecticut',
                            team_name == 'E Michigan' ~ 'Eastern Michigan',
                            team_name == 'FAU' ~ 'Florida Atlantic',
                            team_name == 'Florida Intl' ~ 'Florida International',
                            team_name == 'FIU' ~ 'Florida International',
                            team_name == 'Georgia So' ~ 'Georgia Southern',
                            team_name == 'UL Monroe' ~ 'Louisiana Monroe',
                            team_name == 'LA Tech' ~ 'Louisiana Tech',
                            team_name == 'MTSU' ~ 'Middle Tennessee',
                            team_name == 'Mississippi St' ~ 'Mississippi State',
                            team_name == 'New Mexico St' ~ 'New Mexico State',
                            team_name == 'N Illinois' ~ 'Northern Illinois',
                            team_name == 'Oklahoma St' ~ 'Oklahoma State',
                            team_name == 'Oregon St' ~ 'Oregon State',
                            team_name == 'San Jose State' ~ 'San José State',
                            team_name == 'Southern Miss' ~ 'Southern Mississippi',
                            team_name == 'UTSA' ~ 'UT San Antonio',
                            team_name == 'Washington St' ~ 'Washington State',
                            team_name == 'Western KY' ~ 'Western Kentucky',
                            team_name == 'W Michigan' ~ 'Western Michigan',
                            team_name == 'Arizona St' ~ 'Arizona State',
                            team_name == 'Arkansas St' ~ 'Arkansas State',
                            team_name == 'Boise St' ~ 'Boise State',
                            team_name == 'Colorado St' ~ 'Colorado State',
                            team_name == 'Florida St' ~ 'Florida State',
                            team_name == 'Fresno St' ~ 'Fresno State',
                            team_name == 'Georgia St' ~ 'Georgia State',
                            team_name == 'Kansas St' ~ 'Kansas State',
                            team_name == 'Miami OH' ~ 'Miami (OH)',
                            team_name == 'Michigan St' ~ 'Michigan State',
                            team_name == 'Pitt' ~ 'Pittsburgh',
                            team_name == 'San Diego St' ~ 'San Diego State',
                            team_name == 'San José St' ~ 'San José State',
                            team_name == 'Texas St' ~ 'Texas State',
                            team_name == 'Sam Houston' ~ 'Sam Houston State',
                            team_name == 'GA Southern' ~ 'Georgia Southern',
                            team_name == 'Massachusetts' ~ 'UMass',
                            team_name == "J'Ville St" ~ 'Jacksonville State',
                            TRUE ~ team_name)) |>
    select(team, fpi, w, l)
  FPI_df_PY2 <- FPI_df_PY2 |>
    mutate(team = case_when(team_name == 'Appalachian St' ~ 'Appalachian State',
                            team_name == 'C Michigan' ~ 'Central Michigan',
                            team_name == 'Coast Carolina' ~ 'Coastal Carolina',
                            team_name == 'Coastal Car' ~ 'Coastal Carolina',
                            team_name == 'UConn' ~ 'Connecticut',
                            team_name == 'E Michigan' ~ 'Eastern Michigan',
                            team_name == 'FAU' ~ 'Florida Atlantic',
                            team_name == 'Florida Intl' ~ 'Florida International',
                            team_name == 'FIU' ~ 'Florida International',
                            team_name == 'Georgia So' ~ 'Georgia Southern',
                            team_name == 'UL Monroe' ~ 'Louisiana Monroe',
                            team_name == 'LA Tech' ~ 'Louisiana Tech',
                            team_name == 'MTSU' ~ 'Middle Tennessee',
                            team_name == 'Mississippi St' ~ 'Mississippi State',
                            team_name == 'New Mexico St' ~ 'New Mexico State',
                            team_name == 'N Illinois' ~ 'Northern Illinois',
                            team_name == 'Oklahoma St' ~ 'Oklahoma State',
                            team_name == 'Oregon St' ~ 'Oregon State',
                            team_name == 'San Jose State' ~ 'San José State',
                            team_name == 'Southern Miss' ~ 'Southern Mississippi',
                            team_name == 'UTSA' ~ 'UT San Antonio',
                            team_name == 'Washington St' ~ 'Washington State',
                            team_name == 'Western KY' ~ 'Western Kentucky',
                            team_name == 'W Michigan' ~ 'Western Michigan',
                            team_name == 'Arizona St' ~ 'Arizona State',
                            team_name == 'Arkansas St' ~ 'Arkansas State',
                            team_name == 'Boise St' ~ 'Boise State',
                            team_name == 'Colorado St' ~ 'Colorado State',
                            team_name == 'Florida St' ~ 'Florida State',
                            team_name == 'Fresno St' ~ 'Fresno State',
                            team_name == 'Georgia St' ~ 'Georgia State',
                            team_name == 'Kansas St' ~ 'Kansas State',
                            team_name == 'Miami OH' ~ 'Miami (OH)',
                            team_name == 'Michigan St' ~ 'Michigan State',
                            team_name == 'Pitt' ~ 'Pittsburgh',
                            team_name == 'San Diego St' ~ 'San Diego State',
                            team_name == 'San José St' ~ 'San José State',
                            team_name == 'Texas St' ~ 'Texas State',
                            team_name == 'Sam Houston' ~ 'Sam Houston State',
                            team_name == 'GA Southern' ~ 'Georgia Southern',
                            team_name == 'Massachusetts' ~ 'UMass',
                            team_name == "J'Ville St" ~ 'Jacksonville State',
                            TRUE ~ team_name)) |>
    select(team, fpi, w, l)
  ## changing column names here since all of the columns used in the VoA are extracted in the first step
  FPI_PY1_colnames <- c("team", "FPI_PY1", "Wins_PY1", "Losses_PY1")
  FPI_PY2_colnames <- c("team", "FPI_PY2", "Wins_PY2", "Losses_PY2")
  colnames(FPI_df_PY1) <- FPI_PY1_colnames
  colnames(FPI_df_PY2) <- FPI_PY2_colnames
  
  ## pulling in recruiting rankings
  recruit_PY1 <- cfbd_recruiting_team(year = as.numeric(year) - 1) |>
    filter(team != "Sam Houston State" & team != "Jacksonville State") |>
    filter(team %in% Stats_PY1$team) |>
    select(team, points)
  recruit_PY1[,2] <- recruit_PY1[,2] |> mutate_if(is.character, as.numeric)
  colnames(recruit_PY1) <- c("team", "recruit_pts_PY1")
  
  ## pulling in talent rankings
  talent_df_PY1 <- cfbd_team_talent(year = as.numeric(year) - 1) |>
    filter(school != "Jacksonville State" & school != "Sam Houston State") |>
    filter(school %in% Stats_PY1$team) |>
    select(school, talent)
  colnames(talent_df_PY1) <- c("team", "talent_PY1")
  
  ## pulling in recruiting rankings
  recruit_PY2 <- cfbd_recruiting_team(year = as.numeric(year) - 2) |>
    filter(team != "Jacksonville State" & team != "Sam Houston State" & team != "James Madison") |>
    filter(team %in% Stats_PY2$team) |>
    select(team, points)
  recruit_PY2[,2] <- recruit_PY2[,2] |> mutate_if(is.character, as.numeric)
  colnames(recruit_PY2) <- c("team", "recruit_pts_PY2")
  
  ## pulling in talent rankings
  talent_df_PY2 <- cfbd_team_talent(year = as.numeric(year) - 2) |>
    filter(school != "Jacksonville State" & school != "Sam Houston State" & school != "James Madison") |>
    filter(school %in% Stats_PY2$team) |>
    select(school, talent)
  colnames(talent_df_PY2) <- c("team", "talent_PY2")
  
  ## pulling in recruiting rankings
  recruit_PY3 <- cfbd_recruiting_team(year = as.numeric(year) - 3) |>
    filter(team %in% Stats$team) |>
    select(team, points)
  recruit_PY3[,2] <- recruit_PY3[,2] |> mutate_if(is.character, as.numeric)
  colnames(recruit_PY3) <- c("team", "recruit_pts_PY3")

  ## incoming recruiting class rankings
  recruit <- cfbd_recruiting_team(year = as.numeric(year)) |>
    select(team, points) |>
    mutate(school = case_when(team == "Florida Intl" ~ "Florida International",
                              TRUE ~ team)) |>
    filter(school %in% Stats$team) |>
    select(school, points)
  recruit[,2] <- recruit[,2] |> mutate_if(is.character, as.numeric)
  colnames(recruit) <- c("team", "recruit_pts")
  
  ## SRS PY2
  SRS_PY2 <- cfbd_ratings_srs(year = as.numeric(year) - 2) |>
    select(team, rating)
  colnames(SRS_PY2) <- c("team", "SRS_rating_PY2")
  ## IFF SRS data for PY2 is missing due to whatever issue
  missing_SRSPY2_teams <- anti_join(Stats_PY1, SRS_PY2) |>
    filter(team != "James Madison")
  if (nrow(missing_SRSPY2_teams) > 0) {
    SampleSRS_PY2 <- cfbd_ratings_srs(year = as.numeric(year) - 2) |>
      select(team, conference, rating)
    missing_SRSPY2teams <- missing_SRSPY2_teams |>
      select(team) |>
      mutate(SRS_rating_PY2 = mean(SampleSRS_PY2$rating))
    SRS_PY2 <- rbind(SRS_PY2, missing_SRSPY2teams)
  } else {
    print("no teams missing from SRS_PY2 data frame")
  }
  
  ## SRS PY1
  SRS_PY1 <- cfbd_ratings_srs(year = as.numeric(year) - 1) |>
    select(team, rating) |>
    filter(team %in% Stats_PY1$team)
  colnames(SRS_PY1) <- c("team", "SRS_rating_PY1")
  ## IFF SRS data for PY1 is missing due to whatever issue
  missing_SRSPY1_teams <- anti_join(Stats_PY1, SRS_PY1)
  if (nrow(missing_SRSPY1_teams) > 0) {
    SampleSRS_PY1 <- cfbd_ratings_srs(year = as.numeric(year) - 1) |>
      select(team, conference, rating)
    missing_SRSPY1teams <- missing_SRSPY1_teams |>
      select(team) |>
      mutate(SRS_rating_PY1 = mean(SampleSRS_PY1$rating))
    SRS_PY1 <- rbind(SRS_PY1, missing_SRSPY1teams)
  } else {
    print("no teams missing from SRS_PY1 data frame")
  }
  
  # ## Current SRS
  # SRS <- cfbd_ratings_srs(year = as.numeric(year)) |>
  #   select(team, rating) |>
  #   filter(team %in% Stats$team)
  # colnames(SRS) <- c("team", "SRS_rating")
  # ## IFF SRS data for PY1 is missing due to whatever issue
  # missing_SRS_teams <- anti_join(Stats, SRS)
  # if (nrow(missing_SRS_teams) > 0) {
  #   SampleSRS <- cfbd_ratings_srs(year = as.numeric(year)) |>
  #     select(team, conference, rating)
  #   missing_SRSteams <- missing_SRS_teams |>
  #     select(team) |>
  #     mutate(SRS_rating_PY1 = mean(SampleSRS$rating))
  #   SRS <- rbind(SRS, missing_SRSteams)
  # } else {
  #   print("no teams missing from SRS data frame")
  # }
} else if (as.numeric(week) <= 6) {
  ##### WEEKS 5-6 Data Pull #####
  ## CURRENT SEASON STATS
  Stats <- cfbd_stats_season_team(year = as.integer(year), start_week = 1, end_week = as.numeric(week)) |>
    mutate(total_yds_pg = total_yds/games,
           pass_yds_pg = net_pass_yds/games,
           rush_yds_pg = rush_yds/games,
           first_downs_pg = first_downs/games,
           def_interceptions_pg = passes_intercepted/games,
           pass_ypa = net_pass_yds / pass_atts,
           off_ypp = total_yds / (rush_atts + pass_atts),
           completion_pct = pass_comps / pass_atts,
           pass_ypr = net_pass_yds / pass_comps,
           int_pct = interceptions / pass_atts,
           rush_ypc = rush_yds / rush_atts,
           turnovers_pg = turnovers / games,
           third_conv_rate = third_down_convs / third_downs,
           fourth_conv_rate = fourth_down_convs / fourth_downs,
           penalty_yds_pg = penalty_yds / games,
           yards_per_penalty = penalty_yds / penalties,
           kick_return_avg = kick_return_yds / kick_returns,
           punt_return_avg = punt_return_yds / punt_returns)
  Stats[is.na(Stats)] = 0
  
  ## advanced stats data
  Adv_Stats <- cfbd_stats_season_advanced(year = as.integer(year), excl_garbage_time = FALSE, start_week = 1, end_week = as.numeric(week)) |>
    select(team, off_ppa, off_success_rate, off_explosiveness, off_power_success,
           off_stuff_rate, off_line_yds, off_second_lvl_yds, off_open_field_yds,
           off_pts_per_opp, off_field_pos_avg_predicted_points, off_havoc_total, 
           off_havoc_front_seven, off_havoc_db, off_standard_downs_ppa,
           off_standard_downs_success_rate, off_standard_downs_explosiveness,
           off_passing_downs_ppa, off_passing_downs_success_rate,
           off_passing_downs_explosiveness, off_rushing_plays_ppa,
           off_rushing_plays_success_rate, off_rushing_plays_explosiveness,
           off_passing_plays_ppa, off_passing_plays_success_rate,
           off_passing_plays_explosiveness, def_ppa, def_success_rate,
           def_explosiveness, def_power_success, def_stuff_rate, def_line_yds,
           def_second_lvl_yds, def_open_field_yds, def_pts_per_opp, 
           def_field_pos_avg_predicted_points, def_havoc_total, def_havoc_front_seven,
           def_havoc_db, def_standard_downs_ppa, def_standard_downs_success_rate,
           def_standard_downs_explosiveness , def_passing_downs_ppa,
           def_passing_downs_success_rate, def_passing_downs_explosiveness,
           def_rushing_plays_ppa, def_rushing_plays_success_rate,
           def_rushing_plays_explosiveness, def_passing_plays_ppa,
           def_passing_plays_success_rate, def_passing_plays_explosiveness)
  ## removing NAs
  Adv_Stats[is.na(Adv_Stats)] = 0
  ## current FPI data as of this week
  ## pulling FPI data
  FPI_df <- espn_ratings_fpi(year = as.integer(year)) |>
    select(team_name, fpi, w, l)
  ## converting character columns to numeric
  FPI_df[,2:ncol(FPI_df)] <- FPI_df[,2:ncol(FPI_df)] |> mutate_if(is.character,as.numeric)
  
  ## Changing FPI team names to match up with outputs of cfbdata functions
  ## Changing team names in FPI df to match what appears in cfbfastR stats function
  FPI_df <- FPI_df |>
    mutate(team = case_when(team_name == 'Appalachian St' ~ 'Appalachian State',
                            team_name == 'C Michigan' ~ 'Central Michigan',
                            team_name == 'Coast Carolina' ~ 'Coastal Carolina',
                            team_name == 'Coastal Car' ~ 'Coastal Carolina',
                            team_name == 'UConn' ~ 'Connecticut',
                            team_name == 'E Michigan' ~ 'Eastern Michigan',
                            team_name == 'FAU' ~ 'Florida Atlantic',
                            team_name == 'Florida Intl' ~ 'Florida International',
                            team_name == 'FIU' ~ 'Florida International',
                            team_name == 'Georgia So' ~ 'Georgia Southern',
                            team_name == 'UL Monroe' ~ 'Louisiana Monroe',
                            team_name == 'LA Tech' ~ 'Louisiana Tech',
                            team_name == 'MTSU' ~ 'Middle Tennessee',
                            team_name == 'Mississippi St' ~ 'Mississippi State',
                            team_name == 'New Mexico St' ~ 'New Mexico State',
                            team_name == 'N Illinois' ~ 'Northern Illinois',
                            team_name == 'Oklahoma St' ~ 'Oklahoma State',
                            team_name == 'Oregon St' ~ 'Oregon State',
                            team_name == 'San Jose State' ~ 'San José State',
                            team_name == 'Southern Miss' ~ 'Southern Mississippi',
                            team_name == 'UTSA' ~ 'UT San Antonio',
                            team_name == 'Washington St' ~ 'Washington State',
                            team_name == 'Western KY' ~ 'Western Kentucky',
                            team_name == 'W Michigan' ~ 'Western Michigan',
                            team_name == 'Arizona St' ~ 'Arizona State',
                            team_name == 'Arkansas St' ~ 'Arkansas State',
                            team_name == 'Boise St' ~ 'Boise State',
                            team_name == 'Colorado St' ~ 'Colorado State',
                            team_name == 'Florida St' ~ 'Florida State',
                            team_name == 'Fresno St' ~ 'Fresno State',
                            team_name == 'Georgia St' ~ 'Georgia State',
                            team_name == 'Kansas St' ~ 'Kansas State',
                            team_name == 'Miami OH' ~ 'Miami (OH)',
                            team_name == 'Michigan St' ~ 'Michigan State',
                            team_name == 'Pitt' ~ 'Pittsburgh',
                            team_name == 'San Diego St' ~ 'San Diego State',
                            team_name == 'San José St' ~ 'San José State',
                            team_name == 'Texas St' ~ 'Texas State',
                            team_name == 'Sam Houston' ~ 'Sam Houston State',
                            team_name == 'GA Southern' ~ 'Georgia Southern',
                            team_name == 'Massachusetts' ~ 'UMass',
                            team_name == "J'Ville St" ~ 'Jacksonville State',
                            TRUE ~ team)) |>
    select(team, fpi, w, l)
  ## changing column names here since all of the columns used in the VoA are extracted in the first step
  FPI_colnames <- c("team", "FPI", "Wins", "Losses")
  colnames(FPI_df) <- FPI_colnames
  ## Current SP+ data
  # SP_Rankings <-cfbd_ratings_sp(year = as.integer(year)) |>
  #   filter(team != "nationalAverages") |>
  #   select(team, rating, offense_rating, defense_rating, special_teams_rating)
  # colnames(SP_Rankings) <- c("team", "sp_rating", "sp_offense_rating", "sp_defense_rating", "sp_special_teams_rating")
  # ## Eliminating NAs
  # SP_Rankings[is.na(SP_Rankings)] = 0
  
  ## reading in data for previous year
  # FCS transitioners (except JMU)
  FCS_PY1 <- read_csv(here("Data", "VoA2023", "FCSPrevYears", "FCS_PY1.csv"))
  ### PY1 stats only now (except recruiting rankings)
  Stats_PY1 <- cfbd_stats_season_team(year = as.integer(year) - 1, start_week = 1, end_week = 15) |>
    filter(team != "Sam Houston State" & team != "Jacksonville State") |>
    mutate(total_yds_pg = total_yds/games,
           pass_yds_pg = net_pass_yds/games,
           rush_yds_pg = rush_yds/games,
           first_downs_pg = first_downs/games,
           def_interceptions_pg = passes_intercepted/games,
           pass_ypa = net_pass_yds / pass_atts,
           off_ypp = total_yds / (rush_atts + pass_atts),
           completion_pct = pass_comps / pass_atts,
           pass_ypr = net_pass_yds / pass_comps,
           int_pct = interceptions / pass_atts,
           rush_ypc = rush_yds / rush_atts,
           turnovers_pg = turnovers / games,
           third_conv_rate = third_down_convs / third_downs,
           fourth_conv_rate = fourth_down_convs / fourth_downs,
           penalty_yds_pg = penalty_yds / games,
           yards_per_penalty = penalty_yds / penalties,
           kick_return_avg = kick_return_yds / kick_returns,
           punt_return_avg = punt_return_yds / punt_returns)
  ## removing NAs
  Stats_PY1[is.na(Stats_PY1)] = 0
  
  ## advanced stats data
  Adv_Stats_PY1 <- cfbd_stats_season_advanced(year = as.integer(year) - 1, excl_garbage_time = FALSE, start_week = 1, end_week = 15) |>
    filter(team != "Jacksonville State" & team != "Sam Houston State") |>
    select(team, off_ppa, off_success_rate, off_explosiveness, off_power_success,
           off_stuff_rate, off_line_yds, off_second_lvl_yds, off_open_field_yds,
           off_pts_per_opp, off_field_pos_avg_predicted_points, off_havoc_total, 
           off_havoc_front_seven, off_havoc_db, off_standard_downs_ppa,
           off_standard_downs_success_rate, off_standard_downs_explosiveness,
           off_passing_downs_ppa, off_passing_downs_success_rate,
           off_passing_downs_explosiveness, off_rushing_plays_ppa,
           off_rushing_plays_success_rate, off_rushing_plays_explosiveness,
           off_passing_plays_ppa, off_passing_plays_success_rate,
           off_passing_plays_explosiveness, def_ppa, def_success_rate,
           def_explosiveness, def_power_success, def_stuff_rate, def_line_yds,
           def_second_lvl_yds, def_open_field_yds, def_pts_per_opp, 
           def_field_pos_avg_predicted_points, def_havoc_total, def_havoc_front_seven,
           def_havoc_db, def_standard_downs_ppa, def_standard_downs_success_rate,
           def_standard_downs_explosiveness , def_passing_downs_ppa,
           def_passing_downs_success_rate, def_passing_downs_explosiveness,
           def_rushing_plays_ppa, def_rushing_plays_success_rate,
           def_rushing_plays_explosiveness, def_passing_plays_ppa,
           def_passing_plays_success_rate, def_passing_plays_explosiveness)
  ## removing NAs
  Adv_Stats_PY1[is.na(Adv_Stats_PY1)] = 0
  
  ## IFF advanced stats data for current year is missing due to data issues
  missing_adv_teams <- anti_join(Stats, Adv_Stats)
  if (nrow(missing_adv_teams) > 0) {
    missing_adv_teams_adv_stats <- Adv_Stats_PY1 |>
      filter(team %in% missing_adv_teams$team) 
    Adv_Stats <- rbind(Adv_Stats, missing_adv_teams_adv_stats)
  } else {
    print("no teams missing from advanced stats data frame")
  }
  
  ## pulling in SP+ data
  SP_Rankings_PY1 <-cfbd_ratings_sp(year = as.integer(year) - 1) |>
    filter(team != "Sam Houston" & team != "Sam Houston State" & team != "Jacksonville State" & team != "nationalAverages") |>
    select(team, rating, offense_rating, defense_rating, special_teams_rating)
  colnames(SP_Rankings_PY1) <- c("team", "sp_rating_PY1", "sp_offense_rating_PY1", "sp_defense_rating_PY1", "sp_special_teams_rating_PY1")
  ## Eliminating NAs
  SP_Rankings_PY1[is.na(SP_Rankings_PY1)] = 0
  
  ## pulling FPI data
  FPI_df_PY1 <- espn_ratings_fpi(year = as.integer(year) - 1) |>
    filter(team_name != "Sam Houston" & team_name != "Sam Houston State" & team_name != "Jacksonville State") |>
    select(team_name, fpi, w, l)
  
  ## converting character columns to numeric
  FPI_df_PY1[,2:ncol(FPI_df_PY1)] <- FPI_df_PY1[,2:ncol(FPI_df_PY1)] |> mutate_if(is.character,as.numeric)
  ## removing NAs
  FPI_df_PY1[is.na(FPI_df_PY1)] = 0
  
  ## Changing FPI team names to match up with outputs of cfbdata functions
  ## Changing team names in FPI df to match what appears in cfbfastR stats function
  FPI_df_PY1 <- FPI_df_PY1 |>
    mutate(team = case_when(team_name == 'Appalachian St' ~ 'Appalachian State',
                            team_name == 'C Michigan' ~ 'Central Michigan',
                            team_name == 'Coast Carolina' ~ 'Coastal Carolina',
                            team_name == 'Coastal Car' ~ 'Coastal Carolina',
                            team_name == 'UConn' ~ 'Connecticut',
                            team_name == 'E Michigan' ~ 'Eastern Michigan',
                            team_name == 'FAU' ~ 'Florida Atlantic',
                            team_name == 'Florida Intl' ~ 'Florida International',
                            team_name == 'FIU' ~ 'Florida International',
                            team_name == 'Georgia So' ~ 'Georgia Southern',
                            team_name == 'UL Monroe' ~ 'Louisiana Monroe',
                            team_name == 'LA Tech' ~ 'Louisiana Tech',
                            team_name == 'MTSU' ~ 'Middle Tennessee',
                            team_name == 'Mississippi St' ~ 'Mississippi State',
                            team_name == 'New Mexico St' ~ 'New Mexico State',
                            team_name == 'N Illinois' ~ 'Northern Illinois',
                            team_name == 'Oklahoma St' ~ 'Oklahoma State',
                            team_name == 'Oregon St' ~ 'Oregon State',
                            team_name == 'San Jose State' ~ 'San José State',
                            team_name == 'Southern Miss' ~ 'Southern Mississippi',
                            team_name == 'UTSA' ~ 'UT San Antonio',
                            team_name == 'Washington St' ~ 'Washington State',
                            team_name == 'Western KY' ~ 'Western Kentucky',
                            team_name == 'W Michigan' ~ 'Western Michigan',
                            team_name == 'Arizona St' ~ 'Arizona State',
                            team_name == 'Arkansas St' ~ 'Arkansas State',
                            team_name == 'Boise St' ~ 'Boise State',
                            team_name == 'Colorado St' ~ 'Colorado State',
                            team_name == 'Florida St' ~ 'Florida State',
                            team_name == 'Fresno St' ~ 'Fresno State',
                            team_name == 'Georgia St' ~ 'Georgia State',
                            team_name == 'Kansas St' ~ 'Kansas State',
                            team_name == 'Miami OH' ~ 'Miami (OH)',
                            team_name == 'Michigan St' ~ 'Michigan State',
                            team_name == 'Pitt' ~ 'Pittsburgh',
                            team_name == 'San Diego St' ~ 'San Diego State',
                            team_name == 'San José St' ~ 'San José State',
                            team_name == 'Texas St' ~ 'Texas State',
                            team_name == 'Sam Houston' ~ 'Sam Houston State',
                            team_name == 'GA Southern' ~ 'Georgia Southern',
                            team_name == 'Massachusetts' ~ 'UMass',
                            team_name == "J'Ville St" ~ 'Jacksonville State',
                            TRUE ~ team_name)) |>
    select(team, fpi, w, l)
  ## changing column names here since all of the columns used in the VoA are extracted in the first step
  FPI_PY1_colnames <- c("team", "FPI_PY1", "Wins_PY1", "Losses_PY1")
  colnames(FPI_df_PY1) <- FPI_PY1_colnames
  
  ## pulling in recruiting rankings
  recruit_PY1 <- cfbd_recruiting_team(year = as.numeric(year) - 1) |>
    filter(team != "Sam Houston State" & team != "Jacksonville State") |>
    filter(team %in% Stats_PY1$team) |>
    select(team, points)
  recruit_PY1[,2] <- recruit_PY1[,2] |> mutate_if(is.character, as.numeric)
  colnames(recruit_PY1) <- c("team", "recruit_pts_PY1")
  
  ## pulling in talent rankings
  talent_df_PY1 <- cfbd_team_talent(year = as.numeric(year) - 1) |>
    filter(school != "Jacksonville State" & school != "Sam Houston State") |>
    filter(school %in% Stats_PY1$team) |>
    select(school, talent)
  colnames(talent_df_PY1) <- c("team", "talent_PY1")
  
  ## pulling in recruiting rankings
  recruit_PY2 <- cfbd_recruiting_team(year = as.numeric(year) - 2) |>
    filter(team != "Jacksonville State" & team != "Sam Houston State" & team != "James Madison") |>
    filter(team %in% Stats_PY2$team) |>
    select(team, points)
  recruit_PY2[,2] <- recruit_PY2[,2] |> mutate_if(is.character, as.numeric)
  colnames(recruit_PY2) <- c("team", "recruit_pts_PY2")
  
  ## pulling in recruiting rankings
  recruit_PY3 <- cfbd_recruiting_team(year = as.numeric(year) - 3) |>
    filter(team != "James Madison" & team != "Jacksonville State" & team != "Sam Houston State") |>
    filter(team %in% Stats_PY3$team) |>
    select(team, points)
  recruit_PY3[,2] <- recruit_PY3[,2] |> mutate_if(is.character, as.numeric)
  colnames(recruit_PY3) <- c("team", "recruit_pts_PY3")
  
  ## incoming recruiting class rankings
  recruit <- cfbd_recruiting_team(year = as.numeric(year)) |>
    select(team, points) |>
    mutate(school = case_when(team == "Florida Intl" ~ "Florida International",
                              TRUE ~ team)) |>
    filter(school %in% Stats$team) |>
    select(school, points)
  recruit[,2] <- recruit[,2] |> mutate_if(is.character, as.numeric)
  colnames(recruit) <- c("team", "recruit_pts")
  
  ## commenting out PY1 SRS ratings, but leaving code here in case current season
  # SRS ratings are unavailable
  # ## SRS PY1
  # SRS_PY1 <- cfbd_ratings_srs(year = as.numeric(year) - 1) |>
  #   select(team, rating) |>
  #   filter(team %in% Stats_PY1$team)
  # colnames(SRS_PY1) <- c("team", "SRS_rating_PY1")
  # ## IFF SRS data for PY1 is missing due to whatever issue
  # missing_SRSPY1_teams <- anti_join(Stats_PY1, SRS_PY1)
  # if (nrow(missing_SRSPY1_teams) > 0) {
  #   SampleSRS_PY1 <- cfbd_ratings_srs(year = as.numeric(year) - 1) |>
  #     select(team, conference, rating)
  #   missing_SRSPY1teams <- missing_SRSPY1_teams |>
  #     select(team) |>
  #     mutate(SRS_rating_PY1 = mean(SampleSRS_PY1$rating))
  #   SRS_PY1 <- rbind(SRS_PY1, missing_SRSPY1teams)
  # } else {
  #   print("no teams missing from SRS_PY1 data frame")
  # }
  
  ## Current SRS
  SRS <- cfbd_ratings_srs(year = as.numeric(year)) |>
    select(team, rating) |>
    filter(team %in% Stats$team)
  colnames(SRS) <- c("team", "SRS_rating")
  ## IFF SRS data for PY1 is missing due to whatever issue
  missing_SRS_teams <- anti_join(Stats, SRS)
  if (nrow(missing_SRS_teams) > 0) {
    SampleSRS <- cfbd_ratings_srs(year = as.numeric(year)) |>
      select(team, conference, rating)
    missing_SRSteams <- missing_SRS_teams |>
      select(team) |>
      mutate(SRS_rating_PY1 = mean(SampleSRS$rating))
    SRS <- rbind(SRS, missing_SRSteams)
  } else {
    print("no teams missing from SRS data frame")
  }
} else {
  ##### CURRENT SEASON STATS ONLY Data Pull #####
  Stats <- cfbd_stats_season_team(year = as.integer(year), start_week = 1, end_week = as.numeric(week)) |>
    mutate(total_yds_pg = total_yds/games,
           pass_yds_pg = net_pass_yds/games,
           rush_yds_pg = rush_yds/games,
           first_downs_pg = first_downs/games,
           def_interceptions_pg = passes_intercepted/games,
           pass_ypa = net_pass_yds / pass_atts,
           off_ypp = total_yds / (rush_atts + pass_atts),
           completion_pct = pass_comps / pass_atts,
           pass_ypr = net_pass_yds / pass_comps,
           int_pct = interceptions / pass_atts,
           rush_ypc = rush_yds / rush_atts,
           turnovers_pg = turnovers / games,
           third_conv_rate = third_down_convs / third_downs,
           fourth_conv_rate = fourth_down_convs / fourth_downs,
           penalty_yds_pg = penalty_yds / games,
           yards_per_penalty = penalty_yds / penalties,
           kick_return_avg = kick_return_yds / kick_returns,
           punt_return_avg = punt_return_yds / punt_returns)
  ## removing NAs
  Stats[is.na(Stats)] = 0
  
  ## advanced stats data
  Adv_Stats <- cfbd_stats_season_advanced(year = as.integer(year), excl_garbage_time = FALSE, start_week = 1, end_week = as.numeric(week)) |>
    select(team, off_ppa, off_success_rate, off_explosiveness, off_power_success,
           off_stuff_rate, off_line_yds, off_second_lvl_yds, off_open_field_yds,
           off_pts_per_opp, off_field_pos_avg_predicted_points, off_havoc_total, 
           off_havoc_front_seven, off_havoc_db, off_standard_downs_ppa,
           off_standard_downs_success_rate, off_standard_downs_explosiveness,
           off_passing_downs_ppa, off_passing_downs_success_rate,
           off_passing_downs_explosiveness, off_rushing_plays_ppa,
           off_rushing_plays_success_rate, off_rushing_plays_explosiveness,
           off_passing_plays_ppa, off_passing_plays_success_rate,
           off_passing_plays_explosiveness, def_ppa, def_success_rate,
           def_explosiveness, def_power_success, def_stuff_rate, def_line_yds,
           def_second_lvl_yds, def_open_field_yds, def_pts_per_opp, 
           def_field_pos_avg_predicted_points, def_havoc_total, def_havoc_front_seven,
           def_havoc_db, def_standard_downs_ppa, def_standard_downs_success_rate,
           def_standard_downs_explosiveness , def_passing_downs_ppa,
           def_passing_downs_success_rate, def_passing_downs_explosiveness,
           def_rushing_plays_ppa, def_rushing_plays_success_rate,
           def_rushing_plays_explosiveness, def_passing_plays_ppa,
           def_passing_plays_success_rate, def_passing_plays_explosiveness)
  ## removing NAs
  Adv_Stats[is.na(Adv_Stats)] = 0
  
  ## current FPI data as of this week
  ## pulling FPI data
  FPI_df <- espn_ratings_fpi(year = as.integer(year)) |>
    select(team_name, fpi, w, l)
  ## converting character columns to numeric
  FPI_df[,2:ncol(FPI_df)] <- FPI_df[,2:ncol(FPI_df)] |> mutate_if(is.character,as.numeric)
  
  ## Changing FPI team names to match up with outputs of cfbdata functions
  ## Changing team names in FPI df to match what appears in cfbfastR stats function
  FPI_df <- FPI_df |>
    mutate(team = case_when(team_name == 'Appalachian St' ~ 'Appalachian State',
                            team_name == 'C Michigan' ~ 'Central Michigan',
                            team_name == 'Coast Carolina' ~ 'Coastal Carolina',
                            team_name == 'Coastal Car' ~ 'Coastal Carolina',
                            team_name == 'UConn' ~ 'Connecticut',
                            team_name == 'E Michigan' ~ 'Eastern Michigan',
                            team_name == 'FAU' ~ 'Florida Atlantic',
                            team_name == 'Florida Intl' ~ 'Florida International',
                            team_name == 'FIU' ~ 'Florida International',
                            team_name == 'Georgia So' ~ 'Georgia Southern',
                            team_name == 'UL Monroe' ~ 'Louisiana Monroe',
                            team_name == 'LA Tech' ~ 'Louisiana Tech',
                            team_name == 'MTSU' ~ 'Middle Tennessee',
                            team_name == 'Mississippi St' ~ 'Mississippi State',
                            team_name == 'New Mexico St' ~ 'New Mexico State',
                            team_name == 'N Illinois' ~ 'Northern Illinois',
                            team_name == 'Oklahoma St' ~ 'Oklahoma State',
                            team_name == 'Oregon St' ~ 'Oregon State',
                            team_name == 'San Jose State' ~ 'San José State',
                            team_name == 'Southern Miss' ~ 'Southern Mississippi',
                            team_name == 'UTSA' ~ 'UT San Antonio',
                            team_name == 'Washington St' ~ 'Washington State',
                            team_name == 'Western KY' ~ 'Western Kentucky',
                            team_name == 'W Michigan' ~ 'Western Michigan',
                            team_name == 'Arizona St' ~ 'Arizona State',
                            team_name == 'Arkansas St' ~ 'Arkansas State',
                            team_name == 'Boise St' ~ 'Boise State',
                            team_name == 'Colorado St' ~ 'Colorado State',
                            team_name == 'Florida St' ~ 'Florida State',
                            team_name == 'Fresno St' ~ 'Fresno State',
                            team_name == 'Georgia St' ~ 'Georgia State',
                            team_name == 'Kansas St' ~ 'Kansas State',
                            team_name == 'Miami OH' ~ 'Miami (OH)',
                            team_name == 'Michigan St' ~ 'Michigan State',
                            team_name == 'Pitt' ~ 'Pittsburgh',
                            team_name == 'San Diego St' ~ 'San Diego State',
                            team_name == 'San José St' ~ 'San José State',
                            team_name == 'Texas St' ~ 'Texas State',
                            team_name == 'Sam Houston' ~ 'Sam Houston State',
                            team_name == 'GA Southern' ~ 'Georgia Southern',
                            team_name == 'Massachusetts' ~ 'UMass',
                            team_name == "J'Ville St" ~ 'Jacksonville State',
                            TRUE ~ team)) |>
    select(team, fpi, w, l)
  ## changing column names here since all of the columns used in the VoA are extracted in the first step
  FPI_colnames <- c("team", "FPI", "Wins", "Losses")
  colnames(FPI_df) <- FPI_colnames
  
  ## Current SP+ data
  # SP_Rankings <-cfbd_ratings_sp(year = as.integer(year)) |>
  #   filter(team != "nationalAverages") |>
  #   select(team, rating, offense_rating, defense_rating, special_teams_rating)
  # colnames(SP_Rankings) <- c("team", "sp_rating", "sp_offense_rating", "sp_defense_rating", "sp_special_teams_rating")
  # ## Eliminating NAs
  # SP_Rankings[is.na(SP_Rankings)] = 0
  
  ## incoming recruiting class rankings
  recruit <- cfbd_recruiting_team(year = as.numeric(year)) |>
    select(team, points)
  recruit <- recruit |>
    mutate(school = case_when(team == "Florida Intl" ~ "Florida International",
                              TRUE ~ team)) |>
    filter(school %in% Stats$team) |>
    select(school, points)
  recruit[,2] <- recruit[,2] |> mutate_if(is.character, as.numeric)
  colnames(recruit) <- c("team", "recruit_pts")
  
  ## pulling in recruiting rankings
  recruit_PY1 <- cfbd_recruiting_team(year = as.numeric(year) - 1) |>
    # filter(team != "Sam Houston State" & team != "Jacksonville State") |>
    filter(team %in% Stats$team) |>
    select(team, points)
  recruit_PY1[,2] <- recruit_PY1[,2] |> mutate_if(is.character, as.numeric)
  colnames(recruit_PY1) <- c("team", "recruit_pts_PY1")
  
  ## pulling in recruiting rankings
  recruit_PY2 <- cfbd_recruiting_team(year = as.numeric(year) - 2) |>
    # filter(team != "Jacksonville State" & team != "Sam Houston State" & team != "James Madison") |>
    filter(team %in% Stats$team) |>
    select(team, points)
  recruit_PY2[,2] <- recruit_PY2[,2] |> mutate_if(is.character, as.numeric)
  colnames(recruit_PY2) <- c("team", "recruit_pts_PY2")
  
  ## pulling in recruiting rankings
  recruit_PY3 <- cfbd_recruiting_team(year = as.numeric(year) - 3) |>
    # filter(team != "James Madison" & team != "Jacksonville State" & team != "Sam Houston State") |>
    filter(team %in% Stats$team) |>
    select(team, points)
  recruit_PY3[,2] <- recruit_PY3[,2] |> mutate_if(is.character, as.numeric)
  colnames(recruit_PY3) <- c("team", "recruit_pts_PY3")
}

##### merging data frames together #####
if (as.numeric(week) == 0) {
  ##### WEEK 0 DF Merge #####
  ## merging data frames together, arranging columns
  ## need to merge stats and advanced stats together first so I can change column names to avoid duplicate column names later on
  # then I will be merging data frames for the same year together
  # then I will merge years together by team
  PY3_stats_adv_stats_list <- list(Stats_PY3, Adv_Stats_PY3)
  PY3_stats_adv_stats_merge <- PY3_stats_adv_stats_list |>
    reduce(full_join, by = "team") |>
    select(season, team, conference, games, completion_pct, 
           pass_ypa, pass_ypr, int_pct, rush_ypc, turnovers_pg, 
           third_conv_rate, fourth_conv_rate, penalty_yds_pg, 
           yards_per_penalty, kick_return_avg, punt_return_avg, total_yds_pg, 
           pass_yds_pg, rush_yds_pg, first_downs_pg, off_ypp, def_interceptions_pg, off_ppa, 
           off_success_rate, off_explosiveness, off_power_success, off_stuff_rate, 
           off_line_yds, off_second_lvl_yds, off_open_field_yds, off_pts_per_opp, 
           off_field_pos_avg_predicted_points, off_havoc_total, off_havoc_front_seven, 
           off_havoc_db, off_standard_downs_ppa, off_standard_downs_success_rate, 
           off_standard_downs_explosiveness, off_passing_downs_ppa, 
           off_passing_downs_success_rate, off_passing_downs_explosiveness, 
           off_rushing_plays_ppa, off_rushing_plays_success_rate, 
           off_rushing_plays_explosiveness, off_passing_plays_ppa, 
           off_passing_plays_success_rate, off_passing_plays_explosiveness, def_ppa, 
           def_success_rate, def_explosiveness, def_power_success, def_stuff_rate, 
           def_line_yds, def_second_lvl_yds, def_open_field_yds, def_pts_per_opp, 
           def_field_pos_avg_predicted_points, def_havoc_total, def_havoc_front_seven, 
           def_havoc_db, def_standard_downs_ppa, def_standard_downs_success_rate, 
           def_standard_downs_explosiveness, def_passing_downs_ppa, 
           def_passing_downs_success_rate, def_passing_downs_explosiveness, 
           def_rushing_plays_ppa, def_rushing_plays_success_rate, 
           def_rushing_plays_explosiveness, def_passing_plays_ppa, 
           def_passing_plays_success_rate, def_passing_plays_explosiveness)
  colnames(PY3_stats_adv_stats_merge) <- c("season", "team", "conference", "games_PY3", "completion_pct_PY3",
                                           "pass_ypa_PY3", "pass_ypr_PY3", "int_pct_PY3", "rush_ypc_PY3", "turnovers_pg_PY3",
                                           "third_conv_rate_PY3", "fourth_conv_rate_PY3", "penalty_yds_pg_PY3",
                                           "yards_per_penalty_PY3", "kick_return_avg_PY3", "punt_return_avg_PY3", "total_yds_pg_PY3",
                                           "pass_yds_pg_PY3", "rush_yds_pg_PY3", "first_downs_pg_PY3", "off_ypp_PY3", "def_interceptions_pg_PY3", "off_ppa_PY3",
                                           "off_success_rate_PY3", "off_explosiveness_PY3", "off_power_success_PY3", "off_stuff_rate_PY3",
                                           "off_line_yds_PY3", "off_second_lvl_yds_PY3", "off_open_field_yds_PY3", "off_pts_per_opp_PY3",
                                           "off_field_pos_avg_predicted_points_PY3", "off_havoc_total_PY3", "off_havoc_front_seven_PY3",
                                           "off_havoc_db_PY3", "off_standard_downs_ppa_PY3", "off_standard_downs_success_rate_PY3",
                                           "off_standard_downs_explosiveness_PY3", "off_passing_downs_ppa_PY3",
                                           "off_passing_downs_success_rate_PY3", "off_passing_downs_explosiveness_PY3",
                                           "off_rushing_plays_ppa_PY3", "off_rushing_plays_success_rate_PY3",
                                           "off_rushing_plays_explosiveness_PY3", "off_passing_plays_ppa_PY3",
                                           "off_passing_plays_success_rate_PY3", "off_passing_plays_explosiveness_PY3", "def_ppa_PY3",
                                           "def_success_rate_PY3", "def_explosiveness_PY3", "def_power_success_PY3", "def_stuff_rate_PY3",
                                           "def_line_yds_PY3", "def_second_lvl_yds_PY3", "def_open_field_yds_PY3", "def_pts_per_opp_PY3",
                                           "def_field_pos_avg_predicted_points_PY3", "def_havoc_total_PY3", "def_havoc_front_seven_PY3",
                                           "def_havoc_db_PY3", "def_standard_downs_ppa_PY3", "def_standard_downs_success_rate_PY3",
                                           "def_standard_downs_explosiveness_PY3", "def_passing_downs_ppa_PY3",
                                           "def_passing_downs_success_rate_PY3", "def_passing_downs_explosiveness_PY3",
                                           "def_rushing_plays_ppa_PY3", "def_rushing_plays_success_rate_PY3",
                                           "def_rushing_plays_explosiveness_PY3", "def_passing_plays_ppa_PY3",
                                           "def_passing_plays_success_rate_PY3", "def_passing_plays_explosiveness_PY3")
  PY3_df_list <- list(PY3_stats_adv_stats_merge, recruit_PY3, talent_df_PY3, SP_Rankings_PY3, FPI_df_PY3, SRS_PY3)
  PY3_df <- PY3_df_list |>
    reduce(full_join, by = "team")
  PY3_df <- rbind(PY3_df, COVID_Optouts_df)
  PY3_df <- rbind(PY3_df, FCS_PY3)
  
  PY2_stats_adv_stats_list <- list(Stats_PY2, Adv_Stats_PY2)
  PY2_stats_adv_stats_merge <- PY2_stats_adv_stats_list |>
    reduce(full_join, by = "team") |>
    select(team, games, completion_pct, pass_ypa, pass_ypr, int_pct, rush_ypc, 
           turnovers_pg, third_conv_rate, fourth_conv_rate, penalty_yds_pg, 
           yards_per_penalty, kick_return_avg, punt_return_avg, total_yds_pg, 
           pass_yds_pg, rush_yds_pg, first_downs_pg, off_ypp, def_interceptions_pg, off_ppa, 
           off_success_rate, off_explosiveness, off_power_success, off_stuff_rate, 
           off_line_yds, off_second_lvl_yds, off_open_field_yds, off_pts_per_opp, 
           off_field_pos_avg_predicted_points, off_havoc_total, off_havoc_front_seven, 
           off_havoc_db, off_standard_downs_ppa, off_standard_downs_success_rate, 
           off_standard_downs_explosiveness, off_passing_downs_ppa, 
           off_passing_downs_success_rate, off_passing_downs_explosiveness, 
           off_rushing_plays_ppa, off_rushing_plays_success_rate, 
           off_rushing_plays_explosiveness, off_passing_plays_ppa, 
           off_passing_plays_success_rate, off_passing_plays_explosiveness, def_ppa, 
           def_success_rate, def_explosiveness, def_power_success, def_stuff_rate, 
           def_line_yds, def_second_lvl_yds, def_open_field_yds, def_pts_per_opp, 
           def_field_pos_avg_predicted_points, def_havoc_total, def_havoc_front_seven, 
           def_havoc_db, def_standard_downs_ppa, def_standard_downs_success_rate, 
           def_standard_downs_explosiveness, def_passing_downs_ppa, 
           def_passing_downs_success_rate, def_passing_downs_explosiveness, 
           def_rushing_plays_ppa, def_rushing_plays_success_rate, 
           def_rushing_plays_explosiveness, def_passing_plays_ppa, 
           def_passing_plays_success_rate, def_passing_plays_explosiveness)
  colnames(PY2_stats_adv_stats_merge) <- c("team", "games_PY2", "completion_pct_PY2",
                                           "pass_ypa_PY2", "pass_ypr_PY2", "int_pct_PY2", "rush_ypc_PY2", "turnovers_pg_PY2",
                                           "third_conv_rate_PY2", "fourth_conv_rate_PY2", "penalty_yds_pg_PY2",
                                           "yards_per_penalty_PY2", "kick_return_avg_PY2", "punt_return_avg_PY2", "total_yds_pg_PY2",
                                           "pass_yds_pg_PY2", "rush_yds_pg_PY2", "first_downs_pg_PY2", "off_ypp_PY2", "def_interceptions_pg_PY2", "off_ppa_PY2",
                                           "off_success_rate_PY2", "off_explosiveness_PY2", "off_power_success_PY2", "off_stuff_rate_PY2",
                                           "off_line_yds_PY2", "off_second_lvl_yds_PY2", "off_open_field_yds_PY2", "off_pts_per_opp_PY2",
                                           "off_field_pos_avg_predicted_points_PY2", "off_havoc_total_PY2", "off_havoc_front_seven_PY2",
                                           "off_havoc_db_PY2", "off_standard_downs_ppa_PY2", "off_standard_downs_success_rate_PY2",
                                           "off_standard_downs_explosiveness_PY2", "off_passing_downs_ppa_PY2",
                                           "off_passing_downs_success_rate_PY2", "off_passing_downs_explosiveness_PY2",
                                           "off_rushing_plays_ppa_PY2", "off_rushing_plays_success_rate_PY2",
                                           "off_rushing_plays_explosiveness_PY2", "off_passing_plays_ppa_PY2",
                                           "off_passing_plays_success_rate_PY2", "off_passing_plays_explosiveness_PY2", "def_ppa_PY2",
                                           "def_success_rate_PY2", "def_explosiveness_PY2", "def_power_success_PY2", "def_stuff_rate_PY2",
                                           "def_line_yds_PY2", "def_second_lvl_yds_PY2", "def_open_field_yds_PY2", "def_pts_per_opp_PY2",
                                           "def_field_pos_avg_predicted_points_PY2", "def_havoc_total_PY2", "def_havoc_front_seven_PY2",
                                           "def_havoc_db_PY2", "def_standard_downs_ppa_PY2", "def_standard_downs_success_rate_PY2",
                                           "def_standard_downs_explosiveness_PY2", "def_passing_downs_ppa_PY2",
                                           "def_passing_downs_success_rate_PY2", "def_passing_downs_explosiveness_PY2",
                                           "def_rushing_plays_ppa_PY2", "def_rushing_plays_success_rate_PY2",
                                           "def_rushing_plays_explosiveness_PY2", "def_passing_plays_ppa_PY2",
                                           "def_passing_plays_success_rate_PY2", "def_passing_plays_explosiveness_PY2")
  PY2_df_list <- list(PY2_stats_adv_stats_merge, recruit_PY2, talent_df_PY2, SP_Rankings_PY2, FPI_df_PY2, SRS_PY2)
  PY2_df <- PY2_df_list |>
    reduce(full_join, by = "team")
  PY2_df <- rbind(PY2_df, FCS_PY2)
  
  
  PY1_stats_adv_stats_list <- list(Stats_PY1, Adv_Stats_PY1)
  PY1_stats_adv_stats_merge <- PY1_stats_adv_stats_list |>
    reduce(full_join, by = "team") |>
    select(team, games, completion_pct, pass_ypa, pass_ypr, int_pct, rush_ypc, 
           turnovers_pg, third_conv_rate, fourth_conv_rate, penalty_yds_pg, 
           yards_per_penalty, kick_return_avg, punt_return_avg, total_yds_pg, 
           pass_yds_pg, rush_yds_pg, first_downs_pg, off_ypp, def_interceptions_pg, off_ppa, 
           off_success_rate, off_explosiveness, off_power_success, off_stuff_rate, 
           off_line_yds, off_second_lvl_yds, off_open_field_yds, off_pts_per_opp, 
           off_field_pos_avg_predicted_points, off_havoc_total, off_havoc_front_seven, 
           off_havoc_db, off_standard_downs_ppa, off_standard_downs_success_rate, 
           off_standard_downs_explosiveness, off_passing_downs_ppa, 
           off_passing_downs_success_rate, off_passing_downs_explosiveness, 
           off_rushing_plays_ppa, off_rushing_plays_success_rate, 
           off_rushing_plays_explosiveness, off_passing_plays_ppa, 
           off_passing_plays_success_rate, off_passing_plays_explosiveness, def_ppa, 
           def_success_rate, def_explosiveness, def_power_success, def_stuff_rate, 
           def_line_yds, def_second_lvl_yds, def_open_field_yds, def_pts_per_opp, 
           def_field_pos_avg_predicted_points, def_havoc_total, def_havoc_front_seven, 
           def_havoc_db, def_standard_downs_ppa, def_standard_downs_success_rate, 
           def_standard_downs_explosiveness, def_passing_downs_ppa, 
           def_passing_downs_success_rate, def_passing_downs_explosiveness, 
           def_rushing_plays_ppa, def_rushing_plays_success_rate, 
           def_rushing_plays_explosiveness, def_passing_plays_ppa, 
           def_passing_plays_success_rate, def_passing_plays_explosiveness)
  colnames(PY1_stats_adv_stats_merge) <- c("team", "games_PY1", "completion_pct_PY1",
                                           "pass_ypa_PY1", "pass_ypr_PY1", "int_pct_PY1", "rush_ypc_PY1", "turnovers_pg_PY1",
                                           "third_conv_rate_PY1", "fourth_conv_rate_PY1", "penalty_yds_pg_PY1",
                                           "yards_per_penalty_PY1", "kick_return_avg_PY1", "punt_return_avg_PY1", "total_yds_pg_PY1",
                                           "pass_yds_pg_PY1", "rush_yds_pg_PY1", "first_downs_pg_PY1", "off_ypp_PY1", "def_interceptions_pg_PY1", "off_ppa_PY1",
                                           "off_success_rate_PY1", "off_explosiveness_PY1", "off_power_success_PY1", "off_stuff_rate_PY1",
                                           "off_line_yds_PY1", "off_second_lvl_yds_PY1", "off_open_field_yds_PY1", "off_pts_per_opp_PY1",
                                           "off_field_pos_avg_predicted_points_PY1", "off_havoc_total_PY1", "off_havoc_front_seven_PY1",
                                           "off_havoc_db_PY1", "off_standard_downs_ppa_PY1", "off_standard_downs_success_rate_PY1",
                                           "off_standard_downs_explosiveness_PY1", "off_passing_downs_ppa_PY1",
                                           "off_passing_downs_success_rate_PY1", "off_passing_downs_explosiveness_PY1",
                                           "off_rushing_plays_ppa_PY1", "off_rushing_plays_success_rate_PY1",
                                           "off_rushing_plays_explosiveness_PY1", "off_passing_plays_ppa_PY1",
                                           "off_passing_plays_success_rate_PY1", "off_passing_plays_explosiveness_PY1", "def_ppa_PY1",
                                           "def_success_rate_PY1", "def_explosiveness_PY1", "def_power_success_PY1", "def_stuff_rate_PY1",
                                           "def_line_yds_PY1", "def_second_lvl_yds_PY1", "def_open_field_yds_PY1", "def_pts_per_opp_PY1",
                                           "def_field_pos_avg_predicted_points_PY1", "def_havoc_total_PY1", "def_havoc_front_seven_PY1",
                                           "def_havoc_db_PY1", "def_standard_downs_ppa_PY1", "def_standard_downs_success_rate_PY1",
                                           "def_standard_downs_explosiveness_PY1", "def_passing_downs_ppa_PY1",
                                           "def_passing_downs_success_rate_PY1", "def_passing_downs_explosiveness_PY1",
                                           "def_rushing_plays_ppa_PY1", "def_rushing_plays_success_rate_PY1",
                                           "def_rushing_plays_explosiveness_PY1", "def_passing_plays_ppa_PY1",
                                           "def_passing_plays_success_rate_PY1", "def_passing_plays_explosiveness_PY1")
  PY1_df_list <- list(PY1_stats_adv_stats_merge, recruit_PY1, talent_df_PY1, SP_Rankings_PY1, FPI_df_PY1, SRS_PY1)
  PY1_df <- PY1_df_list |>
    reduce(full_join, by = "team")
  PY1_df <- rbind(PY1_df, FCS_PY1)
  
  ## merging all data frames in order of PY3, PY2, PY1
  all_PY_df_list <- list(PY3_df, PY2_df, PY1_df, recruit)
  VoA_Variables <- all_PY_df_list |>
    reduce(full_join, by = "team")
  
  ## Making values numeric
  VoA_Variables[,4:ncol(VoA_Variables)] <- VoA_Variables[,4:ncol(VoA_Variables)] |> mutate_if(is.character,as.numeric)
  VoA_Variables <- VoA_Variables |>
    mutate(FPI_SP_SRS_PY3_mean = (sp_rating_PY3 + FPI_PY3 + SRS_rating_PY3) / 3,
           FPI_SP_SRS_PY2_mean = (sp_rating_PY2 + FPI_PY2 + SRS_rating_PY2) / 3,
           FPI_SP_SRS_PY1_mean = (sp_rating_PY1 + FPI_PY1 + SRS_rating_PY1) / 3,
           AllPY_FPI_SP_SRS_mean = (FPI_SP_SRS_PY3_mean + FPI_SP_SRS_PY2_mean + FPI_SP_SRS_PY1_mean) / 3,
           WeightedAllPY_FPI_SP_SRS_mean = ((FPI_SP_SRS_PY3_mean * 0.1) + (FPI_SP_SRS_PY2_mean * 0.25) + (FPI_SP_SRS_PY1_mean * 0.65)) / 3)
  
} else if (as.numeric(week) == 1) {
  ##### WEEK 1 DF Merge #####
  ## merging data frames together, arranging columns
  ## need to merge stats and advanced stats together first so I can change column names to avoid duplicate column names later on
  # then I will be merging data frames for the same year together
  # then I will merge years together by team
  PY3_stats_adv_stats_list <- list(Stats_PY3, Adv_Stats_PY3)
  PY3_stats_adv_stats_merge <- PY3_stats_adv_stats_list |>
    reduce(full_join, by = "team") |>
    select(team, games, completion_pct, 
           pass_ypa, pass_ypr, int_pct, rush_ypc, turnovers_pg, 
           third_conv_rate, fourth_conv_rate, penalty_yds_pg, 
           yards_per_penalty, kick_return_avg, punt_return_avg, total_yds_pg, 
           pass_yds_pg, rush_yds_pg, first_downs_pg, off_ypp, def_interceptions_pg, off_ppa, 
           off_success_rate, off_explosiveness, off_power_success, off_stuff_rate, 
           off_line_yds, off_second_lvl_yds, off_open_field_yds, off_pts_per_opp, 
           off_field_pos_avg_predicted_points, off_havoc_total, off_havoc_front_seven, 
           off_havoc_db, off_standard_downs_ppa, off_standard_downs_success_rate, 
           off_standard_downs_explosiveness, off_passing_downs_ppa, 
           off_passing_downs_success_rate, off_passing_downs_explosiveness, 
           off_rushing_plays_ppa, off_rushing_plays_success_rate, 
           off_rushing_plays_explosiveness, off_passing_plays_ppa, 
           off_passing_plays_success_rate, off_passing_plays_explosiveness, def_ppa, 
           def_success_rate, def_explosiveness, def_power_success, def_stuff_rate, 
           def_line_yds, def_second_lvl_yds, def_open_field_yds, def_pts_per_opp, 
           def_field_pos_avg_predicted_points, def_havoc_total, def_havoc_front_seven, 
           def_havoc_db, def_standard_downs_ppa, def_standard_downs_success_rate, 
           def_standard_downs_explosiveness, def_passing_downs_ppa, 
           def_passing_downs_success_rate, def_passing_downs_explosiveness, 
           def_rushing_plays_ppa, def_rushing_plays_success_rate, 
           def_rushing_plays_explosiveness, def_passing_plays_ppa, 
           def_passing_plays_success_rate, def_passing_plays_explosiveness)
  colnames(PY3_stats_adv_stats_merge) <- c("team", "games_PY3", "completion_pct_PY3",
                                           "pass_ypa_PY3", "pass_ypr_PY3", "int_pct_PY3", "rush_ypc_PY3", "turnovers_pg_PY3",
                                           "third_conv_rate_PY3", "fourth_conv_rate_PY3", "penalty_yds_pg_PY3",
                                           "yards_per_penalty_PY3", "kick_return_avg_PY3", "punt_return_avg_PY3", "total_yds_pg_PY3",
                                           "pass_yds_pg_PY3", "rush_yds_pg_PY3", "first_downs_pg_PY3", "off_ypp_PY3", "def_interceptions_pg_PY3", "off_ppa_PY3",
                                           "off_success_rate_PY3", "off_explosiveness_PY3", "off_power_success_PY3", "off_stuff_rate_PY3",
                                           "off_line_yds_PY3", "off_second_lvl_yds_PY3", "off_open_field_yds_PY3", "off_pts_per_opp_PY3",
                                           "off_field_pos_avg_predicted_points_PY3", "off_havoc_total_PY3", "off_havoc_front_seven_PY3",
                                           "off_havoc_db_PY3", "off_standard_downs_ppa_PY3", "off_standard_downs_success_rate_PY3",
                                           "off_standard_downs_explosiveness_PY3", "off_passing_downs_ppa_PY3",
                                           "off_passing_downs_success_rate_PY3", "off_passing_downs_explosiveness_PY3",
                                           "off_rushing_plays_ppa_PY3", "off_rushing_plays_success_rate_PY3",
                                           "off_rushing_plays_explosiveness_PY3", "off_passing_plays_ppa_PY3",
                                           "off_passing_plays_success_rate_PY3", "off_passing_plays_explosiveness_PY3", "def_ppa_PY3",
                                           "def_success_rate_PY3", "def_explosiveness_PY3", "def_power_success_PY3", "def_stuff_rate_PY3",
                                           "def_line_yds_PY3", "def_second_lvl_yds_PY3", "def_open_field_yds_PY3", "def_pts_per_opp_PY3",
                                           "def_field_pos_avg_predicted_points_PY3", "def_havoc_total_PY3", "def_havoc_front_seven_PY3",
                                           "def_havoc_db_PY3", "def_standard_downs_ppa_PY3", "def_standard_downs_success_rate_PY3",
                                           "def_standard_downs_explosiveness_PY3", "def_passing_downs_ppa_PY3",
                                           "def_passing_downs_success_rate_PY3", "def_passing_downs_explosiveness_PY3",
                                           "def_rushing_plays_ppa_PY3", "def_rushing_plays_success_rate_PY3",
                                           "def_rushing_plays_explosiveness_PY3", "def_passing_plays_ppa_PY3",
                                           "def_passing_plays_success_rate_PY3", "def_passing_plays_explosiveness_PY3")
  PY3_df_list <- list(PY3_stats_adv_stats_merge, recruit_PY3, talent_df_PY3, SP_Rankings_PY3, FPI_df_PY3, SRS_PY3)
  PY3_df <- PY3_df_list |>
    reduce(full_join, by = "team")
  ## removing season and conference columns from COVID Optouts and FCS data frames
  COVID_Optouts_df <- COVID_Optouts_df |>
    select(-one_of('season', 'conference'))
  FCS_PY3 <- FCS_PY3 |>
    select(-one_of('season', 'conference'))
  PY3_df <- rbind(PY3_df, COVID_Optouts_df)
  PY3_df <- rbind(PY3_df, FCS_PY3)
  PY3_df[,2] <- PY3_df[,2] |> mutate_if(is.character, as.numeric)
  
  PY2_stats_adv_stats_list <- list(Stats_PY2, Adv_Stats_PY2)
  PY2_stats_adv_stats_merge <- PY2_stats_adv_stats_list |>
    reduce(full_join, by = "team") |>
    select(team, games, completion_pct, pass_ypa, pass_ypr, int_pct, rush_ypc, 
           turnovers_pg, third_conv_rate, fourth_conv_rate, penalty_yds_pg, 
           yards_per_penalty, kick_return_avg, punt_return_avg, total_yds_pg, 
           pass_yds_pg, rush_yds_pg, first_downs_pg, off_ypp, def_interceptions_pg, off_ppa, 
           off_success_rate, off_explosiveness, off_power_success, off_stuff_rate, 
           off_line_yds, off_second_lvl_yds, off_open_field_yds, off_pts_per_opp, 
           off_field_pos_avg_predicted_points, off_havoc_total, off_havoc_front_seven, 
           off_havoc_db, off_standard_downs_ppa, off_standard_downs_success_rate, 
           off_standard_downs_explosiveness, off_passing_downs_ppa, 
           off_passing_downs_success_rate, off_passing_downs_explosiveness, 
           off_rushing_plays_ppa, off_rushing_plays_success_rate, 
           off_rushing_plays_explosiveness, off_passing_plays_ppa, 
           off_passing_plays_success_rate, off_passing_plays_explosiveness, def_ppa, 
           def_success_rate, def_explosiveness, def_power_success, def_stuff_rate, 
           def_line_yds, def_second_lvl_yds, def_open_field_yds, def_pts_per_opp, 
           def_field_pos_avg_predicted_points, def_havoc_total, def_havoc_front_seven, 
           def_havoc_db, def_standard_downs_ppa, def_standard_downs_success_rate, 
           def_standard_downs_explosiveness, def_passing_downs_ppa, 
           def_passing_downs_success_rate, def_passing_downs_explosiveness, 
           def_rushing_plays_ppa, def_rushing_plays_success_rate, 
           def_rushing_plays_explosiveness, def_passing_plays_ppa, 
           def_passing_plays_success_rate, def_passing_plays_explosiveness)
  colnames(PY2_stats_adv_stats_merge) <- c("team", "games_PY2", "completion_pct_PY2",
                                           "pass_ypa_PY2", "pass_ypr_PY2", "int_pct_PY2", "rush_ypc_PY2", "turnovers_pg_PY2",
                                           "third_conv_rate_PY2", "fourth_conv_rate_PY2", "penalty_yds_pg_PY2",
                                           "yards_per_penalty_PY2", "kick_return_avg_PY2", "punt_return_avg_PY2", "total_yds_pg_PY2",
                                           "pass_yds_pg_PY2", "rush_yds_pg_PY2", "first_downs_pg_PY2", "off_ypp_PY2", "def_interceptions_pg_PY2", "off_ppa_PY2",
                                           "off_success_rate_PY2", "off_explosiveness_PY2", "off_power_success_PY2", "off_stuff_rate_PY2",
                                           "off_line_yds_PY2", "off_second_lvl_yds_PY2", "off_open_field_yds_PY2", "off_pts_per_opp_PY2",
                                           "off_field_pos_avg_predicted_points_PY2", "off_havoc_total_PY2", "off_havoc_front_seven_PY2",
                                           "off_havoc_db_PY2", "off_standard_downs_ppa_PY2", "off_standard_downs_success_rate_PY2",
                                           "off_standard_downs_explosiveness_PY2", "off_passing_downs_ppa_PY2",
                                           "off_passing_downs_success_rate_PY2", "off_passing_downs_explosiveness_PY2",
                                           "off_rushing_plays_ppa_PY2", "off_rushing_plays_success_rate_PY2",
                                           "off_rushing_plays_explosiveness_PY2", "off_passing_plays_ppa_PY2",
                                           "off_passing_plays_success_rate_PY2", "off_passing_plays_explosiveness_PY2", "def_ppa_PY2",
                                           "def_success_rate_PY2", "def_explosiveness_PY2", "def_power_success_PY2", "def_stuff_rate_PY2",
                                           "def_line_yds_PY2", "def_second_lvl_yds_PY2", "def_open_field_yds_PY2", "def_pts_per_opp_PY2",
                                           "def_field_pos_avg_predicted_points_PY2", "def_havoc_total_PY2", "def_havoc_front_seven_PY2",
                                           "def_havoc_db_PY2", "def_standard_downs_ppa_PY2", "def_standard_downs_success_rate_PY2",
                                           "def_standard_downs_explosiveness_PY2", "def_passing_downs_ppa_PY2",
                                           "def_passing_downs_success_rate_PY2", "def_passing_downs_explosiveness_PY2",
                                           "def_rushing_plays_ppa_PY2", "def_rushing_plays_success_rate_PY2",
                                           "def_rushing_plays_explosiveness_PY2", "def_passing_plays_ppa_PY2",
                                           "def_passing_plays_success_rate_PY2", "def_passing_plays_explosiveness_PY2")
  PY2_df_list <- list(PY2_stats_adv_stats_merge, recruit_PY2, talent_df_PY2, SP_Rankings_PY2, FPI_df_PY2, SRS_PY2)
  PY2_df <- PY2_df_list |>
    reduce(full_join, by = "team")
  PY2_df <- rbind(PY2_df, FCS_PY2)
  PY2_df[,2] <- PY2_df[,2] |> mutate_if(is.character, as.numeric)
  
  
  PY1_stats_adv_stats_list <- list(Stats_PY1, Adv_Stats_PY1)
  PY1_stats_adv_stats_merge <- PY1_stats_adv_stats_list |>
    reduce(full_join, by = "team") |>
    select(team, games, completion_pct, pass_ypa, pass_ypr, int_pct, rush_ypc, 
           turnovers_pg, third_conv_rate, fourth_conv_rate, penalty_yds_pg, 
           yards_per_penalty, kick_return_avg, punt_return_avg, total_yds_pg, 
           pass_yds_pg, rush_yds_pg, first_downs_pg, off_ypp, def_interceptions_pg, off_ppa, 
           off_success_rate, off_explosiveness, off_power_success, off_stuff_rate, 
           off_line_yds, off_second_lvl_yds, off_open_field_yds, off_pts_per_opp, 
           off_field_pos_avg_predicted_points, off_havoc_total, off_havoc_front_seven, 
           off_havoc_db, off_standard_downs_ppa, off_standard_downs_success_rate, 
           off_standard_downs_explosiveness, off_passing_downs_ppa, 
           off_passing_downs_success_rate, off_passing_downs_explosiveness, 
           off_rushing_plays_ppa, off_rushing_plays_success_rate, 
           off_rushing_plays_explosiveness, off_passing_plays_ppa, 
           off_passing_plays_success_rate, off_passing_plays_explosiveness, def_ppa, 
           def_success_rate, def_explosiveness, def_power_success, def_stuff_rate, 
           def_line_yds, def_second_lvl_yds, def_open_field_yds, def_pts_per_opp, 
           def_field_pos_avg_predicted_points, def_havoc_total, def_havoc_front_seven, 
           def_havoc_db, def_standard_downs_ppa, def_standard_downs_success_rate, 
           def_standard_downs_explosiveness, def_passing_downs_ppa, 
           def_passing_downs_success_rate, def_passing_downs_explosiveness, 
           def_rushing_plays_ppa, def_rushing_plays_success_rate, 
           def_rushing_plays_explosiveness, def_passing_plays_ppa, 
           def_passing_plays_success_rate, def_passing_plays_explosiveness)
  colnames(PY1_stats_adv_stats_merge) <- c("team", "games_PY1", "completion_pct_PY1",
                                           "pass_ypa_PY1", "pass_ypr_PY1", "int_pct_PY1", "rush_ypc_PY1", "turnovers_pg_PY1",
                                           "third_conv_rate_PY1", "fourth_conv_rate_PY1", "penalty_yds_pg_PY1",
                                           "yards_per_penalty_PY1", "kick_return_avg_PY1", "punt_return_avg_PY1", "total_yds_pg_PY1",
                                           "pass_yds_pg_PY1", "rush_yds_pg_PY1", "first_downs_pg_PY1", "off_ypp_PY1", "def_interceptions_pg_PY1", "off_ppa_PY1",
                                           "off_success_rate_PY1", "off_explosiveness_PY1", "off_power_success_PY1", "off_stuff_rate_PY1",
                                           "off_line_yds_PY1", "off_second_lvl_yds_PY1", "off_open_field_yds_PY1", "off_pts_per_opp_PY1",
                                           "off_field_pos_avg_predicted_points_PY1", "off_havoc_total_PY1", "off_havoc_front_seven_PY1",
                                           "off_havoc_db_PY1", "off_standard_downs_ppa_PY1", "off_standard_downs_success_rate_PY1",
                                           "off_standard_downs_explosiveness_PY1", "off_passing_downs_ppa_PY1",
                                           "off_passing_downs_success_rate_PY1", "off_passing_downs_explosiveness_PY1",
                                           "off_rushing_plays_ppa_PY1", "off_rushing_plays_success_rate_PY1",
                                           "off_rushing_plays_explosiveness_PY1", "off_passing_plays_ppa_PY1",
                                           "off_passing_plays_success_rate_PY1", "off_passing_plays_explosiveness_PY1", "def_ppa_PY1",
                                           "def_success_rate_PY1", "def_explosiveness_PY1", "def_power_success_PY1", "def_stuff_rate_PY1",
                                           "def_line_yds_PY1", "def_second_lvl_yds_PY1", "def_open_field_yds_PY1", "def_pts_per_opp_PY1",
                                           "def_field_pos_avg_predicted_points_PY1", "def_havoc_total_PY1", "def_havoc_front_seven_PY1",
                                           "def_havoc_db_PY1", "def_standard_downs_ppa_PY1", "def_standard_downs_success_rate_PY1",
                                           "def_standard_downs_explosiveness_PY1", "def_passing_downs_ppa_PY1",
                                           "def_passing_downs_success_rate_PY1", "def_passing_downs_explosiveness_PY1",
                                           "def_rushing_plays_ppa_PY1", "def_rushing_plays_success_rate_PY1",
                                           "def_rushing_plays_explosiveness_PY1", "def_passing_plays_ppa_PY1",
                                           "def_passing_plays_success_rate_PY1", "def_passing_plays_explosiveness_PY1")
  PY1_df_list <- list(PY1_stats_adv_stats_merge, recruit_PY1, talent_df_PY1, SP_Rankings_PY1, FPI_df_PY1, SRS_PY1)
  PY1_df <- PY1_df_list |>
    reduce(full_join, by = "team")
  PY1_df <- rbind(PY1_df, FCS_PY1)
  
  ## Current Years dataframes
  stats_adv_stats_list <- list(Stats, Adv_Stats)
  stats_adv_stats_merge <- stats_adv_stats_list |>
    reduce(full_join, by = "team") |>
    select(season, team, conference, games, completion_pct, pass_ypa, pass_ypr, int_pct, rush_ypc, 
           turnovers_pg, third_conv_rate, fourth_conv_rate, penalty_yds_pg, 
           yards_per_penalty, kick_return_avg, punt_return_avg, total_yds_pg, 
           pass_yds_pg, rush_yds_pg, first_downs_pg, off_ypp, def_interceptions_pg, off_ppa, 
           off_success_rate, off_explosiveness, off_power_success, off_stuff_rate, 
           off_line_yds, off_second_lvl_yds, off_open_field_yds, off_pts_per_opp, 
           off_field_pos_avg_predicted_points, off_havoc_total, off_havoc_front_seven, 
           off_havoc_db, off_standard_downs_ppa, off_standard_downs_success_rate, 
           off_standard_downs_explosiveness, off_passing_downs_ppa, 
           off_passing_downs_success_rate, off_passing_downs_explosiveness, 
           off_rushing_plays_ppa, off_rushing_plays_success_rate, 
           off_rushing_plays_explosiveness, off_passing_plays_ppa, 
           off_passing_plays_success_rate, off_passing_plays_explosiveness, def_ppa, 
           def_success_rate, def_explosiveness, def_power_success, def_stuff_rate, 
           def_line_yds, def_second_lvl_yds, def_open_field_yds, def_pts_per_opp, 
           def_field_pos_avg_predicted_points, def_havoc_total, def_havoc_front_seven, 
           def_havoc_db, def_standard_downs_ppa, def_standard_downs_success_rate, 
           def_standard_downs_explosiveness, def_passing_downs_ppa, 
           def_passing_downs_success_rate, def_passing_downs_explosiveness, 
           def_rushing_plays_ppa, def_rushing_plays_success_rate, 
           def_rushing_plays_explosiveness, def_passing_plays_ppa, 
           def_passing_plays_success_rate, def_passing_plays_explosiveness)
  ## merging all current year data frames
  # due to availability issues, SP_Rankings not included with current data
  # SRS left out for Week 1
  Current_df_list <- list(stats_adv_stats_merge, recruit, FPI_df)
  Current_df <- Current_df_list |>
    reduce(full_join, by = "team")
  
  ## merging all data frames in order of PY3, PY2, PY1
  all_PY_df_list <- list(PY3_df, PY2_df, PY1_df)
  all_PY_df <- all_PY_df_list |>
    reduce(full_join, by = "team")
  
  ## after binding JMU csv as new row to PY df
  all_df_list <- list(Current_df, all_PY_df)
  VoA_Variables <- all_df_list |>
    reduce(full_join, by = "team") |>
    mutate(FPI_SP_SRS_PY3_mean = (sp_rating_PY3 + FPI_PY3 + SRS_rating_PY3) / 3,
           FPI_SP_SRS_PY2_mean = (sp_rating_PY2 + FPI_PY2 + SRS_rating_PY2) / 3,
           FPI_SP_SRS_PY1_mean = (sp_rating_PY1 + FPI_PY1 + SRS_rating_PY1) / 3,
           AllPY_FPI_SP_SRS_mean = (FPI_SP_SRS_PY3_mean + FPI_SP_SRS_PY2_mean + FPI_SP_SRS_PY1_mean) / 3,
           WeightedAllPY_FPI_SP_SRS_mean = ((FPI_SP_SRS_PY3_mean * 0.1) + (FPI_SP_SRS_PY2_mean * 0.25) + (FPI_SP_SRS_PY1_mean * 0.65)) / 3,
           FPI_SRS_mean = (FPI + SRS_rating_PY1) / 2)
           # FPI_SP_mean = (sp_rating + FPI) / 2)
  # due to availability issues, SP_Rankings not included with current data
} else if (as.numeric(week) <= 4) {
  ##### WEEKS 2-4 DF Merge #####
  ## merging data frames together, arranging columns
  ## need to merge stats and advanced stats together first so I can change column names to avoid duplicate column names later on
  # then I will be merging data frames for the same year together
  # then I will merge years together by team
  PY2_stats_adv_stats_list <- list(Stats_PY2, Adv_Stats_PY2)
  PY2_stats_adv_stats_merge <- PY2_stats_adv_stats_list |>
    reduce(full_join, by = "team") |>
    select(team, games, completion_pct, pass_ypa, pass_ypr, int_pct, rush_ypc, 
           turnovers_pg, third_conv_rate, fourth_conv_rate, penalty_yds_pg, 
           yards_per_penalty, kick_return_avg, punt_return_avg, total_yds_pg, 
           pass_yds_pg, rush_yds_pg, first_downs_pg, off_ypp, def_interceptions_pg, off_ppa, 
           off_success_rate, off_explosiveness, off_power_success, off_stuff_rate, 
           off_line_yds, off_second_lvl_yds, off_open_field_yds, off_pts_per_opp, 
           off_field_pos_avg_predicted_points, off_havoc_total, off_havoc_front_seven, 
           off_havoc_db, off_standard_downs_ppa, off_standard_downs_success_rate, 
           off_standard_downs_explosiveness, off_passing_downs_ppa, 
           off_passing_downs_success_rate, off_passing_downs_explosiveness, 
           off_rushing_plays_ppa, off_rushing_plays_success_rate, 
           off_rushing_plays_explosiveness, off_passing_plays_ppa, 
           off_passing_plays_success_rate, off_passing_plays_explosiveness, def_ppa, 
           def_success_rate, def_explosiveness, def_power_success, def_stuff_rate, 
           def_line_yds, def_second_lvl_yds, def_open_field_yds, def_pts_per_opp, 
           def_field_pos_avg_predicted_points, def_havoc_total, def_havoc_front_seven, 
           def_havoc_db, def_standard_downs_ppa, def_standard_downs_success_rate, 
           def_standard_downs_explosiveness, def_passing_downs_ppa, 
           def_passing_downs_success_rate, def_passing_downs_explosiveness, 
           def_rushing_plays_ppa, def_rushing_plays_success_rate, 
           def_rushing_plays_explosiveness, def_passing_plays_ppa, 
           def_passing_plays_success_rate, def_passing_plays_explosiveness)
  colnames(PY2_stats_adv_stats_merge) <- c("team", "games_PY2", "completion_pct_PY2",
                                           "pass_ypa_PY2", "pass_ypr_PY2", "int_pct_PY2", "rush_ypc_PY2", "turnovers_pg_PY2",
                                           "third_conv_rate_PY2", "fourth_conv_rate_PY2", "penalty_yds_pg_PY2",
                                           "yards_per_penalty_PY2", "kick_return_avg_PY2", "punt_return_avg_PY2", "total_yds_pg_PY2",
                                           "pass_yds_pg_PY2", "rush_yds_pg_PY2", "first_downs_pg_PY2", "off_ypp_PY2", "def_interceptions_pg_PY2", "off_ppa_PY2",
                                           "off_success_rate_PY2", "off_explosiveness_PY2", "off_power_success_PY2", "off_stuff_rate_PY2",
                                           "off_line_yds_PY2", "off_second_lvl_yds_PY2", "off_open_field_yds_PY2", "off_pts_per_opp_PY2",
                                           "off_field_pos_avg_predicted_points_PY2", "off_havoc_total_PY2", "off_havoc_front_seven_PY2",
                                           "off_havoc_db_PY2", "off_standard_downs_ppa_PY2", "off_standard_downs_success_rate_PY2",
                                           "off_standard_downs_explosiveness_PY2", "off_passing_downs_ppa_PY2",
                                           "off_passing_downs_success_rate_PY2", "off_passing_downs_explosiveness_PY2",
                                           "off_rushing_plays_ppa_PY2", "off_rushing_plays_success_rate_PY2",
                                           "off_rushing_plays_explosiveness_PY2", "off_passing_plays_ppa_PY2",
                                           "off_passing_plays_success_rate_PY2", "off_passing_plays_explosiveness_PY2", "def_ppa_PY2",
                                           "def_success_rate_PY2", "def_explosiveness_PY2", "def_power_success_PY2", "def_stuff_rate_PY2",
                                           "def_line_yds_PY2", "def_second_lvl_yds_PY2", "def_open_field_yds_PY2", "def_pts_per_opp_PY2",
                                           "def_field_pos_avg_predicted_points_PY2", "def_havoc_total_PY2", "def_havoc_front_seven_PY2",
                                           "def_havoc_db_PY2", "def_standard_downs_ppa_PY2", "def_standard_downs_success_rate_PY2",
                                           "def_standard_downs_explosiveness_PY2", "def_passing_downs_ppa_PY2",
                                           "def_passing_downs_success_rate_PY2", "def_passing_downs_explosiveness_PY2",
                                           "def_rushing_plays_ppa_PY2", "def_rushing_plays_success_rate_PY2",
                                           "def_rushing_plays_explosiveness_PY2", "def_passing_plays_ppa_PY2",
                                           "def_passing_plays_success_rate_PY2", "def_passing_plays_explosiveness_PY2")
  PY2_df_list <- list(PY2_stats_adv_stats_merge, recruit_PY2, talent_df_PY2, SP_Rankings_PY2, FPI_df_PY2, SRS_PY2)
  PY2_df <- PY2_df_list |>
    reduce(full_join, by = "team")
  PY2_df <- rbind(PY2_df, FCS_PY2)
  
  ## PY1 data frames being merged
  PY1_stats_adv_stats_list <- list(Stats_PY1, Adv_Stats_PY1)
  PY1_stats_adv_stats_merge <- PY1_stats_adv_stats_list |>
    reduce(full_join, by = "team") |>
    select(team, games, completion_pct, pass_ypa, pass_ypr, int_pct, rush_ypc, 
           turnovers_pg, third_conv_rate, fourth_conv_rate, penalty_yds_pg, 
           yards_per_penalty, kick_return_avg, punt_return_avg, total_yds_pg, 
           pass_yds_pg, rush_yds_pg, first_downs_pg, off_ypp, def_interceptions_pg, off_ppa, 
           off_success_rate, off_explosiveness, off_power_success, off_stuff_rate, 
           off_line_yds, off_second_lvl_yds, off_open_field_yds, off_pts_per_opp, 
           off_field_pos_avg_predicted_points, off_havoc_total, off_havoc_front_seven, 
           off_havoc_db, off_standard_downs_ppa, off_standard_downs_success_rate, 
           off_standard_downs_explosiveness, off_passing_downs_ppa, 
           off_passing_downs_success_rate, off_passing_downs_explosiveness, 
           off_rushing_plays_ppa, off_rushing_plays_success_rate, 
           off_rushing_plays_explosiveness, off_passing_plays_ppa, 
           off_passing_plays_success_rate, off_passing_plays_explosiveness, def_ppa, 
           def_success_rate, def_explosiveness, def_power_success, def_stuff_rate, 
           def_line_yds, def_second_lvl_yds, def_open_field_yds, def_pts_per_opp, 
           def_field_pos_avg_predicted_points, def_havoc_total, def_havoc_front_seven, 
           def_havoc_db, def_standard_downs_ppa, def_standard_downs_success_rate, 
           def_standard_downs_explosiveness, def_passing_downs_ppa, 
           def_passing_downs_success_rate, def_passing_downs_explosiveness, 
           def_rushing_plays_ppa, def_rushing_plays_success_rate, 
           def_rushing_plays_explosiveness, def_passing_plays_ppa, 
           def_passing_plays_success_rate, def_passing_plays_explosiveness)
  colnames(PY1_stats_adv_stats_merge) <- c("team", "games_PY1", "completion_pct_PY1",
                                           "pass_ypa_PY1", "pass_ypr_PY1", "int_pct_PY1", "rush_ypc_PY1", "turnovers_pg_PY1",
                                           "third_conv_rate_PY1", "fourth_conv_rate_PY1", "penalty_yds_pg_PY1",
                                           "yards_per_penalty_PY1", "kick_return_avg_PY1", "punt_return_avg_PY1", "total_yds_pg_PY1",
                                           "pass_yds_pg_PY1", "rush_yds_pg_PY1", "first_downs_pg_PY1", "off_ypp_PY1", "def_interceptions_pg_PY1", "off_ppa_PY1",
                                           "off_success_rate_PY1", "off_explosiveness_PY1", "off_power_success_PY1", "off_stuff_rate_PY1",
                                           "off_line_yds_PY1", "off_second_lvl_yds_PY1", "off_open_field_yds_PY1", "off_pts_per_opp_PY1",
                                           "off_field_pos_avg_predicted_points_PY1", "off_havoc_total_PY1", "off_havoc_front_seven_PY1",
                                           "off_havoc_db_PY1", "off_standard_downs_ppa_PY1", "off_standard_downs_success_rate_PY1",
                                           "off_standard_downs_explosiveness_PY1", "off_passing_downs_ppa_PY1",
                                           "off_passing_downs_success_rate_PY1", "off_passing_downs_explosiveness_PY1",
                                           "off_rushing_plays_ppa_PY1", "off_rushing_plays_success_rate_PY1",
                                           "off_rushing_plays_explosiveness_PY1", "off_passing_plays_ppa_PY1",
                                           "off_passing_plays_success_rate_PY1", "off_passing_plays_explosiveness_PY1", "def_ppa_PY1",
                                           "def_success_rate_PY1", "def_explosiveness_PY1", "def_power_success_PY1", "def_stuff_rate_PY1",
                                           "def_line_yds_PY1", "def_second_lvl_yds_PY1", "def_open_field_yds_PY1", "def_pts_per_opp_PY1",
                                           "def_field_pos_avg_predicted_points_PY1", "def_havoc_total_PY1", "def_havoc_front_seven_PY1",
                                           "def_havoc_db_PY1", "def_standard_downs_ppa_PY1", "def_standard_downs_success_rate_PY1",
                                           "def_standard_downs_explosiveness_PY1", "def_passing_downs_ppa_PY1",
                                           "def_passing_downs_success_rate_PY1", "def_passing_downs_explosiveness_PY1",
                                           "def_rushing_plays_ppa_PY1", "def_rushing_plays_success_rate_PY1",
                                           "def_rushing_plays_explosiveness_PY1", "def_passing_plays_ppa_PY1",
                                           "def_passing_plays_success_rate_PY1", "def_passing_plays_explosiveness_PY1")
  PY1_df_list <- list(PY1_stats_adv_stats_merge, recruit_PY1, talent_df_PY1, SP_Rankings_PY1, FPI_df_PY1, SRS_PY1)
  PY1_df <- PY1_df_list |>
    reduce(full_join, by = "team")
  PY1_df <- rbind(PY1_df, FCS_PY1)
  
  ## Current Years dataframes
  stats_adv_stats_list <- list(Stats, Adv_Stats)
  stats_adv_stats_merge <- stats_adv_stats_list |>
    reduce(full_join, by = "team") |>
    select(season, team, conference, games, completion_pct, pass_ypa, pass_ypr, int_pct, rush_ypc, 
           turnovers_pg, third_conv_rate, fourth_conv_rate, penalty_yds_pg, 
           yards_per_penalty, kick_return_avg, punt_return_avg, total_yds_pg, 
           pass_yds_pg, rush_yds_pg, first_downs_pg, off_ypp, def_interceptions_pg, off_ppa, 
           off_success_rate, off_explosiveness, off_power_success, off_stuff_rate, 
           off_line_yds, off_second_lvl_yds, off_open_field_yds, off_pts_per_opp, 
           off_field_pos_avg_predicted_points, off_havoc_total, off_havoc_front_seven, 
           off_havoc_db, off_standard_downs_ppa, off_standard_downs_success_rate, 
           off_standard_downs_explosiveness, off_passing_downs_ppa, 
           off_passing_downs_success_rate, off_passing_downs_explosiveness, 
           off_rushing_plays_ppa, off_rushing_plays_success_rate, 
           off_rushing_plays_explosiveness, off_passing_plays_ppa, 
           off_passing_plays_success_rate, off_passing_plays_explosiveness, def_ppa, 
           def_success_rate, def_explosiveness, def_power_success, def_stuff_rate, 
           def_line_yds, def_second_lvl_yds, def_open_field_yds, def_pts_per_opp, 
           def_field_pos_avg_predicted_points, def_havoc_total, def_havoc_front_seven, 
           def_havoc_db, def_standard_downs_ppa, def_standard_downs_success_rate, 
           def_standard_downs_explosiveness, def_passing_downs_ppa, 
           def_passing_downs_success_rate, def_passing_downs_explosiveness, 
           def_rushing_plays_ppa, def_rushing_plays_success_rate, 
           def_rushing_plays_explosiveness, def_passing_plays_ppa, 
           def_passing_plays_success_rate, def_passing_plays_explosiveness)
  ## merging all current year data frames
  # due to availability issues, SP_Rankings not included with current data
  Current_df_list <- list(stats_adv_stats_merge, recruit, FPI_df, SP_Rankings, recruit_PY3)
  Current_df <- Current_df_list |>
    reduce(full_join, by = "team")
  
  ## merging all PY data frames in order of PY2, PY1
  all_PY_df_list <- list(PY2_df, PY1_df)
  all_PY_df <- all_PY_df_list |>
    reduce(full_join, by = "team")
  
  ## after binding JMU csv as new row to PY df
  all_df_list <- list(Current_df, all_PY_df)
  VoA_Variables <- all_df_list |>
    reduce(full_join, by = "team") |>
    mutate(FPI_SP_SRS_PY2_mean = (sp_rating_PY2 + FPI_PY2 + SRS_rating_PY2) / 3,
           FPI_SP_SRS_PY1_mean = (sp_rating_PY1 + FPI_PY1 + SRS_rating_PY1) / 3,
           AllPY_FPI_SP_SRS_mean = (FPI_SP_SRS_PY2_mean + FPI_SP_SRS_PY1_mean) / 2,
           WeightedAllPY_FPI_SP_SRS_mean = ((FPI_SP_SRS_PY2_mean * 0.3) + (FPI_SP_SRS_PY1_mean * 0.7)) / 2,
           FPI_SP_mean = (FPI + sp_rating) / 2,
           FPI_SRS_mean = (FPI + SRS_rating_PY1) / 2,
           FPI_SP_SRS_mean = (FPI + sp_rating + SRS_rating_PY1) / 3)
  # due to availability issues, SP_Rankings sometimes not included with current data
  # as of 2023 week 2, some SP ratings are available.
  # due to availability issues, they may not always be completely up to date
} else if (as.numeric(week) <= 6) {
  ##### WEEKS 5-6 DF Merge #####
  ## merging data frames together, arranging columns
  ## need to merge stats and advanced stats together first so I can change column names to avoid duplicate column names later on
  # then I will be merging data frames for the same year together
  # then I will merge years together by team
  ## PY1 data frames being merged
  PY1_stats_adv_stats_list <- list(Stats_PY1, Adv_Stats_PY1)
  PY1_stats_adv_stats_merge <- PY1_stats_adv_stats_list |>
    reduce(full_join, by = "team") |>
    select(team, games, completion_pct, pass_ypa, pass_ypr, int_pct, rush_ypc, 
           turnovers_pg, third_conv_rate, fourth_conv_rate, penalty_yds_pg, 
           yards_per_penalty, kick_return_avg, punt_return_avg, total_yds_pg, 
           pass_yds_pg, rush_yds_pg, first_downs_pg, off_ypp, def_interceptions_pg, off_ppa, 
           off_success_rate, off_explosiveness, off_power_success, off_stuff_rate, 
           off_line_yds, off_second_lvl_yds, off_open_field_yds, off_pts_per_opp, 
           off_field_pos_avg_predicted_points, off_havoc_total, off_havoc_front_seven, 
           off_havoc_db, off_standard_downs_ppa, off_standard_downs_success_rate, 
           off_standard_downs_explosiveness, off_passing_downs_ppa, 
           off_passing_downs_success_rate, off_passing_downs_explosiveness, 
           off_rushing_plays_ppa, off_rushing_plays_success_rate, 
           off_rushing_plays_explosiveness, off_passing_plays_ppa, 
           off_passing_plays_success_rate, off_passing_plays_explosiveness, def_ppa, 
           def_success_rate, def_explosiveness, def_power_success, def_stuff_rate, 
           def_line_yds, def_second_lvl_yds, def_open_field_yds, def_pts_per_opp, 
           def_field_pos_avg_predicted_points, def_havoc_total, def_havoc_front_seven, 
           def_havoc_db, def_standard_downs_ppa, def_standard_downs_success_rate, 
           def_standard_downs_explosiveness, def_passing_downs_ppa, 
           def_passing_downs_success_rate, def_passing_downs_explosiveness, 
           def_rushing_plays_ppa, def_rushing_plays_success_rate, 
           def_rushing_plays_explosiveness, def_passing_plays_ppa, 
           def_passing_plays_success_rate, def_passing_plays_explosiveness)
  colnames(PY1_stats_adv_stats_merge) <- c("team", "games_PY1", "completion_pct_PY1",
                                           "pass_ypa_PY1", "pass_ypr_PY1", "int_pct_PY1", "rush_ypc_PY1", "turnovers_pg_PY1",
                                           "third_conv_rate_PY1", "fourth_conv_rate_PY1", "penalty_yds_pg_PY1",
                                           "yards_per_penalty_PY1", "kick_return_avg_PY1", "punt_return_avg_PY1", "total_yds_pg_PY1",
                                           "pass_yds_pg_PY1", "rush_yds_pg_PY1", "first_downs_pg_PY1", "off_ypp_PY1", "def_interceptions_pg_PY1", "off_ppa_PY1",
                                           "off_success_rate_PY1", "off_explosiveness_PY1", "off_power_success_PY1", "off_stuff_rate_PY1",
                                           "off_line_yds_PY1", "off_second_lvl_yds_PY1", "off_open_field_yds_PY1", "off_pts_per_opp_PY1",
                                           "off_field_pos_avg_predicted_points_PY1", "off_havoc_total_PY1", "off_havoc_front_seven_PY1",
                                           "off_havoc_db_PY1", "off_standard_downs_ppa_PY1", "off_standard_downs_success_rate_PY1",
                                           "off_standard_downs_explosiveness_PY1", "off_passing_downs_ppa_PY1",
                                           "off_passing_downs_success_rate_PY1", "off_passing_downs_explosiveness_PY1",
                                           "off_rushing_plays_ppa_PY1", "off_rushing_plays_success_rate_PY1",
                                           "off_rushing_plays_explosiveness_PY1", "off_passing_plays_ppa_PY1",
                                           "off_passing_plays_success_rate_PY1", "off_passing_plays_explosiveness_PY1", "def_ppa_PY1",
                                           "def_success_rate_PY1", "def_explosiveness_PY1", "def_power_success_PY1", "def_stuff_rate_PY1",
                                           "def_line_yds_PY1", "def_second_lvl_yds_PY1", "def_open_field_yds_PY1", "def_pts_per_opp_PY1",
                                           "def_field_pos_avg_predicted_points_PY1", "def_havoc_total_PY1", "def_havoc_front_seven_PY1",
                                           "def_havoc_db_PY1", "def_standard_downs_ppa_PY1", "def_standard_downs_success_rate_PY1",
                                           "def_standard_downs_explosiveness_PY1", "def_passing_downs_ppa_PY1",
                                           "def_passing_downs_success_rate_PY1", "def_passing_downs_explosiveness_PY1",
                                           "def_rushing_plays_ppa_PY1", "def_rushing_plays_success_rate_PY1",
                                           "def_rushing_plays_explosiveness_PY1", "def_passing_plays_ppa_PY1",
                                           "def_passing_plays_success_rate_PY1", "def_passing_plays_explosiveness_PY1")
  PY1_df_list <- list(PY1_stats_adv_stats_merge, recruit_PY1, talent_df_PY1, SP_Rankings_PY1, FPI_df_PY1, SRS_PY1)
  PY1_df <- PY1_df_list |>
    reduce(full_join, by = "team")
  PY1_df <- rbind(PY1_df, FCS_PY1)
  
  
  ## Current Years dataframes
  stats_adv_stats_list <- list(Stats, Adv_Stats)
  stats_adv_stats_merge <- stats_adv_stats_list |>
    reduce(full_join, by = "team") |>
    select(season, team, conference, games, completion_pct, pass_ypa, pass_ypr, int_pct, rush_ypc, 
           turnovers_pg, third_conv_rate, fourth_conv_rate, penalty_yds_pg, 
           yards_per_penalty, kick_return_avg, punt_return_avg, total_yds_pg, 
           pass_yds_pg, rush_yds_pg, first_downs_pg, off_ypp, def_interceptions_pg, off_ppa, 
           off_success_rate, off_explosiveness, off_power_success, off_stuff_rate, 
           off_line_yds, off_second_lvl_yds, off_open_field_yds, off_pts_per_opp, 
           off_field_pos_avg_predicted_points, off_havoc_total, off_havoc_front_seven, 
           off_havoc_db, off_standard_downs_ppa, off_standard_downs_success_rate, 
           off_standard_downs_explosiveness, off_passing_downs_ppa, 
           off_passing_downs_success_rate, off_passing_downs_explosiveness, 
           off_rushing_plays_ppa, off_rushing_plays_success_rate, 
           off_rushing_plays_explosiveness, off_passing_plays_ppa, 
           off_passing_plays_success_rate, off_passing_plays_explosiveness, def_ppa, 
           def_success_rate, def_explosiveness, def_power_success, def_stuff_rate, 
           def_line_yds, def_second_lvl_yds, def_open_field_yds, def_pts_per_opp, 
           def_field_pos_avg_predicted_points, def_havoc_total, def_havoc_front_seven, 
           def_havoc_db, def_standard_downs_ppa, def_standard_downs_success_rate, 
           def_standard_downs_explosiveness, def_passing_downs_ppa, 
           def_passing_downs_success_rate, def_passing_downs_explosiveness, 
           def_rushing_plays_ppa, def_rushing_plays_success_rate, 
           def_rushing_plays_explosiveness, def_passing_plays_ppa, 
           def_passing_plays_success_rate, def_passing_plays_explosiveness)
  ## merging all current year data frames
  # due to availability issues, SP_Rankings not included with current data
  Current_df_list <- list(stats_adv_stats_merge, recruit, recruit_PY2, recruit_PY3, FPI_df, SRS)
  Current_df <- Current_df_list |>
    reduce(full_join, by = "team")
  
  ## merging all data frames
  all_df_list <- list(Current_df, PY1_df)
  VoA_Variables <- all_df_list |>
    reduce(full_join, by = "team") |>
    mutate(FPI_SP_SRS_PY1_mean = (sp_rating_PY1 + FPI_PY1 + SRS_rating_PY1) / 3,
           FPI_SRS_mean = (FPI + SRS) / 2)
           # FPI_SP_mean = (sp_rating + FPI) / 2)
           # due to availability issues, SP_Rankings not included with current data
} else {
  ##### CURRENT SEASON ONLY DF Merge #####
  ## Current Years data frames
  stats_adv_stats_list <- list(Stats, Adv_Stats)
  stats_adv_stats_merge <- stats_adv_stats_list |>
    reduce(full_join, by = "team") |>
    select(season, team, conference, games, completion_pct, pass_ypa, pass_ypr, int_pct, rush_ypc, 
           turnovers_pg, third_conv_rate, fourth_conv_rate, penalty_yds_pg, 
           yards_per_penalty, kick_return_avg, punt_return_avg, total_yds_pg, 
           pass_yds_pg, rush_yds_pg, first_downs_pg, off_ypp, def_interceptions_pg, off_ppa, 
           off_success_rate, off_explosiveness, off_power_success, off_stuff_rate, 
           off_line_yds, off_second_lvl_yds, off_open_field_yds, off_pts_per_opp, 
           off_field_pos_avg_predicted_points, off_havoc_total, off_havoc_front_seven, 
           off_havoc_db, off_standard_downs_ppa, off_standard_downs_success_rate, 
           off_standard_downs_explosiveness, off_passing_downs_ppa, 
           off_passing_downs_success_rate, off_passing_downs_explosiveness, 
           off_rushing_plays_ppa, off_rushing_plays_success_rate, 
           off_rushing_plays_explosiveness, off_passing_plays_ppa, 
           off_passing_plays_success_rate, off_passing_plays_explosiveness, def_ppa, 
           def_success_rate, def_explosiveness, def_power_success, def_stuff_rate, 
           def_line_yds, def_second_lvl_yds, def_open_field_yds, def_pts_per_opp, 
           def_field_pos_avg_predicted_points, def_havoc_total, def_havoc_front_seven, 
           def_havoc_db, def_standard_downs_ppa, def_standard_downs_success_rate, 
           def_standard_downs_explosiveness, def_passing_downs_ppa, 
           def_passing_downs_success_rate, def_passing_downs_explosiveness, 
           def_rushing_plays_ppa, def_rushing_plays_success_rate, 
           def_rushing_plays_explosiveness, def_passing_plays_ppa, 
           def_passing_plays_success_rate, def_passing_plays_explosiveness)
  ## merging all current year data frames
  # due to availability issues, SP_Rankings not included with current data
  Current_df_list <- list(stats_adv_stats_merge, FPI_df, SRS)
  VoA_Variables <- Current_df_list |>
    reduce(full_join, by = "team") |>
    mutate(FPI_SRS_mean = (FPI + SRS_rating) / 2)
    ## mutate(FPI_SP_mean = (sp_rating + FPI) / 2)
  
  ## Making values numeric
  VoA_Variables[,4:ncol(VoA_Variables)] <- VoA_Variables[,4:ncol(VoA_Variables)] |> mutate_if(is.character,as.numeric)
} 
## end of if statement

##### Eliminating NAs, fixing conferences, adding Week number to VoA Variables #####
## eliminating NAs that may still exist
# leaving this outside an if statement because this could be an issue regardless of season or CFB_Week
# currently commented out because I added this fix to each individual stat pull in function
## VoA_Variables[is.na(VoA_Variables)] = 0
## above code useful for Week 0, not necessary now that current season data is available
##### Fixing conference errors for Week 0 (Preseason) #####
if (as.numeric(week) == 0) {
  VoA_Variables <- VoA_Variables |>
    select(-conference)
  current_conferences <- cfbd_team_info(year = as.integer(year)) |>
    filter(school %in% VoA_Variables$team) |>
    select(school, conference)
  colnames(current_conferences) <- c("team", "conference")
  VoA_Variables <- full_join(VoA_Variables, current_conferences, by = "team") |>
    relocate(conference, .after = team)
} else {
  print("current season conferences should be in use!")
}

## Adding Column with CFB Week number
# same number for each team, numeric version of number input in readline function at beginning of script
VoA_Variables <- VoA_Variables |>
  mutate(CFB_Week = rep(as.numeric(week), nrow(VoA_Variables)), .before = 2)

## weird recruit pts fix which is needed in redo of 2022 week 8 rankings
# Florida International for some reason continually brings up errors in some form or another
# what I'm saying is that FIU can go fuck itself
# SHSU and JSU get a pass for now
# recruit pts seem to be fine for everybody else
# anyway here's wonderwall
VoA_Variables$recruit_pts_PY1[VoA_Variables$team == "Florida International"] = 123.92
VoA_Variables$recruit_pts[VoA_Variables$team == "Sam Houston State"] = 141.34
VoA_Variables$recruit_pts[VoA_Variables$team == "Jacksonville State"] = 13.85

##### Adding Rank Columns #####
### if Week = 0
# PY3 weighted 1x, PY2 weighted 2x, PY1 weighted 3x
### if Week = 1
# PY3 weighted 1x, PY2 weighted 2x, PY1 weighted 3x, current weighted 1x
### if week = 2
# PY2 weighted 2x, PY1 weighted 3x, current weighted 1x
### if week = 3
# PY2 weighted 2x, PY1 weighted 2x, current weighted 1x
### if week = 4
# PY2 weighted 1x, PY1 weighted 2x, current weighted 2x
### if week <= 6
# PY1 weighted 1x, current weighted 2x
### if week > 6
# current will be only data source used, everything weighted "1x" (aside from special variables)

## different stats weighted differently as described below
## EPA/PPA stats, explosiveness stats, success rates, havoc rates, Yards/Play, pts/scoring opp weighted 2x in PYs, 3x for current season,
# all #x above refer to weighting being done on top of weighting being done based on which year the data is from
## recruiting 3x in PY3 and PY2, 2x in PY1, 1x for current year
# recruiting phased out after only current season stats are being used (currently week 7)
## talent ranked 1x in PY3 and PY2, 3x in PY1
## FPI, SP+ (when available), and SRS only ranked 1x in all years no matter what
if (as.numeric(week) == 0) {
  ##### Week 0 Variable Ranks #####
  # PY3 weighted 1x, PY2 weighted 2x, PY1 weighted 3x
  ## PY3 ranks added first, weighted once
  VoA_Variables <- VoA_Variables |>
    mutate(Rank_Wins_PY3 = dense_rank(desc(Wins_PY3)),
           Rank_Losses_PY3 = dense_rank(Losses_PY3),
           Rank_Comp_Pct_PY3 = dense_rank(desc(completion_pct_PY3)),
           Rank_Pass_YPA_PY3 = dense_rank(desc(pass_ypa_PY3)),
           Rank_Pass_YPR_PY3 = dense_rank(desc(pass_ypr_PY3)),
           Rank_Int_Pct_PY3 = dense_rank(int_pct_PY3),
           Rank_Rush_YPC_PY3 = dense_rank(desc(rush_ypc_PY3)),
           Rank_Turnovers_pg_PY3 = dense_rank(turnovers_pg_PY3),
           Rank_Third_Conv_Rate_PY3 = dense_rank(desc(third_conv_rate_PY3)),
           Rank_Fourth_Conv_Rate_PY3 = dense_rank(desc(fourth_conv_rate_PY3)),
           Rank_Penalty_Yds_pg_PY3 = dense_rank(penalty_yds_pg_PY3),
           Rank_Yds_Per_Penalty_PY3 = dense_rank(yards_per_penalty_PY3),
           Rank_Kick_Return_Avg_PY3 = dense_rank(desc(kick_return_avg_PY3)),
           Rank_Punt_Return_Avg_PY3 = dense_rank(desc(punt_return_avg_PY3)),
           Rank_Total_Yds_pg_PY3 = dense_rank(desc(total_yds_pg_PY3)),
           Rank_Pass_Yds_pg_PY3 = dense_rank(desc(pass_yds_pg_PY3)),
           Rank_Rush_Yds_pg_PY3 = dense_rank(desc(rush_yds_pg_PY3)),
           Rank_First_Downs_pg_PY3 = dense_rank(desc(first_downs_pg_PY3)),
           Rank_Off_YPP_PY3 = dense_rank(desc(off_ypp_PY3)),
           Rank_Def_Ints_pg_PY3 = dense_rank(desc(def_interceptions_pg_PY3)),
           Rank_Off_PPA_PY3 = dense_rank(desc(off_ppa_PY3)),
           Rank_Off_Success_Rt_PY3 = dense_rank(desc(off_success_rate_PY3)),
           Rank_Off_Explosiveness_PY3 = dense_rank(desc(off_explosiveness_PY3)),
           Rank_Off_Pwr_Success_PY3 = dense_rank(desc(off_power_success_PY3)),
           Rank_Off_Stuff_Rt_PY3 = dense_rank(off_stuff_rate_PY3),
           Rank_Off_Line_Yds_PY3 = dense_rank(desc(off_line_yds_PY3)),
           Rank_Off_Second_Lvl_Yds_PY3 = dense_rank(desc(off_second_lvl_yds_PY3)),
           Rank_Off_Open_Field_Yds_PY3 = dense_rank(desc(off_open_field_yds_PY3)),
           Rank_Off_Pts_Per_Opp_PY3 = dense_rank(desc(off_pts_per_opp_PY3)),
           Rank_Off_Field_Pos_Avg_Predicted_Pts_PY3 = dense_rank(desc(off_field_pos_avg_predicted_points_PY3)),
           Rank_Off_Havoc_Total_PY3 = dense_rank(off_havoc_total_PY3),
           Rank_Off_Havoc_Front_PY3 = dense_rank(off_havoc_front_seven_PY3),
           Rank_Off_Havoc_DB_PY3 = dense_rank(off_havoc_db_PY3),
           Rank_Off_Standard_Down_PPA_PY3 = dense_rank(desc(off_standard_downs_ppa_PY3)),
           Rank_Off_Standard_Down_Success_Rt_PY3 = dense_rank(desc(off_standard_downs_success_rate_PY3)),
           Rank_Off_Standard_Down_Explosiveness_PY3 = dense_rank(desc(off_standard_downs_explosiveness_PY3)),
           Rank_Off_Pass_Down_PPA_PY3 = dense_rank(desc(off_passing_downs_ppa_PY3)),
           Rank_Off_Pass_Down_Success_Rt_PY3 = dense_rank(desc(off_passing_downs_success_rate_PY3)),
           Rank_Off_Pass_Down_Explosiveness_PY3 = dense_rank(desc(off_passing_downs_explosiveness_PY3)),
           Rank_Off_Rush_Play_PPA_PY3 = dense_rank(desc(off_rushing_plays_ppa_PY3)),
           Rank_Off_Rush_Play_Success_Rt_PY3 = dense_rank(desc(off_rushing_plays_success_rate_PY3)),
           Rank_Off_Rush_Play_Explosiveness_PY3 = dense_rank(desc(off_rushing_plays_explosiveness_PY3)),
           Rank_Off_Pass_Play_PPA_PY3 = dense_rank(desc(off_passing_plays_ppa_PY3)),
           Rank_Off_Pass_Play_Success_Rt_PY3 = dense_rank(desc(off_passing_plays_success_rate_PY3)),
           Rank_Off_Pass_Play_Explosiveness_PY3 = dense_rank(desc(off_passing_plays_explosiveness_PY3)),
           Rank_Def_PPA_PY3 = dense_rank(def_ppa_PY3),
           Rank_Def_Success_Rt_PY3 = dense_rank(def_success_rate_PY3),
           Rank_Def_Explosiveness_PY3 = dense_rank(def_explosiveness_PY3),
           Rank_Def_Pwr_Success_PY3 = dense_rank(def_power_success_PY3),
           Rank_Def_Stuff_Rt_PY3 = dense_rank(desc(def_stuff_rate_PY3)),
           Rank_Def_Line_Yds_PY3 = dense_rank(def_line_yds_PY3),
           Rank_Def_Second_Lvl_Yds_PY3 = dense_rank(def_second_lvl_yds_PY3),
           Rank_Def_Open_Field_Yds_PY3 = dense_rank(def_open_field_yds_PY3),
           Rank_Def_Pts_Per_Opp_PY3 = dense_rank(def_pts_per_opp_PY3),
           Rank_Def_Field_Pos_Avg_Predicted_Pts_PY3 = dense_rank(def_field_pos_avg_predicted_points_PY3),
           Rank_Def_Havoc_Total_PY3 = dense_rank(desc(def_havoc_total_PY3)),
           Rank_Def_Havoc_Front_Seven_PY3 = dense_rank(desc(def_havoc_front_seven_PY3)),
           Rank_Def_Havoc_DB_PY3 = dense_rank(desc(def_havoc_db_PY3)),
           Rank_Def_Standard_Down_PPA_PY3 = dense_rank(def_standard_downs_ppa_PY3),
           Rank_Def_Standard_Down_Success_Rt_PY3 = dense_rank(def_standard_downs_success_rate_PY3),
           Rank_Def_Standard_Down_Explosiveness_PY3 = dense_rank(def_standard_downs_explosiveness_PY3),
           Rank_Def_Pass_Down_PPA_PY3 = dense_rank(def_passing_downs_ppa_PY3),
           Rank_Def_Pass_Down_Success_Rt_PY3 = dense_rank(def_passing_downs_success_rate_PY3),
           Rank_Def_Pass_Down_Explosiveness_PY3 = dense_rank(def_passing_downs_explosiveness_PY3),
           Rank_Def_Rush_Play_PPA_PY3 = dense_rank(def_rushing_plays_ppa_PY3),
           Rank_Def_Rush_Play_Success_Rt_PY3 = dense_rank(def_rushing_plays_success_rate_PY3),
           Rank_Def_Rush_Play_Explosiveness_PY3 = dense_rank(def_rushing_plays_explosiveness_PY3),
           Rank_Def_Pass_Play_PPA_PY3 = dense_rank(def_passing_plays_ppa_PY3),
           Rank_Def_Pass_Play_Success_Rt_PY3 = dense_rank(def_passing_plays_success_rate_PY3),
           Rank_Def_Pass_Play_Explosiveness_PY3 = dense_rank(def_passing_plays_explosiveness_PY3),
           Rank_Recruit_Pts_PY3 = dense_rank(desc(recruit_pts_PY3)),
           Rank_Talent_PY3 = dense_rank(desc(talent_PY3)),
           Rank_SP_Rating_PY3 = dense_rank(desc(sp_rating_PY3)),
           Rank_SP_Off_Rating_PY3 = dense_rank(desc(sp_offense_rating_PY3)),
           Rank_SP_Def_Rating_PY3 = dense_rank(sp_defense_rating_PY3),
           Rank_SP_SpecialTeams_Rating_PY3 = dense_rank(desc(sp_special_teams_rating_PY3)),
           Rank_FPI_PY3 = dense_rank(desc(FPI_PY3)),
           Rank_SRS_rating_PY3 = dense_rank(desc(SRS_rating_PY3)),
           ## Extra weighted variables (2x for Previous Year data, 3x for PY3 recruiting)
           Rank_Off_YPP_PY3_col2 = dense_rank(desc(off_ypp_PY3)),
           Rank_Off_PPA_PY3_col2 = dense_rank(desc(off_ppa_PY3)),
           Rank_Off_Success_Rt_PY3_col2 = dense_rank(desc(off_success_rate_PY3)),
           Rank_Off_Explosiveness_PY3_col2 = dense_rank(desc(off_explosiveness_PY3)),
           Rank_Off_Pwr_Success_PY3_col2 = dense_rank(desc(off_power_success_PY3)),
           Rank_Off_Stuff_Rt_PY3_col2 = dense_rank(off_stuff_rate_PY3),
           Rank_Off_Pts_Per_Opp_PY3_col2 = dense_rank(desc(off_pts_per_opp_PY3)),
           Rank_Off_Havoc_Total_PY3_col2 = dense_rank(off_havoc_total_PY3),
           Rank_Off_Havoc_Front_PY3_col2 = dense_rank(off_havoc_front_seven_PY3),
           Rank_Off_Havoc_DB_PY3_col2 = dense_rank(off_havoc_db_PY3),
           Rank_Off_Standard_Down_PPA_PY3_col2 = dense_rank(desc(off_standard_downs_ppa_PY3)),
           Rank_Off_Standard_Down_Success_Rt_PY3_col2 = dense_rank(desc(off_standard_downs_success_rate_PY3)),
           Rank_Off_Standard_Down_Explosiveness_PY3_col2 = dense_rank(desc(off_standard_downs_explosiveness_PY3)),
           Rank_Off_Pass_Down_PPA_PY3_col2 = dense_rank(desc(off_passing_downs_ppa_PY3)),
           Rank_Off_Pass_Down_Success_Rt_PY3_col2 = dense_rank(desc(off_passing_downs_success_rate_PY3)),
           Rank_Off_Pass_Down_Explosiveness_PY3_col2 = dense_rank(desc(off_passing_downs_explosiveness_PY3)),
           Rank_Off_Rush_Play_PPA_PY3_col2 = dense_rank(desc(off_rushing_plays_ppa_PY3)),
           Rank_Off_Rush_Play_Success_Rt_PY3_col2 = dense_rank(desc(off_rushing_plays_success_rate_PY3)),
           Rank_Off_Rush_Play_Explosiveness_PY3_col2 = dense_rank(desc(off_rushing_plays_explosiveness_PY3)),
           Rank_Off_Pass_Play_PPA_PY3_col2 = dense_rank(desc(off_passing_plays_ppa_PY3)),
           Rank_Off_Pass_Play_Success_Rt_PY3_col2 = dense_rank(desc(off_passing_plays_success_rate_PY3)),
           Rank_Off_Pass_Play_Explosiveness_PY3_col2 = dense_rank(desc(off_passing_plays_explosiveness_PY3)),
           Rank_Def_PPA_PY3_col2 = dense_rank(def_ppa_PY3),
           Rank_Def_Success_Rt_PY3_col2 = dense_rank(def_success_rate_PY3),
           Rank_Def_Explosiveness_PY3_col2 = dense_rank(def_explosiveness_PY3),
           Rank_Def_Pwr_Success_PY3_col2 = dense_rank(def_power_success_PY3),
           Rank_Def_Stuff_Rt_PY3_col2 = dense_rank(desc(def_stuff_rate_PY3)),
           Rank_Def_Pts_Per_Opp_PY3_col2 = dense_rank(def_pts_per_opp_PY3),
           Rank_Def_Havoc_Total_PY3_col2 = dense_rank(desc(def_havoc_total_PY3)),
           Rank_Def_Havoc_Front_Seven_PY3_col2 = dense_rank(desc(def_havoc_front_seven_PY3)),
           Rank_Def_Havoc_DB_PY3_col2 = dense_rank(desc(def_havoc_db_PY3)),
           Rank_Def_Standard_Down_PPA_PY3_col2 = dense_rank(def_standard_downs_ppa_PY3),
           Rank_Def_Standard_Down_Success_Rt_PY3_col2 = dense_rank(def_standard_downs_success_rate_PY3),
           Rank_Def_Standard_Down_Explosiveness_PY3_col2 = dense_rank(def_standard_downs_explosiveness_PY3),
           Rank_Def_Pass_Down_PPA_PY3_col2 = dense_rank(def_passing_downs_ppa_PY3),
           Rank_Def_Pass_Down_Success_Rt_PY3_col2 = dense_rank(def_passing_downs_success_rate_PY3),
           Rank_Def_Pass_Down_Explosiveness_PY3_col2 = dense_rank(def_passing_downs_explosiveness_PY3),
           Rank_Def_Rush_Play_PPA_PY3_col2 = dense_rank(def_rushing_plays_ppa_PY3),
           Rank_Def_Rush_Play_Success_Rt_PY3_col2 = dense_rank(def_rushing_plays_success_rate_PY3),
           Rank_Def_Rush_Play_Explosiveness_PY3_col2 = dense_rank(def_rushing_plays_explosiveness_PY3),
           Rank_Def_Pass_Play_PPA_PY3_col2 = dense_rank(def_passing_plays_ppa_PY3),
           Rank_Def_Pass_Play_Success_Rt_PY3_col2 = dense_rank(def_passing_plays_success_rate_PY3),
           Rank_Def_Pass_Play_Explosiveness_PY3_col2 = dense_rank(def_passing_plays_explosiveness_PY3),
           Rank_Recruit_Pts_PY3_col2 = dense_rank(desc(recruit_pts_PY3)),
           Rank_Recruit_Pts_PY3_col3 = dense_rank(desc(recruit_pts_PY3)),
           ## PY2 ranks
           Rank_Wins_PY2 = dense_rank(desc(Wins_PY2)),
           Rank_Losses_PY2 = dense_rank(Losses_PY2),
           Rank_Comp_Pct_PY2 = dense_rank(desc(completion_pct_PY2)),
           Rank_Pass_YPA_PY2 = dense_rank(desc(pass_ypa_PY2)),
           Rank_Pass_YPR_PY2 = dense_rank(desc(pass_ypr_PY2)),
           Rank_Int_Pct_PY2 = dense_rank(int_pct_PY2),
           Rank_Rush_YPC_PY2 = dense_rank(desc(rush_ypc_PY2)),
           Rank_Turnovers_pg_PY2 = dense_rank(turnovers_pg_PY2),
           Rank_Third_Conv_Rate_PY2 = dense_rank(desc(third_conv_rate_PY2)),
           Rank_Fourth_Conv_Rate_PY2 = dense_rank(desc(fourth_conv_rate_PY2)),
           Rank_Penalty_Yds_pg_PY2 = dense_rank(penalty_yds_pg_PY2),
           Rank_Yds_Per_Penalty_PY2 = dense_rank(yards_per_penalty_PY2),
           Rank_Kick_Return_Avg_PY2 = dense_rank(desc(kick_return_avg_PY2)),
           Rank_Punt_Return_Avg_PY2 = dense_rank(desc(punt_return_avg_PY2)),
           Rank_Total_Yds_pg_PY2 = dense_rank(desc(total_yds_pg_PY2)),
           Rank_Pass_Yds_pg_PY2 = dense_rank(desc(pass_yds_pg_PY2)),
           Rank_Rush_Yds_pg_PY2 = dense_rank(desc(rush_yds_pg_PY2)),
           Rank_First_Downs_pg_PY2 = dense_rank(desc(first_downs_pg_PY2)),
           Rank_Off_YPP_PY2 = dense_rank(desc(off_ypp_PY2)),
           Rank_Def_Ints_pg_PY2 = dense_rank(desc(def_interceptions_pg_PY2)),
           Rank_Off_PPA_PY2 = dense_rank(desc(off_ppa_PY2)),
           Rank_Off_Success_Rt_PY2 = dense_rank(desc(off_success_rate_PY2)),
           Rank_Off_Explosiveness_PY2 = dense_rank(desc(off_explosiveness_PY2)),
           Rank_Off_Pwr_Success_PY2 = dense_rank(desc(off_power_success_PY2)),
           Rank_Off_Stuff_Rt_PY2 = dense_rank(off_stuff_rate_PY2),
           Rank_Off_Line_Yds_PY2 = dense_rank(desc(off_line_yds_PY2)),
           Rank_Off_Second_Lvl_Yds_PY2 = dense_rank(desc(off_second_lvl_yds_PY2)),
           Rank_Off_Open_Field_Yds_PY2 = dense_rank(desc(off_open_field_yds_PY2)),
           Rank_Off_Pts_Per_Opp_PY2 = dense_rank(desc(off_pts_per_opp_PY2)),
           Rank_Off_Field_Pos_Avg_Predicted_Pts_PY2 = dense_rank(desc(off_field_pos_avg_predicted_points_PY2)),
           Rank_Off_Havoc_Total_PY2 = dense_rank(off_havoc_total_PY2),
           Rank_Off_Havoc_Front_PY2 = dense_rank(off_havoc_front_seven_PY2),
           Rank_Off_Havoc_DB_PY2 = dense_rank(off_havoc_db_PY2),
           Rank_Off_Standard_Down_PPA_PY2 = dense_rank(desc(off_standard_downs_ppa_PY2)),
           Rank_Off_Standard_Down_Success_Rt_PY2 = dense_rank(desc(off_standard_downs_success_rate_PY2)),
           Rank_Off_Standard_Down_Explosiveness_PY2 = dense_rank(desc(off_standard_downs_explosiveness_PY2)),
           Rank_Off_Pass_Down_PPA_PY2 = dense_rank(desc(off_passing_downs_ppa_PY2)),
           Rank_Off_Pass_Down_Success_Rt_PY2 = dense_rank(desc(off_passing_downs_success_rate_PY2)),
           Rank_Off_Pass_Down_Explosiveness_PY2 = dense_rank(desc(off_passing_downs_explosiveness_PY2)),
           Rank_Off_Rush_Play_PPA_PY2 = dense_rank(desc(off_rushing_plays_ppa_PY2)),
           Rank_Off_Rush_Play_Success_Rt_PY2 = dense_rank(desc(off_rushing_plays_success_rate_PY2)),
           Rank_Off_Rush_Play_Explosiveness_PY2 = dense_rank(desc(off_rushing_plays_explosiveness_PY2)),
           Rank_Off_Pass_Play_PPA_PY2 = dense_rank(desc(off_passing_plays_ppa_PY2)),
           Rank_Off_Pass_Play_Success_Rt_PY2 = dense_rank(desc(off_passing_plays_success_rate_PY2)),
           Rank_Off_Pass_Play_Explosiveness_PY2 = dense_rank(desc(off_passing_plays_explosiveness_PY2)),
           Rank_Def_PPA_PY2 = dense_rank(def_ppa_PY2),
           Rank_Def_Success_Rt_PY2 = dense_rank(def_success_rate_PY2),
           Rank_Def_Explosiveness_PY2 = dense_rank(def_explosiveness_PY2),
           Rank_Def_Pwr_Success_PY2 = dense_rank(def_power_success_PY2),
           Rank_Def_Stuff_Rt_PY2 = dense_rank(desc(def_stuff_rate_PY2)),
           Rank_Def_Line_Yds_PY2 = dense_rank(def_line_yds_PY2),
           Rank_Def_Second_Lvl_Yds_PY2 = dense_rank(def_second_lvl_yds_PY2),
           Rank_Def_Open_Field_Yds_PY2 = dense_rank(def_open_field_yds_PY2),
           Rank_Def_Pts_Per_Opp_PY2 = dense_rank(def_pts_per_opp_PY2),
           Rank_Def_Field_Pos_Avg_Predicted_Pts_PY2 = dense_rank(def_field_pos_avg_predicted_points_PY2),
           Rank_Def_Havoc_Total_PY2 = dense_rank(desc(def_havoc_total_PY2)),
           Rank_Def_Havoc_Front_Seven_PY2 = dense_rank(desc(def_havoc_front_seven_PY2)),
           Rank_Def_Havoc_DB_PY2 = dense_rank(desc(def_havoc_db_PY2)),
           Rank_Def_Standard_Down_PPA_PY2 = dense_rank(def_standard_downs_ppa_PY2),
           Rank_Def_Standard_Down_Success_Rt_PY2 = dense_rank(def_standard_downs_success_rate_PY2),
           Rank_Def_Standard_Down_Explosiveness_PY2 = dense_rank(def_standard_downs_explosiveness_PY2),
           Rank_Def_Pass_Down_PPA_PY2 = dense_rank(def_passing_downs_ppa_PY2),
           Rank_Def_Pass_Down_Success_Rt_PY2 = dense_rank(def_passing_downs_success_rate_PY2),
           Rank_Def_Pass_Down_Explosiveness_PY2 = dense_rank(def_passing_downs_explosiveness_PY2),
           Rank_Def_Rush_Play_PPA_PY2 = dense_rank(def_rushing_plays_ppa_PY2),
           Rank_Def_Rush_Play_Success_Rt_PY2 = dense_rank(def_rushing_plays_success_rate_PY2),
           Rank_Def_Rush_Play_Explosiveness_PY2 = dense_rank(def_rushing_plays_explosiveness_PY2),
           Rank_Def_Pass_Play_PPA_PY2 = dense_rank(def_passing_plays_ppa_PY2),
           Rank_Def_Pass_Play_Success_Rt_PY2 = dense_rank(def_passing_plays_success_rate_PY2),
           Rank_Def_Pass_Play_Explosiveness_PY2 = dense_rank(def_passing_plays_explosiveness_PY2),
           Rank_Recruit_Pts_PY2 = dense_rank(desc(recruit_pts_PY2)),
           Rank_Talent_PY2 = dense_rank(desc(talent_PY2)),
           Rank_SP_Rating_PY2 = dense_rank(desc(sp_rating_PY2)),
           Rank_SP_Off_Rating_PY2 = dense_rank(desc(sp_offense_rating_PY2)),
           Rank_SP_Def_Rating_PY2 = dense_rank(sp_defense_rating_PY2),
           Rank_SP_SpecialTeams_Rating_PY2 = dense_rank(desc(sp_special_teams_rating_PY2)),
           Rank_FPI_PY2 = dense_rank(desc(FPI_PY2)),
           Rank_SRS_rating_PY2 = dense_rank(desc(SRS_rating_PY2)),
           ## PY2 weighted twice
           Rank_Wins_PY2_col2 = dense_rank(desc(Wins_PY2)),
           Rank_Losses_PY2_col2 = dense_rank(Losses_PY2),
           Rank_Comp_Pct_PY2_col2 = dense_rank(desc(completion_pct_PY2)),
           Rank_Pass_YPA_PY2_col2 = dense_rank(desc(pass_ypa_PY2)),
           Rank_Pass_YPR_PY2_col2 = dense_rank(desc(pass_ypr_PY2)),
           Rank_Int_Pct_PY2_col2 = dense_rank(int_pct_PY2),
           Rank_Rush_YPC_PY2_col2 = dense_rank(desc(rush_ypc_PY2)),
           Rank_Turnovers_pg_PY2_col2 = dense_rank(turnovers_pg_PY2),
           Rank_Third_Conv_Rate_PY2_col2 = dense_rank(desc(third_conv_rate_PY2)),
           Rank_Fourth_Conv_Rate_PY2_col2 = dense_rank(desc(fourth_conv_rate_PY2)),
           Rank_Penalty_Yds_pg_PY2_col2 = dense_rank(penalty_yds_pg_PY2),
           Rank_Yds_Per_Penalty_PY2_col2 = dense_rank(yards_per_penalty_PY2),
           Rank_Kick_Return_Avg_PY2_col2 = dense_rank(desc(kick_return_avg_PY2)),
           Rank_Punt_Return_Avg_PY2_col2 = dense_rank(desc(punt_return_avg_PY2)),
           Rank_Total_Yds_pg_PY2_col2 = dense_rank(desc(total_yds_pg_PY2)),
           Rank_Pass_Yds_pg_PY2_col2 = dense_rank(desc(pass_yds_pg_PY2)),
           Rank_Rush_Yds_pg_PY2_col2 = dense_rank(desc(rush_yds_pg_PY2)),
           Rank_First_Downs_pg_PY2_col2 = dense_rank(desc(first_downs_pg_PY2)),
           Rank_Off_YPP_PY2_col2 = dense_rank(desc(off_ypp_PY2)),
           Rank_Def_Ints_pg_PY2_col2 = dense_rank(desc(def_interceptions_pg_PY2)),
           Rank_Off_PPA_PY2_col2 = dense_rank(desc(off_ppa_PY2)),
           Rank_Off_Success_Rt_PY2_col2 = dense_rank(desc(off_success_rate_PY2)),
           Rank_Off_Explosiveness_PY2_col2 = dense_rank(desc(off_explosiveness_PY2)),
           Rank_Off_Pwr_Success_PY2_col2 = dense_rank(desc(off_power_success_PY2)),
           Rank_Off_Stuff_Rt_PY2_col2 = dense_rank(off_stuff_rate_PY2),
           Rank_Off_Line_Yds_PY2_col2 = dense_rank(desc(off_line_yds_PY2)),
           Rank_Off_Second_Lvl_Yds_PY2_col2 = dense_rank(desc(off_second_lvl_yds_PY2)),
           Rank_Off_Open_Field_Yds_PY2_col2 = dense_rank(desc(off_open_field_yds_PY2)),
           Rank_Off_Pts_Per_Opp_PY2_col2 = dense_rank(desc(off_pts_per_opp_PY2)),
           Rank_Off_Field_Pos_Avg_Predicted_Pts_PY2_col2 = dense_rank(desc(off_field_pos_avg_predicted_points_PY2)),
           Rank_Off_Havoc_Total_PY2_col2 = dense_rank(off_havoc_total_PY2),
           Rank_Off_Havoc_Front_PY2_col2 = dense_rank(off_havoc_front_seven_PY2),
           Rank_Off_Havoc_DB_PY2_col2 = dense_rank(off_havoc_db_PY2),
           Rank_Off_Standard_Down_PPA_PY2_col2 = dense_rank(desc(off_standard_downs_ppa_PY2)),
           Rank_Off_Standard_Down_Success_Rt_PY2_col2 = dense_rank(desc(off_standard_downs_success_rate_PY2)),
           Rank_Off_Standard_Down_Explosiveness_PY2_col2 = dense_rank(desc(off_standard_downs_explosiveness_PY2)),
           Rank_Off_Pass_Down_PPA_PY2_col2 = dense_rank(desc(off_passing_downs_ppa_PY2)),
           Rank_Off_Pass_Down_Success_Rt_PY2_col2 = dense_rank(desc(off_passing_downs_success_rate_PY2)),
           Rank_Off_Pass_Down_Explosiveness_PY2_col2 = dense_rank(desc(off_passing_downs_explosiveness_PY2)),
           Rank_Off_Rush_Play_PPA_PY2_col2 = dense_rank(desc(off_rushing_plays_ppa_PY2)),
           Rank_Off_Rush_Play_Success_Rt_PY2_col2 = dense_rank(desc(off_rushing_plays_success_rate_PY2)),
           Rank_Off_Rush_Play_Explosiveness_PY2_col2 = dense_rank(desc(off_rushing_plays_explosiveness_PY2)),
           Rank_Off_Pass_Play_PPA_PY2_col2 = dense_rank(desc(off_passing_plays_ppa_PY2)),
           Rank_Off_Pass_Play_Success_Rt_PY2_col2 = dense_rank(desc(off_passing_plays_success_rate_PY2)),
           Rank_Off_Pass_Play_Explosiveness_PY2_col2 = dense_rank(desc(off_passing_plays_explosiveness_PY2)),
           Rank_Def_PPA_PY2_col2 = dense_rank(def_ppa_PY2),
           Rank_Def_Success_Rt_PY2_col2 = dense_rank(def_success_rate_PY2),
           Rank_Def_Explosiveness_PY2_col2 = dense_rank(def_explosiveness_PY2),
           Rank_Def_Pwr_Success_PY2_col2 = dense_rank(def_power_success_PY2),
           Rank_Def_Stuff_Rt_PY2_col2 = dense_rank(desc(def_stuff_rate_PY2)),
           Rank_Def_Line_Yds_PY2_col2 = dense_rank(def_line_yds_PY2),
           Rank_Def_Second_Lvl_Yds_PY2_col2 = dense_rank(def_second_lvl_yds_PY2),
           Rank_Def_Open_Field_Yds_PY2_col2 = dense_rank(def_open_field_yds_PY2),
           Rank_Def_Pts_Per_Opp_PY2_col2 = dense_rank(def_pts_per_opp_PY2),
           Rank_Def_Field_Pos_Avg_Predicted_Pts_PY2_col2 = dense_rank(def_field_pos_avg_predicted_points_PY2),
           Rank_Def_Havoc_Total_PY2_col2 = dense_rank(desc(def_havoc_total_PY2)),
           Rank_Def_Havoc_Front_Seven_PY2_col2 = dense_rank(desc(def_havoc_front_seven_PY2)),
           Rank_Def_Havoc_DB_PY2_col2 = dense_rank(desc(def_havoc_db_PY2)),
           Rank_Def_Standard_Down_PPA_PY2_col2 = dense_rank(def_standard_downs_ppa_PY2),
           Rank_Def_Standard_Down_Success_Rt_PY2_col2 = dense_rank(def_standard_downs_success_rate_PY2),
           Rank_Def_Standard_Down_Explosiveness_PY2_col2 = dense_rank(def_standard_downs_explosiveness_PY2),
           Rank_Def_Pass_Down_PPA_PY2_col2 = dense_rank(def_passing_downs_ppa_PY2),
           Rank_Def_Pass_Down_Success_Rt_PY2_col2 = dense_rank(def_passing_downs_success_rate_PY2),
           Rank_Def_Pass_Down_Explosiveness_PY2_col2 = dense_rank(def_passing_downs_explosiveness_PY2),
           Rank_Def_Rush_Play_PPA_PY2_col2 = dense_rank(def_rushing_plays_ppa_PY2),
           Rank_Def_Rush_Play_Success_Rt_PY2_col2 = dense_rank(def_rushing_plays_success_rate_PY2),
           Rank_Def_Rush_Play_Explosiveness_PY2_col2 = dense_rank(def_rushing_plays_explosiveness_PY2),
           Rank_Def_Pass_Play_PPA_PY2_col2 = dense_rank(def_passing_plays_ppa_PY2),
           Rank_Def_Pass_Play_Success_Rt_PY2_col2 = dense_rank(def_passing_plays_success_rate_PY2),
           Rank_Def_Pass_Play_Explosiveness_PY2_col2 = dense_rank(def_passing_plays_explosiveness_PY2),
           Rank_Recruit_Pts_PY2_col2 = dense_rank(desc(recruit_pts_PY2)),
           ## PY2 extra weighted variables
           Rank_Off_YPP_PY2_col3 = dense_rank(desc(off_ypp_PY2)),
           Rank_Off_PPA_PY2_col3 = dense_rank(desc(off_ppa_PY2)),
           Rank_Off_Success_Rt_PY2_col3 = dense_rank(desc(off_success_rate_PY2)),
           Rank_Off_Explosiveness_PY2_col3 = dense_rank(desc(off_explosiveness_PY2)),
           Rank_Off_Pwr_Success_PY2_col3 = dense_rank(desc(off_power_success_PY2)),
           Rank_Off_Stuff_Rt_PY2_col3 = dense_rank(off_stuff_rate_PY2),
           Rank_Off_Pts_Per_Opp_PY2_col3 = dense_rank(desc(off_pts_per_opp_PY2)),
           Rank_Off_Havoc_Total_PY2_col3 = dense_rank(off_havoc_total_PY2),
           Rank_Off_Havoc_Front_PY2_col3 = dense_rank(off_havoc_front_seven_PY2),
           Rank_Off_Havoc_DB_PY2_col3 = dense_rank(off_havoc_db_PY2),
           Rank_Off_Standard_Down_PPA_PY2_col3 = dense_rank(desc(off_standard_downs_ppa_PY2)),
           Rank_Off_Standard_Down_Success_Rt_PY2_col3 = dense_rank(desc(off_standard_downs_success_rate_PY2)),
           Rank_Off_Standard_Down_Explosiveness_PY2_col3 = dense_rank(desc(off_standard_downs_explosiveness_PY2)),
           Rank_Off_Pass_Down_PPA_PY2_col3 = dense_rank(desc(off_passing_downs_ppa_PY2)),
           Rank_Off_Pass_Down_Success_Rt_PY2_col3 = dense_rank(desc(off_passing_downs_success_rate_PY2)),
           Rank_Off_Pass_Down_Explosiveness_PY2_col3 = dense_rank(desc(off_passing_downs_explosiveness_PY2)),
           Rank_Off_Rush_Play_PPA_PY2_col3 = dense_rank(desc(off_rushing_plays_ppa_PY2)),
           Rank_Off_Rush_Play_Success_Rt_PY2_col3 = dense_rank(desc(off_rushing_plays_success_rate_PY2)),
           Rank_Off_Rush_Play_Explosiveness_PY2_col3 = dense_rank(desc(off_rushing_plays_explosiveness_PY2)),
           Rank_Off_Pass_Play_PPA_PY2_col3 = dense_rank(desc(off_passing_plays_ppa_PY2)),
           Rank_Off_Pass_Play_Success_Rt_PY2_col3 = dense_rank(desc(off_passing_plays_success_rate_PY2)),
           Rank_Off_Pass_Play_Explosiveness_PY2_col3 = dense_rank(desc(off_passing_plays_explosiveness_PY2)),
           Rank_Def_PPA_PY2_col3 = dense_rank(def_ppa_PY2),
           Rank_Def_Success_Rt_PY2_col3 = dense_rank(def_success_rate_PY2),
           Rank_Def_Explosiveness_PY2_col3 = dense_rank(def_explosiveness_PY2),
           Rank_Def_Pwr_Success_PY2_col3 = dense_rank(def_power_success_PY2),
           Rank_Def_Stuff_Rt_PY2_col3 = dense_rank(desc(def_stuff_rate_PY2)),
           Rank_Def_Pts_Per_Opp_PY2_col3 = dense_rank(def_pts_per_opp_PY2),
           Rank_Def_Havoc_Total_PY2_col3 = dense_rank(desc(def_havoc_total_PY2)),
           Rank_Def_Havoc_Front_Seven_PY2_col3 = dense_rank(desc(def_havoc_front_seven_PY2)),
           Rank_Def_Havoc_DB_PY2_col3 = dense_rank(desc(def_havoc_db_PY2)),
           Rank_Def_Standard_Down_PPA_PY2_col3 = dense_rank(def_standard_downs_ppa_PY2),
           Rank_Def_Standard_Down_Success_Rt_PY2_col3 = dense_rank(def_standard_downs_success_rate_PY2),
           Rank_Def_Standard_Down_Explosiveness_PY2_col3 = dense_rank(def_standard_downs_explosiveness_PY2),
           Rank_Def_Pass_Down_PPA_PY2_col3 = dense_rank(def_passing_downs_ppa_PY2),
           Rank_Def_Pass_Down_Success_Rt_PY2_col3 = dense_rank(def_passing_downs_success_rate_PY2),
           Rank_Def_Pass_Down_Explosiveness_PY2_col3 = dense_rank(def_passing_downs_explosiveness_PY2),
           Rank_Def_Rush_Play_PPA_PY2_col3 = dense_rank(def_rushing_plays_ppa_PY2),
           Rank_Def_Rush_Play_Success_Rt_PY2_col3 = dense_rank(def_rushing_plays_success_rate_PY2),
           Rank_Def_Rush_Play_Explosiveness_PY2_col3 = dense_rank(def_rushing_plays_explosiveness_PY2),
           Rank_Def_Pass_Play_PPA_PY2_col3 = dense_rank(def_passing_plays_ppa_PY2),
           Rank_Def_Pass_Play_Success_Rt_PY2_col3 = dense_rank(def_passing_plays_success_rate_PY2),
           Rank_Def_Pass_Play_Explosiveness_PY2_col3 = dense_rank(def_passing_plays_explosiveness_PY2),
           Rank_Recruit_Pts_PY2_col3 = dense_rank(desc(recruit_pts_PY2)),
           ## PY2 extra weighted variables (2x)
           Rank_Off_YPP_PY2_col4 = dense_rank(desc(off_ypp_PY2)),
           Rank_Off_PPA_PY2_col4 = dense_rank(desc(off_ppa_PY2)),
           Rank_Off_Success_Rt_PY2_col4 = dense_rank(desc(off_success_rate_PY2)),
           Rank_Off_Explosiveness_PY2_col4 = dense_rank(desc(off_explosiveness_PY2)),
           Rank_Off_Pwr_Success_PY2_col4 = dense_rank(desc(off_power_success_PY2)),
           Rank_Off_Stuff_Rt_PY2_col4 = dense_rank(off_stuff_rate_PY2),
           Rank_Off_Pts_Per_Opp_PY2_col4 = dense_rank(desc(off_pts_per_opp_PY2)),
           Rank_Off_Havoc_Total_PY2_col4 = dense_rank(off_havoc_total_PY2),
           Rank_Off_Havoc_Front_PY2_col4 = dense_rank(off_havoc_front_seven_PY2),
           Rank_Off_Havoc_DB_PY2_col4 = dense_rank(off_havoc_db_PY2),
           Rank_Off_Standard_Down_PPA_PY2_col4 = dense_rank(desc(off_standard_downs_ppa_PY2)),
           Rank_Off_Standard_Down_Success_Rt_PY2_col4 = dense_rank(desc(off_standard_downs_success_rate_PY2)),
           Rank_Off_Standard_Down_Explosiveness_PY2_col4 = dense_rank(desc(off_standard_downs_explosiveness_PY2)),
           Rank_Off_Pass_Down_PPA_PY2_col4 = dense_rank(desc(off_passing_downs_ppa_PY2)),
           Rank_Off_Pass_Down_Success_Rt_PY2_col4 = dense_rank(desc(off_passing_downs_success_rate_PY2)),
           Rank_Off_Pass_Down_Explosiveness_PY2_col4 = dense_rank(desc(off_passing_downs_explosiveness_PY2)),
           Rank_Off_Rush_Play_PPA_PY2_col4 = dense_rank(desc(off_rushing_plays_ppa_PY2)),
           Rank_Off_Rush_Play_Success_Rt_PY2_col4 = dense_rank(desc(off_rushing_plays_success_rate_PY2)),
           Rank_Off_Rush_Play_Explosiveness_PY2_col4 = dense_rank(desc(off_rushing_plays_explosiveness_PY2)),
           Rank_Off_Pass_Play_PPA_PY2_col4 = dense_rank(desc(off_passing_plays_ppa_PY2)),
           Rank_Off_Pass_Play_Success_Rt_PY2_col4 = dense_rank(desc(off_passing_plays_success_rate_PY2)),
           Rank_Off_Pass_Play_Explosiveness_PY2_col4 = dense_rank(desc(off_passing_plays_explosiveness_PY2)),
           Rank_Def_PPA_PY2_col4 = dense_rank(def_ppa_PY2),
           Rank_Def_Success_Rt_PY2_col4 = dense_rank(def_success_rate_PY2),
           Rank_Def_Explosiveness_PY2_col4 = dense_rank(def_explosiveness_PY2),
           Rank_Def_Pwr_Success_PY2_col4 = dense_rank(def_power_success_PY2),
           Rank_Def_Stuff_Rt_PY2_col4 = dense_rank(desc(def_stuff_rate_PY2)),
           Rank_Def_Pts_Per_Opp_PY2_col4 = dense_rank(def_pts_per_opp_PY2),
           Rank_Def_Havoc_Total_PY2_col4 = dense_rank(desc(def_havoc_total_PY2)),
           Rank_Def_Havoc_Front_Seven_PY2_col4 = dense_rank(desc(def_havoc_front_seven_PY2)),
           Rank_Def_Havoc_DB_PY2_col4 = dense_rank(desc(def_havoc_db_PY2)),
           Rank_Def_Standard_Down_PPA_PY2_col4 = dense_rank(def_standard_downs_ppa_PY2),
           Rank_Def_Standard_Down_Success_Rt_PY2_col4 = dense_rank(def_standard_downs_success_rate_PY2),
           Rank_Def_Standard_Down_Explosiveness_PY2_col4 = dense_rank(def_standard_downs_explosiveness_PY2),
           Rank_Def_Pass_Down_PPA_PY2_col4 = dense_rank(def_passing_downs_ppa_PY2),
           Rank_Def_Pass_Down_Success_Rt_PY2_col4 = dense_rank(def_passing_downs_success_rate_PY2),
           Rank_Def_Pass_Down_Explosiveness_PY2_col4 = dense_rank(def_passing_downs_explosiveness_PY2),
           Rank_Def_Rush_Play_PPA_PY2_col4 = dense_rank(def_rushing_plays_ppa_PY2),
           Rank_Def_Rush_Play_Success_Rt_PY2_col4 = dense_rank(def_rushing_plays_success_rate_PY2),
           Rank_Def_Rush_Play_Explosiveness_PY2_col4 = dense_rank(def_rushing_plays_explosiveness_PY2),
           Rank_Def_Pass_Play_PPA_PY2_col4 = dense_rank(def_passing_plays_ppa_PY2),
           Rank_Def_Pass_Play_Success_Rt_PY2_col4 = dense_rank(def_passing_plays_success_rate_PY2),
           Rank_Def_Pass_Play_Explosiveness_PY2_col4 = dense_rank(def_passing_plays_explosiveness_PY2),
           ## PY1 ranks
           Rank_Wins_PY1 = dense_rank(desc(Wins_PY1)),
           Rank_Losses_PY1 = dense_rank(Losses_PY1),
           Rank_Comp_Pct_PY1 = dense_rank(desc(completion_pct_PY1)),
           Rank_Pass_YPA_PY1 = dense_rank(desc(pass_ypa_PY1)),
           Rank_Pass_YPR_PY1 = dense_rank(desc(pass_ypr_PY1)),
           Rank_Int_Pct_PY1 = dense_rank(int_pct_PY1),
           Rank_Rush_YPC_PY1 = dense_rank(desc(rush_ypc_PY1)),
           Rank_Turnovers_pg_PY1 = dense_rank(turnovers_pg_PY1),
           Rank_Third_Conv_Rate_PY1 = dense_rank(desc(third_conv_rate_PY1)),
           Rank_Fourth_Conv_Rate_PY1 = dense_rank(desc(fourth_conv_rate_PY1)),
           Rank_Penalty_Yds_pg_PY1 = dense_rank(penalty_yds_pg_PY1),
           Rank_Yds_Per_Penalty_PY1 = dense_rank(yards_per_penalty_PY1),
           Rank_Kick_Return_Avg_PY1 = dense_rank(desc(kick_return_avg_PY1)),
           Rank_Punt_Return_Avg_PY1 = dense_rank(desc(punt_return_avg_PY1)),
           Rank_Total_Yds_pg_PY1 = dense_rank(desc(total_yds_pg_PY1)),
           Rank_Pass_Yds_pg_PY1 = dense_rank(desc(pass_yds_pg_PY1)),
           Rank_Rush_Yds_pg_PY1 = dense_rank(desc(rush_yds_pg_PY1)),
           Rank_First_Downs_pg_PY1 = dense_rank(desc(first_downs_pg_PY1)),
           Rank_Off_YPP_PY1 = dense_rank(desc(off_ypp_PY1)),
           Rank_Def_Ints_pg_PY1 = dense_rank(desc(def_interceptions_pg_PY1)),
           Rank_Off_PPA_PY1 = dense_rank(desc(off_ppa_PY1)),
           Rank_Off_Success_Rt_PY1 = dense_rank(desc(off_success_rate_PY1)),
           Rank_Off_Explosiveness_PY1 = dense_rank(desc(off_explosiveness_PY1)),
           Rank_Off_Pwr_Success_PY1 = dense_rank(desc(off_power_success_PY1)),
           Rank_Off_Stuff_Rt_PY1 = dense_rank(off_stuff_rate_PY1),
           Rank_Off_Line_Yds_PY1 = dense_rank(desc(off_line_yds_PY1)),
           Rank_Off_Second_Lvl_Yds_PY1 = dense_rank(desc(off_second_lvl_yds_PY1)),
           Rank_Off_Open_Field_Yds_PY1 = dense_rank(desc(off_open_field_yds_PY1)),
           Rank_Off_Pts_Per_Opp_PY1 = dense_rank(desc(off_pts_per_opp_PY1)),
           Rank_Off_Field_Pos_Avg_Predicted_Pts_PY1 = dense_rank(desc(off_field_pos_avg_predicted_points_PY1)),
           Rank_Off_Havoc_Total_PY1 = dense_rank(off_havoc_total_PY1),
           Rank_Off_Havoc_Front_PY1 = dense_rank(off_havoc_front_seven_PY1),
           Rank_Off_Havoc_DB_PY1 = dense_rank(off_havoc_db_PY1),
           Rank_Off_Standard_Down_PPA_PY1 = dense_rank(desc(off_standard_downs_ppa_PY1)),
           Rank_Off_Standard_Down_Success_Rt_PY1 = dense_rank(desc(off_standard_downs_success_rate_PY1)),
           Rank_Off_Standard_Down_Explosiveness_PY1 = dense_rank(desc(off_standard_downs_explosiveness_PY1)),
           Rank_Off_Pass_Down_PPA_PY1 = dense_rank(desc(off_passing_downs_ppa_PY1)),
           Rank_Off_Pass_Down_Success_Rt_PY1 = dense_rank(desc(off_passing_downs_success_rate_PY1)),
           Rank_Off_Pass_Down_Explosiveness_PY1 = dense_rank(desc(off_passing_downs_explosiveness_PY1)),
           Rank_Off_Rush_Play_PPA_PY1 = dense_rank(desc(off_rushing_plays_ppa_PY1)),
           Rank_Off_Rush_Play_Success_Rt_PY1 = dense_rank(desc(off_rushing_plays_success_rate_PY1)),
           Rank_Off_Rush_Play_Explosiveness_PY1 = dense_rank(desc(off_rushing_plays_explosiveness_PY1)),
           Rank_Off_Pass_Play_PPA_PY1 = dense_rank(desc(off_passing_plays_ppa_PY1)),
           Rank_Off_Pass_Play_Success_Rt_PY1 = dense_rank(desc(off_passing_plays_success_rate_PY1)),
           Rank_Off_Pass_Play_Explosiveness_PY1 = dense_rank(desc(off_passing_plays_explosiveness_PY1)),
           Rank_Def_PPA_PY1 = dense_rank(def_ppa_PY1),
           Rank_Def_Success_Rt_PY1 = dense_rank(def_success_rate_PY1),
           Rank_Def_Explosiveness_PY1 = dense_rank(def_explosiveness_PY1),
           Rank_Def_Pwr_Success_PY1 = dense_rank(def_power_success_PY1),
           Rank_Def_Stuff_Rt_PY1 = dense_rank(desc(def_stuff_rate_PY1)),
           Rank_Def_Line_Yds_PY1 = dense_rank(def_line_yds_PY1),
           Rank_Def_Second_Lvl_Yds_PY1 = dense_rank(def_second_lvl_yds_PY1),
           Rank_Def_Open_Field_Yds_PY1 = dense_rank(def_open_field_yds_PY1),
           Rank_Def_Pts_Per_Opp_PY1 = dense_rank(def_pts_per_opp_PY1),
           Rank_Def_Field_Pos_Avg_Predicted_Pts_PY1 = dense_rank(def_field_pos_avg_predicted_points_PY1),
           Rank_Def_Havoc_Total_PY1 = dense_rank(desc(def_havoc_total_PY1)),
           Rank_Def_Havoc_Front_Seven_PY1 = dense_rank(desc(def_havoc_front_seven_PY1)),
           Rank_Def_Havoc_DB_PY1 = dense_rank(desc(def_havoc_db_PY1)),
           Rank_Def_Standard_Down_PPA_PY1 = dense_rank(def_standard_downs_ppa_PY1),
           Rank_Def_Standard_Down_Success_Rt_PY1 = dense_rank(def_standard_downs_success_rate_PY1),
           Rank_Def_Standard_Down_Explosiveness_PY1 = dense_rank(def_standard_downs_explosiveness_PY1),
           Rank_Def_Pass_Down_PPA_PY1 = dense_rank(def_passing_downs_ppa_PY1),
           Rank_Def_Pass_Down_Success_Rt_PY1 = dense_rank(def_passing_downs_success_rate_PY1),
           Rank_Def_Pass_Down_Explosiveness_PY1 = dense_rank(def_passing_downs_explosiveness_PY1),
           Rank_Def_Rush_Play_PPA_PY1 = dense_rank(def_rushing_plays_ppa_PY1),
           Rank_Def_Rush_Play_Success_Rt_PY1 = dense_rank(def_rushing_plays_success_rate_PY1),
           Rank_Def_Rush_Play_Explosiveness_PY1 = dense_rank(def_rushing_plays_explosiveness_PY1),
           Rank_Def_Pass_Play_PPA_PY1 = dense_rank(def_passing_plays_ppa_PY1),
           Rank_Def_Pass_Play_Success_Rt_PY1 = dense_rank(def_passing_plays_success_rate_PY1),
           Rank_Def_Pass_Play_Explosiveness_PY1 = dense_rank(def_passing_plays_explosiveness_PY1),
           Rank_Recruit_Pts_PY1 = dense_rank(desc(recruit_pts_PY1)),
           Rank_Talent_PY1 = dense_rank(desc(talent_PY1)),
           Rank_SP_Rating_PY1 = dense_rank(desc(sp_rating_PY1)),
           Rank_SP_Off_Rating_PY1 = dense_rank(desc(sp_offense_rating_PY1)),
           Rank_SP_Def_Rating_PY1 = dense_rank(sp_defense_rating_PY1),
           Rank_SP_SpecialTeams_Rating_PY1 = dense_rank(desc(sp_special_teams_rating_PY1)),
           Rank_FPI_PY1 = dense_rank(desc(FPI_PY1)),
           Rank_SRS_rating_PY1 = dense_rank(desc(SRS_rating_PY1)),
           ## PY1 weighted 3 times
           Rank_Wins_PY1_col2 = dense_rank(desc(Wins_PY1)),
           Rank_Losses_PY1_col2 = dense_rank(Losses_PY1),
           Rank_Comp_Pct_PY1_col2 = dense_rank(desc(completion_pct_PY1)),
           Rank_Pass_YPA_PY1_col2 = dense_rank(desc(pass_ypa_PY1)),
           Rank_Pass_YPR_PY1_col2 = dense_rank(desc(pass_ypr_PY1)),
           Rank_Int_Pct_PY1_col2 = dense_rank(int_pct_PY1),
           Rank_Rush_YPC_PY1_col2 = dense_rank(desc(rush_ypc_PY1)),
           Rank_Turnovers_pg_PY1_col2 = dense_rank(turnovers_pg_PY1),
           Rank_Third_Conv_Rate_PY1_col2 = dense_rank(desc(third_conv_rate_PY1)),
           Rank_Fourth_Conv_Rate_PY1_col2 = dense_rank(desc(fourth_conv_rate_PY1)),
           Rank_Penalty_Yds_pg_PY1_col2 = dense_rank(penalty_yds_pg_PY1),
           Rank_Yds_Per_Penalty_PY1_col2 = dense_rank(yards_per_penalty_PY1),
           Rank_Kick_Return_Avg_PY1_col2 = dense_rank(desc(kick_return_avg_PY1)),
           Rank_Punt_Return_Avg_PY1_col2 = dense_rank(desc(punt_return_avg_PY1)),
           Rank_Total_Yds_pg_PY1_col2 = dense_rank(desc(total_yds_pg_PY1)),
           Rank_Pass_Yds_pg_PY1_col2 = dense_rank(desc(pass_yds_pg_PY1)),
           Rank_Rush_Yds_pg_PY1_col2 = dense_rank(desc(rush_yds_pg_PY1)),
           Rank_First_Downs_pg_PY1_col2 = dense_rank(desc(first_downs_pg_PY1)),
           Rank_Off_YPP_PY1_col2 = dense_rank(desc(off_ypp_PY1)),
           Rank_Def_Ints_pg_PY1_col2 = dense_rank(desc(def_interceptions_pg_PY1)),
           Rank_Off_PPA_PY1_col2 = dense_rank(desc(off_ppa_PY1)),
           Rank_Off_Success_Rt_PY1_col2 = dense_rank(desc(off_success_rate_PY1)),
           Rank_Off_Explosiveness_PY1_col2 = dense_rank(desc(off_explosiveness_PY1)),
           Rank_Off_Pwr_Success_PY1_col2 = dense_rank(desc(off_power_success_PY1)),
           Rank_Off_Stuff_Rt_PY1_col2 = dense_rank(off_stuff_rate_PY1),
           Rank_Off_Line_Yds_PY1_col2 = dense_rank(desc(off_line_yds_PY1)),
           Rank_Off_Second_Lvl_Yds_PY1_col2 = dense_rank(desc(off_second_lvl_yds_PY1)),
           Rank_Off_Open_Field_Yds_PY1_col2 = dense_rank(desc(off_open_field_yds_PY1)),
           Rank_Off_Pts_Per_Opp_PY1_col2 = dense_rank(desc(off_pts_per_opp_PY1)),
           Rank_Off_Field_Pos_Avg_Predicted_Pts_PY1_col2 = dense_rank(desc(off_field_pos_avg_predicted_points_PY1)),
           Rank_Off_Havoc_Total_PY1_col2 = dense_rank(off_havoc_total_PY1),
           Rank_Off_Havoc_Front_PY1_col2 = dense_rank(off_havoc_front_seven_PY1),
           Rank_Off_Havoc_DB_PY1_col2 = dense_rank(off_havoc_db_PY1),
           Rank_Off_Standard_Down_PPA_PY1_col2 = dense_rank(desc(off_standard_downs_ppa_PY1)),
           Rank_Off_Standard_Down_Success_Rt_PY1_col2 = dense_rank(desc(off_standard_downs_success_rate_PY1)),
           Rank_Off_Standard_Down_Explosiveness_PY1_col2 = dense_rank(desc(off_standard_downs_explosiveness_PY1)),
           Rank_Off_Pass_Down_PPA_PY1_col2 = dense_rank(desc(off_passing_downs_ppa_PY1)),
           Rank_Off_Pass_Down_Success_Rt_PY1_col2 = dense_rank(desc(off_passing_downs_success_rate_PY1)),
           Rank_Off_Pass_Down_Explosiveness_PY1_col2 = dense_rank(desc(off_passing_downs_explosiveness_PY1)),
           Rank_Off_Rush_Play_PPA_PY1_col2 = dense_rank(desc(off_rushing_plays_ppa_PY1)),
           Rank_Off_Rush_Play_Success_Rt_PY1_col2 = dense_rank(desc(off_rushing_plays_success_rate_PY1)),
           Rank_Off_Rush_Play_Explosiveness_PY1_col2 = dense_rank(desc(off_rushing_plays_explosiveness_PY1)),
           Rank_Off_Pass_Play_PPA_PY1_col2 = dense_rank(desc(off_passing_plays_ppa_PY1)),
           Rank_Off_Pass_Play_Success_Rt_PY1_col2 = dense_rank(desc(off_passing_plays_success_rate_PY1)),
           Rank_Off_Pass_Play_Explosiveness_PY1_col2 = dense_rank(desc(off_passing_plays_explosiveness_PY1)),
           Rank_Def_PPA_PY1_col2 = dense_rank(def_ppa_PY1),
           Rank_Def_Success_Rt_PY1_col2 = dense_rank(def_success_rate_PY1),
           Rank_Def_Explosiveness_PY1_col2 = dense_rank(def_explosiveness_PY1),
           Rank_Def_Pwr_Success_PY1_col2 = dense_rank(def_power_success_PY1),
           Rank_Def_Stuff_Rt_PY1_col2 = dense_rank(desc(def_stuff_rate_PY1)),
           Rank_Def_Line_Yds_PY1_col2 = dense_rank(def_line_yds_PY1),
           Rank_Def_Second_Lvl_Yds_PY1_col2 = dense_rank(def_second_lvl_yds_PY1),
           Rank_Def_Open_Field_Yds_PY1_col2 = dense_rank(def_open_field_yds_PY1),
           Rank_Def_Pts_Per_Opp_PY1_col2 = dense_rank(def_pts_per_opp_PY1),
           Rank_Def_Field_Pos_Avg_Predicted_Pts_PY1_col2 = dense_rank(def_field_pos_avg_predicted_points_PY1),
           Rank_Def_Havoc_Total_PY1_col2 = dense_rank(desc(def_havoc_total_PY1)),
           Rank_Def_Havoc_Front_Seven_PY1_col2 = dense_rank(desc(def_havoc_front_seven_PY1)),
           Rank_Def_Havoc_DB_PY1_col2 = dense_rank(desc(def_havoc_db_PY1)),
           Rank_Def_Standard_Down_PPA_PY1_col2 = dense_rank(def_standard_downs_ppa_PY1),
           Rank_Def_Standard_Down_Success_Rt_PY1_col2 = dense_rank(def_standard_downs_success_rate_PY1),
           Rank_Def_Standard_Down_Explosiveness_PY1_col2 = dense_rank(def_standard_downs_explosiveness_PY1),
           Rank_Def_Pass_Down_PPA_PY1_col2 = dense_rank(def_passing_downs_ppa_PY1),
           Rank_Def_Pass_Down_Success_Rt_PY1_col2 = dense_rank(def_passing_downs_success_rate_PY1),
           Rank_Def_Pass_Down_Explosiveness_PY1_col2 = dense_rank(def_passing_downs_explosiveness_PY1),
           Rank_Def_Rush_Play_PPA_PY1_col2 = dense_rank(def_rushing_plays_ppa_PY1),
           Rank_Def_Rush_Play_Success_Rt_PY1_col2 = dense_rank(def_rushing_plays_success_rate_PY1),
           Rank_Def_Rush_Play_Explosiveness_PY1_col2 = dense_rank(def_rushing_plays_explosiveness_PY1),
           Rank_Def_Pass_Play_PPA_PY1_col2 = dense_rank(def_passing_plays_ppa_PY1),
           Rank_Def_Pass_Play_Success_Rt_PY1_col2 = dense_rank(def_passing_plays_success_rate_PY1),
           Rank_Def_Pass_Play_Explosiveness_PY1_col2 = dense_rank(def_passing_plays_explosiveness_PY1),
           Rank_Recruit_Pts_PY1_col2 = dense_rank(desc(recruit_pts_PY1)),
           Rank_Talent_PY1_col2 = dense_rank(desc(talent_PY1)),
           ## PY1 weighted 3 times
           Rank_Wins_PY1_col3 = dense_rank(desc(Wins_PY1)),
           Rank_Losses_PY1_col3 = dense_rank(Losses_PY1),
           Rank_Comp_Pct_PY1_col3 = dense_rank(desc(completion_pct_PY1)),
           Rank_Pass_YPA_PY1_col3 = dense_rank(desc(pass_ypa_PY1)),
           Rank_Pass_YPR_PY1_col3 = dense_rank(desc(pass_ypr_PY1)),
           Rank_Int_Pct_PY1_col3 = dense_rank(int_pct_PY1),
           Rank_Rush_YPC_PY1_col3 = dense_rank(desc(rush_ypc_PY1)),
           Rank_Turnovers_pg_PY1_col3 = dense_rank(turnovers_pg_PY1),
           Rank_Third_Conv_Rate_PY1_col3 = dense_rank(desc(third_conv_rate_PY1)),
           Rank_Fourth_Conv_Rate_PY1_col3 = dense_rank(desc(fourth_conv_rate_PY1)),
           Rank_Penalty_Yds_pg_PY1_col3 = dense_rank(penalty_yds_pg_PY1),
           Rank_Yds_Per_Penalty_PY1_col3 = dense_rank(yards_per_penalty_PY1),
           Rank_Kick_Return_Avg_PY1_col3 = dense_rank(desc(kick_return_avg_PY1)),
           Rank_Punt_Return_Avg_PY1_col3 = dense_rank(desc(punt_return_avg_PY1)),
           Rank_Total_Yds_pg_PY1_col3 = dense_rank(desc(total_yds_pg_PY1)),
           Rank_Pass_Yds_pg_PY1_col3 = dense_rank(desc(pass_yds_pg_PY1)),
           Rank_Rush_Yds_pg_PY1_col3 = dense_rank(desc(rush_yds_pg_PY1)),
           Rank_First_Downs_pg_PY1_col3 = dense_rank(desc(first_downs_pg_PY1)),
           Rank_Off_YPP_PY1_col3 = dense_rank(desc(off_ypp_PY1)),
           Rank_Def_Ints_pg_PY1_col3 = dense_rank(desc(def_interceptions_pg_PY1)),
           Rank_Off_PPA_PY1_col3 = dense_rank(desc(off_ppa_PY1)),
           Rank_Off_Success_Rt_PY1_col3 = dense_rank(desc(off_success_rate_PY1)),
           Rank_Off_Explosiveness_PY1_col3 = dense_rank(desc(off_explosiveness_PY1)),
           Rank_Off_Pwr_Success_PY1_col3 = dense_rank(desc(off_power_success_PY1)),
           Rank_Off_Stuff_Rt_PY1_col3 = dense_rank(off_stuff_rate_PY1),
           Rank_Off_Line_Yds_PY1_col3 = dense_rank(desc(off_line_yds_PY1)),
           Rank_Off_Second_Lvl_Yds_PY1_col3 = dense_rank(desc(off_second_lvl_yds_PY1)),
           Rank_Off_Open_Field_Yds_PY1_col3 = dense_rank(desc(off_open_field_yds_PY1)),
           Rank_Off_Pts_Per_Opp_PY1_col3 = dense_rank(desc(off_pts_per_opp_PY1)),
           Rank_Off_Field_Pos_Avg_Predicted_Pts_PY1_col3 = dense_rank(desc(off_field_pos_avg_predicted_points_PY1)),
           Rank_Off_Havoc_Total_PY1_col3 = dense_rank(off_havoc_total_PY1),
           Rank_Off_Havoc_Front_PY1_col3 = dense_rank(off_havoc_front_seven_PY1),
           Rank_Off_Havoc_DB_PY1_col3 = dense_rank(off_havoc_db_PY1),
           Rank_Off_Standard_Down_PPA_PY1_col3 = dense_rank(desc(off_standard_downs_ppa_PY1)),
           Rank_Off_Standard_Down_Success_Rt_PY1_col3 = dense_rank(desc(off_standard_downs_success_rate_PY1)),
           Rank_Off_Standard_Down_Explosiveness_PY1_col3 = dense_rank(desc(off_standard_downs_explosiveness_PY1)),
           Rank_Off_Pass_Down_PPA_PY1_col3 = dense_rank(desc(off_passing_downs_ppa_PY1)),
           Rank_Off_Pass_Down_Success_Rt_PY1_col3 = dense_rank(desc(off_passing_downs_success_rate_PY1)),
           Rank_Off_Pass_Down_Explosiveness_PY1_col3 = dense_rank(desc(off_passing_downs_explosiveness_PY1)),
           Rank_Off_Rush_Play_PPA_PY1_col3 = dense_rank(desc(off_rushing_plays_ppa_PY1)),
           Rank_Off_Rush_Play_Success_Rt_PY1_col3 = dense_rank(desc(off_rushing_plays_success_rate_PY1)),
           Rank_Off_Rush_Play_Explosiveness_PY1_col3 = dense_rank(desc(off_rushing_plays_explosiveness_PY1)),
           Rank_Off_Pass_Play_PPA_PY1_col3 = dense_rank(desc(off_passing_plays_ppa_PY1)),
           Rank_Off_Pass_Play_Success_Rt_PY1_col3 = dense_rank(desc(off_passing_plays_success_rate_PY1)),
           Rank_Off_Pass_Play_Explosiveness_PY1_col3 = dense_rank(desc(off_passing_plays_explosiveness_PY1)),
           Rank_Def_PPA_PY1_col3 = dense_rank(def_ppa_PY1),
           Rank_Def_Success_Rt_PY1_col3 = dense_rank(def_success_rate_PY1),
           Rank_Def_Explosiveness_PY1_col3 = dense_rank(def_explosiveness_PY1),
           Rank_Def_Pwr_Success_PY1_col3 = dense_rank(def_power_success_PY1),
           Rank_Def_Stuff_Rt_PY1_col3 = dense_rank(desc(def_stuff_rate_PY1)),
           Rank_Def_Line_Yds_PY1_col3 = dense_rank(def_line_yds_PY1),
           Rank_Def_Second_Lvl_Yds_PY1_col3 = dense_rank(def_second_lvl_yds_PY1),
           Rank_Def_Open_Field_Yds_PY1_col3 = dense_rank(def_open_field_yds_PY1),
           Rank_Def_Pts_Per_Opp_PY1_col3 = dense_rank(def_pts_per_opp_PY1),
           Rank_Def_Field_Pos_Avg_Predicted_Pts_PY1_col3 = dense_rank(def_field_pos_avg_predicted_points_PY1),
           Rank_Def_Havoc_Total_PY1_col3 = dense_rank(desc(def_havoc_total_PY1)),
           Rank_Def_Havoc_Front_Seven_PY1_col3 = dense_rank(desc(def_havoc_front_seven_PY1)),
           Rank_Def_Havoc_DB_PY1_col3 = dense_rank(desc(def_havoc_db_PY1)),
           Rank_Def_Standard_Down_PPA_PY1_col3 = dense_rank(def_standard_downs_ppa_PY1),
           Rank_Def_Standard_Down_Success_Rt_PY1_col3 = dense_rank(def_standard_downs_success_rate_PY1),
           Rank_Def_Standard_Down_Explosiveness_PY1_col3 = dense_rank(def_standard_downs_explosiveness_PY1),
           Rank_Def_Pass_Down_PPA_PY1_col3 = dense_rank(def_passing_downs_ppa_PY1),
           Rank_Def_Pass_Down_Success_Rt_PY1_col3 = dense_rank(def_passing_downs_success_rate_PY1),
           Rank_Def_Pass_Down_Explosiveness_PY1_col3 = dense_rank(def_passing_downs_explosiveness_PY1),
           Rank_Def_Rush_Play_PPA_PY1_col3 = dense_rank(def_rushing_plays_ppa_PY1),
           Rank_Def_Rush_Play_Success_Rt_PY1_col3 = dense_rank(def_rushing_plays_success_rate_PY1),
           Rank_Def_Rush_Play_Explosiveness_PY1_col3 = dense_rank(def_rushing_plays_explosiveness_PY1),
           Rank_Def_Pass_Play_PPA_PY1_col3 = dense_rank(def_passing_plays_ppa_PY1),
           Rank_Def_Pass_Play_Success_Rt_PY1_col3 = dense_rank(def_passing_plays_success_rate_PY1),
           Rank_Def_Pass_Play_Explosiveness_PY1_col3 = dense_rank(def_passing_plays_explosiveness_PY1),
           Rank_Talent_PY1_col3 = dense_rank(desc(talent_PY1)),
           ## incoming recruiting class, weighted once
           Rank_Recruit_Pts = dense_rank(desc(recruit_pts)),
           ## Extra weighted variables, weighted 2x (3 more times)
           Rank_Off_YPP_PY1_col4 = dense_rank(desc(off_ypp_PY1)),
           Rank_Off_PPA_PY1_col4 = dense_rank(desc(off_ppa_PY1)),
           Rank_Off_Success_Rt_PY1_col4 = dense_rank(desc(off_success_rate_PY1)),
           Rank_Off_Explosiveness_PY1_col4 = dense_rank(desc(off_explosiveness_PY1)),
           Rank_Off_Pwr_Success_PY1_col4 = dense_rank(desc(off_power_success_PY1)),
           Rank_Off_Stuff_Rt_PY1_col4 = dense_rank(off_stuff_rate_PY1),
           Rank_Off_Pts_Per_Opp_PY1_col4 = dense_rank(desc(off_pts_per_opp_PY1)),
           Rank_Off_Havoc_Total_PY1_col4 = dense_rank(off_havoc_total_PY1),
           Rank_Off_Havoc_Front_PY1_col4 = dense_rank(off_havoc_front_seven_PY1),
           Rank_Off_Havoc_DB_PY1_col4 = dense_rank(off_havoc_db_PY1),
           Rank_Off_Standard_Down_PPA_PY1_col4 = dense_rank(desc(off_standard_downs_ppa_PY1)),
           Rank_Off_Standard_Down_Success_Rt_PY1_col4 = dense_rank(desc(off_standard_downs_success_rate_PY1)),
           Rank_Off_Standard_Down_Explosiveness_PY1_col4 = dense_rank(desc(off_standard_downs_explosiveness_PY1)),
           Rank_Off_Pass_Down_PPA_PY1_col4 = dense_rank(desc(off_passing_downs_ppa_PY1)),
           Rank_Off_Pass_Down_Success_Rt_PY1_col4 = dense_rank(desc(off_passing_downs_success_rate_PY1)),
           Rank_Off_Pass_Down_Explosiveness_PY1_col4 = dense_rank(desc(off_passing_downs_explosiveness_PY1)),
           Rank_Off_Rush_Play_PPA_PY1_col4 = dense_rank(desc(off_rushing_plays_ppa_PY1)),
           Rank_Off_Rush_Play_Success_Rt_PY1_col4 = dense_rank(desc(off_rushing_plays_success_rate_PY1)),
           Rank_Off_Rush_Play_Explosiveness_PY1_col4 = dense_rank(desc(off_rushing_plays_explosiveness_PY1)),
           Rank_Off_Pass_Play_PPA_PY1_col4 = dense_rank(desc(off_passing_plays_ppa_PY1)),
           Rank_Off_Pass_Play_Success_Rt_PY1_col4 = dense_rank(desc(off_passing_plays_success_rate_PY1)),
           Rank_Off_Pass_Play_Explosiveness_PY1_col4 = dense_rank(desc(off_passing_plays_explosiveness_PY1)),
           Rank_Def_PPA_PY1_col4 = dense_rank(def_ppa_PY1),
           Rank_Def_Success_Rt_PY1_col4 = dense_rank(def_success_rate_PY1),
           Rank_Def_Explosiveness_PY1_col4 = dense_rank(def_explosiveness_PY1),
           Rank_Def_Pwr_Success_PY1_col4 = dense_rank(def_power_success_PY1),
           Rank_Def_Stuff_Rt_PY1_col4 = dense_rank(desc(def_stuff_rate_PY1)),
           Rank_Def_Pts_Per_Opp_PY1_col4 = dense_rank(def_pts_per_opp_PY1),
           Rank_Def_Havoc_Total_PY1_col4 = dense_rank(desc(def_havoc_total_PY1)),
           Rank_Def_Havoc_Front_Seven_PY1_col4 = dense_rank(desc(def_havoc_front_seven_PY1)),
           Rank_Def_Havoc_DB_PY1_col4 = dense_rank(desc(def_havoc_db_PY1)),
           Rank_Def_Standard_Down_PPA_PY1_col4 = dense_rank(def_standard_downs_ppa_PY1),
           Rank_Def_Standard_Down_Success_Rt_PY1_col4 = dense_rank(def_standard_downs_success_rate_PY1),
           Rank_Def_Standard_Down_Explosiveness_PY1_col4 = dense_rank(def_standard_downs_explosiveness_PY1),
           Rank_Def_Pass_Down_PPA_PY1_col4 = dense_rank(def_passing_downs_ppa_PY1),
           Rank_Def_Pass_Down_Success_Rt_PY1_col4 = dense_rank(def_passing_downs_success_rate_PY1),
           Rank_Def_Pass_Down_Explosiveness_PY1_col4 = dense_rank(def_passing_downs_explosiveness_PY1),
           Rank_Def_Rush_Play_PPA_PY1_col4 = dense_rank(def_rushing_plays_ppa_PY1),
           Rank_Def_Rush_Play_Success_Rt_PY1_col4 = dense_rank(def_rushing_plays_success_rate_PY1),
           Rank_Def_Rush_Play_Explosiveness_PY1_col4 = dense_rank(def_rushing_plays_explosiveness_PY1),
           Rank_Def_Pass_Play_PPA_PY1_col4 = dense_rank(def_passing_plays_ppa_PY1),
           Rank_Def_Pass_Play_Success_Rt_PY1_col4 = dense_rank(def_passing_plays_success_rate_PY1),
           Rank_Def_Pass_Play_Explosiveness_PY1_col4 = dense_rank(def_passing_plays_explosiveness_PY1),
           ## Extra weighted variables, weighted 2x (2 more times)
           Rank_Off_YPP_PY1_col5 = dense_rank(desc(off_ypp_PY1)),
           Rank_Off_PPA_PY1_col5 = dense_rank(desc(off_ppa_PY1)),
           Rank_Off_Success_Rt_PY1_col5 = dense_rank(desc(off_success_rate_PY1)),
           Rank_Off_Explosiveness_PY1_col5 = dense_rank(desc(off_explosiveness_PY1)),
           Rank_Off_Pwr_Success_PY1_col5 = dense_rank(desc(off_power_success_PY1)),
           Rank_Off_Stuff_Rt_PY1_col5 = dense_rank(off_stuff_rate_PY1),
           Rank_Off_Pts_Per_Opp_PY1_col5 = dense_rank(desc(off_pts_per_opp_PY1)),
           Rank_Off_Havoc_Total_PY1_col5 = dense_rank(off_havoc_total_PY1),
           Rank_Off_Havoc_Front_PY1_col5 = dense_rank(off_havoc_front_seven_PY1),
           Rank_Off_Havoc_DB_PY1_col5 = dense_rank(off_havoc_db_PY1),
           Rank_Off_Standard_Down_PPA_PY1_col5 = dense_rank(desc(off_standard_downs_ppa_PY1)),
           Rank_Off_Standard_Down_Success_Rt_PY1_col5 = dense_rank(desc(off_standard_downs_success_rate_PY1)),
           Rank_Off_Standard_Down_Explosiveness_PY1_col5 = dense_rank(desc(off_standard_downs_explosiveness_PY1)),
           Rank_Off_Pass_Down_PPA_PY1_col5 = dense_rank(desc(off_passing_downs_ppa_PY1)),
           Rank_Off_Pass_Down_Success_Rt_PY1_col5 = dense_rank(desc(off_passing_downs_success_rate_PY1)),
           Rank_Off_Pass_Down_Explosiveness_PY1_col5 = dense_rank(desc(off_passing_downs_explosiveness_PY1)),
           Rank_Off_Rush_Play_PPA_PY1_col5 = dense_rank(desc(off_rushing_plays_ppa_PY1)),
           Rank_Off_Rush_Play_Success_Rt_PY1_col5 = dense_rank(desc(off_rushing_plays_success_rate_PY1)),
           Rank_Off_Rush_Play_Explosiveness_PY1_col5 = dense_rank(desc(off_rushing_plays_explosiveness_PY1)),
           Rank_Off_Pass_Play_PPA_PY1_col5 = dense_rank(desc(off_passing_plays_ppa_PY1)),
           Rank_Off_Pass_Play_Success_Rt_PY1_col5 = dense_rank(desc(off_passing_plays_success_rate_PY1)),
           Rank_Off_Pass_Play_Explosiveness_PY1_col5 = dense_rank(desc(off_passing_plays_explosiveness_PY1)),
           Rank_Def_PPA_PY1_col5 = dense_rank(def_ppa_PY1),
           Rank_Def_Success_Rt_PY1_col5 = dense_rank(def_success_rate_PY1),
           Rank_Def_Explosiveness_PY1_col5 = dense_rank(def_explosiveness_PY1),
           Rank_Def_Pwr_Success_PY1_col5 = dense_rank(def_power_success_PY1),
           Rank_Def_Stuff_Rt_PY1_col5 = dense_rank(desc(def_stuff_rate_PY1)),
           Rank_Def_Pts_Per_Opp_PY1_col5 = dense_rank(def_pts_per_opp_PY1),
           Rank_Def_Havoc_Total_PY1_col5 = dense_rank(desc(def_havoc_total_PY1)),
           Rank_Def_Havoc_Front_Seven_PY1_col5 = dense_rank(desc(def_havoc_front_seven_PY1)),
           Rank_Def_Havoc_DB_PY1_col5 = dense_rank(desc(def_havoc_db_PY1)),
           Rank_Def_Standard_Down_PPA_PY1_col5 = dense_rank(def_standard_downs_ppa_PY1),
           Rank_Def_Standard_Down_Success_Rt_PY1_col5 = dense_rank(def_standard_downs_success_rate_PY1),
           Rank_Def_Standard_Down_Explosiveness_PY1_col5 = dense_rank(def_standard_downs_explosiveness_PY1),
           Rank_Def_Pass_Down_PPA_PY1_col5 = dense_rank(def_passing_downs_ppa_PY1),
           Rank_Def_Pass_Down_Success_Rt_PY1_col5 = dense_rank(def_passing_downs_success_rate_PY1),
           Rank_Def_Pass_Down_Explosiveness_PY1_col5 = dense_rank(def_passing_downs_explosiveness_PY1),
           Rank_Def_Rush_Play_PPA_PY1_col5 = dense_rank(def_rushing_plays_ppa_PY1),
           Rank_Def_Rush_Play_Success_Rt_PY1_col5 = dense_rank(def_rushing_plays_success_rate_PY1),
           Rank_Def_Rush_Play_Explosiveness_PY1_col5 = dense_rank(def_rushing_plays_explosiveness_PY1),
           Rank_Def_Pass_Play_PPA_PY1_col5 = dense_rank(def_passing_plays_ppa_PY1),
           Rank_Def_Pass_Play_Success_Rt_PY1_col5 = dense_rank(def_passing_plays_success_rate_PY1),
           Rank_Def_Pass_Play_Explosiveness_PY1_col5 = dense_rank(def_passing_plays_explosiveness_PY1),
           ## Extra weighted variables, weighted 2x (1 more time)
           Rank_Off_YPP_PY1_col6 = dense_rank(desc(off_ypp_PY1)),
           Rank_Off_PPA_PY1_col6 = dense_rank(desc(off_ppa_PY1)),
           Rank_Off_Success_Rt_PY1_col6 = dense_rank(desc(off_success_rate_PY1)),
           Rank_Off_Explosiveness_PY1_col6 = dense_rank(desc(off_explosiveness_PY1)),
           Rank_Off_Pwr_Success_PY1_col6 = dense_rank(desc(off_power_success_PY1)),
           Rank_Off_Stuff_Rt_PY1_col6 = dense_rank(off_stuff_rate_PY1),
           Rank_Off_Pts_Per_Opp_PY1_col6 = dense_rank(desc(off_pts_per_opp_PY1)),
           Rank_Off_Havoc_Total_PY1_col6 = dense_rank(off_havoc_total_PY1),
           Rank_Off_Havoc_Front_PY1_col6 = dense_rank(off_havoc_front_seven_PY1),
           Rank_Off_Havoc_DB_PY1_col6 = dense_rank(off_havoc_db_PY1),
           Rank_Off_Standard_Down_PPA_PY1_col6 = dense_rank(desc(off_standard_downs_ppa_PY1)),
           Rank_Off_Standard_Down_Success_Rt_PY1_col6 = dense_rank(desc(off_standard_downs_success_rate_PY1)),
           Rank_Off_Standard_Down_Explosiveness_PY1_col6 = dense_rank(desc(off_standard_downs_explosiveness_PY1)),
           Rank_Off_Pass_Down_PPA_PY1_col6 = dense_rank(desc(off_passing_downs_ppa_PY1)),
           Rank_Off_Pass_Down_Success_Rt_PY1_col6 = dense_rank(desc(off_passing_downs_success_rate_PY1)),
           Rank_Off_Pass_Down_Explosiveness_PY1_col6 = dense_rank(desc(off_passing_downs_explosiveness_PY1)),
           Rank_Off_Rush_Play_PPA_PY1_col6 = dense_rank(desc(off_rushing_plays_ppa_PY1)),
           Rank_Off_Rush_Play_Success_Rt_PY1_col6 = dense_rank(desc(off_rushing_plays_success_rate_PY1)),
           Rank_Off_Rush_Play_Explosiveness_PY1_col6 = dense_rank(desc(off_rushing_plays_explosiveness_PY1)),
           Rank_Off_Pass_Play_PPA_PY1_col6 = dense_rank(desc(off_passing_plays_ppa_PY1)),
           Rank_Off_Pass_Play_Success_Rt_PY1_col6 = dense_rank(desc(off_passing_plays_success_rate_PY1)),
           Rank_Off_Pass_Play_Explosiveness_PY1_col6 = dense_rank(desc(off_passing_plays_explosiveness_PY1)),
           Rank_Def_PPA_PY1_col6 = dense_rank(def_ppa_PY1),
           Rank_Def_Success_Rt_PY1_col6 = dense_rank(def_success_rate_PY1),
           Rank_Def_Explosiveness_PY1_col6 = dense_rank(def_explosiveness_PY1),
           Rank_Def_Pwr_Success_PY1_col6 = dense_rank(def_power_success_PY1),
           Rank_Def_Stuff_Rt_PY1_col6 = dense_rank(desc(def_stuff_rate_PY1)),
           Rank_Def_Pts_Per_Opp_PY1_col6 = dense_rank(def_pts_per_opp_PY1),
           Rank_Def_Havoc_Total_PY1_col6 = dense_rank(desc(def_havoc_total_PY1)),
           Rank_Def_Havoc_Front_Seven_PY1_col6 = dense_rank(desc(def_havoc_front_seven_PY1)),
           Rank_Def_Havoc_DB_PY1_col6 = dense_rank(desc(def_havoc_db_PY1)),
           Rank_Def_Standard_Down_PPA_PY1_col6 = dense_rank(def_standard_downs_ppa_PY1),
           Rank_Def_Standard_Down_Success_Rt_PY1_col6 = dense_rank(def_standard_downs_success_rate_PY1),
           Rank_Def_Standard_Down_Explosiveness_PY1_col6 = dense_rank(def_standard_downs_explosiveness_PY1),
           Rank_Def_Pass_Down_PPA_PY1_col6 = dense_rank(def_passing_downs_ppa_PY1),
           Rank_Def_Pass_Down_Success_Rt_PY1_col6 = dense_rank(def_passing_downs_success_rate_PY1),
           Rank_Def_Pass_Down_Explosiveness_PY1_col6 = dense_rank(def_passing_downs_explosiveness_PY1),
           Rank_Def_Rush_Play_PPA_PY1_col6 = dense_rank(def_rushing_plays_ppa_PY1),
           Rank_Def_Rush_Play_Success_Rt_PY1_col6 = dense_rank(def_rushing_plays_success_rate_PY1),
           Rank_Def_Rush_Play_Explosiveness_PY1_col6 = dense_rank(def_rushing_plays_explosiveness_PY1),
           Rank_Def_Pass_Play_PPA_PY1_col6 = dense_rank(def_passing_plays_ppa_PY1),
           Rank_Def_Pass_Play_Success_Rt_PY1_col6 = dense_rank(def_passing_plays_success_rate_PY1),
           Rank_Def_Pass_Play_Explosiveness_PY1_col6 = dense_rank(def_passing_plays_explosiveness_PY1),
           ## FPI_SP mean ranks added at the end, weighted once
           Rank_FPI_SP_SRS_PY3_mean = dense_rank(desc(FPI_SP_SRS_PY3_mean)),
           Rank_FPI_SP_SRS_PY2_mean = dense_rank(desc(FPI_SP_SRS_PY2_mean)),
           Rank_FPI_SP_SRS_PY1_mean = dense_rank(desc(FPI_SP_SRS_PY1_mean)),
           Rank_AllPY_FPI_SP_SRS_mean = dense_rank(desc(AllPY_FPI_SP_SRS_mean)),
           Rank_WeightedAllPY_FPI_SP_SRS_mean = dense_rank(desc(WeightedAllPY_FPI_SP_SRS_mean)))
} else if (as.numeric(week) == 1) {
  ##### Week 1 Variable Ranks #####
  # PY3 weighted 1x, PY2 weighted 2x, PY1 weighted 3x, current weighted 1x
  ## PY3 ranks added first, weighted once
  VoA_Variables <- VoA_Variables |>
    mutate(Rank_Wins_PY3 = dense_rank(desc(Wins_PY3)),
           Rank_Losses_PY3 = dense_rank(Losses_PY3),
           Rank_Comp_Pct_PY3 = dense_rank(desc(completion_pct_PY3)),
           Rank_Pass_YPA_PY3 = dense_rank(desc(pass_ypa_PY3)),
           Rank_Pass_YPR_PY3 = dense_rank(desc(pass_ypr_PY3)),
           Rank_Int_Pct_PY3 = dense_rank(int_pct_PY3),
           Rank_Rush_YPC_PY3 = dense_rank(desc(rush_ypc_PY3)),
           Rank_Turnovers_pg_PY3 = dense_rank(turnovers_pg_PY3),
           Rank_Third_Conv_Rate_PY3 = dense_rank(desc(third_conv_rate_PY3)),
           Rank_Fourth_Conv_Rate_PY3 = dense_rank(desc(fourth_conv_rate_PY3)),
           Rank_Penalty_Yds_pg_PY3 = dense_rank(penalty_yds_pg_PY3),
           Rank_Yds_Per_Penalty_PY3 = dense_rank(yards_per_penalty_PY3),
           Rank_Kick_Return_Avg_PY3 = dense_rank(desc(kick_return_avg_PY3)),
           Rank_Punt_Return_Avg_PY3 = dense_rank(desc(punt_return_avg_PY3)),
           Rank_Total_Yds_pg_PY3 = dense_rank(desc(total_yds_pg_PY3)),
           Rank_Pass_Yds_pg_PY3 = dense_rank(desc(pass_yds_pg_PY3)),
           Rank_Rush_Yds_pg_PY3 = dense_rank(desc(rush_yds_pg_PY3)),
           Rank_First_Downs_pg_PY3 = dense_rank(desc(first_downs_pg_PY3)),
           Rank_Off_YPP_PY3 = dense_rank(desc(off_ypp_PY3)),
           Rank_Def_Ints_pg_PY3 = dense_rank(desc(def_interceptions_pg_PY3)),
           Rank_Off_PPA_PY3 = dense_rank(desc(off_ppa_PY3)),
           Rank_Off_Success_Rt_PY3 = dense_rank(desc(off_success_rate_PY3)),
           Rank_Off_Explosiveness_PY3 = dense_rank(desc(off_explosiveness_PY3)),
           Rank_Off_Pwr_Success_PY3 = dense_rank(desc(off_power_success_PY3)),
           Rank_Off_Stuff_Rt_PY3 = dense_rank(off_stuff_rate_PY3),
           Rank_Off_Line_Yds_PY3 = dense_rank(desc(off_line_yds_PY3)),
           Rank_Off_Second_Lvl_Yds_PY3 = dense_rank(desc(off_second_lvl_yds_PY3)),
           Rank_Off_Open_Field_Yds_PY3 = dense_rank(desc(off_open_field_yds_PY3)),
           Rank_Off_Pts_Per_Opp_PY3 = dense_rank(desc(off_pts_per_opp_PY3)),
           Rank_Off_Field_Pos_Avg_Predicted_Pts_PY3 = dense_rank(desc(off_field_pos_avg_predicted_points_PY3)),
           Rank_Off_Havoc_Total_PY3 = dense_rank(off_havoc_total_PY3),
           Rank_Off_Havoc_Front_PY3 = dense_rank(off_havoc_front_seven_PY3),
           Rank_Off_Havoc_DB_PY3 = dense_rank(off_havoc_db_PY3),
           Rank_Off_Standard_Down_PPA_PY3 = dense_rank(desc(off_standard_downs_ppa_PY3)),
           Rank_Off_Standard_Down_Success_Rt_PY3 = dense_rank(desc(off_standard_downs_success_rate_PY3)),
           Rank_Off_Standard_Down_Explosiveness_PY3 = dense_rank(desc(off_standard_downs_explosiveness_PY3)),
           Rank_Off_Pass_Down_PPA_PY3 = dense_rank(desc(off_passing_downs_ppa_PY3)),
           Rank_Off_Pass_Down_Success_Rt_PY3 = dense_rank(desc(off_passing_downs_success_rate_PY3)),
           Rank_Off_Pass_Down_Explosiveness_PY3 = dense_rank(desc(off_passing_downs_explosiveness_PY3)),
           Rank_Off_Rush_Play_PPA_PY3 = dense_rank(desc(off_rushing_plays_ppa_PY3)),
           Rank_Off_Rush_Play_Success_Rt_PY3 = dense_rank(desc(off_rushing_plays_success_rate_PY3)),
           Rank_Off_Rush_Play_Explosiveness_PY3 = dense_rank(desc(off_rushing_plays_explosiveness_PY3)),
           Rank_Off_Pass_Play_PPA_PY3 = dense_rank(desc(off_passing_plays_ppa_PY3)),
           Rank_Off_Pass_Play_Success_Rt_PY3 = dense_rank(desc(off_passing_plays_success_rate_PY3)),
           Rank_Off_Pass_Play_Explosiveness_PY3 = dense_rank(desc(off_passing_plays_explosiveness_PY3)),
           Rank_Def_PPA_PY3 = dense_rank(def_ppa_PY3),
           Rank_Def_Success_Rt_PY3 = dense_rank(def_success_rate_PY3),
           Rank_Def_Explosiveness_PY3 = dense_rank(def_explosiveness_PY3),
           Rank_Def_Pwr_Success_PY3 = dense_rank(def_power_success_PY3),
           Rank_Def_Stuff_Rt_PY3 = dense_rank(desc(def_stuff_rate_PY3)),
           Rank_Def_Line_Yds_PY3 = dense_rank(def_line_yds_PY3),
           Rank_Def_Second_Lvl_Yds_PY3 = dense_rank(def_second_lvl_yds_PY3),
           Rank_Def_Open_Field_Yds_PY3 = dense_rank(def_open_field_yds_PY3),
           Rank_Def_Pts_Per_Opp_PY3 = dense_rank(def_pts_per_opp_PY3),
           Rank_Def_Field_Pos_Avg_Predicted_Pts_PY3 = dense_rank(def_field_pos_avg_predicted_points_PY3),
           Rank_Def_Havoc_Total_PY3 = dense_rank(desc(def_havoc_total_PY3)),
           Rank_Def_Havoc_Front_Seven_PY3 = dense_rank(desc(def_havoc_front_seven_PY3)),
           Rank_Def_Havoc_DB_PY3 = dense_rank(desc(def_havoc_db_PY3)),
           Rank_Def_Standard_Down_PPA_PY3 = dense_rank(def_standard_downs_ppa_PY3),
           Rank_Def_Standard_Down_Success_Rt_PY3 = dense_rank(def_standard_downs_success_rate_PY3),
           Rank_Def_Standard_Down_Explosiveness_PY3 = dense_rank(def_standard_downs_explosiveness_PY3),
           Rank_Def_Pass_Down_PPA_PY3 = dense_rank(def_passing_downs_ppa_PY3),
           Rank_Def_Pass_Down_Success_Rt_PY3 = dense_rank(def_passing_downs_success_rate_PY3),
           Rank_Def_Pass_Down_Explosiveness_PY3 = dense_rank(def_passing_downs_explosiveness_PY3),
           Rank_Def_Rush_Play_PPA_PY3 = dense_rank(def_rushing_plays_ppa_PY3),
           Rank_Def_Rush_Play_Success_Rt_PY3 = dense_rank(def_rushing_plays_success_rate_PY3),
           Rank_Def_Rush_Play_Explosiveness_PY3 = dense_rank(def_rushing_plays_explosiveness_PY3),
           Rank_Def_Pass_Play_PPA_PY3 = dense_rank(def_passing_plays_ppa_PY3),
           Rank_Def_Pass_Play_Success_Rt_PY3 = dense_rank(def_passing_plays_success_rate_PY3),
           Rank_Def_Pass_Play_Explosiveness_PY3 = dense_rank(def_passing_plays_explosiveness_PY3),
           Rank_Recruit_Pts_PY3 = dense_rank(desc(recruit_pts_PY3)),
           Rank_Talent_PY3 = dense_rank(desc(talent_PY3)),
           Rank_SP_Rating_PY3 = dense_rank(desc(sp_rating_PY3)),
           Rank_SP_Off_Rating_PY3 = dense_rank(desc(sp_offense_rating_PY3)),
           Rank_SP_Def_Rating_PY3 = dense_rank(sp_defense_rating_PY3),
           Rank_SP_SpecialTeams_Rating_PY3 = dense_rank(desc(sp_special_teams_rating_PY3)),
           Rank_FPI_PY3 = dense_rank(desc(FPI_PY3)),
           Rank_SRS_rating_PY3 = dense_rank(desc(SRS_rating_PY3)),
           ## Extra weighted variables (2x for Previous Year data, 3x for PY3 recruiting)
           Rank_Off_YPP_PY3_col2 = dense_rank(desc(off_ypp_PY3)),
           Rank_Off_PPA_PY3_col2 = dense_rank(desc(off_ppa_PY3)),
           Rank_Off_Success_Rt_PY3_col2 = dense_rank(desc(off_success_rate_PY3)),
           Rank_Off_Explosiveness_PY3_col2 = dense_rank(desc(off_explosiveness_PY3)),
           Rank_Off_Pwr_Success_PY3_col2 = dense_rank(desc(off_power_success_PY3)),
           Rank_Off_Stuff_Rt_PY3_col2 = dense_rank(off_stuff_rate_PY3),
           Rank_Off_Pts_Per_Opp_PY3_col2 = dense_rank(desc(off_pts_per_opp_PY3)),
           Rank_Off_Havoc_Total_PY3_col2 = dense_rank(off_havoc_total_PY3),
           Rank_Off_Havoc_Front_PY3_col2 = dense_rank(off_havoc_front_seven_PY3),
           Rank_Off_Havoc_DB_PY3_col2 = dense_rank(off_havoc_db_PY3),
           Rank_Off_Standard_Down_PPA_PY3_col2 = dense_rank(desc(off_standard_downs_ppa_PY3)),
           Rank_Off_Standard_Down_Success_Rt_PY3_col2 = dense_rank(desc(off_standard_downs_success_rate_PY3)),
           Rank_Off_Standard_Down_Explosiveness_PY3_col2 = dense_rank(desc(off_standard_downs_explosiveness_PY3)),
           Rank_Off_Pass_Down_PPA_PY3_col2 = dense_rank(desc(off_passing_downs_ppa_PY3)),
           Rank_Off_Pass_Down_Success_Rt_PY3_col2 = dense_rank(desc(off_passing_downs_success_rate_PY3)),
           Rank_Off_Pass_Down_Explosiveness_PY3_col2 = dense_rank(desc(off_passing_downs_explosiveness_PY3)),
           Rank_Off_Rush_Play_PPA_PY3_col2 = dense_rank(desc(off_rushing_plays_ppa_PY3)),
           Rank_Off_Rush_Play_Success_Rt_PY3_col2 = dense_rank(desc(off_rushing_plays_success_rate_PY3)),
           Rank_Off_Rush_Play_Explosiveness_PY3_col2 = dense_rank(desc(off_rushing_plays_explosiveness_PY3)),
           Rank_Off_Pass_Play_PPA_PY3_col2 = dense_rank(desc(off_passing_plays_ppa_PY3)),
           Rank_Off_Pass_Play_Success_Rt_PY3_col2 = dense_rank(desc(off_passing_plays_success_rate_PY3)),
           Rank_Off_Pass_Play_Explosiveness_PY3_col2 = dense_rank(desc(off_passing_plays_explosiveness_PY3)),
           Rank_Def_PPA_PY3_col2 = dense_rank(def_ppa_PY3),
           Rank_Def_Success_Rt_PY3_col2 = dense_rank(def_success_rate_PY3),
           Rank_Def_Explosiveness_PY3_col2 = dense_rank(def_explosiveness_PY3),
           Rank_Def_Pwr_Success_PY3_col2 = dense_rank(def_power_success_PY3),
           Rank_Def_Stuff_Rt_PY3_col2 = dense_rank(desc(def_stuff_rate_PY3)),
           Rank_Def_Pts_Per_Opp_PY3_col2 = dense_rank(def_pts_per_opp_PY3),
           Rank_Def_Havoc_Total_PY3_col2 = dense_rank(desc(def_havoc_total_PY3)),
           Rank_Def_Havoc_Front_Seven_PY3_col2 = dense_rank(desc(def_havoc_front_seven_PY3)),
           Rank_Def_Havoc_DB_PY3_col2 = dense_rank(desc(def_havoc_db_PY3)),
           Rank_Def_Standard_Down_PPA_PY3_col2 = dense_rank(def_standard_downs_ppa_PY3),
           Rank_Def_Standard_Down_Success_Rt_PY3_col2 = dense_rank(def_standard_downs_success_rate_PY3),
           Rank_Def_Standard_Down_Explosiveness_PY3_col2 = dense_rank(def_standard_downs_explosiveness_PY3),
           Rank_Def_Pass_Down_PPA_PY3_col2 = dense_rank(def_passing_downs_ppa_PY3),
           Rank_Def_Pass_Down_Success_Rt_PY3_col2 = dense_rank(def_passing_downs_success_rate_PY3),
           Rank_Def_Pass_Down_Explosiveness_PY3_col2 = dense_rank(def_passing_downs_explosiveness_PY3),
           Rank_Def_Rush_Play_PPA_PY3_col2 = dense_rank(def_rushing_plays_ppa_PY3),
           Rank_Def_Rush_Play_Success_Rt_PY3_col2 = dense_rank(def_rushing_plays_success_rate_PY3),
           Rank_Def_Rush_Play_Explosiveness_PY3_col2 = dense_rank(def_rushing_plays_explosiveness_PY3),
           Rank_Def_Pass_Play_PPA_PY3_col2 = dense_rank(def_passing_plays_ppa_PY3),
           Rank_Def_Pass_Play_Success_Rt_PY3_col2 = dense_rank(def_passing_plays_success_rate_PY3),
           Rank_Def_Pass_Play_Explosiveness_PY3_col2 = dense_rank(def_passing_plays_explosiveness_PY3),
           Rank_Recruit_Pts_PY3_col2 = dense_rank(desc(recruit_pts_PY3)),
           Rank_Recruit_Pts_PY3_col3 = dense_rank(desc(recruit_pts_PY3)),
           ## PY2 ranks
           Rank_Wins_PY2 = dense_rank(desc(Wins_PY2)),
           Rank_Losses_PY2 = dense_rank(Losses_PY2),
           Rank_Comp_Pct_PY2 = dense_rank(desc(completion_pct_PY2)),
           Rank_Pass_YPA_PY2 = dense_rank(desc(pass_ypa_PY2)),
           Rank_Pass_YPR_PY2 = dense_rank(desc(pass_ypr_PY2)),
           Rank_Int_Pct_PY2 = dense_rank(int_pct_PY2),
           Rank_Rush_YPC_PY2 = dense_rank(desc(rush_ypc_PY2)),
           Rank_Turnovers_pg_PY2 = dense_rank(turnovers_pg_PY2),
           Rank_Third_Conv_Rate_PY2 = dense_rank(desc(third_conv_rate_PY2)),
           Rank_Fourth_Conv_Rate_PY2 = dense_rank(desc(fourth_conv_rate_PY2)),
           Rank_Penalty_Yds_pg_PY2 = dense_rank(penalty_yds_pg_PY2),
           Rank_Yds_Per_Penalty_PY2 = dense_rank(yards_per_penalty_PY2),
           Rank_Kick_Return_Avg_PY2 = dense_rank(desc(kick_return_avg_PY2)),
           Rank_Punt_Return_Avg_PY2 = dense_rank(desc(punt_return_avg_PY2)),
           Rank_Total_Yds_pg_PY2 = dense_rank(desc(total_yds_pg_PY2)),
           Rank_Pass_Yds_pg_PY2 = dense_rank(desc(pass_yds_pg_PY2)),
           Rank_Rush_Yds_pg_PY2 = dense_rank(desc(rush_yds_pg_PY2)),
           Rank_First_Downs_pg_PY2 = dense_rank(desc(first_downs_pg_PY2)),
           Rank_Off_YPP_PY2 = dense_rank(desc(off_ypp_PY2)),
           Rank_Def_Ints_pg_PY2 = dense_rank(desc(def_interceptions_pg_PY2)),
           Rank_Off_PPA_PY2 = dense_rank(desc(off_ppa_PY2)),
           Rank_Off_Success_Rt_PY2 = dense_rank(desc(off_success_rate_PY2)),
           Rank_Off_Explosiveness_PY2 = dense_rank(desc(off_explosiveness_PY2)),
           Rank_Off_Pwr_Success_PY2 = dense_rank(desc(off_power_success_PY2)),
           Rank_Off_Stuff_Rt_PY2 = dense_rank(off_stuff_rate_PY2),
           Rank_Off_Line_Yds_PY2 = dense_rank(desc(off_line_yds_PY2)),
           Rank_Off_Second_Lvl_Yds_PY2 = dense_rank(desc(off_second_lvl_yds_PY2)),
           Rank_Off_Open_Field_Yds_PY2 = dense_rank(desc(off_open_field_yds_PY2)),
           Rank_Off_Pts_Per_Opp_PY2 = dense_rank(desc(off_pts_per_opp_PY2)),
           Rank_Off_Field_Pos_Avg_Predicted_Pts_PY2 = dense_rank(desc(off_field_pos_avg_predicted_points_PY2)),
           Rank_Off_Havoc_Total_PY2 = dense_rank(off_havoc_total_PY2),
           Rank_Off_Havoc_Front_PY2 = dense_rank(off_havoc_front_seven_PY2),
           Rank_Off_Havoc_DB_PY2 = dense_rank(off_havoc_db_PY2),
           Rank_Off_Standard_Down_PPA_PY2 = dense_rank(desc(off_standard_downs_ppa_PY2)),
           Rank_Off_Standard_Down_Success_Rt_PY2 = dense_rank(desc(off_standard_downs_success_rate_PY2)),
           Rank_Off_Standard_Down_Explosiveness_PY2 = dense_rank(desc(off_standard_downs_explosiveness_PY2)),
           Rank_Off_Pass_Down_PPA_PY2 = dense_rank(desc(off_passing_downs_ppa_PY2)),
           Rank_Off_Pass_Down_Success_Rt_PY2 = dense_rank(desc(off_passing_downs_success_rate_PY2)),
           Rank_Off_Pass_Down_Explosiveness_PY2 = dense_rank(desc(off_passing_downs_explosiveness_PY2)),
           Rank_Off_Rush_Play_PPA_PY2 = dense_rank(desc(off_rushing_plays_ppa_PY2)),
           Rank_Off_Rush_Play_Success_Rt_PY2 = dense_rank(desc(off_rushing_plays_success_rate_PY2)),
           Rank_Off_Rush_Play_Explosiveness_PY2 = dense_rank(desc(off_rushing_plays_explosiveness_PY2)),
           Rank_Off_Pass_Play_PPA_PY2 = dense_rank(desc(off_passing_plays_ppa_PY2)),
           Rank_Off_Pass_Play_Success_Rt_PY2 = dense_rank(desc(off_passing_plays_success_rate_PY2)),
           Rank_Off_Pass_Play_Explosiveness_PY2 = dense_rank(desc(off_passing_plays_explosiveness_PY2)),
           Rank_Def_PPA_PY2 = dense_rank(def_ppa_PY2),
           Rank_Def_Success_Rt_PY2 = dense_rank(def_success_rate_PY2),
           Rank_Def_Explosiveness_PY2 = dense_rank(def_explosiveness_PY2),
           Rank_Def_Pwr_Success_PY2 = dense_rank(def_power_success_PY2),
           Rank_Def_Stuff_Rt_PY2 = dense_rank(desc(def_stuff_rate_PY2)),
           Rank_Def_Line_Yds_PY2 = dense_rank(def_line_yds_PY2),
           Rank_Def_Second_Lvl_Yds_PY2 = dense_rank(def_second_lvl_yds_PY2),
           Rank_Def_Open_Field_Yds_PY2 = dense_rank(def_open_field_yds_PY2),
           Rank_Def_Pts_Per_Opp_PY2 = dense_rank(def_pts_per_opp_PY2),
           Rank_Def_Field_Pos_Avg_Predicted_Pts_PY2 = dense_rank(def_field_pos_avg_predicted_points_PY2),
           Rank_Def_Havoc_Total_PY2 = dense_rank(desc(def_havoc_total_PY2)),
           Rank_Def_Havoc_Front_Seven_PY2 = dense_rank(desc(def_havoc_front_seven_PY2)),
           Rank_Def_Havoc_DB_PY2 = dense_rank(desc(def_havoc_db_PY2)),
           Rank_Def_Standard_Down_PPA_PY2 = dense_rank(def_standard_downs_ppa_PY2),
           Rank_Def_Standard_Down_Success_Rt_PY2 = dense_rank(def_standard_downs_success_rate_PY2),
           Rank_Def_Standard_Down_Explosiveness_PY2 = dense_rank(def_standard_downs_explosiveness_PY2),
           Rank_Def_Pass_Down_PPA_PY2 = dense_rank(def_passing_downs_ppa_PY2),
           Rank_Def_Pass_Down_Success_Rt_PY2 = dense_rank(def_passing_downs_success_rate_PY2),
           Rank_Def_Pass_Down_Explosiveness_PY2 = dense_rank(def_passing_downs_explosiveness_PY2),
           Rank_Def_Rush_Play_PPA_PY2 = dense_rank(def_rushing_plays_ppa_PY2),
           Rank_Def_Rush_Play_Success_Rt_PY2 = dense_rank(def_rushing_plays_success_rate_PY2),
           Rank_Def_Rush_Play_Explosiveness_PY2 = dense_rank(def_rushing_plays_explosiveness_PY2),
           Rank_Def_Pass_Play_PPA_PY2 = dense_rank(def_passing_plays_ppa_PY2),
           Rank_Def_Pass_Play_Success_Rt_PY2 = dense_rank(def_passing_plays_success_rate_PY2),
           Rank_Def_Pass_Play_Explosiveness_PY2 = dense_rank(def_passing_plays_explosiveness_PY2),
           Rank_Recruit_Pts_PY2 = dense_rank(desc(recruit_pts_PY2)),
           Rank_Talent_PY2 = dense_rank(desc(talent_PY2)),
           Rank_SP_Rating_PY2 = dense_rank(desc(sp_rating_PY2)),
           Rank_SP_Off_Rating_PY2 = dense_rank(desc(sp_offense_rating_PY2)),
           Rank_SP_Def_Rating_PY2 = dense_rank(sp_defense_rating_PY2),
           Rank_SP_SpecialTeams_Rating_PY2 = dense_rank(desc(sp_special_teams_rating_PY2)),
           Rank_FPI_PY2 = dense_rank(desc(FPI_PY2)),
           Rank_SRS_rating_PY2 = dense_rank(desc(SRS_rating_PY2)),
           ## PY2 weighted twice
           Rank_Wins_PY2_col2 = dense_rank(desc(Wins_PY2)),
           Rank_Losses_PY2_col2 = dense_rank(Losses_PY2),
           Rank_Comp_Pct_PY2_col2 = dense_rank(desc(completion_pct_PY2)),
           Rank_Pass_YPA_PY2_col2 = dense_rank(desc(pass_ypa_PY2)),
           Rank_Pass_YPR_PY2_col2 = dense_rank(desc(pass_ypr_PY2)),
           Rank_Int_Pct_PY2_col2 = dense_rank(int_pct_PY2),
           Rank_Rush_YPC_PY2_col2 = dense_rank(desc(rush_ypc_PY2)),
           Rank_Turnovers_pg_PY2_col2 = dense_rank(turnovers_pg_PY2),
           Rank_Third_Conv_Rate_PY2_col2 = dense_rank(desc(third_conv_rate_PY2)),
           Rank_Fourth_Conv_Rate_PY2_col2 = dense_rank(desc(fourth_conv_rate_PY2)),
           Rank_Penalty_Yds_pg_PY2_col2 = dense_rank(penalty_yds_pg_PY2),
           Rank_Yds_Per_Penalty_PY2_col2 = dense_rank(yards_per_penalty_PY2),
           Rank_Kick_Return_Avg_PY2_col2 = dense_rank(desc(kick_return_avg_PY2)),
           Rank_Punt_Return_Avg_PY2_col2 = dense_rank(desc(punt_return_avg_PY2)),
           Rank_Total_Yds_pg_PY2_col2 = dense_rank(desc(total_yds_pg_PY2)),
           Rank_Pass_Yds_pg_PY2_col2 = dense_rank(desc(pass_yds_pg_PY2)),
           Rank_Rush_Yds_pg_PY2_col2 = dense_rank(desc(rush_yds_pg_PY2)),
           Rank_First_Downs_pg_PY2_col2 = dense_rank(desc(first_downs_pg_PY2)),
           Rank_Off_YPP_PY2_col2 = dense_rank(desc(off_ypp_PY2)),
           Rank_Def_Ints_pg_PY2_col2 = dense_rank(desc(def_interceptions_pg_PY2)),
           Rank_Off_PPA_PY2_col2 = dense_rank(desc(off_ppa_PY2)),
           Rank_Off_Success_Rt_PY2_col2 = dense_rank(desc(off_success_rate_PY2)),
           Rank_Off_Explosiveness_PY2_col2 = dense_rank(desc(off_explosiveness_PY2)),
           Rank_Off_Pwr_Success_PY2_col2 = dense_rank(desc(off_power_success_PY2)),
           Rank_Off_Stuff_Rt_PY2_col2 = dense_rank(off_stuff_rate_PY2),
           Rank_Off_Line_Yds_PY2_col2 = dense_rank(desc(off_line_yds_PY2)),
           Rank_Off_Second_Lvl_Yds_PY2_col2 = dense_rank(desc(off_second_lvl_yds_PY2)),
           Rank_Off_Open_Field_Yds_PY2_col2 = dense_rank(desc(off_open_field_yds_PY2)),
           Rank_Off_Pts_Per_Opp_PY2_col2 = dense_rank(desc(off_pts_per_opp_PY2)),
           Rank_Off_Field_Pos_Avg_Predicted_Pts_PY2_col2 = dense_rank(desc(off_field_pos_avg_predicted_points_PY2)),
           Rank_Off_Havoc_Total_PY2_col2 = dense_rank(off_havoc_total_PY2),
           Rank_Off_Havoc_Front_PY2_col2 = dense_rank(off_havoc_front_seven_PY2),
           Rank_Off_Havoc_DB_PY2_col2 = dense_rank(off_havoc_db_PY2),
           Rank_Off_Standard_Down_PPA_PY2_col2 = dense_rank(desc(off_standard_downs_ppa_PY2)),
           Rank_Off_Standard_Down_Success_Rt_PY2_col2 = dense_rank(desc(off_standard_downs_success_rate_PY2)),
           Rank_Off_Standard_Down_Explosiveness_PY2_col2 = dense_rank(desc(off_standard_downs_explosiveness_PY2)),
           Rank_Off_Pass_Down_PPA_PY2_col2 = dense_rank(desc(off_passing_downs_ppa_PY2)),
           Rank_Off_Pass_Down_Success_Rt_PY2_col2 = dense_rank(desc(off_passing_downs_success_rate_PY2)),
           Rank_Off_Pass_Down_Explosiveness_PY2_col2 = dense_rank(desc(off_passing_downs_explosiveness_PY2)),
           Rank_Off_Rush_Play_PPA_PY2_col2 = dense_rank(desc(off_rushing_plays_ppa_PY2)),
           Rank_Off_Rush_Play_Success_Rt_PY2_col2 = dense_rank(desc(off_rushing_plays_success_rate_PY2)),
           Rank_Off_Rush_Play_Explosiveness_PY2_col2 = dense_rank(desc(off_rushing_plays_explosiveness_PY2)),
           Rank_Off_Pass_Play_PPA_PY2_col2 = dense_rank(desc(off_passing_plays_ppa_PY2)),
           Rank_Off_Pass_Play_Success_Rt_PY2_col2 = dense_rank(desc(off_passing_plays_success_rate_PY2)),
           Rank_Off_Pass_Play_Explosiveness_PY2_col2 = dense_rank(desc(off_passing_plays_explosiveness_PY2)),
           Rank_Def_PPA_PY2_col2 = dense_rank(def_ppa_PY2),
           Rank_Def_Success_Rt_PY2_col2 = dense_rank(def_success_rate_PY2),
           Rank_Def_Explosiveness_PY2_col2 = dense_rank(def_explosiveness_PY2),
           Rank_Def_Pwr_Success_PY2_col2 = dense_rank(def_power_success_PY2),
           Rank_Def_Stuff_Rt_PY2_col2 = dense_rank(desc(def_stuff_rate_PY2)),
           Rank_Def_Line_Yds_PY2_col2 = dense_rank(def_line_yds_PY2),
           Rank_Def_Second_Lvl_Yds_PY2_col2 = dense_rank(def_second_lvl_yds_PY2),
           Rank_Def_Open_Field_Yds_PY2_col2 = dense_rank(def_open_field_yds_PY2),
           Rank_Def_Pts_Per_Opp_PY2_col2 = dense_rank(def_pts_per_opp_PY2),
           Rank_Def_Field_Pos_Avg_Predicted_Pts_PY2_col2 = dense_rank(def_field_pos_avg_predicted_points_PY2),
           Rank_Def_Havoc_Total_PY2_col2 = dense_rank(desc(def_havoc_total_PY2)),
           Rank_Def_Havoc_Front_Seven_PY2_col2 = dense_rank(desc(def_havoc_front_seven_PY2)),
           Rank_Def_Havoc_DB_PY2_col2 = dense_rank(desc(def_havoc_db_PY2)),
           Rank_Def_Standard_Down_PPA_PY2_col2 = dense_rank(def_standard_downs_ppa_PY2),
           Rank_Def_Standard_Down_Success_Rt_PY2_col2 = dense_rank(def_standard_downs_success_rate_PY2),
           Rank_Def_Standard_Down_Explosiveness_PY2_col2 = dense_rank(def_standard_downs_explosiveness_PY2),
           Rank_Def_Pass_Down_PPA_PY2_col2 = dense_rank(def_passing_downs_ppa_PY2),
           Rank_Def_Pass_Down_Success_Rt_PY2_col2 = dense_rank(def_passing_downs_success_rate_PY2),
           Rank_Def_Pass_Down_Explosiveness_PY2_col2 = dense_rank(def_passing_downs_explosiveness_PY2),
           Rank_Def_Rush_Play_PPA_PY2_col2 = dense_rank(def_rushing_plays_ppa_PY2),
           Rank_Def_Rush_Play_Success_Rt_PY2_col2 = dense_rank(def_rushing_plays_success_rate_PY2),
           Rank_Def_Rush_Play_Explosiveness_PY2_col2 = dense_rank(def_rushing_plays_explosiveness_PY2),
           Rank_Def_Pass_Play_PPA_PY2_col2 = dense_rank(def_passing_plays_ppa_PY2),
           Rank_Def_Pass_Play_Success_Rt_PY2_col2 = dense_rank(def_passing_plays_success_rate_PY2),
           Rank_Def_Pass_Play_Explosiveness_PY2_col2 = dense_rank(def_passing_plays_explosiveness_PY2),
           Rank_Recruit_Pts_PY2_col2 = dense_rank(desc(recruit_pts_PY2)),
           ## PY2 extra weighted variables
           Rank_Off_YPP_PY2_col3 = dense_rank(desc(off_ypp_PY2)),
           Rank_Off_PPA_PY2_col3 = dense_rank(desc(off_ppa_PY2)),
           Rank_Off_Success_Rt_PY2_col3 = dense_rank(desc(off_success_rate_PY2)),
           Rank_Off_Explosiveness_PY2_col3 = dense_rank(desc(off_explosiveness_PY2)),
           Rank_Off_Pwr_Success_PY2_col3 = dense_rank(desc(off_power_success_PY2)),
           Rank_Off_Stuff_Rt_PY2_col3 = dense_rank(off_stuff_rate_PY2),
           Rank_Off_Pts_Per_Opp_PY2_col3 = dense_rank(desc(off_pts_per_opp_PY2)),
           Rank_Off_Havoc_Total_PY2_col3 = dense_rank(off_havoc_total_PY2),
           Rank_Off_Havoc_Front_PY2_col3 = dense_rank(off_havoc_front_seven_PY2),
           Rank_Off_Havoc_DB_PY2_col3 = dense_rank(off_havoc_db_PY2),
           Rank_Off_Standard_Down_PPA_PY2_col3 = dense_rank(desc(off_standard_downs_ppa_PY2)),
           Rank_Off_Standard_Down_Success_Rt_PY2_col3 = dense_rank(desc(off_standard_downs_success_rate_PY2)),
           Rank_Off_Standard_Down_Explosiveness_PY2_col3 = dense_rank(desc(off_standard_downs_explosiveness_PY2)),
           Rank_Off_Pass_Down_PPA_PY2_col3 = dense_rank(desc(off_passing_downs_ppa_PY2)),
           Rank_Off_Pass_Down_Success_Rt_PY2_col3 = dense_rank(desc(off_passing_downs_success_rate_PY2)),
           Rank_Off_Pass_Down_Explosiveness_PY2_col3 = dense_rank(desc(off_passing_downs_explosiveness_PY2)),
           Rank_Off_Rush_Play_PPA_PY2_col3 = dense_rank(desc(off_rushing_plays_ppa_PY2)),
           Rank_Off_Rush_Play_Success_Rt_PY2_col3 = dense_rank(desc(off_rushing_plays_success_rate_PY2)),
           Rank_Off_Rush_Play_Explosiveness_PY2_col3 = dense_rank(desc(off_rushing_plays_explosiveness_PY2)),
           Rank_Off_Pass_Play_PPA_PY2_col3 = dense_rank(desc(off_passing_plays_ppa_PY2)),
           Rank_Off_Pass_Play_Success_Rt_PY2_col3 = dense_rank(desc(off_passing_plays_success_rate_PY2)),
           Rank_Off_Pass_Play_Explosiveness_PY2_col3 = dense_rank(desc(off_passing_plays_explosiveness_PY2)),
           Rank_Def_PPA_PY2_col3 = dense_rank(def_ppa_PY2),
           Rank_Def_Success_Rt_PY2_col3 = dense_rank(def_success_rate_PY2),
           Rank_Def_Explosiveness_PY2_col3 = dense_rank(def_explosiveness_PY2),
           Rank_Def_Pwr_Success_PY2_col3 = dense_rank(def_power_success_PY2),
           Rank_Def_Stuff_Rt_PY2_col3 = dense_rank(desc(def_stuff_rate_PY2)),
           Rank_Def_Pts_Per_Opp_PY2_col3 = dense_rank(def_pts_per_opp_PY2),
           Rank_Def_Havoc_Total_PY2_col3 = dense_rank(desc(def_havoc_total_PY2)),
           Rank_Def_Havoc_Front_Seven_PY2_col3 = dense_rank(desc(def_havoc_front_seven_PY2)),
           Rank_Def_Havoc_DB_PY2_col3 = dense_rank(desc(def_havoc_db_PY2)),
           Rank_Def_Standard_Down_PPA_PY2_col3 = dense_rank(def_standard_downs_ppa_PY2),
           Rank_Def_Standard_Down_Success_Rt_PY2_col3 = dense_rank(def_standard_downs_success_rate_PY2),
           Rank_Def_Standard_Down_Explosiveness_PY2_col3 = dense_rank(def_standard_downs_explosiveness_PY2),
           Rank_Def_Pass_Down_PPA_PY2_col3 = dense_rank(def_passing_downs_ppa_PY2),
           Rank_Def_Pass_Down_Success_Rt_PY2_col3 = dense_rank(def_passing_downs_success_rate_PY2),
           Rank_Def_Pass_Down_Explosiveness_PY2_col3 = dense_rank(def_passing_downs_explosiveness_PY2),
           Rank_Def_Rush_Play_PPA_PY2_col3 = dense_rank(def_rushing_plays_ppa_PY2),
           Rank_Def_Rush_Play_Success_Rt_PY2_col3 = dense_rank(def_rushing_plays_success_rate_PY2),
           Rank_Def_Rush_Play_Explosiveness_PY2_col3 = dense_rank(def_rushing_plays_explosiveness_PY2),
           Rank_Def_Pass_Play_PPA_PY2_col3 = dense_rank(def_passing_plays_ppa_PY2),
           Rank_Def_Pass_Play_Success_Rt_PY2_col3 = dense_rank(def_passing_plays_success_rate_PY2),
           Rank_Def_Pass_Play_Explosiveness_PY2_col3 = dense_rank(def_passing_plays_explosiveness_PY2),
           Rank_Recruit_Pts_PY2_col3 = dense_rank(desc(recruit_pts_PY2)),
           ## PY2 extra weighted variables (2x)
           Rank_Off_YPP_PY2_col4 = dense_rank(desc(off_ypp_PY2)),
           Rank_Off_PPA_PY2_col4 = dense_rank(desc(off_ppa_PY2)),
           Rank_Off_Success_Rt_PY2_col4 = dense_rank(desc(off_success_rate_PY2)),
           Rank_Off_Explosiveness_PY2_col4 = dense_rank(desc(off_explosiveness_PY2)),
           Rank_Off_Pwr_Success_PY2_col4 = dense_rank(desc(off_power_success_PY2)),
           Rank_Off_Stuff_Rt_PY2_col4 = dense_rank(off_stuff_rate_PY2),
           Rank_Off_Pts_Per_Opp_PY2_col4 = dense_rank(desc(off_pts_per_opp_PY2)),
           Rank_Off_Havoc_Total_PY2_col4 = dense_rank(off_havoc_total_PY2),
           Rank_Off_Havoc_Front_PY2_col4 = dense_rank(off_havoc_front_seven_PY2),
           Rank_Off_Havoc_DB_PY2_col4 = dense_rank(off_havoc_db_PY2),
           Rank_Off_Standard_Down_PPA_PY2_col4 = dense_rank(desc(off_standard_downs_ppa_PY2)),
           Rank_Off_Standard_Down_Success_Rt_PY2_col4 = dense_rank(desc(off_standard_downs_success_rate_PY2)),
           Rank_Off_Standard_Down_Explosiveness_PY2_col4 = dense_rank(desc(off_standard_downs_explosiveness_PY2)),
           Rank_Off_Pass_Down_PPA_PY2_col4 = dense_rank(desc(off_passing_downs_ppa_PY2)),
           Rank_Off_Pass_Down_Success_Rt_PY2_col4 = dense_rank(desc(off_passing_downs_success_rate_PY2)),
           Rank_Off_Pass_Down_Explosiveness_PY2_col4 = dense_rank(desc(off_passing_downs_explosiveness_PY2)),
           Rank_Off_Rush_Play_PPA_PY2_col4 = dense_rank(desc(off_rushing_plays_ppa_PY2)),
           Rank_Off_Rush_Play_Success_Rt_PY2_col4 = dense_rank(desc(off_rushing_plays_success_rate_PY2)),
           Rank_Off_Rush_Play_Explosiveness_PY2_col4 = dense_rank(desc(off_rushing_plays_explosiveness_PY2)),
           Rank_Off_Pass_Play_PPA_PY2_col4 = dense_rank(desc(off_passing_plays_ppa_PY2)),
           Rank_Off_Pass_Play_Success_Rt_PY2_col4 = dense_rank(desc(off_passing_plays_success_rate_PY2)),
           Rank_Off_Pass_Play_Explosiveness_PY2_col4 = dense_rank(desc(off_passing_plays_explosiveness_PY2)),
           Rank_Def_PPA_PY2_col4 = dense_rank(def_ppa_PY2),
           Rank_Def_Success_Rt_PY2_col4 = dense_rank(def_success_rate_PY2),
           Rank_Def_Explosiveness_PY2_col4 = dense_rank(def_explosiveness_PY2),
           Rank_Def_Pwr_Success_PY2_col4 = dense_rank(def_power_success_PY2),
           Rank_Def_Stuff_Rt_PY2_col4 = dense_rank(desc(def_stuff_rate_PY2)),
           Rank_Def_Pts_Per_Opp_PY2_col4 = dense_rank(def_pts_per_opp_PY2),
           Rank_Def_Havoc_Total_PY2_col4 = dense_rank(desc(def_havoc_total_PY2)),
           Rank_Def_Havoc_Front_Seven_PY2_col4 = dense_rank(desc(def_havoc_front_seven_PY2)),
           Rank_Def_Havoc_DB_PY2_col4 = dense_rank(desc(def_havoc_db_PY2)),
           Rank_Def_Standard_Down_PPA_PY2_col4 = dense_rank(def_standard_downs_ppa_PY2),
           Rank_Def_Standard_Down_Success_Rt_PY2_col4 = dense_rank(def_standard_downs_success_rate_PY2),
           Rank_Def_Standard_Down_Explosiveness_PY2_col4 = dense_rank(def_standard_downs_explosiveness_PY2),
           Rank_Def_Pass_Down_PPA_PY2_col4 = dense_rank(def_passing_downs_ppa_PY2),
           Rank_Def_Pass_Down_Success_Rt_PY2_col4 = dense_rank(def_passing_downs_success_rate_PY2),
           Rank_Def_Pass_Down_Explosiveness_PY2_col4 = dense_rank(def_passing_downs_explosiveness_PY2),
           Rank_Def_Rush_Play_PPA_PY2_col4 = dense_rank(def_rushing_plays_ppa_PY2),
           Rank_Def_Rush_Play_Success_Rt_PY2_col4 = dense_rank(def_rushing_plays_success_rate_PY2),
           Rank_Def_Rush_Play_Explosiveness_PY2_col4 = dense_rank(def_rushing_plays_explosiveness_PY2),
           Rank_Def_Pass_Play_PPA_PY2_col4 = dense_rank(def_passing_plays_ppa_PY2),
           Rank_Def_Pass_Play_Success_Rt_PY2_col4 = dense_rank(def_passing_plays_success_rate_PY2),
           Rank_Def_Pass_Play_Explosiveness_PY2_col4 = dense_rank(def_passing_plays_explosiveness_PY2),
           ## PY1 ranks
           Rank_Wins_PY1 = dense_rank(desc(Wins_PY1)),
           Rank_Losses_PY1 = dense_rank(Losses_PY1),
           Rank_Comp_Pct_PY1 = dense_rank(desc(completion_pct_PY1)),
           Rank_Pass_YPA_PY1 = dense_rank(desc(pass_ypa_PY1)),
           Rank_Pass_YPR_PY1 = dense_rank(desc(pass_ypr_PY1)),
           Rank_Int_Pct_PY1 = dense_rank(int_pct_PY1),
           Rank_Rush_YPC_PY1 = dense_rank(desc(rush_ypc_PY1)),
           Rank_Turnovers_pg_PY1 = dense_rank(turnovers_pg_PY1),
           Rank_Third_Conv_Rate_PY1 = dense_rank(desc(third_conv_rate_PY1)),
           Rank_Fourth_Conv_Rate_PY1 = dense_rank(desc(fourth_conv_rate_PY1)),
           Rank_Penalty_Yds_pg_PY1 = dense_rank(penalty_yds_pg_PY1),
           Rank_Yds_Per_Penalty_PY1 = dense_rank(yards_per_penalty_PY1),
           Rank_Kick_Return_Avg_PY1 = dense_rank(desc(kick_return_avg_PY1)),
           Rank_Punt_Return_Avg_PY1 = dense_rank(desc(punt_return_avg_PY1)),
           Rank_Total_Yds_pg_PY1 = dense_rank(desc(total_yds_pg_PY1)),
           Rank_Pass_Yds_pg_PY1 = dense_rank(desc(pass_yds_pg_PY1)),
           Rank_Rush_Yds_pg_PY1 = dense_rank(desc(rush_yds_pg_PY1)),
           Rank_First_Downs_pg_PY1 = dense_rank(desc(first_downs_pg_PY1)),
           Rank_Off_YPP_PY1 = dense_rank(desc(off_ypp_PY1)),
           Rank_Def_Ints_pg_PY1 = dense_rank(desc(def_interceptions_pg_PY1)),
           Rank_Off_PPA_PY1 = dense_rank(desc(off_ppa_PY1)),
           Rank_Off_Success_Rt_PY1 = dense_rank(desc(off_success_rate_PY1)),
           Rank_Off_Explosiveness_PY1 = dense_rank(desc(off_explosiveness_PY1)),
           Rank_Off_Pwr_Success_PY1 = dense_rank(desc(off_power_success_PY1)),
           Rank_Off_Stuff_Rt_PY1 = dense_rank(off_stuff_rate_PY1),
           Rank_Off_Line_Yds_PY1 = dense_rank(desc(off_line_yds_PY1)),
           Rank_Off_Second_Lvl_Yds_PY1 = dense_rank(desc(off_second_lvl_yds_PY1)),
           Rank_Off_Open_Field_Yds_PY1 = dense_rank(desc(off_open_field_yds_PY1)),
           Rank_Off_Pts_Per_Opp_PY1 = dense_rank(desc(off_pts_per_opp_PY1)),
           Rank_Off_Field_Pos_Avg_Predicted_Pts_PY1 = dense_rank(desc(off_field_pos_avg_predicted_points_PY1)),
           Rank_Off_Havoc_Total_PY1 = dense_rank(off_havoc_total_PY1),
           Rank_Off_Havoc_Front_PY1 = dense_rank(off_havoc_front_seven_PY1),
           Rank_Off_Havoc_DB_PY1 = dense_rank(off_havoc_db_PY1),
           Rank_Off_Standard_Down_PPA_PY1 = dense_rank(desc(off_standard_downs_ppa_PY1)),
           Rank_Off_Standard_Down_Success_Rt_PY1 = dense_rank(desc(off_standard_downs_success_rate_PY1)),
           Rank_Off_Standard_Down_Explosiveness_PY1 = dense_rank(desc(off_standard_downs_explosiveness_PY1)),
           Rank_Off_Pass_Down_PPA_PY1 = dense_rank(desc(off_passing_downs_ppa_PY1)),
           Rank_Off_Pass_Down_Success_Rt_PY1 = dense_rank(desc(off_passing_downs_success_rate_PY1)),
           Rank_Off_Pass_Down_Explosiveness_PY1 = dense_rank(desc(off_passing_downs_explosiveness_PY1)),
           Rank_Off_Rush_Play_PPA_PY1 = dense_rank(desc(off_rushing_plays_ppa_PY1)),
           Rank_Off_Rush_Play_Success_Rt_PY1 = dense_rank(desc(off_rushing_plays_success_rate_PY1)),
           Rank_Off_Rush_Play_Explosiveness_PY1 = dense_rank(desc(off_rushing_plays_explosiveness_PY1)),
           Rank_Off_Pass_Play_PPA_PY1 = dense_rank(desc(off_passing_plays_ppa_PY1)),
           Rank_Off_Pass_Play_Success_Rt_PY1 = dense_rank(desc(off_passing_plays_success_rate_PY1)),
           Rank_Off_Pass_Play_Explosiveness_PY1 = dense_rank(desc(off_passing_plays_explosiveness_PY1)),
           Rank_Def_PPA_PY1 = dense_rank(def_ppa_PY1),
           Rank_Def_Success_Rt_PY1 = dense_rank(def_success_rate_PY1),
           Rank_Def_Explosiveness_PY1 = dense_rank(def_explosiveness_PY1),
           Rank_Def_Pwr_Success_PY1 = dense_rank(def_power_success_PY1),
           Rank_Def_Stuff_Rt_PY1 = dense_rank(desc(def_stuff_rate_PY1)),
           Rank_Def_Line_Yds_PY1 = dense_rank(def_line_yds_PY1),
           Rank_Def_Second_Lvl_Yds_PY1 = dense_rank(def_second_lvl_yds_PY1),
           Rank_Def_Open_Field_Yds_PY1 = dense_rank(def_open_field_yds_PY1),
           Rank_Def_Pts_Per_Opp_PY1 = dense_rank(def_pts_per_opp_PY1),
           Rank_Def_Field_Pos_Avg_Predicted_Pts_PY1 = dense_rank(def_field_pos_avg_predicted_points_PY1),
           Rank_Def_Havoc_Total_PY1 = dense_rank(desc(def_havoc_total_PY1)),
           Rank_Def_Havoc_Front_Seven_PY1 = dense_rank(desc(def_havoc_front_seven_PY1)),
           Rank_Def_Havoc_DB_PY1 = dense_rank(desc(def_havoc_db_PY1)),
           Rank_Def_Standard_Down_PPA_PY1 = dense_rank(def_standard_downs_ppa_PY1),
           Rank_Def_Standard_Down_Success_Rt_PY1 = dense_rank(def_standard_downs_success_rate_PY1),
           Rank_Def_Standard_Down_Explosiveness_PY1 = dense_rank(def_standard_downs_explosiveness_PY1),
           Rank_Def_Pass_Down_PPA_PY1 = dense_rank(def_passing_downs_ppa_PY1),
           Rank_Def_Pass_Down_Success_Rt_PY1 = dense_rank(def_passing_downs_success_rate_PY1),
           Rank_Def_Pass_Down_Explosiveness_PY1 = dense_rank(def_passing_downs_explosiveness_PY1),
           Rank_Def_Rush_Play_PPA_PY1 = dense_rank(def_rushing_plays_ppa_PY1),
           Rank_Def_Rush_Play_Success_Rt_PY1 = dense_rank(def_rushing_plays_success_rate_PY1),
           Rank_Def_Rush_Play_Explosiveness_PY1 = dense_rank(def_rushing_plays_explosiveness_PY1),
           Rank_Def_Pass_Play_PPA_PY1 = dense_rank(def_passing_plays_ppa_PY1),
           Rank_Def_Pass_Play_Success_Rt_PY1 = dense_rank(def_passing_plays_success_rate_PY1),
           Rank_Def_Pass_Play_Explosiveness_PY1 = dense_rank(def_passing_plays_explosiveness_PY1),
           Rank_Recruit_Pts_PY1 = dense_rank(desc(recruit_pts_PY1)),
           Rank_Talent_PY1 = dense_rank(desc(talent_PY1)),
           Rank_SP_Rating_PY1 = dense_rank(desc(sp_rating_PY1)),
           Rank_SP_Off_Rating_PY1 = dense_rank(desc(sp_offense_rating_PY1)),
           Rank_SP_Def_Rating_PY1 = dense_rank(sp_defense_rating_PY1),
           Rank_SP_SpecialTeams_Rating_PY1 = dense_rank(desc(sp_special_teams_rating_PY1)),
           Rank_FPI_PY1 = dense_rank(desc(FPI_PY1)),
           Rank_SRS_rating_PY1 = dense_rank(desc(SRS_rating_PY1)),
           ## PY1 weighted 3 times
           Rank_Wins_PY1_col2 = dense_rank(desc(Wins_PY1)),
           Rank_Losses_PY1_col2 = dense_rank(Losses_PY1),
           Rank_Comp_Pct_PY1_col2 = dense_rank(desc(completion_pct_PY1)),
           Rank_Pass_YPA_PY1_col2 = dense_rank(desc(pass_ypa_PY1)),
           Rank_Pass_YPR_PY1_col2 = dense_rank(desc(pass_ypr_PY1)),
           Rank_Int_Pct_PY1_col2 = dense_rank(int_pct_PY1),
           Rank_Rush_YPC_PY1_col2 = dense_rank(desc(rush_ypc_PY1)),
           Rank_Turnovers_pg_PY1_col2 = dense_rank(turnovers_pg_PY1),
           Rank_Third_Conv_Rate_PY1_col2 = dense_rank(desc(third_conv_rate_PY1)),
           Rank_Fourth_Conv_Rate_PY1_col2 = dense_rank(desc(fourth_conv_rate_PY1)),
           Rank_Penalty_Yds_pg_PY1_col2 = dense_rank(penalty_yds_pg_PY1),
           Rank_Yds_Per_Penalty_PY1_col2 = dense_rank(yards_per_penalty_PY1),
           Rank_Kick_Return_Avg_PY1_col2 = dense_rank(desc(kick_return_avg_PY1)),
           Rank_Punt_Return_Avg_PY1_col2 = dense_rank(desc(punt_return_avg_PY1)),
           Rank_Total_Yds_pg_PY1_col2 = dense_rank(desc(total_yds_pg_PY1)),
           Rank_Pass_Yds_pg_PY1_col2 = dense_rank(desc(pass_yds_pg_PY1)),
           Rank_Rush_Yds_pg_PY1_col2 = dense_rank(desc(rush_yds_pg_PY1)),
           Rank_First_Downs_pg_PY1_col2 = dense_rank(desc(first_downs_pg_PY1)),
           Rank_Off_YPP_PY1_col2 = dense_rank(desc(off_ypp_PY1)),
           Rank_Def_Ints_pg_PY1_col2 = dense_rank(desc(def_interceptions_pg_PY1)),
           Rank_Off_PPA_PY1_col2 = dense_rank(desc(off_ppa_PY1)),
           Rank_Off_Success_Rt_PY1_col2 = dense_rank(desc(off_success_rate_PY1)),
           Rank_Off_Explosiveness_PY1_col2 = dense_rank(desc(off_explosiveness_PY1)),
           Rank_Off_Pwr_Success_PY1_col2 = dense_rank(desc(off_power_success_PY1)),
           Rank_Off_Stuff_Rt_PY1_col2 = dense_rank(off_stuff_rate_PY1),
           Rank_Off_Line_Yds_PY1_col2 = dense_rank(desc(off_line_yds_PY1)),
           Rank_Off_Second_Lvl_Yds_PY1_col2 = dense_rank(desc(off_second_lvl_yds_PY1)),
           Rank_Off_Open_Field_Yds_PY1_col2 = dense_rank(desc(off_open_field_yds_PY1)),
           Rank_Off_Pts_Per_Opp_PY1_col2 = dense_rank(desc(off_pts_per_opp_PY1)),
           Rank_Off_Field_Pos_Avg_Predicted_Pts_PY1_col2 = dense_rank(desc(off_field_pos_avg_predicted_points_PY1)),
           Rank_Off_Havoc_Total_PY1_col2 = dense_rank(off_havoc_total_PY1),
           Rank_Off_Havoc_Front_PY1_col2 = dense_rank(off_havoc_front_seven_PY1),
           Rank_Off_Havoc_DB_PY1_col2 = dense_rank(off_havoc_db_PY1),
           Rank_Off_Standard_Down_PPA_PY1_col2 = dense_rank(desc(off_standard_downs_ppa_PY1)),
           Rank_Off_Standard_Down_Success_Rt_PY1_col2 = dense_rank(desc(off_standard_downs_success_rate_PY1)),
           Rank_Off_Standard_Down_Explosiveness_PY1_col2 = dense_rank(desc(off_standard_downs_explosiveness_PY1)),
           Rank_Off_Pass_Down_PPA_PY1_col2 = dense_rank(desc(off_passing_downs_ppa_PY1)),
           Rank_Off_Pass_Down_Success_Rt_PY1_col2 = dense_rank(desc(off_passing_downs_success_rate_PY1)),
           Rank_Off_Pass_Down_Explosiveness_PY1_col2 = dense_rank(desc(off_passing_downs_explosiveness_PY1)),
           Rank_Off_Rush_Play_PPA_PY1_col2 = dense_rank(desc(off_rushing_plays_ppa_PY1)),
           Rank_Off_Rush_Play_Success_Rt_PY1_col2 = dense_rank(desc(off_rushing_plays_success_rate_PY1)),
           Rank_Off_Rush_Play_Explosiveness_PY1_col2 = dense_rank(desc(off_rushing_plays_explosiveness_PY1)),
           Rank_Off_Pass_Play_PPA_PY1_col2 = dense_rank(desc(off_passing_plays_ppa_PY1)),
           Rank_Off_Pass_Play_Success_Rt_PY1_col2 = dense_rank(desc(off_passing_plays_success_rate_PY1)),
           Rank_Off_Pass_Play_Explosiveness_PY1_col2 = dense_rank(desc(off_passing_plays_explosiveness_PY1)),
           Rank_Def_PPA_PY1_col2 = dense_rank(def_ppa_PY1),
           Rank_Def_Success_Rt_PY1_col2 = dense_rank(def_success_rate_PY1),
           Rank_Def_Explosiveness_PY1_col2 = dense_rank(def_explosiveness_PY1),
           Rank_Def_Pwr_Success_PY1_col2 = dense_rank(def_power_success_PY1),
           Rank_Def_Stuff_Rt_PY1_col2 = dense_rank(desc(def_stuff_rate_PY1)),
           Rank_Def_Line_Yds_PY1_col2 = dense_rank(def_line_yds_PY1),
           Rank_Def_Second_Lvl_Yds_PY1_col2 = dense_rank(def_second_lvl_yds_PY1),
           Rank_Def_Open_Field_Yds_PY1_col2 = dense_rank(def_open_field_yds_PY1),
           Rank_Def_Pts_Per_Opp_PY1_col2 = dense_rank(def_pts_per_opp_PY1),
           Rank_Def_Field_Pos_Avg_Predicted_Pts_PY1_col2 = dense_rank(def_field_pos_avg_predicted_points_PY1),
           Rank_Def_Havoc_Total_PY1_col2 = dense_rank(desc(def_havoc_total_PY1)),
           Rank_Def_Havoc_Front_Seven_PY1_col2 = dense_rank(desc(def_havoc_front_seven_PY1)),
           Rank_Def_Havoc_DB_PY1_col2 = dense_rank(desc(def_havoc_db_PY1)),
           Rank_Def_Standard_Down_PPA_PY1_col2 = dense_rank(def_standard_downs_ppa_PY1),
           Rank_Def_Standard_Down_Success_Rt_PY1_col2 = dense_rank(def_standard_downs_success_rate_PY1),
           Rank_Def_Standard_Down_Explosiveness_PY1_col2 = dense_rank(def_standard_downs_explosiveness_PY1),
           Rank_Def_Pass_Down_PPA_PY1_col2 = dense_rank(def_passing_downs_ppa_PY1),
           Rank_Def_Pass_Down_Success_Rt_PY1_col2 = dense_rank(def_passing_downs_success_rate_PY1),
           Rank_Def_Pass_Down_Explosiveness_PY1_col2 = dense_rank(def_passing_downs_explosiveness_PY1),
           Rank_Def_Rush_Play_PPA_PY1_col2 = dense_rank(def_rushing_plays_ppa_PY1),
           Rank_Def_Rush_Play_Success_Rt_PY1_col2 = dense_rank(def_rushing_plays_success_rate_PY1),
           Rank_Def_Rush_Play_Explosiveness_PY1_col2 = dense_rank(def_rushing_plays_explosiveness_PY1),
           Rank_Def_Pass_Play_PPA_PY1_col2 = dense_rank(def_passing_plays_ppa_PY1),
           Rank_Def_Pass_Play_Success_Rt_PY1_col2 = dense_rank(def_passing_plays_success_rate_PY1),
           Rank_Def_Pass_Play_Explosiveness_PY1_col2 = dense_rank(def_passing_plays_explosiveness_PY1),
           Rank_Recruit_Pts_PY1_col2 = dense_rank(desc(recruit_pts_PY1)),
           Rank_Talent_PY1_col2 = dense_rank(desc(talent_PY1)),
           ## PY1 weighted 3 times
           Rank_Wins_PY1_col3 = dense_rank(desc(Wins_PY1)),
           Rank_Losses_PY1_col3 = dense_rank(Losses_PY1),
           Rank_Comp_Pct_PY1_col3 = dense_rank(desc(completion_pct_PY1)),
           Rank_Pass_YPA_PY1_col3 = dense_rank(desc(pass_ypa_PY1)),
           Rank_Pass_YPR_PY1_col3 = dense_rank(desc(pass_ypr_PY1)),
           Rank_Int_Pct_PY1_col3 = dense_rank(int_pct_PY1),
           Rank_Rush_YPC_PY1_col3 = dense_rank(desc(rush_ypc_PY1)),
           Rank_Turnovers_pg_PY1_col3 = dense_rank(turnovers_pg_PY1),
           Rank_Third_Conv_Rate_PY1_col3 = dense_rank(desc(third_conv_rate_PY1)),
           Rank_Fourth_Conv_Rate_PY1_col3 = dense_rank(desc(fourth_conv_rate_PY1)),
           Rank_Penalty_Yds_pg_PY1_col3 = dense_rank(penalty_yds_pg_PY1),
           Rank_Yds_Per_Penalty_PY1_col3 = dense_rank(yards_per_penalty_PY1),
           Rank_Kick_Return_Avg_PY1_col3 = dense_rank(desc(kick_return_avg_PY1)),
           Rank_Punt_Return_Avg_PY1_col3 = dense_rank(desc(punt_return_avg_PY1)),
           Rank_Total_Yds_pg_PY1_col3 = dense_rank(desc(total_yds_pg_PY1)),
           Rank_Pass_Yds_pg_PY1_col3 = dense_rank(desc(pass_yds_pg_PY1)),
           Rank_Rush_Yds_pg_PY1_col3 = dense_rank(desc(rush_yds_pg_PY1)),
           Rank_First_Downs_pg_PY1_col3 = dense_rank(desc(first_downs_pg_PY1)),
           Rank_Off_YPP_PY1_col3 = dense_rank(desc(off_ypp_PY1)),
           Rank_Def_Ints_pg_PY1_col3 = dense_rank(desc(def_interceptions_pg_PY1)),
           Rank_Off_PPA_PY1_col3 = dense_rank(desc(off_ppa_PY1)),
           Rank_Off_Success_Rt_PY1_col3 = dense_rank(desc(off_success_rate_PY1)),
           Rank_Off_Explosiveness_PY1_col3 = dense_rank(desc(off_explosiveness_PY1)),
           Rank_Off_Pwr_Success_PY1_col3 = dense_rank(desc(off_power_success_PY1)),
           Rank_Off_Stuff_Rt_PY1_col3 = dense_rank(off_stuff_rate_PY1),
           Rank_Off_Line_Yds_PY1_col3 = dense_rank(desc(off_line_yds_PY1)),
           Rank_Off_Second_Lvl_Yds_PY1_col3 = dense_rank(desc(off_second_lvl_yds_PY1)),
           Rank_Off_Open_Field_Yds_PY1_col3 = dense_rank(desc(off_open_field_yds_PY1)),
           Rank_Off_Pts_Per_Opp_PY1_col3 = dense_rank(desc(off_pts_per_opp_PY1)),
           Rank_Off_Field_Pos_Avg_Predicted_Pts_PY1_col3 = dense_rank(desc(off_field_pos_avg_predicted_points_PY1)),
           Rank_Off_Havoc_Total_PY1_col3 = dense_rank(off_havoc_total_PY1),
           Rank_Off_Havoc_Front_PY1_col3 = dense_rank(off_havoc_front_seven_PY1),
           Rank_Off_Havoc_DB_PY1_col3 = dense_rank(off_havoc_db_PY1),
           Rank_Off_Standard_Down_PPA_PY1_col3 = dense_rank(desc(off_standard_downs_ppa_PY1)),
           Rank_Off_Standard_Down_Success_Rt_PY1_col3 = dense_rank(desc(off_standard_downs_success_rate_PY1)),
           Rank_Off_Standard_Down_Explosiveness_PY1_col3 = dense_rank(desc(off_standard_downs_explosiveness_PY1)),
           Rank_Off_Pass_Down_PPA_PY1_col3 = dense_rank(desc(off_passing_downs_ppa_PY1)),
           Rank_Off_Pass_Down_Success_Rt_PY1_col3 = dense_rank(desc(off_passing_downs_success_rate_PY1)),
           Rank_Off_Pass_Down_Explosiveness_PY1_col3 = dense_rank(desc(off_passing_downs_explosiveness_PY1)),
           Rank_Off_Rush_Play_PPA_PY1_col3 = dense_rank(desc(off_rushing_plays_ppa_PY1)),
           Rank_Off_Rush_Play_Success_Rt_PY1_col3 = dense_rank(desc(off_rushing_plays_success_rate_PY1)),
           Rank_Off_Rush_Play_Explosiveness_PY1_col3 = dense_rank(desc(off_rushing_plays_explosiveness_PY1)),
           Rank_Off_Pass_Play_PPA_PY1_col3 = dense_rank(desc(off_passing_plays_ppa_PY1)),
           Rank_Off_Pass_Play_Success_Rt_PY1_col3 = dense_rank(desc(off_passing_plays_success_rate_PY1)),
           Rank_Off_Pass_Play_Explosiveness_PY1_col3 = dense_rank(desc(off_passing_plays_explosiveness_PY1)),
           Rank_Def_PPA_PY1_col3 = dense_rank(def_ppa_PY1),
           Rank_Def_Success_Rt_PY1_col3 = dense_rank(def_success_rate_PY1),
           Rank_Def_Explosiveness_PY1_col3 = dense_rank(def_explosiveness_PY1),
           Rank_Def_Pwr_Success_PY1_col3 = dense_rank(def_power_success_PY1),
           Rank_Def_Stuff_Rt_PY1_col3 = dense_rank(desc(def_stuff_rate_PY1)),
           Rank_Def_Line_Yds_PY1_col3 = dense_rank(def_line_yds_PY1),
           Rank_Def_Second_Lvl_Yds_PY1_col3 = dense_rank(def_second_lvl_yds_PY1),
           Rank_Def_Open_Field_Yds_PY1_col3 = dense_rank(def_open_field_yds_PY1),
           Rank_Def_Pts_Per_Opp_PY1_col3 = dense_rank(def_pts_per_opp_PY1),
           Rank_Def_Field_Pos_Avg_Predicted_Pts_PY1_col3 = dense_rank(def_field_pos_avg_predicted_points_PY1),
           Rank_Def_Havoc_Total_PY1_col3 = dense_rank(desc(def_havoc_total_PY1)),
           Rank_Def_Havoc_Front_Seven_PY1_col3 = dense_rank(desc(def_havoc_front_seven_PY1)),
           Rank_Def_Havoc_DB_PY1_col3 = dense_rank(desc(def_havoc_db_PY1)),
           Rank_Def_Standard_Down_PPA_PY1_col3 = dense_rank(def_standard_downs_ppa_PY1),
           Rank_Def_Standard_Down_Success_Rt_PY1_col3 = dense_rank(def_standard_downs_success_rate_PY1),
           Rank_Def_Standard_Down_Explosiveness_PY1_col3 = dense_rank(def_standard_downs_explosiveness_PY1),
           Rank_Def_Pass_Down_PPA_PY1_col3 = dense_rank(def_passing_downs_ppa_PY1),
           Rank_Def_Pass_Down_Success_Rt_PY1_col3 = dense_rank(def_passing_downs_success_rate_PY1),
           Rank_Def_Pass_Down_Explosiveness_PY1_col3 = dense_rank(def_passing_downs_explosiveness_PY1),
           Rank_Def_Rush_Play_PPA_PY1_col3 = dense_rank(def_rushing_plays_ppa_PY1),
           Rank_Def_Rush_Play_Success_Rt_PY1_col3 = dense_rank(def_rushing_plays_success_rate_PY1),
           Rank_Def_Rush_Play_Explosiveness_PY1_col3 = dense_rank(def_rushing_plays_explosiveness_PY1),
           Rank_Def_Pass_Play_PPA_PY1_col3 = dense_rank(def_passing_plays_ppa_PY1),
           Rank_Def_Pass_Play_Success_Rt_PY1_col3 = dense_rank(def_passing_plays_success_rate_PY1),
           Rank_Def_Pass_Play_Explosiveness_PY1_col3 = dense_rank(def_passing_plays_explosiveness_PY1),
           Rank_Talent_PY1_col3 = dense_rank(desc(talent_PY1)),
           ## incoming recruiting class, weighted once
           Rank_Recruit_Pts = dense_rank(desc(recruit_pts)),
           ## Extra weighted variables, weighted 2x (3 more times)
           Rank_Off_YPP_PY1_col4 = dense_rank(desc(off_ypp_PY1)),
           Rank_Off_PPA_PY1_col4 = dense_rank(desc(off_ppa_PY1)),
           Rank_Off_Success_Rt_PY1_col4 = dense_rank(desc(off_success_rate_PY1)),
           Rank_Off_Explosiveness_PY1_col4 = dense_rank(desc(off_explosiveness_PY1)),
           Rank_Off_Pwr_Success_PY1_col4 = dense_rank(desc(off_power_success_PY1)),
           Rank_Off_Stuff_Rt_PY1_col4 = dense_rank(off_stuff_rate_PY1),
           Rank_Off_Pts_Per_Opp_PY1_col4 = dense_rank(desc(off_pts_per_opp_PY1)),
           Rank_Off_Havoc_Total_PY1_col4 = dense_rank(off_havoc_total_PY1),
           Rank_Off_Havoc_Front_PY1_col4 = dense_rank(off_havoc_front_seven_PY1),
           Rank_Off_Havoc_DB_PY1_col4 = dense_rank(off_havoc_db_PY1),
           Rank_Off_Standard_Down_PPA_PY1_col4 = dense_rank(desc(off_standard_downs_ppa_PY1)),
           Rank_Off_Standard_Down_Success_Rt_PY1_col4 = dense_rank(desc(off_standard_downs_success_rate_PY1)),
           Rank_Off_Standard_Down_Explosiveness_PY1_col4 = dense_rank(desc(off_standard_downs_explosiveness_PY1)),
           Rank_Off_Pass_Down_PPA_PY1_col4 = dense_rank(desc(off_passing_downs_ppa_PY1)),
           Rank_Off_Pass_Down_Success_Rt_PY1_col4 = dense_rank(desc(off_passing_downs_success_rate_PY1)),
           Rank_Off_Pass_Down_Explosiveness_PY1_col4 = dense_rank(desc(off_passing_downs_explosiveness_PY1)),
           Rank_Off_Rush_Play_PPA_PY1_col4 = dense_rank(desc(off_rushing_plays_ppa_PY1)),
           Rank_Off_Rush_Play_Success_Rt_PY1_col4 = dense_rank(desc(off_rushing_plays_success_rate_PY1)),
           Rank_Off_Rush_Play_Explosiveness_PY1_col4 = dense_rank(desc(off_rushing_plays_explosiveness_PY1)),
           Rank_Off_Pass_Play_PPA_PY1_col4 = dense_rank(desc(off_passing_plays_ppa_PY1)),
           Rank_Off_Pass_Play_Success_Rt_PY1_col4 = dense_rank(desc(off_passing_plays_success_rate_PY1)),
           Rank_Off_Pass_Play_Explosiveness_PY1_col4 = dense_rank(desc(off_passing_plays_explosiveness_PY1)),
           Rank_Def_PPA_PY1_col4 = dense_rank(def_ppa_PY1),
           Rank_Def_Success_Rt_PY1_col4 = dense_rank(def_success_rate_PY1),
           Rank_Def_Explosiveness_PY1_col4 = dense_rank(def_explosiveness_PY1),
           Rank_Def_Pwr_Success_PY1_col4 = dense_rank(def_power_success_PY1),
           Rank_Def_Stuff_Rt_PY1_col4 = dense_rank(desc(def_stuff_rate_PY1)),
           Rank_Def_Pts_Per_Opp_PY1_col4 = dense_rank(def_pts_per_opp_PY1),
           Rank_Def_Havoc_Total_PY1_col4 = dense_rank(desc(def_havoc_total_PY1)),
           Rank_Def_Havoc_Front_Seven_PY1_col4 = dense_rank(desc(def_havoc_front_seven_PY1)),
           Rank_Def_Havoc_DB_PY1_col4 = dense_rank(desc(def_havoc_db_PY1)),
           Rank_Def_Standard_Down_PPA_PY1_col4 = dense_rank(def_standard_downs_ppa_PY1),
           Rank_Def_Standard_Down_Success_Rt_PY1_col4 = dense_rank(def_standard_downs_success_rate_PY1),
           Rank_Def_Standard_Down_Explosiveness_PY1_col4 = dense_rank(def_standard_downs_explosiveness_PY1),
           Rank_Def_Pass_Down_PPA_PY1_col4 = dense_rank(def_passing_downs_ppa_PY1),
           Rank_Def_Pass_Down_Success_Rt_PY1_col4 = dense_rank(def_passing_downs_success_rate_PY1),
           Rank_Def_Pass_Down_Explosiveness_PY1_col4 = dense_rank(def_passing_downs_explosiveness_PY1),
           Rank_Def_Rush_Play_PPA_PY1_col4 = dense_rank(def_rushing_plays_ppa_PY1),
           Rank_Def_Rush_Play_Success_Rt_PY1_col4 = dense_rank(def_rushing_plays_success_rate_PY1),
           Rank_Def_Rush_Play_Explosiveness_PY1_col4 = dense_rank(def_rushing_plays_explosiveness_PY1),
           Rank_Def_Pass_Play_PPA_PY1_col4 = dense_rank(def_passing_plays_ppa_PY1),
           Rank_Def_Pass_Play_Success_Rt_PY1_col4 = dense_rank(def_passing_plays_success_rate_PY1),
           Rank_Def_Pass_Play_Explosiveness_PY1_col4 = dense_rank(def_passing_plays_explosiveness_PY1),
           ## Extra weighted variables, weighted 2x (2 more times)
           Rank_Off_YPP_PY1_col5 = dense_rank(desc(off_ypp_PY1)),
           Rank_Off_PPA_PY1_col5 = dense_rank(desc(off_ppa_PY1)),
           Rank_Off_Success_Rt_PY1_col5 = dense_rank(desc(off_success_rate_PY1)),
           Rank_Off_Explosiveness_PY1_col5 = dense_rank(desc(off_explosiveness_PY1)),
           Rank_Off_Pwr_Success_PY1_col5 = dense_rank(desc(off_power_success_PY1)),
           Rank_Off_Stuff_Rt_PY1_col5 = dense_rank(off_stuff_rate_PY1),
           Rank_Off_Pts_Per_Opp_PY1_col5 = dense_rank(desc(off_pts_per_opp_PY1)),
           Rank_Off_Havoc_Total_PY1_col5 = dense_rank(off_havoc_total_PY1),
           Rank_Off_Havoc_Front_PY1_col5 = dense_rank(off_havoc_front_seven_PY1),
           Rank_Off_Havoc_DB_PY1_col5 = dense_rank(off_havoc_db_PY1),
           Rank_Off_Standard_Down_PPA_PY1_col5 = dense_rank(desc(off_standard_downs_ppa_PY1)),
           Rank_Off_Standard_Down_Success_Rt_PY1_col5 = dense_rank(desc(off_standard_downs_success_rate_PY1)),
           Rank_Off_Standard_Down_Explosiveness_PY1_col5 = dense_rank(desc(off_standard_downs_explosiveness_PY1)),
           Rank_Off_Pass_Down_PPA_PY1_col5 = dense_rank(desc(off_passing_downs_ppa_PY1)),
           Rank_Off_Pass_Down_Success_Rt_PY1_col5 = dense_rank(desc(off_passing_downs_success_rate_PY1)),
           Rank_Off_Pass_Down_Explosiveness_PY1_col5 = dense_rank(desc(off_passing_downs_explosiveness_PY1)),
           Rank_Off_Rush_Play_PPA_PY1_col5 = dense_rank(desc(off_rushing_plays_ppa_PY1)),
           Rank_Off_Rush_Play_Success_Rt_PY1_col5 = dense_rank(desc(off_rushing_plays_success_rate_PY1)),
           Rank_Off_Rush_Play_Explosiveness_PY1_col5 = dense_rank(desc(off_rushing_plays_explosiveness_PY1)),
           Rank_Off_Pass_Play_PPA_PY1_col5 = dense_rank(desc(off_passing_plays_ppa_PY1)),
           Rank_Off_Pass_Play_Success_Rt_PY1_col5 = dense_rank(desc(off_passing_plays_success_rate_PY1)),
           Rank_Off_Pass_Play_Explosiveness_PY1_col5 = dense_rank(desc(off_passing_plays_explosiveness_PY1)),
           Rank_Def_PPA_PY1_col5 = dense_rank(def_ppa_PY1),
           Rank_Def_Success_Rt_PY1_col5 = dense_rank(def_success_rate_PY1),
           Rank_Def_Explosiveness_PY1_col5 = dense_rank(def_explosiveness_PY1),
           Rank_Def_Pwr_Success_PY1_col5 = dense_rank(def_power_success_PY1),
           Rank_Def_Stuff_Rt_PY1_col5 = dense_rank(desc(def_stuff_rate_PY1)),
           Rank_Def_Pts_Per_Opp_PY1_col5 = dense_rank(def_pts_per_opp_PY1),
           Rank_Def_Havoc_Total_PY1_col5 = dense_rank(desc(def_havoc_total_PY1)),
           Rank_Def_Havoc_Front_Seven_PY1_col5 = dense_rank(desc(def_havoc_front_seven_PY1)),
           Rank_Def_Havoc_DB_PY1_col5 = dense_rank(desc(def_havoc_db_PY1)),
           Rank_Def_Standard_Down_PPA_PY1_col5 = dense_rank(def_standard_downs_ppa_PY1),
           Rank_Def_Standard_Down_Success_Rt_PY1_col5 = dense_rank(def_standard_downs_success_rate_PY1),
           Rank_Def_Standard_Down_Explosiveness_PY1_col5 = dense_rank(def_standard_downs_explosiveness_PY1),
           Rank_Def_Pass_Down_PPA_PY1_col5 = dense_rank(def_passing_downs_ppa_PY1),
           Rank_Def_Pass_Down_Success_Rt_PY1_col5 = dense_rank(def_passing_downs_success_rate_PY1),
           Rank_Def_Pass_Down_Explosiveness_PY1_col5 = dense_rank(def_passing_downs_explosiveness_PY1),
           Rank_Def_Rush_Play_PPA_PY1_col5 = dense_rank(def_rushing_plays_ppa_PY1),
           Rank_Def_Rush_Play_Success_Rt_PY1_col5 = dense_rank(def_rushing_plays_success_rate_PY1),
           Rank_Def_Rush_Play_Explosiveness_PY1_col5 = dense_rank(def_rushing_plays_explosiveness_PY1),
           Rank_Def_Pass_Play_PPA_PY1_col5 = dense_rank(def_passing_plays_ppa_PY1),
           Rank_Def_Pass_Play_Success_Rt_PY1_col5 = dense_rank(def_passing_plays_success_rate_PY1),
           Rank_Def_Pass_Play_Explosiveness_PY1_col5 = dense_rank(def_passing_plays_explosiveness_PY1),
           ## Extra weighted variables, weighted 2x (1 more time)
           Rank_Off_YPP_PY1_col6 = dense_rank(desc(off_ypp_PY1)),
           Rank_Off_PPA_PY1_col6 = dense_rank(desc(off_ppa_PY1)),
           Rank_Off_Success_Rt_PY1_col6 = dense_rank(desc(off_success_rate_PY1)),
           Rank_Off_Explosiveness_PY1_col6 = dense_rank(desc(off_explosiveness_PY1)),
           Rank_Off_Pwr_Success_PY1_col6 = dense_rank(desc(off_power_success_PY1)),
           Rank_Off_Stuff_Rt_PY1_col6 = dense_rank(off_stuff_rate_PY1),
           Rank_Off_Pts_Per_Opp_PY1_col6 = dense_rank(desc(off_pts_per_opp_PY1)),
           Rank_Off_Havoc_Total_PY1_col6 = dense_rank(off_havoc_total_PY1),
           Rank_Off_Havoc_Front_PY1_col6 = dense_rank(off_havoc_front_seven_PY1),
           Rank_Off_Havoc_DB_PY1_col6 = dense_rank(off_havoc_db_PY1),
           Rank_Off_Standard_Down_PPA_PY1_col6 = dense_rank(desc(off_standard_downs_ppa_PY1)),
           Rank_Off_Standard_Down_Success_Rt_PY1_col6 = dense_rank(desc(off_standard_downs_success_rate_PY1)),
           Rank_Off_Standard_Down_Explosiveness_PY1_col6 = dense_rank(desc(off_standard_downs_explosiveness_PY1)),
           Rank_Off_Pass_Down_PPA_PY1_col6 = dense_rank(desc(off_passing_downs_ppa_PY1)),
           Rank_Off_Pass_Down_Success_Rt_PY1_col6 = dense_rank(desc(off_passing_downs_success_rate_PY1)),
           Rank_Off_Pass_Down_Explosiveness_PY1_col6 = dense_rank(desc(off_passing_downs_explosiveness_PY1)),
           Rank_Off_Rush_Play_PPA_PY1_col6 = dense_rank(desc(off_rushing_plays_ppa_PY1)),
           Rank_Off_Rush_Play_Success_Rt_PY1_col6 = dense_rank(desc(off_rushing_plays_success_rate_PY1)),
           Rank_Off_Rush_Play_Explosiveness_PY1_col6 = dense_rank(desc(off_rushing_plays_explosiveness_PY1)),
           Rank_Off_Pass_Play_PPA_PY1_col6 = dense_rank(desc(off_passing_plays_ppa_PY1)),
           Rank_Off_Pass_Play_Success_Rt_PY1_col6 = dense_rank(desc(off_passing_plays_success_rate_PY1)),
           Rank_Off_Pass_Play_Explosiveness_PY1_col6 = dense_rank(desc(off_passing_plays_explosiveness_PY1)),
           Rank_Def_PPA_PY1_col6 = dense_rank(def_ppa_PY1),
           Rank_Def_Success_Rt_PY1_col6 = dense_rank(def_success_rate_PY1),
           Rank_Def_Explosiveness_PY1_col6 = dense_rank(def_explosiveness_PY1),
           Rank_Def_Pwr_Success_PY1_col6 = dense_rank(def_power_success_PY1),
           Rank_Def_Stuff_Rt_PY1_col6 = dense_rank(desc(def_stuff_rate_PY1)),
           Rank_Def_Pts_Per_Opp_PY1_col6 = dense_rank(def_pts_per_opp_PY1),
           Rank_Def_Havoc_Total_PY1_col6 = dense_rank(desc(def_havoc_total_PY1)),
           Rank_Def_Havoc_Front_Seven_PY1_col6 = dense_rank(desc(def_havoc_front_seven_PY1)),
           Rank_Def_Havoc_DB_PY1_col6 = dense_rank(desc(def_havoc_db_PY1)),
           Rank_Def_Standard_Down_PPA_PY1_col6 = dense_rank(def_standard_downs_ppa_PY1),
           Rank_Def_Standard_Down_Success_Rt_PY1_col6 = dense_rank(def_standard_downs_success_rate_PY1),
           Rank_Def_Standard_Down_Explosiveness_PY1_col6 = dense_rank(def_standard_downs_explosiveness_PY1),
           Rank_Def_Pass_Down_PPA_PY1_col6 = dense_rank(def_passing_downs_ppa_PY1),
           Rank_Def_Pass_Down_Success_Rt_PY1_col6 = dense_rank(def_passing_downs_success_rate_PY1),
           Rank_Def_Pass_Down_Explosiveness_PY1_col6 = dense_rank(def_passing_downs_explosiveness_PY1),
           Rank_Def_Rush_Play_PPA_PY1_col6 = dense_rank(def_rushing_plays_ppa_PY1),
           Rank_Def_Rush_Play_Success_Rt_PY1_col6 = dense_rank(def_rushing_plays_success_rate_PY1),
           Rank_Def_Rush_Play_Explosiveness_PY1_col6 = dense_rank(def_rushing_plays_explosiveness_PY1),
           Rank_Def_Pass_Play_PPA_PY1_col6 = dense_rank(def_passing_plays_ppa_PY1),
           Rank_Def_Pass_Play_Success_Rt_PY1_col6 = dense_rank(def_passing_plays_success_rate_PY1),
           Rank_Def_Pass_Play_Explosiveness_PY1_col6 = dense_rank(def_passing_plays_explosiveness_PY1),
           ## FPI_SP mean ranks added at the end, weighted once
           Rank_FPI_SP_SRS_PY3_mean = dense_rank(desc(FPI_SP_SRS_PY3_mean)),
           Rank_FPI_SP_SRS_PY2_mean = dense_rank(desc(FPI_SP_SRS_PY2_mean)),
           Rank_FPI_SP_SRS_PY1_mean = dense_rank(desc(FPI_SP_SRS_PY1_mean)),
           Rank_AllPY_FPI_SP_SRS_mean = dense_rank(desc(AllPY_FPI_SP_SRS_mean)),
           Rank_WeightedAllPY_FPI_SP_SRS_mean = dense_rank(desc(WeightedAllPY_FPI_SP_SRS_mean)),
           ## Ranking current stats
           Rank_Wins = dense_rank(desc(Wins)),
           Rank_Losses = dense_rank(Losses),
           Rank_Comp_Pct = dense_rank(desc(completion_pct)),
           Rank_Pass_YPA = dense_rank(desc(pass_ypa)),
           Rank_Pass_YPR = dense_rank(desc(pass_ypr)),
           Rank_Int_Pct = dense_rank(int_pct),
           Rank_Rush_YPC = dense_rank(desc(rush_ypc)),
           Rank_Turnovers_pg = dense_rank(turnovers_pg),
           Rank_Third_Conv_Rate = dense_rank(desc(third_conv_rate)),
           Rank_Fourth_Conv_Rate = dense_rank(desc(fourth_conv_rate)),
           Rank_Penalty_Yds_pg = dense_rank(penalty_yds_pg),
           Rank_Yds_Per_Penalty = dense_rank(yards_per_penalty),
           Rank_Kick_Return_Avg = dense_rank(desc(kick_return_avg)),
           Rank_Punt_Return_Avg = dense_rank(desc(punt_return_avg)),
           Rank_Total_Yds_pg = dense_rank(desc(total_yds_pg)),
           Rank_Pass_Yds_pg = dense_rank(desc(pass_yds_pg)),
           Rank_Rush_Yds_pg = dense_rank(desc(rush_yds_pg)),
           Rank_First_Downs_pg = dense_rank(desc(first_downs_pg)),
           Rank_Off_YPP = dense_rank(desc(off_ypp)),
           Rank_Def_Ints_pg = dense_rank(desc(def_interceptions_pg)),
           Rank_Off_PPA = dense_rank(desc(off_ppa)),
           Rank_Off_Success_Rt = dense_rank(desc(off_success_rate)),
           Rank_Off_Explosiveness = dense_rank(desc(off_explosiveness)),
           Rank_Off_Pwr_Success = dense_rank(desc(off_power_success)),
           Rank_Off_Stuff_Rt = dense_rank(off_stuff_rate),
           Rank_Off_Line_Yds = dense_rank(desc(off_line_yds)),
           Rank_Off_Second_Lvl_Yds = dense_rank(desc(off_second_lvl_yds)),
           Rank_Off_Open_Field_Yds = dense_rank(desc(off_open_field_yds)),
           Rank_Off_Pts_Per_Opp = dense_rank(desc(off_pts_per_opp)),
           Rank_Off_Field_Pos_Avg_Predicted_Pts = dense_rank(desc(off_field_pos_avg_predicted_points)),
           Rank_Off_Havoc_Total = dense_rank(off_havoc_total),
           Rank_Off_Havoc_Front = dense_rank(off_havoc_front_seven),
           Rank_Off_Havoc_DB = dense_rank(off_havoc_db),
           Rank_Off_Standard_Down_PPA = dense_rank(desc(off_standard_downs_ppa)),
           Rank_Off_Standard_Down_Success_Rt = dense_rank(desc(off_standard_downs_success_rate)),
           Rank_Off_Standard_Down_Explosiveness = dense_rank(desc(off_standard_downs_explosiveness)),
           Rank_Off_Pass_Down_PPA = dense_rank(desc(off_passing_downs_ppa)),
           Rank_Off_Pass_Down_Success_Rt = dense_rank(desc(off_passing_downs_success_rate)),
           Rank_Off_Pass_Down_Explosiveness = dense_rank(desc(off_passing_downs_explosiveness)),
           Rank_Off_Rush_Play_PPA = dense_rank(desc(off_rushing_plays_ppa)),
           Rank_Off_Rush_Play_Success_Rt = dense_rank(desc(off_rushing_plays_success_rate)),
           Rank_Off_Rush_Play_Explosiveness = dense_rank(desc(off_rushing_plays_explosiveness)),
           Rank_Off_Pass_Play_PPA = dense_rank(desc(off_passing_plays_ppa)),
           Rank_Off_Pass_Play_Success_Rt = dense_rank(desc(off_passing_plays_success_rate)),
           Rank_Off_Pass_Play_Explosiveness = dense_rank(desc(off_passing_plays_explosiveness)),
           Rank_Def_PPA = dense_rank(def_ppa),
           Rank_Def_Success_Rt = dense_rank(def_success_rate),
           Rank_Def_Explosiveness = dense_rank(def_explosiveness),
           Rank_Def_Pwr_Success = dense_rank(def_power_success),
           Rank_Def_Stuff_Rt = dense_rank(desc(def_stuff_rate)),
           Rank_Def_Line_Yds = dense_rank(def_line_yds),
           Rank_Def_Second_Lvl_Yds = dense_rank(def_second_lvl_yds),
           Rank_Def_Open_Field_Yds = dense_rank(def_open_field_yds),
           Rank_Def_Pts_Per_Opp = dense_rank(def_pts_per_opp),
           Rank_Def_Field_Pos_Avg_Predicted_Pts = dense_rank(def_field_pos_avg_predicted_points),
           Rank_Def_Havoc_Total = dense_rank(desc(def_havoc_total)),
           Rank_Def_Havoc_Front_Seven = dense_rank(desc(def_havoc_front_seven)),
           Rank_Def_Havoc_DB = dense_rank(desc(def_havoc_db)),
           Rank_Def_Standard_Down_PPA = dense_rank(def_standard_downs_ppa),
           Rank_Def_Standard_Down_Success_Rt = dense_rank(def_standard_downs_success_rate),
           Rank_Def_Standard_Down_Explosiveness = dense_rank(def_standard_downs_explosiveness),
           Rank_Def_Pass_Down_PPA = dense_rank(def_passing_downs_ppa),
           Rank_Def_Pass_Down_Success_Rt = dense_rank(def_passing_downs_success_rate),
           Rank_Def_Pass_Down_Explosiveness = dense_rank(def_passing_downs_explosiveness),
           Rank_Def_Rush_Play_PPA = dense_rank(def_rushing_plays_ppa),
           Rank_Def_Rush_Play_Success_Rt = dense_rank(def_rushing_plays_success_rate),
           Rank_Def_Rush_Play_Explosiveness = dense_rank(def_rushing_plays_explosiveness),
           Rank_Def_Pass_Play_PPA = dense_rank(def_passing_plays_ppa),
           Rank_Def_Pass_Play_Success_Rt = dense_rank(def_passing_plays_success_rate),
           Rank_Def_Pass_Play_Explosiveness = dense_rank(def_passing_plays_explosiveness),
           # Rank_SP_Rating = dense_rank(desc(sp_rating)),
           # Rank_SP_Off_Rating = dense_rank(desc(sp_offense_rating)),
           # Rank_SP_Def_Rating = dense_rank(sp_defense_rating),
           # Rank_SP_SpecialTeams_Rating = dense_rank(desc(sp_special_teams_rating)),
           Rank_FPI = dense_rank(desc(FPI)),
           # Rank_SRS = dense_rank(desc(SRS_rating)),
           # Rank_FPI_SP_mean = dense_rank(desc(FPI_SP_mean)),
           Rank_FPI_SRS_mean = dense_rank(desc(FPI_SRS_mean)),
           # due to availability issues, SP_Rankings not included with current data
           ## Extra weighted variables for current year
           Rank_Wins_col2 = dense_rank(desc(Wins)),
           Rank_Losses_col2 = dense_rank(Losses),
           Rank_Off_YPP_col2 = dense_rank(desc(off_ypp)),
           Rank_Off_PPA_col2 = dense_rank(desc(off_ppa)),
           Rank_Off_Success_Rt_col2 = dense_rank(desc(off_success_rate)),
           Rank_Off_Explosiveness_col2 = dense_rank(desc(off_explosiveness)),
           Rank_Off_Pwr_Success_col2 = dense_rank(desc(off_power_success)),
           Rank_Off_Stuff_Rt_col2 = dense_rank(off_stuff_rate),
           Rank_Off_Pts_Per_Opp_col2 = dense_rank(desc(off_pts_per_opp)),
           Rank_Off_Havoc_Total_col2 = dense_rank(off_havoc_total),
           Rank_Off_Havoc_Front_col2 = dense_rank(off_havoc_front_seven),
           Rank_Off_Havoc_DB_col2 = dense_rank(off_havoc_db),
           Rank_Off_Standard_Down_PPA_col2 = dense_rank(desc(off_standard_downs_ppa)),
           Rank_Off_Standard_Down_Success_Rt_col2 = dense_rank(desc(off_standard_downs_success_rate)),
           Rank_Off_Standard_Down_Explosiveness_col2 = dense_rank(desc(off_standard_downs_explosiveness)),
           Rank_Off_Pass_Down_PPA_col2 = dense_rank(desc(off_passing_downs_ppa)),
           Rank_Off_Pass_Down_Success_Rt_col2 = dense_rank(desc(off_passing_downs_success_rate)),
           Rank_Off_Pass_Down_Explosiveness_col2 = dense_rank(desc(off_passing_downs_explosiveness)),
           Rank_Off_Rush_Play_PPA_col2 = dense_rank(desc(off_rushing_plays_ppa)),
           Rank_Off_Rush_Play_Success_Rt_col2 = dense_rank(desc(off_rushing_plays_success_rate)),
           Rank_Off_Rush_Play_Explosiveness_col2 = dense_rank(desc(off_rushing_plays_explosiveness)),
           Rank_Off_Pass_Play_PPA_col2 = dense_rank(desc(off_passing_plays_ppa)),
           Rank_Off_Pass_Play_Success_Rt_col2 = dense_rank(desc(off_passing_plays_success_rate)),
           Rank_Off_Pass_Play_Explosiveness_col2 = dense_rank(desc(off_passing_plays_explosiveness)),
           Rank_Def_PPA_col2 = dense_rank(def_ppa),
           Rank_Def_Success_Rt_col2 = dense_rank(def_success_rate),
           Rank_Def_Explosiveness_col2 = dense_rank(def_explosiveness),
           Rank_Def_Pwr_Success_col2 = dense_rank(def_power_success),
           Rank_Def_Stuff_Rt_col2 = dense_rank(desc(def_stuff_rate)),
           Rank_Def_Pts_Per_Opp_col2 = dense_rank(def_pts_per_opp),
           Rank_Def_Havoc_Total_col2 = dense_rank(desc(def_havoc_total)),
           Rank_Def_Havoc_Front_Seven_col2 = dense_rank(desc(def_havoc_front_seven)),
           Rank_Def_Havoc_DB_col2 = dense_rank(desc(def_havoc_db)),
           Rank_Def_Standard_Down_PPA_col2 = dense_rank(def_standard_downs_ppa),
           Rank_Def_Standard_Down_Success_Rt_col2 = dense_rank(def_standard_downs_success_rate),
           Rank_Def_Standard_Down_Explosiveness_col2 = dense_rank(def_standard_downs_explosiveness),
           Rank_Def_Pass_Down_PPA_col2 = dense_rank(def_passing_downs_ppa),
           Rank_Def_Pass_Down_Success_Rt_col2 = dense_rank(def_passing_downs_success_rate),
           Rank_Def_Pass_Down_Explosiveness_col2 = dense_rank(def_passing_downs_explosiveness),
           Rank_Def_Rush_Play_PPA_col2 = dense_rank(def_rushing_plays_ppa),
           Rank_Def_Rush_Play_Success_Rt_col2 = dense_rank(def_rushing_plays_success_rate),
           Rank_Def_Rush_Play_Explosiveness_col2 = dense_rank(def_rushing_plays_explosiveness),
           Rank_Def_Pass_Play_PPA_col2 = dense_rank(def_passing_plays_ppa),
           Rank_Def_Pass_Play_Success_Rt_col2 = dense_rank(def_passing_plays_success_rate),
           Rank_Def_Pass_Play_Explosiveness_col2 = dense_rank(def_passing_plays_explosiveness))
} else if (as.numeric(week) == 2) {
  ##### Week 2 Variable Ranks #####
  # PY2 weighted 2x, PY1 weighted 3x, current weighted 1x
  VoA_Variables <- VoA_Variables |>
    ## PY2 ranks
    mutate(Rank_Wins_PY2 = dense_rank(desc(Wins_PY2)),
           Rank_Losses_PY2 = dense_rank(Losses_PY2),
           Rank_Comp_Pct_PY2 = dense_rank(desc(completion_pct_PY2)),
           Rank_Pass_YPA_PY2 = dense_rank(desc(pass_ypa_PY2)),
           Rank_Pass_YPR_PY2 = dense_rank(desc(pass_ypr_PY2)),
           Rank_Int_Pct_PY2 = dense_rank(int_pct_PY2),
           Rank_Rush_YPC_PY2 = dense_rank(desc(rush_ypc_PY2)),
      Rank_Turnovers_pg_PY2 = dense_rank(turnovers_pg_PY2),
      Rank_Third_Conv_Rate_PY2 = dense_rank(desc(third_conv_rate_PY2)),
      Rank_Fourth_Conv_Rate_PY2 = dense_rank(desc(fourth_conv_rate_PY2)),
      Rank_Penalty_Yds_pg_PY2 = dense_rank(penalty_yds_pg_PY2),
      Rank_Yds_Per_Penalty_PY2 = dense_rank(yards_per_penalty_PY2),
      Rank_Kick_Return_Avg_PY2 = dense_rank(desc(kick_return_avg_PY2)),
      Rank_Punt_Return_Avg_PY2 = dense_rank(desc(punt_return_avg_PY2)),
      Rank_Total_Yds_pg_PY2 = dense_rank(desc(total_yds_pg_PY2)),
      Rank_Pass_Yds_pg_PY2 = dense_rank(desc(pass_yds_pg_PY2)),
      Rank_Rush_Yds_pg_PY2 = dense_rank(desc(rush_yds_pg_PY2)),
      Rank_First_Downs_pg_PY2 = dense_rank(desc(first_downs_pg_PY2)),
      Rank_Off_YPP_PY2 = dense_rank(desc(off_ypp_PY2)),
      Rank_Def_Ints_pg_PY2 = dense_rank(desc(def_interceptions_pg_PY2)),
      Rank_Off_PPA_PY2 = dense_rank(desc(off_ppa_PY2)),
      Rank_Off_Success_Rt_PY2 = dense_rank(desc(off_success_rate_PY2)),
      Rank_Off_Explosiveness_PY2 = dense_rank(desc(off_explosiveness_PY2)),
      Rank_Off_Pwr_Success_PY2 = dense_rank(desc(off_power_success_PY2)),
      Rank_Off_Stuff_Rt_PY2 = dense_rank(off_stuff_rate_PY2),
      Rank_Off_Line_Yds_PY2 = dense_rank(desc(off_line_yds_PY2)),
      Rank_Off_Second_Lvl_Yds_PY2 = dense_rank(desc(off_second_lvl_yds_PY2)),
      Rank_Off_Open_Field_Yds_PY2 = dense_rank(desc(off_open_field_yds_PY2)),
      Rank_Off_Pts_Per_Opp_PY2 = dense_rank(desc(off_pts_per_opp_PY2)),
      Rank_Off_Field_Pos_Avg_Predicted_Pts_PY2 = dense_rank(desc(off_field_pos_avg_predicted_points_PY2)),
      Rank_Off_Havoc_Total_PY2 = dense_rank(off_havoc_total_PY2),
      Rank_Off_Havoc_Front_PY2 = dense_rank(off_havoc_front_seven_PY2),
      Rank_Off_Havoc_DB_PY2 = dense_rank(off_havoc_db_PY2),
      Rank_Off_Standard_Down_PPA_PY2 = dense_rank(desc(off_standard_downs_ppa_PY2)),
      Rank_Off_Standard_Down_Success_Rt_PY2 = dense_rank(desc(off_standard_downs_success_rate_PY2)),
      Rank_Off_Standard_Down_Explosiveness_PY2 = dense_rank(desc(off_standard_downs_explosiveness_PY2)),
      Rank_Off_Pass_Down_PPA_PY2 = dense_rank(desc(off_passing_downs_ppa_PY2)),
      Rank_Off_Pass_Down_Success_Rt_PY2 = dense_rank(desc(off_passing_downs_success_rate_PY2)),
      Rank_Off_Pass_Down_Explosiveness_PY2 = dense_rank(desc(off_passing_downs_explosiveness_PY2)),
      Rank_Off_Rush_Play_PPA_PY2 = dense_rank(desc(off_rushing_plays_ppa_PY2)),
      Rank_Off_Rush_Play_Success_Rt_PY2 = dense_rank(desc(off_rushing_plays_success_rate_PY2)),
      Rank_Off_Rush_Play_Explosiveness_PY2 = dense_rank(desc(off_rushing_plays_explosiveness_PY2)),
      Rank_Off_Pass_Play_PPA_PY2 = dense_rank(desc(off_passing_plays_ppa_PY2)),
      Rank_Off_Pass_Play_Success_Rt_PY2 = dense_rank(desc(off_passing_plays_success_rate_PY2)),
      Rank_Off_Pass_Play_Explosiveness_PY2 = dense_rank(desc(off_passing_plays_explosiveness_PY2)),
      Rank_Def_PPA_PY2 = dense_rank(def_ppa_PY2),
      Rank_Def_Success_Rt_PY2 = dense_rank(def_success_rate_PY2),
      Rank_Def_Explosiveness_PY2 = dense_rank(def_explosiveness_PY2),
      Rank_Def_Pwr_Success_PY2 = dense_rank(def_power_success_PY2),
      Rank_Def_Stuff_Rt_PY2 = dense_rank(desc(def_stuff_rate_PY2)),
      Rank_Def_Line_Yds_PY2 = dense_rank(def_line_yds_PY2),
      Rank_Def_Second_Lvl_Yds_PY2 = dense_rank(def_second_lvl_yds_PY2),
      Rank_Def_Open_Field_Yds_PY2 = dense_rank(def_open_field_yds_PY2),
      Rank_Def_Pts_Per_Opp_PY2 = dense_rank(def_pts_per_opp_PY2),
      Rank_Def_Field_Pos_Avg_Predicted_Pts_PY2 = dense_rank(def_field_pos_avg_predicted_points_PY2),
      Rank_Def_Havoc_Total_PY2 = dense_rank(desc(def_havoc_total_PY2)),
      Rank_Def_Havoc_Front_Seven_PY2 = dense_rank(desc(def_havoc_front_seven_PY2)),
      Rank_Def_Havoc_DB_PY2 = dense_rank(desc(def_havoc_db_PY2)),
      Rank_Def_Standard_Down_PPA_PY2 = dense_rank(def_standard_downs_ppa_PY2),
      Rank_Def_Standard_Down_Success_Rt_PY2 = dense_rank(def_standard_downs_success_rate_PY2),
      Rank_Def_Standard_Down_Explosiveness_PY2 = dense_rank(def_standard_downs_explosiveness_PY2),
      Rank_Def_Pass_Down_PPA_PY2 = dense_rank(def_passing_downs_ppa_PY2),
      Rank_Def_Pass_Down_Success_Rt_PY2 = dense_rank(def_passing_downs_success_rate_PY2),
      Rank_Def_Pass_Down_Explosiveness_PY2 = dense_rank(def_passing_downs_explosiveness_PY2),
      Rank_Def_Rush_Play_PPA_PY2 = dense_rank(def_rushing_plays_ppa_PY2),
      Rank_Def_Rush_Play_Success_Rt_PY2 = dense_rank(def_rushing_plays_success_rate_PY2),
      Rank_Def_Rush_Play_Explosiveness_PY2 = dense_rank(def_rushing_plays_explosiveness_PY2),
      Rank_Def_Pass_Play_PPA_PY2 = dense_rank(def_passing_plays_ppa_PY2),
      Rank_Def_Pass_Play_Success_Rt_PY2 = dense_rank(def_passing_plays_success_rate_PY2),
      Rank_Def_Pass_Play_Explosiveness_PY2 = dense_rank(def_passing_plays_explosiveness_PY2),
      Rank_Recruit_Pts_PY2 = dense_rank(desc(recruit_pts_PY2)),
      Rank_Talent_PY2 = dense_rank(desc(talent_PY2)),
      Rank_SP_Rating_PY2 = dense_rank(desc(sp_rating_PY2)),
      Rank_SP_Off_Rating_PY2 = dense_rank(desc(sp_offense_rating_PY2)),
      Rank_SP_Def_Rating_PY2 = dense_rank(sp_defense_rating_PY2),
      Rank_SP_SpecialTeams_Rating_PY2 = dense_rank(desc(sp_special_teams_rating_PY2)),
      Rank_FPI_PY2 = dense_rank(desc(FPI_PY2)),
      Rank_SRS_PY2 = dense_rank(desc(SRS_rating_PY2)),
      ## PY2 weighted twice
      Rank_Wins_PY2_col2 = dense_rank(desc(Wins_PY2)),
      Rank_Losses_PY2_col2 = dense_rank(Losses_PY2),
      Rank_Comp_Pct_PY2_col2 = dense_rank(desc(completion_pct_PY2)),
      Rank_Pass_YPA_PY2_col2 = dense_rank(desc(pass_ypa_PY2)),
      Rank_Pass_YPR_PY2_col2 = dense_rank(desc(pass_ypr_PY2)),
      Rank_Int_Pct_PY2_col2 = dense_rank(int_pct_PY2),
      Rank_Rush_YPC_PY2_col2 = dense_rank(desc(rush_ypc_PY2)),
      Rank_Turnovers_pg_PY2_col2 = dense_rank(turnovers_pg_PY2),
      Rank_Third_Conv_Rate_PY2_col2 = dense_rank(desc(third_conv_rate_PY2)),
      Rank_Fourth_Conv_Rate_PY2_col2 = dense_rank(desc(fourth_conv_rate_PY2)),
      Rank_Penalty_Yds_pg_PY2_col2 = dense_rank(penalty_yds_pg_PY2),
      Rank_Yds_Per_Penalty_PY2_col2 = dense_rank(yards_per_penalty_PY2),
      Rank_Kick_Return_Avg_PY2_col2 = dense_rank(desc(kick_return_avg_PY2)),
      Rank_Punt_Return_Avg_PY2_col2 = dense_rank(desc(punt_return_avg_PY2)),
      Rank_Total_Yds_pg_PY2_col2 = dense_rank(desc(total_yds_pg_PY2)),
      Rank_Pass_Yds_pg_PY2_col2 = dense_rank(desc(pass_yds_pg_PY2)),
      Rank_Rush_Yds_pg_PY2_col2 = dense_rank(desc(rush_yds_pg_PY2)),
      Rank_First_Downs_pg_PY2_col2 = dense_rank(desc(first_downs_pg_PY2)),
      Rank_Off_YPP_PY2_col2 = dense_rank(desc(off_ypp_PY2)),
      Rank_Def_Ints_pg_PY2_col2 = dense_rank(desc(def_interceptions_pg_PY2)),
      Rank_Off_PPA_PY2_col2 = dense_rank(desc(off_ppa_PY2)),
      Rank_Off_Success_Rt_PY2_col2 = dense_rank(desc(off_success_rate_PY2)),
      Rank_Off_Explosiveness_PY2_col2 = dense_rank(desc(off_explosiveness_PY2)),
      Rank_Off_Pwr_Success_PY2_col2 = dense_rank(desc(off_power_success_PY2)),
      Rank_Off_Stuff_Rt_PY2_col2 = dense_rank(off_stuff_rate_PY2),
      Rank_Off_Line_Yds_PY2_col2 = dense_rank(desc(off_line_yds_PY2)),
      Rank_Off_Second_Lvl_Yds_PY2_col2 = dense_rank(desc(off_second_lvl_yds_PY2)),
      Rank_Off_Open_Field_Yds_PY2_col2 = dense_rank(desc(off_open_field_yds_PY2)),
      Rank_Off_Pts_Per_Opp_PY2_col2 = dense_rank(desc(off_pts_per_opp_PY2)),
      Rank_Off_Field_Pos_Avg_Predicted_Pts_PY2_col2 = dense_rank(desc(off_field_pos_avg_predicted_points_PY2)),
      Rank_Off_Havoc_Total_PY2_col2 = dense_rank(off_havoc_total_PY2),
      Rank_Off_Havoc_Front_PY2_col2 = dense_rank(off_havoc_front_seven_PY2),
      Rank_Off_Havoc_DB_PY2_col2 = dense_rank(off_havoc_db_PY2),
      Rank_Off_Standard_Down_PPA_PY2_col2 = dense_rank(desc(off_standard_downs_ppa_PY2)),
      Rank_Off_Standard_Down_Success_Rt_PY2_col2 = dense_rank(desc(off_standard_downs_success_rate_PY2)),
      Rank_Off_Standard_Down_Explosiveness_PY2_col2 = dense_rank(desc(off_standard_downs_explosiveness_PY2)),
      Rank_Off_Pass_Down_PPA_PY2_col2 = dense_rank(desc(off_passing_downs_ppa_PY2)),
      Rank_Off_Pass_Down_Success_Rt_PY2_col2 = dense_rank(desc(off_passing_downs_success_rate_PY2)),
      Rank_Off_Pass_Down_Explosiveness_PY2_col2 = dense_rank(desc(off_passing_downs_explosiveness_PY2)),
      Rank_Off_Rush_Play_PPA_PY2_col2 = dense_rank(desc(off_rushing_plays_ppa_PY2)),
      Rank_Off_Rush_Play_Success_Rt_PY2_col2 = dense_rank(desc(off_rushing_plays_success_rate_PY2)),
      Rank_Off_Rush_Play_Explosiveness_PY2_col2 = dense_rank(desc(off_rushing_plays_explosiveness_PY2)),
      Rank_Off_Pass_Play_PPA_PY2_col2 = dense_rank(desc(off_passing_plays_ppa_PY2)),
      Rank_Off_Pass_Play_Success_Rt_PY2_col2 = dense_rank(desc(off_passing_plays_success_rate_PY2)),
      Rank_Off_Pass_Play_Explosiveness_PY2_col2 = dense_rank(desc(off_passing_plays_explosiveness_PY2)),
      Rank_Def_PPA_PY2_col2 = dense_rank(def_ppa_PY2),
      Rank_Def_Success_Rt_PY2_col2 = dense_rank(def_success_rate_PY2),
      Rank_Def_Explosiveness_PY2_col2 = dense_rank(def_explosiveness_PY2),
      Rank_Def_Pwr_Success_PY2_col2 = dense_rank(def_power_success_PY2),
      Rank_Def_Stuff_Rt_PY2_col2 = dense_rank(desc(def_stuff_rate_PY2)),
      Rank_Def_Line_Yds_PY2_col2 = dense_rank(def_line_yds_PY2),
      Rank_Def_Second_Lvl_Yds_PY2_col2 = dense_rank(def_second_lvl_yds_PY2),
      Rank_Def_Open_Field_Yds_PY2_col2 = dense_rank(def_open_field_yds_PY2),
      Rank_Def_Pts_Per_Opp_PY2_col2 = dense_rank(def_pts_per_opp_PY2),
      Rank_Def_Field_Pos_Avg_Predicted_Pts_PY2_col2 = dense_rank(def_field_pos_avg_predicted_points_PY2),
      Rank_Def_Havoc_Total_PY2_col2 = dense_rank(desc(def_havoc_total_PY2)),
      Rank_Def_Havoc_Front_Seven_PY2_col2 = dense_rank(desc(def_havoc_front_seven_PY2)),
      Rank_Def_Havoc_DB_PY2_col2 = dense_rank(desc(def_havoc_db_PY2)),
      Rank_Def_Standard_Down_PPA_PY2_col2 = dense_rank(def_standard_downs_ppa_PY2),
      Rank_Def_Standard_Down_Success_Rt_PY2_col2 = dense_rank(def_standard_downs_success_rate_PY2),
      Rank_Def_Standard_Down_Explosiveness_PY2_col2 = dense_rank(def_standard_downs_explosiveness_PY2),
      Rank_Def_Pass_Down_PPA_PY2_col2 = dense_rank(def_passing_downs_ppa_PY2),
      Rank_Def_Pass_Down_Success_Rt_PY2_col2 = dense_rank(def_passing_downs_success_rate_PY2),
      Rank_Def_Pass_Down_Explosiveness_PY2_col2 = dense_rank(def_passing_downs_explosiveness_PY2),
      Rank_Def_Rush_Play_PPA_PY2_col2 = dense_rank(def_rushing_plays_ppa_PY2),
      Rank_Def_Rush_Play_Success_Rt_PY2_col2 = dense_rank(def_rushing_plays_success_rate_PY2),
      Rank_Def_Rush_Play_Explosiveness_PY2_col2 = dense_rank(def_rushing_plays_explosiveness_PY2),
      Rank_Def_Pass_Play_PPA_PY2_col2 = dense_rank(def_passing_plays_ppa_PY2),
      Rank_Def_Pass_Play_Success_Rt_PY2_col2 = dense_rank(def_passing_plays_success_rate_PY2),
      Rank_Def_Pass_Play_Explosiveness_PY2_col2 = dense_rank(def_passing_plays_explosiveness_PY2),
      Rank_Recruit_Pts_PY2_col2 = dense_rank(desc(recruit_pts_PY2)),
      ## PY2 extra weighted variables
      Rank_Off_YPP_PY2_col3 = dense_rank(desc(off_ypp_PY2)),
      Rank_Off_PPA_PY2_col3 = dense_rank(desc(off_ppa_PY2)),
      Rank_Off_Success_Rt_PY2_col3 = dense_rank(desc(off_success_rate_PY2)),
      Rank_Off_Explosiveness_PY2_col3 = dense_rank(desc(off_explosiveness_PY2)),
      Rank_Off_Pwr_Success_PY2_col3 = dense_rank(desc(off_power_success_PY2)),
      Rank_Off_Stuff_Rt_PY2_col3 = dense_rank(off_stuff_rate_PY2),
      Rank_Off_Pts_Per_Opp_PY2_col3 = dense_rank(desc(off_pts_per_opp_PY2)),
      Rank_Off_Havoc_Total_PY2_col3 = dense_rank(off_havoc_total_PY2),
      Rank_Off_Havoc_Front_PY2_col3 = dense_rank(off_havoc_front_seven_PY2),
      Rank_Off_Havoc_DB_PY2_col3 = dense_rank(off_havoc_db_PY2),
      Rank_Off_Standard_Down_PPA_PY2_col3 = dense_rank(desc(off_standard_downs_ppa_PY2)),
      Rank_Off_Standard_Down_Success_Rt_PY2_col3 = dense_rank(desc(off_standard_downs_success_rate_PY2)),
      Rank_Off_Standard_Down_Explosiveness_PY2_col3 = dense_rank(desc(off_standard_downs_explosiveness_PY2)),
      Rank_Off_Pass_Down_PPA_PY2_col3 = dense_rank(desc(off_passing_downs_ppa_PY2)),
      Rank_Off_Pass_Down_Success_Rt_PY2_col3 = dense_rank(desc(off_passing_downs_success_rate_PY2)),
      Rank_Off_Pass_Down_Explosiveness_PY2_col3 = dense_rank(desc(off_passing_downs_explosiveness_PY2)),
      Rank_Off_Rush_Play_PPA_PY2_col3 = dense_rank(desc(off_rushing_plays_ppa_PY2)),
      Rank_Off_Rush_Play_Success_Rt_PY2_col3 = dense_rank(desc(off_rushing_plays_success_rate_PY2)),
      Rank_Off_Rush_Play_Explosiveness_PY2_col3 = dense_rank(desc(off_rushing_plays_explosiveness_PY2)),
      Rank_Off_Pass_Play_PPA_PY2_col3 = dense_rank(desc(off_passing_plays_ppa_PY2)),
      Rank_Off_Pass_Play_Success_Rt_PY2_col3 = dense_rank(desc(off_passing_plays_success_rate_PY2)),
      Rank_Off_Pass_Play_Explosiveness_PY2_col3 = dense_rank(desc(off_passing_plays_explosiveness_PY2)),
      Rank_Def_PPA_PY2_col3 = dense_rank(def_ppa_PY2),
      Rank_Def_Success_Rt_PY2_col3 = dense_rank(def_success_rate_PY2),
      Rank_Def_Explosiveness_PY2_col3 = dense_rank(def_explosiveness_PY2),
      Rank_Def_Pwr_Success_PY2_col3 = dense_rank(def_power_success_PY2),
      Rank_Def_Stuff_Rt_PY2_col3 = dense_rank(desc(def_stuff_rate_PY2)),
      Rank_Def_Pts_Per_Opp_PY2_col3 = dense_rank(def_pts_per_opp_PY2),
      Rank_Def_Havoc_Total_PY2_col3 = dense_rank(desc(def_havoc_total_PY2)),
      Rank_Def_Havoc_Front_Seven_PY2_col3 = dense_rank(desc(def_havoc_front_seven_PY2)),
      Rank_Def_Havoc_DB_PY2_col3 = dense_rank(desc(def_havoc_db_PY2)),
      Rank_Def_Standard_Down_PPA_PY2_col3 = dense_rank(def_standard_downs_ppa_PY2),
      Rank_Def_Standard_Down_Success_Rt_PY2_col3 = dense_rank(def_standard_downs_success_rate_PY2),
      Rank_Def_Standard_Down_Explosiveness_PY2_col3 = dense_rank(def_standard_downs_explosiveness_PY2),
      Rank_Def_Pass_Down_PPA_PY2_col3 = dense_rank(def_passing_downs_ppa_PY2),
      Rank_Def_Pass_Down_Success_Rt_PY2_col3 = dense_rank(def_passing_downs_success_rate_PY2),
      Rank_Def_Pass_Down_Explosiveness_PY2_col3 = dense_rank(def_passing_downs_explosiveness_PY2),
      Rank_Def_Rush_Play_PPA_PY2_col3 = dense_rank(def_rushing_plays_ppa_PY2),
      Rank_Def_Rush_Play_Success_Rt_PY2_col3 = dense_rank(def_rushing_plays_success_rate_PY2),
      Rank_Def_Rush_Play_Explosiveness_PY2_col3 = dense_rank(def_rushing_plays_explosiveness_PY2),
      Rank_Def_Pass_Play_PPA_PY2_col3 = dense_rank(def_passing_plays_ppa_PY2),
      Rank_Def_Pass_Play_Success_Rt_PY2_col3 = dense_rank(def_passing_plays_success_rate_PY2),
      Rank_Def_Pass_Play_Explosiveness_PY2_col3 = dense_rank(def_passing_plays_explosiveness_PY2),
      Rank_Recruit_Pts_PY2_col3 = dense_rank(desc(recruit_pts_PY2)),
      ## PY2 extra weighted variables (2x)
      Rank_Off_YPP_PY2_col4 = dense_rank(desc(off_ypp_PY2)),
      Rank_Off_PPA_PY2_col4 = dense_rank(desc(off_ppa_PY2)),
      Rank_Off_Success_Rt_PY2_col4 = dense_rank(desc(off_success_rate_PY2)),
      Rank_Off_Explosiveness_PY2_col4 = dense_rank(desc(off_explosiveness_PY2)),
      Rank_Off_Pwr_Success_PY2_col4 = dense_rank(desc(off_power_success_PY2)),
      Rank_Off_Stuff_Rt_PY2_col4 = dense_rank(off_stuff_rate_PY2),
      Rank_Off_Pts_Per_Opp_PY2_col4 = dense_rank(desc(off_pts_per_opp_PY2)),
      Rank_Off_Havoc_Total_PY2_col4 = dense_rank(off_havoc_total_PY2),
      Rank_Off_Havoc_Front_PY2_col4 = dense_rank(off_havoc_front_seven_PY2),
      Rank_Off_Havoc_DB_PY2_col4 = dense_rank(off_havoc_db_PY2),
      Rank_Off_Standard_Down_PPA_PY2_col4 = dense_rank(desc(off_standard_downs_ppa_PY2)),
      Rank_Off_Standard_Down_Success_Rt_PY2_col4 = dense_rank(desc(off_standard_downs_success_rate_PY2)),
      Rank_Off_Standard_Down_Explosiveness_PY2_col4 = dense_rank(desc(off_standard_downs_explosiveness_PY2)),
      Rank_Off_Pass_Down_PPA_PY2_col4 = dense_rank(desc(off_passing_downs_ppa_PY2)),
      Rank_Off_Pass_Down_Success_Rt_PY2_col4 = dense_rank(desc(off_passing_downs_success_rate_PY2)),
      Rank_Off_Pass_Down_Explosiveness_PY2_col4 = dense_rank(desc(off_passing_downs_explosiveness_PY2)),
      Rank_Off_Rush_Play_PPA_PY2_col4 = dense_rank(desc(off_rushing_plays_ppa_PY2)),
      Rank_Off_Rush_Play_Success_Rt_PY2_col4 = dense_rank(desc(off_rushing_plays_success_rate_PY2)),
      Rank_Off_Rush_Play_Explosiveness_PY2_col4 = dense_rank(desc(off_rushing_plays_explosiveness_PY2)),
      Rank_Off_Pass_Play_PPA_PY2_col4 = dense_rank(desc(off_passing_plays_ppa_PY2)),
      Rank_Off_Pass_Play_Success_Rt_PY2_col4 = dense_rank(desc(off_passing_plays_success_rate_PY2)),
      Rank_Off_Pass_Play_Explosiveness_PY2_col4 = dense_rank(desc(off_passing_plays_explosiveness_PY2)),
      Rank_Def_PPA_PY2_col4 = dense_rank(def_ppa_PY2),
      Rank_Def_Success_Rt_PY2_col4 = dense_rank(def_success_rate_PY2),
      Rank_Def_Explosiveness_PY2_col4 = dense_rank(def_explosiveness_PY2),
      Rank_Def_Pwr_Success_PY2_col4 = dense_rank(def_power_success_PY2),
      Rank_Def_Stuff_Rt_PY2_col4 = dense_rank(desc(def_stuff_rate_PY2)),
      Rank_Def_Pts_Per_Opp_PY2_col4 = dense_rank(def_pts_per_opp_PY2),
      Rank_Def_Havoc_Total_PY2_col4 = dense_rank(desc(def_havoc_total_PY2)),
      Rank_Def_Havoc_Front_Seven_PY2_col4 = dense_rank(desc(def_havoc_front_seven_PY2)),
      Rank_Def_Havoc_DB_PY2_col4 = dense_rank(desc(def_havoc_db_PY2)),
      Rank_Def_Standard_Down_PPA_PY2_col4 = dense_rank(def_standard_downs_ppa_PY2),
      Rank_Def_Standard_Down_Success_Rt_PY2_col4 = dense_rank(def_standard_downs_success_rate_PY2),
      Rank_Def_Standard_Down_Explosiveness_PY2_col4 = dense_rank(def_standard_downs_explosiveness_PY2),
      Rank_Def_Pass_Down_PPA_PY2_col4 = dense_rank(def_passing_downs_ppa_PY2),
      Rank_Def_Pass_Down_Success_Rt_PY2_col4 = dense_rank(def_passing_downs_success_rate_PY2),
      Rank_Def_Pass_Down_Explosiveness_PY2_col4 = dense_rank(def_passing_downs_explosiveness_PY2),
      Rank_Def_Rush_Play_PPA_PY2_col4 = dense_rank(def_rushing_plays_ppa_PY2),
      Rank_Def_Rush_Play_Success_Rt_PY2_col4 = dense_rank(def_rushing_plays_success_rate_PY2),
      Rank_Def_Rush_Play_Explosiveness_PY2_col4 = dense_rank(def_rushing_plays_explosiveness_PY2),
      Rank_Def_Pass_Play_PPA_PY2_col4 = dense_rank(def_passing_plays_ppa_PY2),
      Rank_Def_Pass_Play_Success_Rt_PY2_col4 = dense_rank(def_passing_plays_success_rate_PY2),
      Rank_Def_Pass_Play_Explosiveness_PY2_col4 = dense_rank(def_passing_plays_explosiveness_PY2),
      ## PY1 ranks
      Rank_Wins_PY1 = dense_rank(desc(Wins_PY1)),
      Rank_Losses_PY1 = dense_rank(Losses_PY1),
      Rank_Comp_Pct_PY1 = dense_rank(desc(completion_pct_PY1)),
      Rank_Pass_YPA_PY1 = dense_rank(desc(pass_ypa_PY1)),
      Rank_Pass_YPR_PY1 = dense_rank(desc(pass_ypr_PY1)),
      Rank_Int_Pct_PY1 = dense_rank(int_pct_PY1),
      Rank_Rush_YPC_PY1 = dense_rank(desc(rush_ypc_PY1)),
      Rank_Turnovers_pg_PY1 = dense_rank(turnovers_pg_PY1),
      Rank_Third_Conv_Rate_PY1 = dense_rank(desc(third_conv_rate_PY1)),
      Rank_Fourth_Conv_Rate_PY1 = dense_rank(desc(fourth_conv_rate_PY1)),
      Rank_Penalty_Yds_pg_PY1 = dense_rank(penalty_yds_pg_PY1),
      Rank_Yds_Per_Penalty_PY1 = dense_rank(yards_per_penalty_PY1),
      Rank_Kick_Return_Avg_PY1 = dense_rank(desc(kick_return_avg_PY1)),
      Rank_Punt_Return_Avg_PY1 = dense_rank(desc(punt_return_avg_PY1)),
      Rank_Total_Yds_pg_PY1 = dense_rank(desc(total_yds_pg_PY1)),
      Rank_Pass_Yds_pg_PY1 = dense_rank(desc(pass_yds_pg_PY1)),
      Rank_Rush_Yds_pg_PY1 = dense_rank(desc(rush_yds_pg_PY1)),
      Rank_First_Downs_pg_PY1 = dense_rank(desc(first_downs_pg_PY1)),
      Rank_Off_YPP_PY1 = dense_rank(desc(off_ypp_PY1)),
      Rank_Def_Ints_pg_PY1 = dense_rank(desc(def_interceptions_pg_PY1)),
      Rank_Off_PPA_PY1 = dense_rank(desc(off_ppa_PY1)),
      Rank_Off_Success_Rt_PY1 = dense_rank(desc(off_success_rate_PY1)),
      Rank_Off_Explosiveness_PY1 = dense_rank(desc(off_explosiveness_PY1)),
      Rank_Off_Pwr_Success_PY1 = dense_rank(desc(off_power_success_PY1)),
      Rank_Off_Stuff_Rt_PY1 = dense_rank(off_stuff_rate_PY1),
      Rank_Off_Line_Yds_PY1 = dense_rank(desc(off_line_yds_PY1)),
      Rank_Off_Second_Lvl_Yds_PY1 = dense_rank(desc(off_second_lvl_yds_PY1)),
      Rank_Off_Open_Field_Yds_PY1 = dense_rank(desc(off_open_field_yds_PY1)),
      Rank_Off_Pts_Per_Opp_PY1 = dense_rank(desc(off_pts_per_opp_PY1)),
      Rank_Off_Field_Pos_Avg_Predicted_Pts_PY1 = dense_rank(desc(off_field_pos_avg_predicted_points_PY1)),
      Rank_Off_Havoc_Total_PY1 = dense_rank(off_havoc_total_PY1),
      Rank_Off_Havoc_Front_PY1 = dense_rank(off_havoc_front_seven_PY1),
      Rank_Off_Havoc_DB_PY1 = dense_rank(off_havoc_db_PY1),
      Rank_Off_Standard_Down_PPA_PY1 = dense_rank(desc(off_standard_downs_ppa_PY1)),
      Rank_Off_Standard_Down_Success_Rt_PY1 = dense_rank(desc(off_standard_downs_success_rate_PY1)),
      Rank_Off_Standard_Down_Explosiveness_PY1 = dense_rank(desc(off_standard_downs_explosiveness_PY1)),
      Rank_Off_Pass_Down_PPA_PY1 = dense_rank(desc(off_passing_downs_ppa_PY1)),
      Rank_Off_Pass_Down_Success_Rt_PY1 = dense_rank(desc(off_passing_downs_success_rate_PY1)),
      Rank_Off_Pass_Down_Explosiveness_PY1 = dense_rank(desc(off_passing_downs_explosiveness_PY1)),
      Rank_Off_Rush_Play_PPA_PY1 = dense_rank(desc(off_rushing_plays_ppa_PY1)),
      Rank_Off_Rush_Play_Success_Rt_PY1 = dense_rank(desc(off_rushing_plays_success_rate_PY1)),
      Rank_Off_Rush_Play_Explosiveness_PY1 = dense_rank(desc(off_rushing_plays_explosiveness_PY1)),
      Rank_Off_Pass_Play_PPA_PY1 = dense_rank(desc(off_passing_plays_ppa_PY1)),
      Rank_Off_Pass_Play_Success_Rt_PY1 = dense_rank(desc(off_passing_plays_success_rate_PY1)),
      Rank_Off_Pass_Play_Explosiveness_PY1 = dense_rank(desc(off_passing_plays_explosiveness_PY1)),
      Rank_Def_PPA_PY1 = dense_rank(def_ppa_PY1),
      Rank_Def_Success_Rt_PY1 = dense_rank(def_success_rate_PY1),
      Rank_Def_Explosiveness_PY1 = dense_rank(def_explosiveness_PY1),
      Rank_Def_Pwr_Success_PY1 = dense_rank(def_power_success_PY1),
      Rank_Def_Stuff_Rt_PY1 = dense_rank(desc(def_stuff_rate_PY1)),
      Rank_Def_Line_Yds_PY1 = dense_rank(def_line_yds_PY1),
      Rank_Def_Second_Lvl_Yds_PY1 = dense_rank(def_second_lvl_yds_PY1),
      Rank_Def_Open_Field_Yds_PY1 = dense_rank(def_open_field_yds_PY1),
      Rank_Def_Pts_Per_Opp_PY1 = dense_rank(def_pts_per_opp_PY1),
      Rank_Def_Field_Pos_Avg_Predicted_Pts_PY1 = dense_rank(def_field_pos_avg_predicted_points_PY1),
      Rank_Def_Havoc_Total_PY1 = dense_rank(desc(def_havoc_total_PY1)),
      Rank_Def_Havoc_Front_Seven_PY1 = dense_rank(desc(def_havoc_front_seven_PY1)),
      Rank_Def_Havoc_DB_PY1 = dense_rank(desc(def_havoc_db_PY1)),
      Rank_Def_Standard_Down_PPA_PY1 = dense_rank(def_standard_downs_ppa_PY1),
      Rank_Def_Standard_Down_Success_Rt_PY1 = dense_rank(def_standard_downs_success_rate_PY1),
      Rank_Def_Standard_Down_Explosiveness_PY1 = dense_rank(def_standard_downs_explosiveness_PY1),
      Rank_Def_Pass_Down_PPA_PY1 = dense_rank(def_passing_downs_ppa_PY1),
      Rank_Def_Pass_Down_Success_Rt_PY1 = dense_rank(def_passing_downs_success_rate_PY1),
      Rank_Def_Pass_Down_Explosiveness_PY1 = dense_rank(def_passing_downs_explosiveness_PY1),
      Rank_Def_Rush_Play_PPA_PY1 = dense_rank(def_rushing_plays_ppa_PY1),
      Rank_Def_Rush_Play_Success_Rt_PY1 = dense_rank(def_rushing_plays_success_rate_PY1),
      Rank_Def_Rush_Play_Explosiveness_PY1 = dense_rank(def_rushing_plays_explosiveness_PY1),
      Rank_Def_Pass_Play_PPA_PY1 = dense_rank(def_passing_plays_ppa_PY1),
      Rank_Def_Pass_Play_Success_Rt_PY1 = dense_rank(def_passing_plays_success_rate_PY1),
      Rank_Def_Pass_Play_Explosiveness_PY1 = dense_rank(def_passing_plays_explosiveness_PY1),
      Rank_Recruit_Pts_PY1 = dense_rank(desc(recruit_pts_PY1)),
      Rank_Talent_PY1 = dense_rank(desc(talent_PY1)),
      Rank_SP_Rating_PY1 = dense_rank(desc(sp_rating_PY1)),
      Rank_SP_Off_Rating_PY1 = dense_rank(desc(sp_offense_rating_PY1)),
      Rank_SP_Def_Rating_PY1 = dense_rank(sp_defense_rating_PY1),
      Rank_SP_SpecialTeams_Rating_PY1 = dense_rank(desc(sp_special_teams_rating_PY1)),
      Rank_FPI_PY1 = dense_rank(desc(FPI_PY1)),
      Rank_SRS_PY1 = dense_rank(desc(SRS_rating_PY1)),
      ## PY1 weighted 3 times
      Rank_Wins_PY1_col2 = dense_rank(desc(Wins_PY1)),
      Rank_Losses_PY1_col2 = dense_rank(Losses_PY1),
      Rank_Comp_Pct_PY1_col2 = dense_rank(desc(completion_pct_PY1)),
      Rank_Pass_YPA_PY1_col2 = dense_rank(desc(pass_ypa_PY1)),
      Rank_Pass_YPR_PY1_col2 = dense_rank(desc(pass_ypr_PY1)),
      Rank_Int_Pct_PY1_col2 = dense_rank(int_pct_PY1),
      Rank_Rush_YPC_PY1_col2 = dense_rank(desc(rush_ypc_PY1)),
      Rank_Turnovers_pg_PY1_col2 = dense_rank(turnovers_pg_PY1),
      Rank_Third_Conv_Rate_PY1_col2 = dense_rank(desc(third_conv_rate_PY1)),
      Rank_Fourth_Conv_Rate_PY1_col2 = dense_rank(desc(fourth_conv_rate_PY1)),
      Rank_Penalty_Yds_pg_PY1_col2 = dense_rank(penalty_yds_pg_PY1),
      Rank_Yds_Per_Penalty_PY1_col2 = dense_rank(yards_per_penalty_PY1),
      Rank_Kick_Return_Avg_PY1_col2 = dense_rank(desc(kick_return_avg_PY1)),
      Rank_Punt_Return_Avg_PY1_col2 = dense_rank(desc(punt_return_avg_PY1)),
      Rank_Total_Yds_pg_PY1_col2 = dense_rank(desc(total_yds_pg_PY1)),
      Rank_Pass_Yds_pg_PY1_col2 = dense_rank(desc(pass_yds_pg_PY1)),
      Rank_Rush_Yds_pg_PY1_col2 = dense_rank(desc(rush_yds_pg_PY1)),
      Rank_First_Downs_pg_PY1_col2 = dense_rank(desc(first_downs_pg_PY1)),
      Rank_Off_YPP_PY1_col2 = dense_rank(desc(off_ypp_PY1)),
      Rank_Def_Ints_pg_PY1_col2 = dense_rank(desc(def_interceptions_pg_PY1)),
      Rank_Off_PPA_PY1_col2 = dense_rank(desc(off_ppa_PY1)),
      Rank_Off_Success_Rt_PY1_col2 = dense_rank(desc(off_success_rate_PY1)),
      Rank_Off_Explosiveness_PY1_col2 = dense_rank(desc(off_explosiveness_PY1)),
      Rank_Off_Pwr_Success_PY1_col2 = dense_rank(desc(off_power_success_PY1)),
      Rank_Off_Stuff_Rt_PY1_col2 = dense_rank(off_stuff_rate_PY1),
      Rank_Off_Line_Yds_PY1_col2 = dense_rank(desc(off_line_yds_PY1)),
      Rank_Off_Second_Lvl_Yds_PY1_col2 = dense_rank(desc(off_second_lvl_yds_PY1)),
      Rank_Off_Open_Field_Yds_PY1_col2 = dense_rank(desc(off_open_field_yds_PY1)),
      Rank_Off_Pts_Per_Opp_PY1_col2 = dense_rank(desc(off_pts_per_opp_PY1)),
      Rank_Off_Field_Pos_Avg_Predicted_Pts_PY1_col2 = dense_rank(desc(off_field_pos_avg_predicted_points_PY1)),
      Rank_Off_Havoc_Total_PY1_col2 = dense_rank(off_havoc_total_PY1),
      Rank_Off_Havoc_Front_PY1_col2 = dense_rank(off_havoc_front_seven_PY1),
      Rank_Off_Havoc_DB_PY1_col2 = dense_rank(off_havoc_db_PY1),
      Rank_Off_Standard_Down_PPA_PY1_col2 = dense_rank(desc(off_standard_downs_ppa_PY1)),
      Rank_Off_Standard_Down_Success_Rt_PY1_col2 = dense_rank(desc(off_standard_downs_success_rate_PY1)),
      Rank_Off_Standard_Down_Explosiveness_PY1_col2 = dense_rank(desc(off_standard_downs_explosiveness_PY1)),
      Rank_Off_Pass_Down_PPA_PY1_col2 = dense_rank(desc(off_passing_downs_ppa_PY1)),
      Rank_Off_Pass_Down_Success_Rt_PY1_col2 = dense_rank(desc(off_passing_downs_success_rate_PY1)),
      Rank_Off_Pass_Down_Explosiveness_PY1_col2 = dense_rank(desc(off_passing_downs_explosiveness_PY1)),
      Rank_Off_Rush_Play_PPA_PY1_col2 = dense_rank(desc(off_rushing_plays_ppa_PY1)),
      Rank_Off_Rush_Play_Success_Rt_PY1_col2 = dense_rank(desc(off_rushing_plays_success_rate_PY1)),
      Rank_Off_Rush_Play_Explosiveness_PY1_col2 = dense_rank(desc(off_rushing_plays_explosiveness_PY1)),
      Rank_Off_Pass_Play_PPA_PY1_col2 = dense_rank(desc(off_passing_plays_ppa_PY1)),
      Rank_Off_Pass_Play_Success_Rt_PY1_col2 = dense_rank(desc(off_passing_plays_success_rate_PY1)),
      Rank_Off_Pass_Play_Explosiveness_PY1_col2 = dense_rank(desc(off_passing_plays_explosiveness_PY1)),
      Rank_Def_PPA_PY1_col2 = dense_rank(def_ppa_PY1),
      Rank_Def_Success_Rt_PY1_col2 = dense_rank(def_success_rate_PY1),
      Rank_Def_Explosiveness_PY1_col2 = dense_rank(def_explosiveness_PY1),
      Rank_Def_Pwr_Success_PY1_col2 = dense_rank(def_power_success_PY1),
      Rank_Def_Stuff_Rt_PY1_col2 = dense_rank(desc(def_stuff_rate_PY1)),
      Rank_Def_Line_Yds_PY1_col2 = dense_rank(def_line_yds_PY1),
      Rank_Def_Second_Lvl_Yds_PY1_col2 = dense_rank(def_second_lvl_yds_PY1),
      Rank_Def_Open_Field_Yds_PY1_col2 = dense_rank(def_open_field_yds_PY1),
      Rank_Def_Pts_Per_Opp_PY1_col2 = dense_rank(def_pts_per_opp_PY1),
      Rank_Def_Field_Pos_Avg_Predicted_Pts_PY1_col2 = dense_rank(def_field_pos_avg_predicted_points_PY1),
      Rank_Def_Havoc_Total_PY1_col2 = dense_rank(desc(def_havoc_total_PY1)),
      Rank_Def_Havoc_Front_Seven_PY1_col2 = dense_rank(desc(def_havoc_front_seven_PY1)),
      Rank_Def_Havoc_DB_PY1_col2 = dense_rank(desc(def_havoc_db_PY1)),
      Rank_Def_Standard_Down_PPA_PY1_col2 = dense_rank(def_standard_downs_ppa_PY1),
      Rank_Def_Standard_Down_Success_Rt_PY1_col2 = dense_rank(def_standard_downs_success_rate_PY1),
      Rank_Def_Standard_Down_Explosiveness_PY1_col2 = dense_rank(def_standard_downs_explosiveness_PY1),
      Rank_Def_Pass_Down_PPA_PY1_col2 = dense_rank(def_passing_downs_ppa_PY1),
      Rank_Def_Pass_Down_Success_Rt_PY1_col2 = dense_rank(def_passing_downs_success_rate_PY1),
      Rank_Def_Pass_Down_Explosiveness_PY1_col2 = dense_rank(def_passing_downs_explosiveness_PY1),
      Rank_Def_Rush_Play_PPA_PY1_col2 = dense_rank(def_rushing_plays_ppa_PY1),
      Rank_Def_Rush_Play_Success_Rt_PY1_col2 = dense_rank(def_rushing_plays_success_rate_PY1),
      Rank_Def_Rush_Play_Explosiveness_PY1_col2 = dense_rank(def_rushing_plays_explosiveness_PY1),
      Rank_Def_Pass_Play_PPA_PY1_col2 = dense_rank(def_passing_plays_ppa_PY1),
      Rank_Def_Pass_Play_Success_Rt_PY1_col2 = dense_rank(def_passing_plays_success_rate_PY1),
      Rank_Def_Pass_Play_Explosiveness_PY1_col2 = dense_rank(def_passing_plays_explosiveness_PY1),
      Rank_Recruit_Pts_PY1_col2 = dense_rank(desc(recruit_pts_PY1)),
      Rank_Talent_PY1_col2 = dense_rank(desc(talent_PY1)),
      ## PY1 weighted 3 times
      Rank_Wins_PY1_col3 = dense_rank(desc(Wins_PY1)),
      Rank_Losses_PY1_col3 = dense_rank(Losses_PY1),
      Rank_Comp_Pct_PY1_col3 = dense_rank(desc(completion_pct_PY1)),
      Rank_Pass_YPA_PY1_col3 = dense_rank(desc(pass_ypa_PY1)),
      Rank_Pass_YPR_PY1_col3 = dense_rank(desc(pass_ypr_PY1)),
      Rank_Int_Pct_PY1_col3 = dense_rank(int_pct_PY1),
      Rank_Rush_YPC_PY1_col3 = dense_rank(desc(rush_ypc_PY1)),
      Rank_Turnovers_pg_PY1_col3 = dense_rank(turnovers_pg_PY1),
      Rank_Third_Conv_Rate_PY1_col3 = dense_rank(desc(third_conv_rate_PY1)),
      Rank_Fourth_Conv_Rate_PY1_col3 = dense_rank(desc(fourth_conv_rate_PY1)),
      Rank_Penalty_Yds_pg_PY1_col3 = dense_rank(penalty_yds_pg_PY1),
      Rank_Yds_Per_Penalty_PY1_col3 = dense_rank(yards_per_penalty_PY1),
      Rank_Kick_Return_Avg_PY1_col3 = dense_rank(desc(kick_return_avg_PY1)),
      Rank_Punt_Return_Avg_PY1_col3 = dense_rank(desc(punt_return_avg_PY1)),
      Rank_Total_Yds_pg_PY1_col3 = dense_rank(desc(total_yds_pg_PY1)),
      Rank_Pass_Yds_pg_PY1_col3 = dense_rank(desc(pass_yds_pg_PY1)),
      Rank_Rush_Yds_pg_PY1_col3 = dense_rank(desc(rush_yds_pg_PY1)),
      Rank_First_Downs_pg_PY1_col3 = dense_rank(desc(first_downs_pg_PY1)),
      Rank_Off_YPP_PY1_col3 = dense_rank(desc(off_ypp_PY1)),
      Rank_Def_Ints_pg_PY1_col3 = dense_rank(desc(def_interceptions_pg_PY1)),
      Rank_Off_PPA_PY1_col3 = dense_rank(desc(off_ppa_PY1)),
      Rank_Off_Success_Rt_PY1_col3 = dense_rank(desc(off_success_rate_PY1)),
      Rank_Off_Explosiveness_PY1_col3 = dense_rank(desc(off_explosiveness_PY1)),
      Rank_Off_Pwr_Success_PY1_col3 = dense_rank(desc(off_power_success_PY1)),
      Rank_Off_Stuff_Rt_PY1_col3 = dense_rank(off_stuff_rate_PY1),
      Rank_Off_Line_Yds_PY1_col3 = dense_rank(desc(off_line_yds_PY1)),
      Rank_Off_Second_Lvl_Yds_PY1_col3 = dense_rank(desc(off_second_lvl_yds_PY1)),
      Rank_Off_Open_Field_Yds_PY1_col3 = dense_rank(desc(off_open_field_yds_PY1)),
      Rank_Off_Pts_Per_Opp_PY1_col3 = dense_rank(desc(off_pts_per_opp_PY1)),
      Rank_Off_Field_Pos_Avg_Predicted_Pts_PY1_col3 = dense_rank(desc(off_field_pos_avg_predicted_points_PY1)),
      Rank_Off_Havoc_Total_PY1_col3 = dense_rank(off_havoc_total_PY1),
      Rank_Off_Havoc_Front_PY1_col3 = dense_rank(off_havoc_front_seven_PY1),
      Rank_Off_Havoc_DB_PY1_col3 = dense_rank(off_havoc_db_PY1),
      Rank_Off_Standard_Down_PPA_PY1_col3 = dense_rank(desc(off_standard_downs_ppa_PY1)),
      Rank_Off_Standard_Down_Success_Rt_PY1_col3 = dense_rank(desc(off_standard_downs_success_rate_PY1)),
      Rank_Off_Standard_Down_Explosiveness_PY1_col3 = dense_rank(desc(off_standard_downs_explosiveness_PY1)),
      Rank_Off_Pass_Down_PPA_PY1_col3 = dense_rank(desc(off_passing_downs_ppa_PY1)),
      Rank_Off_Pass_Down_Success_Rt_PY1_col3 = dense_rank(desc(off_passing_downs_success_rate_PY1)),
      Rank_Off_Pass_Down_Explosiveness_PY1_col3 = dense_rank(desc(off_passing_downs_explosiveness_PY1)),
      Rank_Off_Rush_Play_PPA_PY1_col3 = dense_rank(desc(off_rushing_plays_ppa_PY1)),
      Rank_Off_Rush_Play_Success_Rt_PY1_col3 = dense_rank(desc(off_rushing_plays_success_rate_PY1)),
      Rank_Off_Rush_Play_Explosiveness_PY1_col3 = dense_rank(desc(off_rushing_plays_explosiveness_PY1)),
      Rank_Off_Pass_Play_PPA_PY1_col3 = dense_rank(desc(off_passing_plays_ppa_PY1)),
      Rank_Off_Pass_Play_Success_Rt_PY1_col3 = dense_rank(desc(off_passing_plays_success_rate_PY1)),
      Rank_Off_Pass_Play_Explosiveness_PY1_col3 = dense_rank(desc(off_passing_plays_explosiveness_PY1)),
      Rank_Def_PPA_PY1_col3 = dense_rank(def_ppa_PY1),
      Rank_Def_Success_Rt_PY1_col3 = dense_rank(def_success_rate_PY1),
      Rank_Def_Explosiveness_PY1_col3 = dense_rank(def_explosiveness_PY1),
      Rank_Def_Pwr_Success_PY1_col3 = dense_rank(def_power_success_PY1),
      Rank_Def_Stuff_Rt_PY1_col3 = dense_rank(desc(def_stuff_rate_PY1)),
      Rank_Def_Line_Yds_PY1_col3 = dense_rank(def_line_yds_PY1),
      Rank_Def_Second_Lvl_Yds_PY1_col3 = dense_rank(def_second_lvl_yds_PY1),
      Rank_Def_Open_Field_Yds_PY1_col3 = dense_rank(def_open_field_yds_PY1),
      Rank_Def_Pts_Per_Opp_PY1_col3 = dense_rank(def_pts_per_opp_PY1),
      Rank_Def_Field_Pos_Avg_Predicted_Pts_PY1_col3 = dense_rank(def_field_pos_avg_predicted_points_PY1),
      Rank_Def_Havoc_Total_PY1_col3 = dense_rank(desc(def_havoc_total_PY1)),
      Rank_Def_Havoc_Front_Seven_PY1_col3 = dense_rank(desc(def_havoc_front_seven_PY1)),
      Rank_Def_Havoc_DB_PY1_col3 = dense_rank(desc(def_havoc_db_PY1)),
      Rank_Def_Standard_Down_PPA_PY1_col3 = dense_rank(def_standard_downs_ppa_PY1),
      Rank_Def_Standard_Down_Success_Rt_PY1_col3 = dense_rank(def_standard_downs_success_rate_PY1),
      Rank_Def_Standard_Down_Explosiveness_PY1_col3 = dense_rank(def_standard_downs_explosiveness_PY1),
      Rank_Def_Pass_Down_PPA_PY1_col3 = dense_rank(def_passing_downs_ppa_PY1),
      Rank_Def_Pass_Down_Success_Rt_PY1_col3 = dense_rank(def_passing_downs_success_rate_PY1),
      Rank_Def_Pass_Down_Explosiveness_PY1_col3 = dense_rank(def_passing_downs_explosiveness_PY1),
      Rank_Def_Rush_Play_PPA_PY1_col3 = dense_rank(def_rushing_plays_ppa_PY1),
      Rank_Def_Rush_Play_Success_Rt_PY1_col3 = dense_rank(def_rushing_plays_success_rate_PY1),
      Rank_Def_Rush_Play_Explosiveness_PY1_col3 = dense_rank(def_rushing_plays_explosiveness_PY1),
      Rank_Def_Pass_Play_PPA_PY1_col3 = dense_rank(def_passing_plays_ppa_PY1),
      Rank_Def_Pass_Play_Success_Rt_PY1_col3 = dense_rank(def_passing_plays_success_rate_PY1),
      Rank_Def_Pass_Play_Explosiveness_PY1_col3 = dense_rank(def_passing_plays_explosiveness_PY1),
      Rank_Talent_PY1_col3 = dense_rank(desc(talent_PY1)),
      ## incoming recruiting class, weighted once
      ## PY3 recruit pts added in starting in 2023 season, weighted 3x
      ## other PY3 data still phased out
      Rank_Recruit_Pts = dense_rank(desc(recruit_pts)),
      Rank_Recruit_Pts_PY3 = dense_rank(desc(recruit_pts_PY3)),
      Rank_Recruit_Pts_PY3_col2 = dense_rank(desc(recruit_pts_PY3)),
      Rank_Recruit_Pts_PY3_col3 = dense_rank(desc(recruit_pts_PY3)),
      ## Extra weighted variables, weighted 2x (3 more times)
      Rank_Off_YPP_PY1_col4 = dense_rank(desc(off_ypp_PY1)),
      Rank_Off_PPA_PY1_col4 = dense_rank(desc(off_ppa_PY1)),
      Rank_Off_Success_Rt_PY1_col4 = dense_rank(desc(off_success_rate_PY1)),
      Rank_Off_Explosiveness_PY1_col4 = dense_rank(desc(off_explosiveness_PY1)),
      Rank_Off_Pwr_Success_PY1_col4 = dense_rank(desc(off_power_success_PY1)),
      Rank_Off_Stuff_Rt_PY1_col4 = dense_rank(off_stuff_rate_PY1),
      Rank_Off_Pts_Per_Opp_PY1_col4 = dense_rank(desc(off_pts_per_opp_PY1)),
      Rank_Off_Havoc_Total_PY1_col4 = dense_rank(off_havoc_total_PY1),
      Rank_Off_Havoc_Front_PY1_col4 = dense_rank(off_havoc_front_seven_PY1),
      Rank_Off_Havoc_DB_PY1_col4 = dense_rank(off_havoc_db_PY1),
      Rank_Off_Standard_Down_PPA_PY1_col4 = dense_rank(desc(off_standard_downs_ppa_PY1)),
      Rank_Off_Standard_Down_Success_Rt_PY1_col4 = dense_rank(desc(off_standard_downs_success_rate_PY1)),
      Rank_Off_Standard_Down_Explosiveness_PY1_col4 = dense_rank(desc(off_standard_downs_explosiveness_PY1)),
      Rank_Off_Pass_Down_PPA_PY1_col4 = dense_rank(desc(off_passing_downs_ppa_PY1)),
      Rank_Off_Pass_Down_Success_Rt_PY1_col4 = dense_rank(desc(off_passing_downs_success_rate_PY1)),
      Rank_Off_Pass_Down_Explosiveness_PY1_col4 = dense_rank(desc(off_passing_downs_explosiveness_PY1)),
      Rank_Off_Rush_Play_PPA_PY1_col4 = dense_rank(desc(off_rushing_plays_ppa_PY1)),
      Rank_Off_Rush_Play_Success_Rt_PY1_col4 = dense_rank(desc(off_rushing_plays_success_rate_PY1)),
      Rank_Off_Rush_Play_Explosiveness_PY1_col4 = dense_rank(desc(off_rushing_plays_explosiveness_PY1)),
      Rank_Off_Pass_Play_PPA_PY1_col4 = dense_rank(desc(off_passing_plays_ppa_PY1)),
      Rank_Off_Pass_Play_Success_Rt_PY1_col4 = dense_rank(desc(off_passing_plays_success_rate_PY1)),
      Rank_Off_Pass_Play_Explosiveness_PY1_col4 = dense_rank(desc(off_passing_plays_explosiveness_PY1)),
      Rank_Def_PPA_PY1_col4 = dense_rank(def_ppa_PY1),
      Rank_Def_Success_Rt_PY1_col4 = dense_rank(def_success_rate_PY1),
      Rank_Def_Explosiveness_PY1_col4 = dense_rank(def_explosiveness_PY1),
      Rank_Def_Pwr_Success_PY1_col4 = dense_rank(def_power_success_PY1),
      Rank_Def_Stuff_Rt_PY1_col4 = dense_rank(desc(def_stuff_rate_PY1)),
      Rank_Def_Pts_Per_Opp_PY1_col4 = dense_rank(def_pts_per_opp_PY1),
      Rank_Def_Havoc_Total_PY1_col4 = dense_rank(desc(def_havoc_total_PY1)),
      Rank_Def_Havoc_Front_Seven_PY1_col4 = dense_rank(desc(def_havoc_front_seven_PY1)),
      Rank_Def_Havoc_DB_PY1_col4 = dense_rank(desc(def_havoc_db_PY1)),
      Rank_Def_Standard_Down_PPA_PY1_col4 = dense_rank(def_standard_downs_ppa_PY1),
      Rank_Def_Standard_Down_Success_Rt_PY1_col4 = dense_rank(def_standard_downs_success_rate_PY1),
      Rank_Def_Standard_Down_Explosiveness_PY1_col4 = dense_rank(def_standard_downs_explosiveness_PY1),
      Rank_Def_Pass_Down_PPA_PY1_col4 = dense_rank(def_passing_downs_ppa_PY1),
      Rank_Def_Pass_Down_Success_Rt_PY1_col4 = dense_rank(def_passing_downs_success_rate_PY1),
      Rank_Def_Pass_Down_Explosiveness_PY1_col4 = dense_rank(def_passing_downs_explosiveness_PY1),
      Rank_Def_Rush_Play_PPA_PY1_col4 = dense_rank(def_rushing_plays_ppa_PY1),
      Rank_Def_Rush_Play_Success_Rt_PY1_col4 = dense_rank(def_rushing_plays_success_rate_PY1),
      Rank_Def_Rush_Play_Explosiveness_PY1_col4 = dense_rank(def_rushing_plays_explosiveness_PY1),
      Rank_Def_Pass_Play_PPA_PY1_col4 = dense_rank(def_passing_plays_ppa_PY1),
      Rank_Def_Pass_Play_Success_Rt_PY1_col4 = dense_rank(def_passing_plays_success_rate_PY1),
      Rank_Def_Pass_Play_Explosiveness_PY1_col4 = dense_rank(def_passing_plays_explosiveness_PY1),
      ## Extra weighted variables, weighted 2x (2 more times)
      Rank_Off_YPP_PY1_col5 = dense_rank(desc(off_ypp_PY1)),
      Rank_Off_PPA_PY1_col5 = dense_rank(desc(off_ppa_PY1)),
      Rank_Off_Success_Rt_PY1_col5 = dense_rank(desc(off_success_rate_PY1)),
      Rank_Off_Explosiveness_PY1_col5 = dense_rank(desc(off_explosiveness_PY1)),
      Rank_Off_Pwr_Success_PY1_col5 = dense_rank(desc(off_power_success_PY1)),
      Rank_Off_Stuff_Rt_PY1_col5 = dense_rank(off_stuff_rate_PY1),
      Rank_Off_Pts_Per_Opp_PY1_col5 = dense_rank(desc(off_pts_per_opp_PY1)),
      Rank_Off_Havoc_Total_PY1_col5 = dense_rank(off_havoc_total_PY1),
      Rank_Off_Havoc_Front_PY1_col5 = dense_rank(off_havoc_front_seven_PY1),
      Rank_Off_Havoc_DB_PY1_col5 = dense_rank(off_havoc_db_PY1),
      Rank_Off_Standard_Down_PPA_PY1_col5 = dense_rank(desc(off_standard_downs_ppa_PY1)),
      Rank_Off_Standard_Down_Success_Rt_PY1_col5 = dense_rank(desc(off_standard_downs_success_rate_PY1)),
      Rank_Off_Standard_Down_Explosiveness_PY1_col5 = dense_rank(desc(off_standard_downs_explosiveness_PY1)),
      Rank_Off_Pass_Down_PPA_PY1_col5 = dense_rank(desc(off_passing_downs_ppa_PY1)),
      Rank_Off_Pass_Down_Success_Rt_PY1_col5 = dense_rank(desc(off_passing_downs_success_rate_PY1)),
      Rank_Off_Pass_Down_Explosiveness_PY1_col5 = dense_rank(desc(off_passing_downs_explosiveness_PY1)),
      Rank_Off_Rush_Play_PPA_PY1_col5 = dense_rank(desc(off_rushing_plays_ppa_PY1)),
      Rank_Off_Rush_Play_Success_Rt_PY1_col5 = dense_rank(desc(off_rushing_plays_success_rate_PY1)),
      Rank_Off_Rush_Play_Explosiveness_PY1_col5 = dense_rank(desc(off_rushing_plays_explosiveness_PY1)),
      Rank_Off_Pass_Play_PPA_PY1_col5 = dense_rank(desc(off_passing_plays_ppa_PY1)),
      Rank_Off_Pass_Play_Success_Rt_PY1_col5 = dense_rank(desc(off_passing_plays_success_rate_PY1)),
      Rank_Off_Pass_Play_Explosiveness_PY1_col5 = dense_rank(desc(off_passing_plays_explosiveness_PY1)),
      Rank_Def_PPA_PY1_col5 = dense_rank(def_ppa_PY1),
      Rank_Def_Success_Rt_PY1_col5 = dense_rank(def_success_rate_PY1),
      Rank_Def_Explosiveness_PY1_col5 = dense_rank(def_explosiveness_PY1),
      Rank_Def_Pwr_Success_PY1_col5 = dense_rank(def_power_success_PY1),
      Rank_Def_Stuff_Rt_PY1_col5 = dense_rank(desc(def_stuff_rate_PY1)),
      Rank_Def_Pts_Per_Opp_PY1_col5 = dense_rank(def_pts_per_opp_PY1),
      Rank_Def_Havoc_Total_PY1_col5 = dense_rank(desc(def_havoc_total_PY1)),
      Rank_Def_Havoc_Front_Seven_PY1_col5 = dense_rank(desc(def_havoc_front_seven_PY1)),
      Rank_Def_Havoc_DB_PY1_col5 = dense_rank(desc(def_havoc_db_PY1)),
      Rank_Def_Standard_Down_PPA_PY1_col5 = dense_rank(def_standard_downs_ppa_PY1),
      Rank_Def_Standard_Down_Success_Rt_PY1_col5 = dense_rank(def_standard_downs_success_rate_PY1),
      Rank_Def_Standard_Down_Explosiveness_PY1_col5 = dense_rank(def_standard_downs_explosiveness_PY1),
      Rank_Def_Pass_Down_PPA_PY1_col5 = dense_rank(def_passing_downs_ppa_PY1),
      Rank_Def_Pass_Down_Success_Rt_PY1_col5 = dense_rank(def_passing_downs_success_rate_PY1),
      Rank_Def_Pass_Down_Explosiveness_PY1_col5 = dense_rank(def_passing_downs_explosiveness_PY1),
      Rank_Def_Rush_Play_PPA_PY1_col5 = dense_rank(def_rushing_plays_ppa_PY1),
      Rank_Def_Rush_Play_Success_Rt_PY1_col5 = dense_rank(def_rushing_plays_success_rate_PY1),
      Rank_Def_Rush_Play_Explosiveness_PY1_col5 = dense_rank(def_rushing_plays_explosiveness_PY1),
      Rank_Def_Pass_Play_PPA_PY1_col5 = dense_rank(def_passing_plays_ppa_PY1),
      Rank_Def_Pass_Play_Success_Rt_PY1_col5 = dense_rank(def_passing_plays_success_rate_PY1),
      Rank_Def_Pass_Play_Explosiveness_PY1_col5 = dense_rank(def_passing_plays_explosiveness_PY1),
      ## Extra weighted variables, weighted 2x (1 more time)
      Rank_Off_YPP_PY1_col6 = dense_rank(desc(off_ypp_PY1)),
      Rank_Off_PPA_PY1_col6 = dense_rank(desc(off_ppa_PY1)),
      Rank_Off_Success_Rt_PY1_col6 = dense_rank(desc(off_success_rate_PY1)),
      Rank_Off_Explosiveness_PY1_col6 = dense_rank(desc(off_explosiveness_PY1)),
      Rank_Off_Pwr_Success_PY1_col6 = dense_rank(desc(off_power_success_PY1)),
      Rank_Off_Stuff_Rt_PY1_col6 = dense_rank(off_stuff_rate_PY1),
      Rank_Off_Pts_Per_Opp_PY1_col6 = dense_rank(desc(off_pts_per_opp_PY1)),
      Rank_Off_Havoc_Total_PY1_col6 = dense_rank(off_havoc_total_PY1),
      Rank_Off_Havoc_Front_PY1_col6 = dense_rank(off_havoc_front_seven_PY1),
      Rank_Off_Havoc_DB_PY1_col6 = dense_rank(off_havoc_db_PY1),
      Rank_Off_Standard_Down_PPA_PY1_col6 = dense_rank(desc(off_standard_downs_ppa_PY1)),
      Rank_Off_Standard_Down_Success_Rt_PY1_col6 = dense_rank(desc(off_standard_downs_success_rate_PY1)),
      Rank_Off_Standard_Down_Explosiveness_PY1_col6 = dense_rank(desc(off_standard_downs_explosiveness_PY1)),
      Rank_Off_Pass_Down_PPA_PY1_col6 = dense_rank(desc(off_passing_downs_ppa_PY1)),
      Rank_Off_Pass_Down_Success_Rt_PY1_col6 = dense_rank(desc(off_passing_downs_success_rate_PY1)),
      Rank_Off_Pass_Down_Explosiveness_PY1_col6 = dense_rank(desc(off_passing_downs_explosiveness_PY1)),
      Rank_Off_Rush_Play_PPA_PY1_col6 = dense_rank(desc(off_rushing_plays_ppa_PY1)),
      Rank_Off_Rush_Play_Success_Rt_PY1_col6 = dense_rank(desc(off_rushing_plays_success_rate_PY1)),
      Rank_Off_Rush_Play_Explosiveness_PY1_col6 = dense_rank(desc(off_rushing_plays_explosiveness_PY1)),
      Rank_Off_Pass_Play_PPA_PY1_col6 = dense_rank(desc(off_passing_plays_ppa_PY1)),
      Rank_Off_Pass_Play_Success_Rt_PY1_col6 = dense_rank(desc(off_passing_plays_success_rate_PY1)),
      Rank_Off_Pass_Play_Explosiveness_PY1_col6 = dense_rank(desc(off_passing_plays_explosiveness_PY1)),
      Rank_Def_PPA_PY1_col6 = dense_rank(def_ppa_PY1),
      Rank_Def_Success_Rt_PY1_col6 = dense_rank(def_success_rate_PY1),
      Rank_Def_Explosiveness_PY1_col6 = dense_rank(def_explosiveness_PY1),
      Rank_Def_Pwr_Success_PY1_col6 = dense_rank(def_power_success_PY1),
      Rank_Def_Stuff_Rt_PY1_col6 = dense_rank(desc(def_stuff_rate_PY1)),
      Rank_Def_Pts_Per_Opp_PY1_col6 = dense_rank(def_pts_per_opp_PY1),
      Rank_Def_Havoc_Total_PY1_col6 = dense_rank(desc(def_havoc_total_PY1)),
      Rank_Def_Havoc_Front_Seven_PY1_col6 = dense_rank(desc(def_havoc_front_seven_PY1)),
      Rank_Def_Havoc_DB_PY1_col6 = dense_rank(desc(def_havoc_db_PY1)),
      Rank_Def_Standard_Down_PPA_PY1_col6 = dense_rank(def_standard_downs_ppa_PY1),
      Rank_Def_Standard_Down_Success_Rt_PY1_col6 = dense_rank(def_standard_downs_success_rate_PY1),
      Rank_Def_Standard_Down_Explosiveness_PY1_col6 = dense_rank(def_standard_downs_explosiveness_PY1),
      Rank_Def_Pass_Down_PPA_PY1_col6 = dense_rank(def_passing_downs_ppa_PY1),
      Rank_Def_Pass_Down_Success_Rt_PY1_col6 = dense_rank(def_passing_downs_success_rate_PY1),
      Rank_Def_Pass_Down_Explosiveness_PY1_col6 = dense_rank(def_passing_downs_explosiveness_PY1),
      Rank_Def_Rush_Play_PPA_PY1_col6 = dense_rank(def_rushing_plays_ppa_PY1),
      Rank_Def_Rush_Play_Success_Rt_PY1_col6 = dense_rank(def_rushing_plays_success_rate_PY1),
      Rank_Def_Rush_Play_Explosiveness_PY1_col6 = dense_rank(def_rushing_plays_explosiveness_PY1),
      Rank_Def_Pass_Play_PPA_PY1_col6 = dense_rank(def_passing_plays_ppa_PY1),
      Rank_Def_Pass_Play_Success_Rt_PY1_col6 = dense_rank(def_passing_plays_success_rate_PY1),
      Rank_Def_Pass_Play_Explosiveness_PY1_col6 = dense_rank(def_passing_plays_explosiveness_PY1),
      ## FPI_SP mean ranks added at the end, weighted once
      Rank_FPI_SP_SRS_PY2_mean = dense_rank(desc(FPI_SP_SRS_PY2_mean)),
      Rank_FPI_SP_SRS_PY1_mean = dense_rank(desc(FPI_SP_SRS_PY1_mean)),
      Rank_AllPY_FPI_SP_SRS_mean = dense_rank(desc(AllPY_FPI_SP_SRS_mean)),
      Rank_WeightedAllPY_FPI_SP_SRS_mean = dense_rank(desc(WeightedAllPY_FPI_SP_SRS_mean)),
      ## Ranking current stats
      Rank_Wins = dense_rank(desc(Wins)),
      Rank_Losses = dense_rank(Losses),
      Rank_Comp_Pct = dense_rank(desc(completion_pct)),
      Rank_Pass_YPA = dense_rank(desc(pass_ypa)),
      Rank_Pass_YPR = dense_rank(desc(pass_ypr)),
      Rank_Int_Pct = dense_rank(int_pct),
      Rank_Rush_YPC = dense_rank(desc(rush_ypc)),
      Rank_Turnovers_pg = dense_rank(turnovers_pg),
      Rank_Third_Conv_Rate = dense_rank(desc(third_conv_rate)),
      Rank_Fourth_Conv_Rate = dense_rank(desc(fourth_conv_rate)),
      Rank_Penalty_Yds_pg = dense_rank(penalty_yds_pg),
      Rank_Yds_Per_Penalty = dense_rank(yards_per_penalty),
      Rank_Kick_Return_Avg = dense_rank(desc(kick_return_avg)),
      Rank_Punt_Return_Avg = dense_rank(desc(punt_return_avg)),
      Rank_Total_Yds_pg = dense_rank(desc(total_yds_pg)),
      Rank_Pass_Yds_pg = dense_rank(desc(pass_yds_pg)),
      Rank_Rush_Yds_pg = dense_rank(desc(rush_yds_pg)),
      Rank_First_Downs_pg = dense_rank(desc(first_downs_pg)),
      Rank_Off_YPP = dense_rank(desc(off_ypp)),
      Rank_Def_Ints_pg = dense_rank(desc(def_interceptions_pg)),
      Rank_Off_PPA = dense_rank(desc(off_ppa)),
      Rank_Off_Success_Rt = dense_rank(desc(off_success_rate)),
      Rank_Off_Explosiveness = dense_rank(desc(off_explosiveness)),
      Rank_Off_Pwr_Success = dense_rank(desc(off_power_success)),
      Rank_Off_Stuff_Rt = dense_rank(off_stuff_rate),
      Rank_Off_Line_Yds = dense_rank(desc(off_line_yds)),
      Rank_Off_Second_Lvl_Yds = dense_rank(desc(off_second_lvl_yds)),
      Rank_Off_Open_Field_Yds = dense_rank(desc(off_open_field_yds)),
      Rank_Off_Pts_Per_Opp = dense_rank(desc(off_pts_per_opp)),
      Rank_Off_Field_Pos_Avg_Predicted_Pts = dense_rank(desc(off_field_pos_avg_predicted_points)),
      Rank_Off_Havoc_Total = dense_rank(off_havoc_total),
      Rank_Off_Havoc_Front = dense_rank(off_havoc_front_seven),
      Rank_Off_Havoc_DB = dense_rank(off_havoc_db),
      Rank_Off_Standard_Down_PPA = dense_rank(desc(off_standard_downs_ppa)),
      Rank_Off_Standard_Down_Success_Rt = dense_rank(desc(off_standard_downs_success_rate)),
      Rank_Off_Standard_Down_Explosiveness = dense_rank(desc(off_standard_downs_explosiveness)),
      Rank_Off_Pass_Down_PPA = dense_rank(desc(off_passing_downs_ppa)),
      Rank_Off_Pass_Down_Success_Rt = dense_rank(desc(off_passing_downs_success_rate)),
      Rank_Off_Pass_Down_Explosiveness = dense_rank(desc(off_passing_downs_explosiveness)),
      Rank_Off_Rush_Play_PPA = dense_rank(desc(off_rushing_plays_ppa)),
      Rank_Off_Rush_Play_Success_Rt = dense_rank(desc(off_rushing_plays_success_rate)),
      Rank_Off_Rush_Play_Explosiveness = dense_rank(desc(off_rushing_plays_explosiveness)),
      Rank_Off_Pass_Play_PPA = dense_rank(desc(off_passing_plays_ppa)),
      Rank_Off_Pass_Play_Success_Rt = dense_rank(desc(off_passing_plays_success_rate)),
      Rank_Off_Pass_Play_Explosiveness = dense_rank(desc(off_passing_plays_explosiveness)),
      Rank_Def_PPA = dense_rank(def_ppa),
      Rank_Def_Success_Rt = dense_rank(def_success_rate),
      Rank_Def_Explosiveness = dense_rank(def_explosiveness),
      Rank_Def_Pwr_Success = dense_rank(def_power_success),
      Rank_Def_Stuff_Rt = dense_rank(desc(def_stuff_rate)),
      Rank_Def_Line_Yds = dense_rank(def_line_yds),
      Rank_Def_Second_Lvl_Yds = dense_rank(def_second_lvl_yds),
      Rank_Def_Open_Field_Yds = dense_rank(def_open_field_yds),
      Rank_Def_Pts_Per_Opp = dense_rank(def_pts_per_opp),
      Rank_Def_Field_Pos_Avg_Predicted_Pts = dense_rank(def_field_pos_avg_predicted_points),
      Rank_Def_Havoc_Total = dense_rank(desc(def_havoc_total)),
      Rank_Def_Havoc_Front_Seven = dense_rank(desc(def_havoc_front_seven)),
      Rank_Def_Havoc_DB = dense_rank(desc(def_havoc_db)),
      Rank_Def_Standard_Down_PPA = dense_rank(def_standard_downs_ppa),
      Rank_Def_Standard_Down_Success_Rt = dense_rank(def_standard_downs_success_rate),
      Rank_Def_Standard_Down_Explosiveness = dense_rank(def_standard_downs_explosiveness),
      Rank_Def_Pass_Down_PPA = dense_rank(def_passing_downs_ppa),
      Rank_Def_Pass_Down_Success_Rt = dense_rank(def_passing_downs_success_rate),
      Rank_Def_Pass_Down_Explosiveness = dense_rank(def_passing_downs_explosiveness),
      Rank_Def_Rush_Play_PPA = dense_rank(def_rushing_plays_ppa),
      Rank_Def_Rush_Play_Success_Rt = dense_rank(def_rushing_plays_success_rate),
      Rank_Def_Rush_Play_Explosiveness = dense_rank(def_rushing_plays_explosiveness),
      Rank_Def_Pass_Play_PPA = dense_rank(def_passing_plays_ppa),
      Rank_Def_Pass_Play_Success_Rt = dense_rank(def_passing_plays_success_rate),
      Rank_Def_Pass_Play_Explosiveness = dense_rank(def_passing_plays_explosiveness),
      Rank_SP_Rating = dense_rank(desc(sp_rating)),
      Rank_SP_Off_Rating = dense_rank(desc(sp_offense_rating)),
      Rank_SP_Def_Rating = dense_rank(sp_defense_rating),
      Rank_SP_SpecialTeams_Rating = dense_rank(desc(sp_special_teams_rating)),
      Rank_FPI = dense_rank(desc(FPI)),
      Rank_FPI_SP_mean = dense_rank(desc(FPI_SP_mean)),
      Rank_FPI_SRS_mean = dense_rank(desc(FPI_SRS_mean)),
      # due to availability issues, SP_Rankings not always included with current data
      ## Extra weighted variables for current year
      Rank_Wins_col2 = dense_rank(desc(Wins)),
      Rank_Losses_col2 = dense_rank(Losses),
      Rank_Off_YPP_col2 = dense_rank(desc(off_ypp)),
      Rank_Off_PPA_col2 = dense_rank(desc(off_ppa)),
      Rank_Off_Success_Rt_col2 = dense_rank(desc(off_success_rate)),
      Rank_Off_Explosiveness_col2 = dense_rank(desc(off_explosiveness)),
      Rank_Off_Pwr_Success_col2 = dense_rank(desc(off_power_success)),
      Rank_Off_Stuff_Rt_col2 = dense_rank(off_stuff_rate),
      Rank_Off_Pts_Per_Opp_col2 = dense_rank(desc(off_pts_per_opp)),
      Rank_Off_Havoc_Total_col2 = dense_rank(off_havoc_total),
      Rank_Off_Havoc_Front_col2 = dense_rank(off_havoc_front_seven),
      Rank_Off_Havoc_DB_col2 = dense_rank(off_havoc_db),
      Rank_Off_Standard_Down_PPA_col2 = dense_rank(desc(off_standard_downs_ppa)),
      Rank_Off_Standard_Down_Success_Rt_col2 = dense_rank(desc(off_standard_downs_success_rate)),
      Rank_Off_Standard_Down_Explosiveness_col2 = dense_rank(desc(off_standard_downs_explosiveness)),
      Rank_Off_Pass_Down_PPA_col2 = dense_rank(desc(off_passing_downs_ppa)),
      Rank_Off_Pass_Down_Success_Rt_col2 = dense_rank(desc(off_passing_downs_success_rate)),
      Rank_Off_Pass_Down_Explosiveness_col2 = dense_rank(desc(off_passing_downs_explosiveness)),
      Rank_Off_Rush_Play_PPA_col2 = dense_rank(desc(off_rushing_plays_ppa)),
      Rank_Off_Rush_Play_Success_Rt_col2 = dense_rank(desc(off_rushing_plays_success_rate)),
      Rank_Off_Rush_Play_Explosiveness_col2 = dense_rank(desc(off_rushing_plays_explosiveness)),
      Rank_Off_Pass_Play_PPA_col2 = dense_rank(desc(off_passing_plays_ppa)),
      Rank_Off_Pass_Play_Success_Rt_col2 = dense_rank(desc(off_passing_plays_success_rate)),
      Rank_Off_Pass_Play_Explosiveness_col2 = dense_rank(desc(off_passing_plays_explosiveness)),
      Rank_Def_PPA_col2 = dense_rank(def_ppa),
      Rank_Def_Success_Rt_col2 = dense_rank(def_success_rate),
      Rank_Def_Explosiveness_col2 = dense_rank(def_explosiveness),
      Rank_Def_Pwr_Success_col2 = dense_rank(def_power_success),
      Rank_Def_Stuff_Rt_col2 = dense_rank(desc(def_stuff_rate)),
      Rank_Def_Pts_Per_Opp_col2 = dense_rank(def_pts_per_opp),
      Rank_Def_Havoc_Total_col2 = dense_rank(desc(def_havoc_total)),
      Rank_Def_Havoc_Front_Seven_col2 = dense_rank(desc(def_havoc_front_seven)),
      Rank_Def_Havoc_DB_col2 = dense_rank(desc(def_havoc_db)),
      Rank_Def_Standard_Down_PPA_col2 = dense_rank(def_standard_downs_ppa),
      Rank_Def_Standard_Down_Success_Rt_col2 = dense_rank(def_standard_downs_success_rate),
      Rank_Def_Standard_Down_Explosiveness_col2 = dense_rank(def_standard_downs_explosiveness),
      Rank_Def_Pass_Down_PPA_col2 = dense_rank(def_passing_downs_ppa),
      Rank_Def_Pass_Down_Success_Rt_col2 = dense_rank(def_passing_downs_success_rate),
      Rank_Def_Pass_Down_Explosiveness_col2 = dense_rank(def_passing_downs_explosiveness),
      Rank_Def_Rush_Play_PPA_col2 = dense_rank(def_rushing_plays_ppa),
      Rank_Def_Rush_Play_Success_Rt_col2 = dense_rank(def_rushing_plays_success_rate),
      Rank_Def_Rush_Play_Explosiveness_col2 = dense_rank(def_rushing_plays_explosiveness),
      Rank_Def_Pass_Play_PPA_col2 = dense_rank(def_passing_plays_ppa),
      Rank_Def_Pass_Play_Success_Rt_col2 = dense_rank(def_passing_plays_success_rate),
      Rank_Def_Pass_Play_Explosiveness_col2 = dense_rank(def_passing_plays_explosiveness))
} else if (as.numeric(week) == 3) {
  ##### Week 3 Variable Ranks #####
  # PY2 weighted 2x, PY1 weighted 2x, current weighted 1x
  VoA_Variables <- VoA_Variables |>
    ## PY2 ranks
    mutate(Rank_Wins_PY2 = dense_rank(desc(Wins_PY2)),
      Rank_Losses_PY2 = dense_rank(Losses_PY2),
      Rank_Comp_Pct_PY2 = dense_rank(desc(completion_pct_PY2)),
      Rank_Pass_YPA_PY2 = dense_rank(desc(pass_ypa_PY2)),
      Rank_Pass_YPR_PY2 = dense_rank(desc(pass_ypr_PY2)),
      Rank_Int_Pct_PY2 = dense_rank(int_pct_PY2),
      Rank_Rush_YPC_PY2 = dense_rank(desc(rush_ypc_PY2)),
      Rank_Turnovers_pg_PY2 = dense_rank(turnovers_pg_PY2),
      Rank_Third_Conv_Rate_PY2 = dense_rank(desc(third_conv_rate_PY2)),
      Rank_Fourth_Conv_Rate_PY2 = dense_rank(desc(fourth_conv_rate_PY2)),
      Rank_Penalty_Yds_pg_PY2 = dense_rank(penalty_yds_pg_PY2),
      Rank_Yds_Per_Penalty_PY2 = dense_rank(yards_per_penalty_PY2),
      Rank_Kick_Return_Avg_PY2 = dense_rank(desc(kick_return_avg_PY2)),
      Rank_Punt_Return_Avg_PY2 = dense_rank(desc(punt_return_avg_PY2)),
      Rank_Total_Yds_pg_PY2 = dense_rank(desc(total_yds_pg_PY2)),
      Rank_Pass_Yds_pg_PY2 = dense_rank(desc(pass_yds_pg_PY2)),
      Rank_Rush_Yds_pg_PY2 = dense_rank(desc(rush_yds_pg_PY2)),
      Rank_First_Downs_pg_PY2 = dense_rank(desc(first_downs_pg_PY2)),
      Rank_Off_YPP_PY2 = dense_rank(desc(off_ypp_PY2)),
      Rank_Def_Ints_pg_PY2 = dense_rank(desc(def_interceptions_pg_PY2)),
      Rank_Off_PPA_PY2 = dense_rank(desc(off_ppa_PY2)),
      Rank_Off_Success_Rt_PY2 = dense_rank(desc(off_success_rate_PY2)),
      Rank_Off_Explosiveness_PY2 = dense_rank(desc(off_explosiveness_PY2)),
      Rank_Off_Pwr_Success_PY2 = dense_rank(desc(off_power_success_PY2)),
      Rank_Off_Stuff_Rt_PY2 = dense_rank(off_stuff_rate_PY2),
      Rank_Off_Line_Yds_PY2 = dense_rank(desc(off_line_yds_PY2)),
      Rank_Off_Second_Lvl_Yds_PY2 = dense_rank(desc(off_second_lvl_yds_PY2)),
      Rank_Off_Open_Field_Yds_PY2 = dense_rank(desc(off_open_field_yds_PY2)),
      Rank_Off_Pts_Per_Opp_PY2 = dense_rank(desc(off_pts_per_opp_PY2)),
      Rank_Off_Field_Pos_Avg_Predicted_Pts_PY2 = dense_rank(desc(off_field_pos_avg_predicted_points_PY2)),
      Rank_Off_Havoc_Total_PY2 = dense_rank(off_havoc_total_PY2),
      Rank_Off_Havoc_Front_PY2 = dense_rank(off_havoc_front_seven_PY2),
      Rank_Off_Havoc_DB_PY2 = dense_rank(off_havoc_db_PY2),
      Rank_Off_Standard_Down_PPA_PY2 = dense_rank(desc(off_standard_downs_ppa_PY2)),
      Rank_Off_Standard_Down_Success_Rt_PY2 = dense_rank(desc(off_standard_downs_success_rate_PY2)),
      Rank_Off_Standard_Down_Explosiveness_PY2 = dense_rank(desc(off_standard_downs_explosiveness_PY2)),
      Rank_Off_Pass_Down_PPA_PY2 = dense_rank(desc(off_passing_downs_ppa_PY2)),
      Rank_Off_Pass_Down_Success_Rt_PY2 = dense_rank(desc(off_passing_downs_success_rate_PY2)),
      Rank_Off_Pass_Down_Explosiveness_PY2 = dense_rank(desc(off_passing_downs_explosiveness_PY2)),
      Rank_Off_Rush_Play_PPA_PY2 = dense_rank(desc(off_rushing_plays_ppa_PY2)),
      Rank_Off_Rush_Play_Success_Rt_PY2 = dense_rank(desc(off_rushing_plays_success_rate_PY2)),
      Rank_Off_Rush_Play_Explosiveness_PY2 = dense_rank(desc(off_rushing_plays_explosiveness_PY2)),
      Rank_Off_Pass_Play_PPA_PY2 = dense_rank(desc(off_passing_plays_ppa_PY2)),
      Rank_Off_Pass_Play_Success_Rt_PY2 = dense_rank(desc(off_passing_plays_success_rate_PY2)),
      Rank_Off_Pass_Play_Explosiveness_PY2 = dense_rank(desc(off_passing_plays_explosiveness_PY2)),
      Rank_Def_PPA_PY2 = dense_rank(def_ppa_PY2),
      Rank_Def_Success_Rt_PY2 = dense_rank(def_success_rate_PY2),
      Rank_Def_Explosiveness_PY2 = dense_rank(def_explosiveness_PY2),
      Rank_Def_Pwr_Success_PY2 = dense_rank(def_power_success_PY2),
      Rank_Def_Stuff_Rt_PY2 = dense_rank(desc(def_stuff_rate_PY2)),
      Rank_Def_Line_Yds_PY2 = dense_rank(def_line_yds_PY2),
      Rank_Def_Second_Lvl_Yds_PY2 = dense_rank(def_second_lvl_yds_PY2),
      Rank_Def_Open_Field_Yds_PY2 = dense_rank(def_open_field_yds_PY2),
      Rank_Def_Pts_Per_Opp_PY2 = dense_rank(def_pts_per_opp_PY2),
      Rank_Def_Field_Pos_Avg_Predicted_Pts_PY2 = dense_rank(def_field_pos_avg_predicted_points_PY2),
      Rank_Def_Havoc_Total_PY2 = dense_rank(desc(def_havoc_total_PY2)),
      Rank_Def_Havoc_Front_Seven_PY2 = dense_rank(desc(def_havoc_front_seven_PY2)),
      Rank_Def_Havoc_DB_PY2 = dense_rank(desc(def_havoc_db_PY2)),
      Rank_Def_Standard_Down_PPA_PY2 = dense_rank(def_standard_downs_ppa_PY2),
      Rank_Def_Standard_Down_Success_Rt_PY2 = dense_rank(def_standard_downs_success_rate_PY2),
      Rank_Def_Standard_Down_Explosiveness_PY2 = dense_rank(def_standard_downs_explosiveness_PY2),
      Rank_Def_Pass_Down_PPA_PY2 = dense_rank(def_passing_downs_ppa_PY2),
      Rank_Def_Pass_Down_Success_Rt_PY2 = dense_rank(def_passing_downs_success_rate_PY2),
      Rank_Def_Pass_Down_Explosiveness_PY2 = dense_rank(def_passing_downs_explosiveness_PY2),
      Rank_Def_Rush_Play_PPA_PY2 = dense_rank(def_rushing_plays_ppa_PY2),
      Rank_Def_Rush_Play_Success_Rt_PY2 = dense_rank(def_rushing_plays_success_rate_PY2),
      Rank_Def_Rush_Play_Explosiveness_PY2 = dense_rank(def_rushing_plays_explosiveness_PY2),
      Rank_Def_Pass_Play_PPA_PY2 = dense_rank(def_passing_plays_ppa_PY2),
      Rank_Def_Pass_Play_Success_Rt_PY2 = dense_rank(def_passing_plays_success_rate_PY2),
      Rank_Def_Pass_Play_Explosiveness_PY2 = dense_rank(def_passing_plays_explosiveness_PY2),
      Rank_Recruit_Pts_PY2 = dense_rank(desc(recruit_pts_PY2)),
      Rank_Talent_PY2 = dense_rank(desc(talent_PY2)),
      Rank_SP_Rating_PY2 = dense_rank(desc(sp_rating_PY2)),
      Rank_SP_Off_Rating_PY2 = dense_rank(desc(sp_offense_rating_PY2)),
      Rank_SP_Def_Rating_PY2 = dense_rank(sp_defense_rating_PY2),
      Rank_SP_SpecialTeams_Rating_PY2 = dense_rank(desc(sp_special_teams_rating_PY2)),
      Rank_FPI_PY2 = dense_rank(desc(FPI_PY2)),
      Rank_SRS_rating_PY2 = dense_rank(desc(SRS_rating_PY2)),
      ## PY2 weighted twice
      Rank_Wins_PY2_col2 = dense_rank(desc(Wins_PY2)),
      Rank_Losses_PY2_col2 = dense_rank(Losses_PY2),
      Rank_Comp_Pct_PY2_col2 = dense_rank(desc(completion_pct_PY2)),
      Rank_Pass_YPA_PY2_col2 = dense_rank(desc(pass_ypa_PY2)),
      Rank_Pass_YPR_PY2_col2 = dense_rank(desc(pass_ypr_PY2)),
      Rank_Int_Pct_PY2_col2 = dense_rank(int_pct_PY2),
      Rank_Rush_YPC_PY2_col2 = dense_rank(desc(rush_ypc_PY2)),
      Rank_Turnovers_pg_PY2_col2 = dense_rank(turnovers_pg_PY2),
      Rank_Third_Conv_Rate_PY2_col2 = dense_rank(desc(third_conv_rate_PY2)),
      Rank_Fourth_Conv_Rate_PY2_col2 = dense_rank(desc(fourth_conv_rate_PY2)),
      Rank_Penalty_Yds_pg_PY2_col2 = dense_rank(penalty_yds_pg_PY2),
      Rank_Yds_Per_Penalty_PY2_col2 = dense_rank(yards_per_penalty_PY2),
      Rank_Kick_Return_Avg_PY2_col2 = dense_rank(desc(kick_return_avg_PY2)),
      Rank_Punt_Return_Avg_PY2_col2 = dense_rank(desc(punt_return_avg_PY2)),
      Rank_Total_Yds_pg_PY2_col2 = dense_rank(desc(total_yds_pg_PY2)),
      Rank_Pass_Yds_pg_PY2_col2 = dense_rank(desc(pass_yds_pg_PY2)),
      Rank_Rush_Yds_pg_PY2_col2 = dense_rank(desc(rush_yds_pg_PY2)),
      Rank_First_Downs_pg_PY2_col2 = dense_rank(desc(first_downs_pg_PY2)),
      Rank_Off_YPP_PY2_col2 = dense_rank(desc(off_ypp_PY2)),
      Rank_Def_Ints_pg_PY2_col2 = dense_rank(desc(def_interceptions_pg_PY2)),
      Rank_Off_PPA_PY2_col2 = dense_rank(desc(off_ppa_PY2)),
      Rank_Off_Success_Rt_PY2_col2 = dense_rank(desc(off_success_rate_PY2)),
      Rank_Off_Explosiveness_PY2_col2 = dense_rank(desc(off_explosiveness_PY2)),
      Rank_Off_Pwr_Success_PY2_col2 = dense_rank(desc(off_power_success_PY2)),
      Rank_Off_Stuff_Rt_PY2_col2 = dense_rank(off_stuff_rate_PY2),
      Rank_Off_Line_Yds_PY2_col2 = dense_rank(desc(off_line_yds_PY2)),
      Rank_Off_Second_Lvl_Yds_PY2_col2 = dense_rank(desc(off_second_lvl_yds_PY2)),
      Rank_Off_Open_Field_Yds_PY2_col2 = dense_rank(desc(off_open_field_yds_PY2)),
      Rank_Off_Pts_Per_Opp_PY2_col2 = dense_rank(desc(off_pts_per_opp_PY2)),
      Rank_Off_Field_Pos_Avg_Predicted_Pts_PY2_col2 = dense_rank(desc(off_field_pos_avg_predicted_points_PY2)),
      Rank_Off_Havoc_Total_PY2_col2 = dense_rank(off_havoc_total_PY2),
      Rank_Off_Havoc_Front_PY2_col2 = dense_rank(off_havoc_front_seven_PY2),
      Rank_Off_Havoc_DB_PY2_col2 = dense_rank(off_havoc_db_PY2),
      Rank_Off_Standard_Down_PPA_PY2_col2 = dense_rank(desc(off_standard_downs_ppa_PY2)),
      Rank_Off_Standard_Down_Success_Rt_PY2_col2 = dense_rank(desc(off_standard_downs_success_rate_PY2)),
      Rank_Off_Standard_Down_Explosiveness_PY2_col2 = dense_rank(desc(off_standard_downs_explosiveness_PY2)),
      Rank_Off_Pass_Down_PPA_PY2_col2 = dense_rank(desc(off_passing_downs_ppa_PY2)),
      Rank_Off_Pass_Down_Success_Rt_PY2_col2 = dense_rank(desc(off_passing_downs_success_rate_PY2)),
      Rank_Off_Pass_Down_Explosiveness_PY2_col2 = dense_rank(desc(off_passing_downs_explosiveness_PY2)),
      Rank_Off_Rush_Play_PPA_PY2_col2 = dense_rank(desc(off_rushing_plays_ppa_PY2)),
      Rank_Off_Rush_Play_Success_Rt_PY2_col2 = dense_rank(desc(off_rushing_plays_success_rate_PY2)),
      Rank_Off_Rush_Play_Explosiveness_PY2_col2 = dense_rank(desc(off_rushing_plays_explosiveness_PY2)),
      Rank_Off_Pass_Play_PPA_PY2_col2 = dense_rank(desc(off_passing_plays_ppa_PY2)),
      Rank_Off_Pass_Play_Success_Rt_PY2_col2 = dense_rank(desc(off_passing_plays_success_rate_PY2)),
      Rank_Off_Pass_Play_Explosiveness_PY2_col2 = dense_rank(desc(off_passing_plays_explosiveness_PY2)),
      Rank_Def_PPA_PY2_col2 = dense_rank(def_ppa_PY2),
      Rank_Def_Success_Rt_PY2_col2 = dense_rank(def_success_rate_PY2),
      Rank_Def_Explosiveness_PY2_col2 = dense_rank(def_explosiveness_PY2),
      Rank_Def_Pwr_Success_PY2_col2 = dense_rank(def_power_success_PY2),
      Rank_Def_Stuff_Rt_PY2_col2 = dense_rank(desc(def_stuff_rate_PY2)),
      Rank_Def_Line_Yds_PY2_col2 = dense_rank(def_line_yds_PY2),
      Rank_Def_Second_Lvl_Yds_PY2_col2 = dense_rank(def_second_lvl_yds_PY2),
      Rank_Def_Open_Field_Yds_PY2_col2 = dense_rank(def_open_field_yds_PY2),
      Rank_Def_Pts_Per_Opp_PY2_col2 = dense_rank(def_pts_per_opp_PY2),
      Rank_Def_Field_Pos_Avg_Predicted_Pts_PY2_col2 = dense_rank(def_field_pos_avg_predicted_points_PY2),
      Rank_Def_Havoc_Total_PY2_col2 = dense_rank(desc(def_havoc_total_PY2)),
      Rank_Def_Havoc_Front_Seven_PY2_col2 = dense_rank(desc(def_havoc_front_seven_PY2)),
      Rank_Def_Havoc_DB_PY2_col2 = dense_rank(desc(def_havoc_db_PY2)),
      Rank_Def_Standard_Down_PPA_PY2_col2 = dense_rank(def_standard_downs_ppa_PY2),
      Rank_Def_Standard_Down_Success_Rt_PY2_col2 = dense_rank(def_standard_downs_success_rate_PY2),
      Rank_Def_Standard_Down_Explosiveness_PY2_col2 = dense_rank(def_standard_downs_explosiveness_PY2),
      Rank_Def_Pass_Down_PPA_PY2_col2 = dense_rank(def_passing_downs_ppa_PY2),
      Rank_Def_Pass_Down_Success_Rt_PY2_col2 = dense_rank(def_passing_downs_success_rate_PY2),
      Rank_Def_Pass_Down_Explosiveness_PY2_col2 = dense_rank(def_passing_downs_explosiveness_PY2),
      Rank_Def_Rush_Play_PPA_PY2_col2 = dense_rank(def_rushing_plays_ppa_PY2),
      Rank_Def_Rush_Play_Success_Rt_PY2_col2 = dense_rank(def_rushing_plays_success_rate_PY2),
      Rank_Def_Rush_Play_Explosiveness_PY2_col2 = dense_rank(def_rushing_plays_explosiveness_PY2),
      Rank_Def_Pass_Play_PPA_PY2_col2 = dense_rank(def_passing_plays_ppa_PY2),
      Rank_Def_Pass_Play_Success_Rt_PY2_col2 = dense_rank(def_passing_plays_success_rate_PY2),
      Rank_Def_Pass_Play_Explosiveness_PY2_col2 = dense_rank(def_passing_plays_explosiveness_PY2),
      Rank_Recruit_Pts_PY2_col2 = dense_rank(desc(recruit_pts_PY2)),
      ## PY2 extra weighted variables
      Rank_Off_YPP_PY2_col3 = dense_rank(desc(off_ypp_PY2)),
      Rank_Off_PPA_PY2_col3 = dense_rank(desc(off_ppa_PY2)),
      Rank_Off_Success_Rt_PY2_col3 = dense_rank(desc(off_success_rate_PY2)),
      Rank_Off_Explosiveness_PY2_col3 = dense_rank(desc(off_explosiveness_PY2)),
      Rank_Off_Pwr_Success_PY2_col3 = dense_rank(desc(off_power_success_PY2)),
      Rank_Off_Stuff_Rt_PY2_col3 = dense_rank(off_stuff_rate_PY2),
      Rank_Off_Pts_Per_Opp_PY2_col3 = dense_rank(desc(off_pts_per_opp_PY2)),
      Rank_Off_Havoc_Total_PY2_col3 = dense_rank(off_havoc_total_PY2),
      Rank_Off_Havoc_Front_PY2_col3 = dense_rank(off_havoc_front_seven_PY2),
      Rank_Off_Havoc_DB_PY2_col3 = dense_rank(off_havoc_db_PY2),
      Rank_Off_Standard_Down_PPA_PY2_col3 = dense_rank(desc(off_standard_downs_ppa_PY2)),
      Rank_Off_Standard_Down_Success_Rt_PY2_col3 = dense_rank(desc(off_standard_downs_success_rate_PY2)),
      Rank_Off_Standard_Down_Explosiveness_PY2_col3 = dense_rank(desc(off_standard_downs_explosiveness_PY2)),
      Rank_Off_Pass_Down_PPA_PY2_col3 = dense_rank(desc(off_passing_downs_ppa_PY2)),
      Rank_Off_Pass_Down_Success_Rt_PY2_col3 = dense_rank(desc(off_passing_downs_success_rate_PY2)),
      Rank_Off_Pass_Down_Explosiveness_PY2_col3 = dense_rank(desc(off_passing_downs_explosiveness_PY2)),
      Rank_Off_Rush_Play_PPA_PY2_col3 = dense_rank(desc(off_rushing_plays_ppa_PY2)),
      Rank_Off_Rush_Play_Success_Rt_PY2_col3 = dense_rank(desc(off_rushing_plays_success_rate_PY2)),
      Rank_Off_Rush_Play_Explosiveness_PY2_col3 = dense_rank(desc(off_rushing_plays_explosiveness_PY2)),
      Rank_Off_Pass_Play_PPA_PY2_col3 = dense_rank(desc(off_passing_plays_ppa_PY2)),
      Rank_Off_Pass_Play_Success_Rt_PY2_col3 = dense_rank(desc(off_passing_plays_success_rate_PY2)),
      Rank_Off_Pass_Play_Explosiveness_PY2_col3 = dense_rank(desc(off_passing_plays_explosiveness_PY2)),
      Rank_Def_PPA_PY2_col3 = dense_rank(def_ppa_PY2),
      Rank_Def_Success_Rt_PY2_col3 = dense_rank(def_success_rate_PY2),
      Rank_Def_Explosiveness_PY2_col3 = dense_rank(def_explosiveness_PY2),
      Rank_Def_Pwr_Success_PY2_col3 = dense_rank(def_power_success_PY2),
      Rank_Def_Stuff_Rt_PY2_col3 = dense_rank(desc(def_stuff_rate_PY2)),
      Rank_Def_Pts_Per_Opp_PY2_col3 = dense_rank(def_pts_per_opp_PY2),
      Rank_Def_Havoc_Total_PY2_col3 = dense_rank(desc(def_havoc_total_PY2)),
      Rank_Def_Havoc_Front_Seven_PY2_col3 = dense_rank(desc(def_havoc_front_seven_PY2)),
      Rank_Def_Havoc_DB_PY2_col3 = dense_rank(desc(def_havoc_db_PY2)),
      Rank_Def_Standard_Down_PPA_PY2_col3 = dense_rank(def_standard_downs_ppa_PY2),
      Rank_Def_Standard_Down_Success_Rt_PY2_col3 = dense_rank(def_standard_downs_success_rate_PY2),
      Rank_Def_Standard_Down_Explosiveness_PY2_col3 = dense_rank(def_standard_downs_explosiveness_PY2),
      Rank_Def_Pass_Down_PPA_PY2_col3 = dense_rank(def_passing_downs_ppa_PY2),
      Rank_Def_Pass_Down_Success_Rt_PY2_col3 = dense_rank(def_passing_downs_success_rate_PY2),
      Rank_Def_Pass_Down_Explosiveness_PY2_col3 = dense_rank(def_passing_downs_explosiveness_PY2),
      Rank_Def_Rush_Play_PPA_PY2_col3 = dense_rank(def_rushing_plays_ppa_PY2),
      Rank_Def_Rush_Play_Success_Rt_PY2_col3 = dense_rank(def_rushing_plays_success_rate_PY2),
      Rank_Def_Rush_Play_Explosiveness_PY2_col3 = dense_rank(def_rushing_plays_explosiveness_PY2),
      Rank_Def_Pass_Play_PPA_PY2_col3 = dense_rank(def_passing_plays_ppa_PY2),
      Rank_Def_Pass_Play_Success_Rt_PY2_col3 = dense_rank(def_passing_plays_success_rate_PY2),
      Rank_Def_Pass_Play_Explosiveness_PY2_col3 = dense_rank(def_passing_plays_explosiveness_PY2),
      Rank_Recruit_Pts_PY2_col3 = dense_rank(desc(recruit_pts_PY2)),
      ## PY2 extra weighted variables (2x)
      Rank_Off_YPP_PY2_col4 = dense_rank(desc(off_ypp_PY2)),
      Rank_Off_PPA_PY2_col4 = dense_rank(desc(off_ppa_PY2)),
      Rank_Off_Success_Rt_PY2_col4 = dense_rank(desc(off_success_rate_PY2)),
      Rank_Off_Explosiveness_PY2_col4 = dense_rank(desc(off_explosiveness_PY2)),
      Rank_Off_Pwr_Success_PY2_col4 = dense_rank(desc(off_power_success_PY2)),
      Rank_Off_Stuff_Rt_PY2_col4 = dense_rank(off_stuff_rate_PY2),
      Rank_Off_Pts_Per_Opp_PY2_col4 = dense_rank(desc(off_pts_per_opp_PY2)),
      Rank_Off_Havoc_Total_PY2_col4 = dense_rank(off_havoc_total_PY2),
      Rank_Off_Havoc_Front_PY2_col4 = dense_rank(off_havoc_front_seven_PY2),
      Rank_Off_Havoc_DB_PY2_col4 = dense_rank(off_havoc_db_PY2),
      Rank_Off_Standard_Down_PPA_PY2_col4 = dense_rank(desc(off_standard_downs_ppa_PY2)),
      Rank_Off_Standard_Down_Success_Rt_PY2_col4 = dense_rank(desc(off_standard_downs_success_rate_PY2)),
      Rank_Off_Standard_Down_Explosiveness_PY2_col4 = dense_rank(desc(off_standard_downs_explosiveness_PY2)),
      Rank_Off_Pass_Down_PPA_PY2_col4 = dense_rank(desc(off_passing_downs_ppa_PY2)),
      Rank_Off_Pass_Down_Success_Rt_PY2_col4 = dense_rank(desc(off_passing_downs_success_rate_PY2)),
      Rank_Off_Pass_Down_Explosiveness_PY2_col4 = dense_rank(desc(off_passing_downs_explosiveness_PY2)),
      Rank_Off_Rush_Play_PPA_PY2_col4 = dense_rank(desc(off_rushing_plays_ppa_PY2)),
      Rank_Off_Rush_Play_Success_Rt_PY2_col4 = dense_rank(desc(off_rushing_plays_success_rate_PY2)),
      Rank_Off_Rush_Play_Explosiveness_PY2_col4 = dense_rank(desc(off_rushing_plays_explosiveness_PY2)),
      Rank_Off_Pass_Play_PPA_PY2_col4 = dense_rank(desc(off_passing_plays_ppa_PY2)),
      Rank_Off_Pass_Play_Success_Rt_PY2_col4 = dense_rank(desc(off_passing_plays_success_rate_PY2)),
      Rank_Off_Pass_Play_Explosiveness_PY2_col4 = dense_rank(desc(off_passing_plays_explosiveness_PY2)),
      Rank_Def_PPA_PY2_col4 = dense_rank(def_ppa_PY2),
      Rank_Def_Success_Rt_PY2_col4 = dense_rank(def_success_rate_PY2),
      Rank_Def_Explosiveness_PY2_col4 = dense_rank(def_explosiveness_PY2),
      Rank_Def_Pwr_Success_PY2_col4 = dense_rank(def_power_success_PY2),
      Rank_Def_Stuff_Rt_PY2_col4 = dense_rank(desc(def_stuff_rate_PY2)),
      Rank_Def_Pts_Per_Opp_PY2_col4 = dense_rank(def_pts_per_opp_PY2),
      Rank_Def_Havoc_Total_PY2_col4 = dense_rank(desc(def_havoc_total_PY2)),
      Rank_Def_Havoc_Front_Seven_PY2_col4 = dense_rank(desc(def_havoc_front_seven_PY2)),
      Rank_Def_Havoc_DB_PY2_col4 = dense_rank(desc(def_havoc_db_PY2)),
      Rank_Def_Standard_Down_PPA_PY2_col4 = dense_rank(def_standard_downs_ppa_PY2),
      Rank_Def_Standard_Down_Success_Rt_PY2_col4 = dense_rank(def_standard_downs_success_rate_PY2),
      Rank_Def_Standard_Down_Explosiveness_PY2_col4 = dense_rank(def_standard_downs_explosiveness_PY2),
      Rank_Def_Pass_Down_PPA_PY2_col4 = dense_rank(def_passing_downs_ppa_PY2),
      Rank_Def_Pass_Down_Success_Rt_PY2_col4 = dense_rank(def_passing_downs_success_rate_PY2),
      Rank_Def_Pass_Down_Explosiveness_PY2_col4 = dense_rank(def_passing_downs_explosiveness_PY2),
      Rank_Def_Rush_Play_PPA_PY2_col4 = dense_rank(def_rushing_plays_ppa_PY2),
      Rank_Def_Rush_Play_Success_Rt_PY2_col4 = dense_rank(def_rushing_plays_success_rate_PY2),
      Rank_Def_Rush_Play_Explosiveness_PY2_col4 = dense_rank(def_rushing_plays_explosiveness_PY2),
      Rank_Def_Pass_Play_PPA_PY2_col4 = dense_rank(def_passing_plays_ppa_PY2),
      Rank_Def_Pass_Play_Success_Rt_PY2_col4 = dense_rank(def_passing_plays_success_rate_PY2),
      Rank_Def_Pass_Play_Explosiveness_PY2_col4 = dense_rank(def_passing_plays_explosiveness_PY2),
      ## PY1 ranks
      Rank_Wins_PY1 = dense_rank(desc(Wins_PY1)),
      Rank_Losses_PY1 = dense_rank(Losses_PY1),
      Rank_Comp_Pct_PY1 = dense_rank(desc(completion_pct_PY1)),
      Rank_Pass_YPA_PY1 = dense_rank(desc(pass_ypa_PY1)),
      Rank_Pass_YPR_PY1 = dense_rank(desc(pass_ypr_PY1)),
      Rank_Int_Pct_PY1 = dense_rank(int_pct_PY1),
      Rank_Rush_YPC_PY1 = dense_rank(desc(rush_ypc_PY1)),
      Rank_Turnovers_pg_PY1 = dense_rank(turnovers_pg_PY1),
      Rank_Third_Conv_Rate_PY1 = dense_rank(desc(third_conv_rate_PY1)),
      Rank_Fourth_Conv_Rate_PY1 = dense_rank(desc(fourth_conv_rate_PY1)),
      Rank_Penalty_Yds_pg_PY1 = dense_rank(penalty_yds_pg_PY1),
      Rank_Yds_Per_Penalty_PY1 = dense_rank(yards_per_penalty_PY1),
      Rank_Kick_Return_Avg_PY1 = dense_rank(desc(kick_return_avg_PY1)),
      Rank_Punt_Return_Avg_PY1 = dense_rank(desc(punt_return_avg_PY1)),
      Rank_Total_Yds_pg_PY1 = dense_rank(desc(total_yds_pg_PY1)),
      Rank_Pass_Yds_pg_PY1 = dense_rank(desc(pass_yds_pg_PY1)),
      Rank_Rush_Yds_pg_PY1 = dense_rank(desc(rush_yds_pg_PY1)),
      Rank_First_Downs_pg_PY1 = dense_rank(desc(first_downs_pg_PY1)),
      Rank_Off_YPP_PY1 = dense_rank(desc(off_ypp_PY1)),
      Rank_Def_Ints_pg_PY1 = dense_rank(desc(def_interceptions_pg_PY1)),
      Rank_Off_PPA_PY1 = dense_rank(desc(off_ppa_PY1)),
      Rank_Off_Success_Rt_PY1 = dense_rank(desc(off_success_rate_PY1)),
      Rank_Off_Explosiveness_PY1 = dense_rank(desc(off_explosiveness_PY1)),
      Rank_Off_Pwr_Success_PY1 = dense_rank(desc(off_power_success_PY1)),
      Rank_Off_Stuff_Rt_PY1 = dense_rank(off_stuff_rate_PY1),
      Rank_Off_Line_Yds_PY1 = dense_rank(desc(off_line_yds_PY1)),
      Rank_Off_Second_Lvl_Yds_PY1 = dense_rank(desc(off_second_lvl_yds_PY1)),
      Rank_Off_Open_Field_Yds_PY1 = dense_rank(desc(off_open_field_yds_PY1)),
      Rank_Off_Pts_Per_Opp_PY1 = dense_rank(desc(off_pts_per_opp_PY1)),
      Rank_Off_Field_Pos_Avg_Predicted_Pts_PY1 = dense_rank(desc(off_field_pos_avg_predicted_points_PY1)),
      Rank_Off_Havoc_Total_PY1 = dense_rank(off_havoc_total_PY1),
      Rank_Off_Havoc_Front_PY1 = dense_rank(off_havoc_front_seven_PY1),
      Rank_Off_Havoc_DB_PY1 = dense_rank(off_havoc_db_PY1),
      Rank_Off_Standard_Down_PPA_PY1 = dense_rank(desc(off_standard_downs_ppa_PY1)),
      Rank_Off_Standard_Down_Success_Rt_PY1 = dense_rank(desc(off_standard_downs_success_rate_PY1)),
      Rank_Off_Standard_Down_Explosiveness_PY1 = dense_rank(desc(off_standard_downs_explosiveness_PY1)),
      Rank_Off_Pass_Down_PPA_PY1 = dense_rank(desc(off_passing_downs_ppa_PY1)),
      Rank_Off_Pass_Down_Success_Rt_PY1 = dense_rank(desc(off_passing_downs_success_rate_PY1)),
      Rank_Off_Pass_Down_Explosiveness_PY1 = dense_rank(desc(off_passing_downs_explosiveness_PY1)),
      Rank_Off_Rush_Play_PPA_PY1 = dense_rank(desc(off_rushing_plays_ppa_PY1)),
      Rank_Off_Rush_Play_Success_Rt_PY1 = dense_rank(desc(off_rushing_plays_success_rate_PY1)),
      Rank_Off_Rush_Play_Explosiveness_PY1 = dense_rank(desc(off_rushing_plays_explosiveness_PY1)),
      Rank_Off_Pass_Play_PPA_PY1 = dense_rank(desc(off_passing_plays_ppa_PY1)),
      Rank_Off_Pass_Play_Success_Rt_PY1 = dense_rank(desc(off_passing_plays_success_rate_PY1)),
      Rank_Off_Pass_Play_Explosiveness_PY1 = dense_rank(desc(off_passing_plays_explosiveness_PY1)),
      Rank_Def_PPA_PY1 = dense_rank(def_ppa_PY1),
      Rank_Def_Success_Rt_PY1 = dense_rank(def_success_rate_PY1),
      Rank_Def_Explosiveness_PY1 = dense_rank(def_explosiveness_PY1),
      Rank_Def_Pwr_Success_PY1 = dense_rank(def_power_success_PY1),
      Rank_Def_Stuff_Rt_PY1 = dense_rank(desc(def_stuff_rate_PY1)),
      Rank_Def_Line_Yds_PY1 = dense_rank(def_line_yds_PY1),
      Rank_Def_Second_Lvl_Yds_PY1 = dense_rank(def_second_lvl_yds_PY1),
      Rank_Def_Open_Field_Yds_PY1 = dense_rank(def_open_field_yds_PY1),
      Rank_Def_Pts_Per_Opp_PY1 = dense_rank(def_pts_per_opp_PY1),
      Rank_Def_Field_Pos_Avg_Predicted_Pts_PY1 = dense_rank(def_field_pos_avg_predicted_points_PY1),
      Rank_Def_Havoc_Total_PY1 = dense_rank(desc(def_havoc_total_PY1)),
      Rank_Def_Havoc_Front_Seven_PY1 = dense_rank(desc(def_havoc_front_seven_PY1)),
      Rank_Def_Havoc_DB_PY1 = dense_rank(desc(def_havoc_db_PY1)),
      Rank_Def_Standard_Down_PPA_PY1 = dense_rank(def_standard_downs_ppa_PY1),
      Rank_Def_Standard_Down_Success_Rt_PY1 = dense_rank(def_standard_downs_success_rate_PY1),
      Rank_Def_Standard_Down_Explosiveness_PY1 = dense_rank(def_standard_downs_explosiveness_PY1),
      Rank_Def_Pass_Down_PPA_PY1 = dense_rank(def_passing_downs_ppa_PY1),
      Rank_Def_Pass_Down_Success_Rt_PY1 = dense_rank(def_passing_downs_success_rate_PY1),
      Rank_Def_Pass_Down_Explosiveness_PY1 = dense_rank(def_passing_downs_explosiveness_PY1),
      Rank_Def_Rush_Play_PPA_PY1 = dense_rank(def_rushing_plays_ppa_PY1),
      Rank_Def_Rush_Play_Success_Rt_PY1 = dense_rank(def_rushing_plays_success_rate_PY1),
      Rank_Def_Rush_Play_Explosiveness_PY1 = dense_rank(def_rushing_plays_explosiveness_PY1),
      Rank_Def_Pass_Play_PPA_PY1 = dense_rank(def_passing_plays_ppa_PY1),
      Rank_Def_Pass_Play_Success_Rt_PY1 = dense_rank(def_passing_plays_success_rate_PY1),
      Rank_Def_Pass_Play_Explosiveness_PY1 = dense_rank(def_passing_plays_explosiveness_PY1),
      Rank_Recruit_Pts_PY1 = dense_rank(desc(recruit_pts_PY1)),
      Rank_Talent_PY1 = dense_rank(desc(talent_PY1)),
      Rank_SP_Rating_PY1 = dense_rank(desc(sp_rating_PY1)),
      Rank_SP_Off_Rating_PY1 = dense_rank(desc(sp_offense_rating_PY1)),
      Rank_SP_Def_Rating_PY1 = dense_rank(sp_defense_rating_PY1),
      Rank_SP_SpecialTeams_Rating_PY1 = dense_rank(desc(sp_special_teams_rating_PY1)),
      Rank_FPI_PY1 = dense_rank(desc(FPI_PY1)),
      Rank_SRS_rating_PY1 = dense_rank(desc(SRS_rating_PY1)),
      ## PY1 weighted 2 times
      Rank_Wins_PY1_col2 = dense_rank(desc(Wins_PY1)),
      Rank_Losses_PY1_col2 = dense_rank(Losses_PY1),
      Rank_Comp_Pct_PY1_col2 = dense_rank(desc(completion_pct_PY1)),
      Rank_Pass_YPA_PY1_col2 = dense_rank(desc(pass_ypa_PY1)),
      Rank_Pass_YPR_PY1_col2 = dense_rank(desc(pass_ypr_PY1)),
      Rank_Int_Pct_PY1_col2 = dense_rank(int_pct_PY1),
      Rank_Rush_YPC_PY1_col2 = dense_rank(desc(rush_ypc_PY1)),
      Rank_Turnovers_pg_PY1_col2 = dense_rank(turnovers_pg_PY1),
      Rank_Third_Conv_Rate_PY1_col2 = dense_rank(desc(third_conv_rate_PY1)),
      Rank_Fourth_Conv_Rate_PY1_col2 = dense_rank(desc(fourth_conv_rate_PY1)),
      Rank_Penalty_Yds_pg_PY1_col2 = dense_rank(penalty_yds_pg_PY1),
      Rank_Yds_Per_Penalty_PY1_col2 = dense_rank(yards_per_penalty_PY1),
      Rank_Kick_Return_Avg_PY1_col2 = dense_rank(desc(kick_return_avg_PY1)),
      Rank_Punt_Return_Avg_PY1_col2 = dense_rank(desc(punt_return_avg_PY1)),
      Rank_Total_Yds_pg_PY1_col2 = dense_rank(desc(total_yds_pg_PY1)),
      Rank_Pass_Yds_pg_PY1_col2 = dense_rank(desc(pass_yds_pg_PY1)),
      Rank_Rush_Yds_pg_PY1_col2 = dense_rank(desc(rush_yds_pg_PY1)),
      Rank_First_Downs_pg_PY1_col2 = dense_rank(desc(first_downs_pg_PY1)),
      Rank_Off_YPP_PY1_col2 = dense_rank(desc(off_ypp_PY1)),
      Rank_Def_Ints_pg_PY1_col2 = dense_rank(desc(def_interceptions_pg_PY1)),
      Rank_Off_PPA_PY1_col2 = dense_rank(desc(off_ppa_PY1)),
      Rank_Off_Success_Rt_PY1_col2 = dense_rank(desc(off_success_rate_PY1)),
      Rank_Off_Explosiveness_PY1_col2 = dense_rank(desc(off_explosiveness_PY1)),
      Rank_Off_Pwr_Success_PY1_col2 = dense_rank(desc(off_power_success_PY1)),
      Rank_Off_Stuff_Rt_PY1_col2 = dense_rank(off_stuff_rate_PY1),
      Rank_Off_Line_Yds_PY1_col2 = dense_rank(desc(off_line_yds_PY1)),
      Rank_Off_Second_Lvl_Yds_PY1_col2 = dense_rank(desc(off_second_lvl_yds_PY1)),
      Rank_Off_Open_Field_Yds_PY1_col2 = dense_rank(desc(off_open_field_yds_PY1)),
      Rank_Off_Pts_Per_Opp_PY1_col2 = dense_rank(desc(off_pts_per_opp_PY1)),
      Rank_Off_Field_Pos_Avg_Predicted_Pts_PY1_col2 = dense_rank(desc(off_field_pos_avg_predicted_points_PY1)),
      Rank_Off_Havoc_Total_PY1_col2 = dense_rank(off_havoc_total_PY1),
      Rank_Off_Havoc_Front_PY1_col2 = dense_rank(off_havoc_front_seven_PY1),
      Rank_Off_Havoc_DB_PY1_col2 = dense_rank(off_havoc_db_PY1),
      Rank_Off_Standard_Down_PPA_PY1_col2 = dense_rank(desc(off_standard_downs_ppa_PY1)),
      Rank_Off_Standard_Down_Success_Rt_PY1_col2 = dense_rank(desc(off_standard_downs_success_rate_PY1)),
      Rank_Off_Standard_Down_Explosiveness_PY1_col2 = dense_rank(desc(off_standard_downs_explosiveness_PY1)),
      Rank_Off_Pass_Down_PPA_PY1_col2 = dense_rank(desc(off_passing_downs_ppa_PY1)),
      Rank_Off_Pass_Down_Success_Rt_PY1_col2 = dense_rank(desc(off_passing_downs_success_rate_PY1)),
      Rank_Off_Pass_Down_Explosiveness_PY1_col2 = dense_rank(desc(off_passing_downs_explosiveness_PY1)),
      Rank_Off_Rush_Play_PPA_PY1_col2 = dense_rank(desc(off_rushing_plays_ppa_PY1)),
      Rank_Off_Rush_Play_Success_Rt_PY1_col2 = dense_rank(desc(off_rushing_plays_success_rate_PY1)),
      Rank_Off_Rush_Play_Explosiveness_PY1_col2 = dense_rank(desc(off_rushing_plays_explosiveness_PY1)),
      Rank_Off_Pass_Play_PPA_PY1_col2 = dense_rank(desc(off_passing_plays_ppa_PY1)),
      Rank_Off_Pass_Play_Success_Rt_PY1_col2 = dense_rank(desc(off_passing_plays_success_rate_PY1)),
      Rank_Off_Pass_Play_Explosiveness_PY1_col2 = dense_rank(desc(off_passing_plays_explosiveness_PY1)),
      Rank_Def_PPA_PY1_col2 = dense_rank(def_ppa_PY1),
      Rank_Def_Success_Rt_PY1_col2 = dense_rank(def_success_rate_PY1),
      Rank_Def_Explosiveness_PY1_col2 = dense_rank(def_explosiveness_PY1),
      Rank_Def_Pwr_Success_PY1_col2 = dense_rank(def_power_success_PY1),
      Rank_Def_Stuff_Rt_PY1_col2 = dense_rank(desc(def_stuff_rate_PY1)),
      Rank_Def_Line_Yds_PY1_col2 = dense_rank(def_line_yds_PY1),
      Rank_Def_Second_Lvl_Yds_PY1_col2 = dense_rank(def_second_lvl_yds_PY1),
      Rank_Def_Open_Field_Yds_PY1_col2 = dense_rank(def_open_field_yds_PY1),
      Rank_Def_Pts_Per_Opp_PY1_col2 = dense_rank(def_pts_per_opp_PY1),
      Rank_Def_Field_Pos_Avg_Predicted_Pts_PY1_col2 = dense_rank(def_field_pos_avg_predicted_points_PY1),
      Rank_Def_Havoc_Total_PY1_col2 = dense_rank(desc(def_havoc_total_PY1)),
      Rank_Def_Havoc_Front_Seven_PY1_col2 = dense_rank(desc(def_havoc_front_seven_PY1)),
      Rank_Def_Havoc_DB_PY1_col2 = dense_rank(desc(def_havoc_db_PY1)),
      Rank_Def_Standard_Down_PPA_PY1_col2 = dense_rank(def_standard_downs_ppa_PY1),
      Rank_Def_Standard_Down_Success_Rt_PY1_col2 = dense_rank(def_standard_downs_success_rate_PY1),
      Rank_Def_Standard_Down_Explosiveness_PY1_col2 = dense_rank(def_standard_downs_explosiveness_PY1),
      Rank_Def_Pass_Down_PPA_PY1_col2 = dense_rank(def_passing_downs_ppa_PY1),
      Rank_Def_Pass_Down_Success_Rt_PY1_col2 = dense_rank(def_passing_downs_success_rate_PY1),
      Rank_Def_Pass_Down_Explosiveness_PY1_col2 = dense_rank(def_passing_downs_explosiveness_PY1),
      Rank_Def_Rush_Play_PPA_PY1_col2 = dense_rank(def_rushing_plays_ppa_PY1),
      Rank_Def_Rush_Play_Success_Rt_PY1_col2 = dense_rank(def_rushing_plays_success_rate_PY1),
      Rank_Def_Rush_Play_Explosiveness_PY1_col2 = dense_rank(def_rushing_plays_explosiveness_PY1),
      Rank_Def_Pass_Play_PPA_PY1_col2 = dense_rank(def_passing_plays_ppa_PY1),
      Rank_Def_Pass_Play_Success_Rt_PY1_col2 = dense_rank(def_passing_plays_success_rate_PY1),
      Rank_Def_Pass_Play_Explosiveness_PY1_col2 = dense_rank(def_passing_plays_explosiveness_PY1),
      Rank_Recruit_Pts_PY1_col2 = dense_rank(desc(recruit_pts_PY1)),
      Rank_Talent_PY1_col2 = dense_rank(desc(talent_PY1)),
      ## incoming recruiting class, weighted once
      ## also adding in PY3 recruiting ranks
      # no other PY3 variables still included
      Rank_Recruit_Pts = dense_rank(desc(recruit_pts)),
      Rank_Recruit_Pts_PY3 = dense_rank(desc(recruit_pts_PY3)),
      Rank_Recruit_Pts_PY3_col2 = Rank_Recruit_Pts,
      Rank_Recruit_Pts_PY3_col3 = Rank_Recruit_Pts,
      ## Extra weighted variables, weighted 2x (2 more times)
      Rank_Off_YPP_PY1_col3 = dense_rank(desc(off_ypp_PY1)),
      Rank_Off_PPA_PY1_col3 = dense_rank(desc(off_ppa_PY1)),
      Rank_Off_Success_Rt_PY1_col3 = dense_rank(desc(off_success_rate_PY1)),
      Rank_Off_Explosiveness_PY1_col3 = dense_rank(desc(off_explosiveness_PY1)),
      Rank_Off_Pwr_Success_PY1_col3 = dense_rank(desc(off_power_success_PY1)),
      Rank_Off_Stuff_Rt_PY1_col3 = dense_rank(off_stuff_rate_PY1),
      Rank_Off_Pts_Per_Opp_PY1_col3 = dense_rank(desc(off_pts_per_opp_PY1)),
      Rank_Off_Havoc_Total_PY1_col3 = dense_rank(off_havoc_total_PY1),
      Rank_Off_Havoc_Front_PY1_col3 = dense_rank(off_havoc_front_seven_PY1),
      Rank_Off_Havoc_DB_PY1_col3 = dense_rank(off_havoc_db_PY1),
      Rank_Off_Standard_Down_PPA_PY1_col3 = dense_rank(desc(off_standard_downs_ppa_PY1)),
      Rank_Off_Standard_Down_Success_Rt_PY1_col3 = dense_rank(desc(off_standard_downs_success_rate_PY1)),
      Rank_Off_Standard_Down_Explosiveness_PY1_col3 = dense_rank(desc(off_standard_downs_explosiveness_PY1)),
      Rank_Off_Pass_Down_PPA_PY1_col3 = dense_rank(desc(off_passing_downs_ppa_PY1)),
      Rank_Off_Pass_Down_Success_Rt_PY1_col3 = dense_rank(desc(off_passing_downs_success_rate_PY1)),
      Rank_Off_Pass_Down_Explosiveness_PY1_col3 = dense_rank(desc(off_passing_downs_explosiveness_PY1)),
      Rank_Off_Rush_Play_PPA_PY1_col3 = dense_rank(desc(off_rushing_plays_ppa_PY1)),
      Rank_Off_Rush_Play_Success_Rt_PY1_col3 = dense_rank(desc(off_rushing_plays_success_rate_PY1)),
      Rank_Off_Rush_Play_Explosiveness_PY1_col3 = dense_rank(desc(off_rushing_plays_explosiveness_PY1)),
      Rank_Off_Pass_Play_PPA_PY1_col3 = dense_rank(desc(off_passing_plays_ppa_PY1)),
      Rank_Off_Pass_Play_Success_Rt_PY1_col3 = dense_rank(desc(off_passing_plays_success_rate_PY1)),
      Rank_Off_Pass_Play_Explosiveness_PY1_col3 = dense_rank(desc(off_passing_plays_explosiveness_PY1)),
      Rank_Def_PPA_PY1_col3 = dense_rank(def_ppa_PY1),
      Rank_Def_Success_Rt_PY1_col3 = dense_rank(def_success_rate_PY1),
      Rank_Def_Explosiveness_PY1_col3 = dense_rank(def_explosiveness_PY1),
      Rank_Def_Pwr_Success_PY1_col3 = dense_rank(def_power_success_PY1),
      Rank_Def_Stuff_Rt_PY1_col3 = dense_rank(desc(def_stuff_rate_PY1)),
      Rank_Def_Pts_Per_Opp_PY1_col3 = dense_rank(def_pts_per_opp_PY1),
      Rank_Def_Havoc_Total_PY1_col3 = dense_rank(desc(def_havoc_total_PY1)),
      Rank_Def_Havoc_Front_Seven_PY1_col3 = dense_rank(desc(def_havoc_front_seven_PY1)),
      Rank_Def_Havoc_DB_PY1_col3 = dense_rank(desc(def_havoc_db_PY1)),
      Rank_Def_Standard_Down_PPA_PY1_col3 = dense_rank(def_standard_downs_ppa_PY1),
      Rank_Def_Standard_Down_Success_Rt_PY1_col3 = dense_rank(def_standard_downs_success_rate_PY1),
      Rank_Def_Standard_Down_Explosiveness_PY1_col3 = dense_rank(def_standard_downs_explosiveness_PY1),
      Rank_Def_Pass_Down_PPA_PY1_col3 = dense_rank(def_passing_downs_ppa_PY1),
      Rank_Def_Pass_Down_Success_Rt_PY1_col3 = dense_rank(def_passing_downs_success_rate_PY1),
      Rank_Def_Pass_Down_Explosiveness_PY1_col3 = dense_rank(def_passing_downs_explosiveness_PY1),
      Rank_Def_Rush_Play_PPA_PY1_col3 = dense_rank(def_rushing_plays_ppa_PY1),
      Rank_Def_Rush_Play_Success_Rt_PY1_col3 = dense_rank(def_rushing_plays_success_rate_PY1),
      Rank_Def_Rush_Play_Explosiveness_PY1_col3 = dense_rank(def_rushing_plays_explosiveness_PY1),
      Rank_Def_Pass_Play_PPA_PY1_col3 = dense_rank(def_passing_plays_ppa_PY1),
      Rank_Def_Pass_Play_Success_Rt_PY1_col3 = dense_rank(def_passing_plays_success_rate_PY1),
      Rank_Def_Pass_Play_Explosiveness_PY1_col3 = dense_rank(def_passing_plays_explosiveness_PY1),
      ## Extra weighted variables, weighted 2x (1 more time)
      Rank_Off_YPP_PY1_col4 = dense_rank(desc(off_ypp_PY1)),
      Rank_Off_PPA_PY1_col4 = dense_rank(desc(off_ppa_PY1)),
      Rank_Off_Success_Rt_PY1_col4 = dense_rank(desc(off_success_rate_PY1)),
      Rank_Off_Explosiveness_PY1_col4 = dense_rank(desc(off_explosiveness_PY1)),
      Rank_Off_Pwr_Success_PY1_col4 = dense_rank(desc(off_power_success_PY1)),
      Rank_Off_Stuff_Rt_PY1_col4 = dense_rank(off_stuff_rate_PY1),
      Rank_Off_Pts_Per_Opp_PY1_col4 = dense_rank(desc(off_pts_per_opp_PY1)),
      Rank_Off_Havoc_Total_PY1_col4 = dense_rank(off_havoc_total_PY1),
      Rank_Off_Havoc_Front_PY1_col4 = dense_rank(off_havoc_front_seven_PY1),
      Rank_Off_Havoc_DB_PY1_col4 = dense_rank(off_havoc_db_PY1),
      Rank_Off_Standard_Down_PPA_PY1_col4 = dense_rank(desc(off_standard_downs_ppa_PY1)),
      Rank_Off_Standard_Down_Success_Rt_PY1_col4 = dense_rank(desc(off_standard_downs_success_rate_PY1)),
      Rank_Off_Standard_Down_Explosiveness_PY1_col4 = dense_rank(desc(off_standard_downs_explosiveness_PY1)),
      Rank_Off_Pass_Down_PPA_PY1_col4 = dense_rank(desc(off_passing_downs_ppa_PY1)),
      Rank_Off_Pass_Down_Success_Rt_PY1_col4 = dense_rank(desc(off_passing_downs_success_rate_PY1)),
      Rank_Off_Pass_Down_Explosiveness_PY1_col4 = dense_rank(desc(off_passing_downs_explosiveness_PY1)),
      Rank_Off_Rush_Play_PPA_PY1_col4 = dense_rank(desc(off_rushing_plays_ppa_PY1)),
      Rank_Off_Rush_Play_Success_Rt_PY1_col4 = dense_rank(desc(off_rushing_plays_success_rate_PY1)),
      Rank_Off_Rush_Play_Explosiveness_PY1_col4 = dense_rank(desc(off_rushing_plays_explosiveness_PY1)),
      Rank_Off_Pass_Play_PPA_PY1_col4 = dense_rank(desc(off_passing_plays_ppa_PY1)),
      Rank_Off_Pass_Play_Success_Rt_PY1_col4 = dense_rank(desc(off_passing_plays_success_rate_PY1)),
      Rank_Off_Pass_Play_Explosiveness_PY1_col4 = dense_rank(desc(off_passing_plays_explosiveness_PY1)),
      Rank_Def_PPA_PY1_col4 = dense_rank(def_ppa_PY1),
      Rank_Def_Success_Rt_PY1_col4 = dense_rank(def_success_rate_PY1),
      Rank_Def_Explosiveness_PY1_col4 = dense_rank(def_explosiveness_PY1),
      Rank_Def_Pwr_Success_PY1_col4 = dense_rank(def_power_success_PY1),
      Rank_Def_Stuff_Rt_PY1_col4 = dense_rank(desc(def_stuff_rate_PY1)),
      Rank_Def_Pts_Per_Opp_PY1_col4 = dense_rank(def_pts_per_opp_PY1),
      Rank_Def_Havoc_Total_PY1_col4 = dense_rank(desc(def_havoc_total_PY1)),
      Rank_Def_Havoc_Front_Seven_PY1_col4 = dense_rank(desc(def_havoc_front_seven_PY1)),
      Rank_Def_Havoc_DB_PY1_col4 = dense_rank(desc(def_havoc_db_PY1)),
      Rank_Def_Standard_Down_PPA_PY1_col4 = dense_rank(def_standard_downs_ppa_PY1),
      Rank_Def_Standard_Down_Success_Rt_PY1_col4 = dense_rank(def_standard_downs_success_rate_PY1),
      Rank_Def_Standard_Down_Explosiveness_PY1_col4 = dense_rank(def_standard_downs_explosiveness_PY1),
      Rank_Def_Pass_Down_PPA_PY1_col4 = dense_rank(def_passing_downs_ppa_PY1),
      Rank_Def_Pass_Down_Success_Rt_PY1_col4 = dense_rank(def_passing_downs_success_rate_PY1),
      Rank_Def_Pass_Down_Explosiveness_PY1_col4 = dense_rank(def_passing_downs_explosiveness_PY1),
      Rank_Def_Rush_Play_PPA_PY1_col4 = dense_rank(def_rushing_plays_ppa_PY1),
      Rank_Def_Rush_Play_Success_Rt_PY1_col4 = dense_rank(def_rushing_plays_success_rate_PY1),
      Rank_Def_Rush_Play_Explosiveness_PY1_col4 = dense_rank(def_rushing_plays_explosiveness_PY1),
      Rank_Def_Pass_Play_PPA_PY1_col4 = dense_rank(def_passing_plays_ppa_PY1),
      Rank_Def_Pass_Play_Success_Rt_PY1_col4 = dense_rank(def_passing_plays_success_rate_PY1),
      Rank_Def_Pass_Play_Explosiveness_PY1_col4 = dense_rank(def_passing_plays_explosiveness_PY1),
      ## FPI_SP mean ranks added at the end, weighted once
      Rank_FPI_SP_SRS_PY2_mean = dense_rank(desc(FPI_SP_SRS_PY2_mean)),
      Rank_FPI_SP_SRS_PY1_mean = dense_rank(desc(FPI_SP_SRS_PY1_mean)),
      Rank_AllPY_FPI_SP_SRS_mean = dense_rank(desc(AllPY_FPI_SP_mean)),
      Rank_WeightedAllPY_FPI_SP_SRS_mean = dense_rank(desc(WeightedAllPY_FPI_SP_SRS_mean)),
      ## Ranking current stats
      Rank_Wins = dense_rank(desc(Wins)),
      Rank_Losses = dense_rank(Losses),
      Rank_Comp_Pct = dense_rank(desc(completion_pct)),
      Rank_Pass_YPA = dense_rank(desc(pass_ypa)),
      Rank_Pass_YPR = dense_rank(desc(pass_ypr)),
      Rank_Int_Pct = dense_rank(int_pct),
      Rank_Rush_YPC = dense_rank(desc(rush_ypc)),
      Rank_Turnovers_pg = dense_rank(turnovers_pg),
      Rank_Third_Conv_Rate = dense_rank(desc(third_conv_rate)),
      Rank_Fourth_Conv_Rate = dense_rank(desc(fourth_conv_rate)),
      Rank_Penalty_Yds_pg = dense_rank(penalty_yds_pg),
      Rank_Yds_Per_Penalty = dense_rank(yards_per_penalty),
      Rank_Kick_Return_Avg = dense_rank(desc(kick_return_avg)),
      Rank_Punt_Return_Avg = dense_rank(desc(punt_return_avg)),
      Rank_Total_Yds_pg = dense_rank(desc(total_yds_pg)),
      Rank_Pass_Yds_pg = dense_rank(desc(pass_yds_pg)),
      Rank_Rush_Yds_pg = dense_rank(desc(rush_yds_pg)),
      Rank_First_Downs_pg = dense_rank(desc(first_downs_pg)),
      Rank_Off_YPP = dense_rank(desc(off_ypp)),
      Rank_Def_Ints_pg = dense_rank(desc(def_interceptions_pg)),
      Rank_Off_PPA = dense_rank(desc(off_ppa)),
      Rank_Off_Success_Rt = dense_rank(desc(off_success_rate)),
      Rank_Off_Explosiveness = dense_rank(desc(off_explosiveness)),
      Rank_Off_Pwr_Success = dense_rank(desc(off_power_success)),
      Rank_Off_Stuff_Rt = dense_rank(off_stuff_rate),
      Rank_Off_Line_Yds = dense_rank(desc(off_line_yds)),
      Rank_Off_Second_Lvl_Yds = dense_rank(desc(off_second_lvl_yds)),
      Rank_Off_Open_Field_Yds = dense_rank(desc(off_open_field_yds)),
      Rank_Off_Pts_Per_Opp = dense_rank(desc(off_pts_per_opp)),
      Rank_Off_Field_Pos_Avg_Predicted_Pts = dense_rank(desc(off_field_pos_avg_predicted_points)),
      Rank_Off_Havoc_Total = dense_rank(off_havoc_total),
      Rank_Off_Havoc_Front = dense_rank(off_havoc_front_seven),
      Rank_Off_Havoc_DB = dense_rank(off_havoc_db),
      Rank_Off_Standard_Down_PPA = dense_rank(desc(off_standard_downs_ppa)),
      Rank_Off_Standard_Down_Success_Rt = dense_rank(desc(off_standard_downs_success_rate)),
      Rank_Off_Standard_Down_Explosiveness = dense_rank(desc(off_standard_downs_explosiveness)),
      Rank_Off_Pass_Down_PPA = dense_rank(desc(off_passing_downs_ppa)),
      Rank_Off_Pass_Down_Success_Rt = dense_rank(desc(off_passing_downs_success_rate)),
      Rank_Off_Pass_Down_Explosiveness = dense_rank(desc(off_passing_downs_explosiveness)),
      Rank_Off_Rush_Play_PPA = dense_rank(desc(off_rushing_plays_ppa)),
      Rank_Off_Rush_Play_Success_Rt = dense_rank(desc(off_rushing_plays_success_rate)),
      Rank_Off_Rush_Play_Explosiveness = dense_rank(desc(off_rushing_plays_explosiveness)),
      Rank_Off_Pass_Play_PPA = dense_rank(desc(off_passing_plays_ppa)),
      Rank_Off_Pass_Play_Success_Rt = dense_rank(desc(off_passing_plays_success_rate)),
      Rank_Off_Pass_Play_Explosiveness = dense_rank(desc(off_passing_plays_explosiveness)),
      Rank_Def_PPA = dense_rank(def_ppa),
      Rank_Def_Success_Rt = dense_rank(def_success_rate),
      Rank_Def_Explosiveness = dense_rank(def_explosiveness),
      Rank_Def_Pwr_Success = dense_rank(def_power_success),
      Rank_Def_Stuff_Rt = dense_rank(desc(def_stuff_rate)),
      Rank_Def_Line_Yds = dense_rank(def_line_yds),
      Rank_Def_Second_Lvl_Yds = dense_rank(def_second_lvl_yds),
      Rank_Def_Open_Field_Yds = dense_rank(def_open_field_yds),
      Rank_Def_Pts_Per_Opp = dense_rank(def_pts_per_opp),
      Rank_Def_Field_Pos_Avg_Predicted_Pts = dense_rank(def_field_pos_avg_predicted_points),
      Rank_Def_Havoc_Total = dense_rank(desc(def_havoc_total)),
      Rank_Def_Havoc_Front_Seven = dense_rank(desc(def_havoc_front_seven)),
      Rank_Def_Havoc_DB = dense_rank(desc(def_havoc_db)),
      Rank_Def_Standard_Down_PPA = dense_rank(def_standard_downs_ppa),
      Rank_Def_Standard_Down_Success_Rt = dense_rank(def_standard_downs_success_rate),
      Rank_Def_Standard_Down_Explosiveness = dense_rank(def_standard_downs_explosiveness),
      Rank_Def_Pass_Down_PPA = dense_rank(def_passing_downs_ppa),
      Rank_Def_Pass_Down_Success_Rt = dense_rank(def_passing_downs_success_rate),
      Rank_Def_Pass_Down_Explosiveness = dense_rank(def_passing_downs_explosiveness),
      Rank_Def_Rush_Play_PPA = dense_rank(def_rushing_plays_ppa),
      Rank_Def_Rush_Play_Success_Rt = dense_rank(def_rushing_plays_success_rate),
      Rank_Def_Rush_Play_Explosiveness = dense_rank(def_rushing_plays_explosiveness),
      Rank_Def_Pass_Play_PPA = dense_rank(def_passing_plays_ppa),
      Rank_Def_Pass_Play_Success_Rt = dense_rank(def_passing_plays_success_rate),
      Rank_Def_Pass_Play_Explosiveness = dense_rank(def_passing_plays_explosiveness),
      Rank_SP_Rating = dense_rank(desc(sp_rating)),
      Rank_SP_Off_Rating = dense_rank(desc(sp_offense_rating)),
      Rank_SP_Def_Rating = dense_rank(sp_defense_rating),
      Rank_SP_SpecialTeams_Rating = dense_rank(desc(sp_special_teams_rating)),
      Rank_FPI = dense_rank(desc(FPI)),
      Rank_FPI_SP_mean = dense_rank(desc(FPI_SP_mean)),
      Rank_SRS = dense_rank(desc(SRS_rating)),
      Rank_FPI_SRS_mean = dense_rank(FPI_SRS_mean),
      # due to availability issues, SP_Rankings not always included with current data
      ## Extra weighted variables for current year
      Rank_Wins_col2 = dense_rank(desc(Wins)),
      Rank_Losses_col2 = dense_rank(Losses),
      Rank_Off_YPP_col2 = dense_rank(desc(off_ypp)),
      Rank_Off_PPA_col2 = dense_rank(desc(off_ppa)),
      Rank_Off_Success_Rt_col2 = dense_rank(desc(off_success_rate)),
      Rank_Off_Explosiveness_col2 = dense_rank(desc(off_explosiveness)),
      Rank_Off_Pwr_Success_col2 = dense_rank(desc(off_power_success)),
      Rank_Off_Stuff_Rt_col2 = dense_rank(off_stuff_rate),
      Rank_Off_Pts_Per_Opp_col2 = dense_rank(desc(off_pts_per_opp)),
      Rank_Off_Havoc_Total_col2 = dense_rank(off_havoc_total),
      Rank_Off_Havoc_Front_col2 = dense_rank(off_havoc_front_seven),
      Rank_Off_Havoc_DB_col2 = dense_rank(off_havoc_db),
      Rank_Off_Standard_Down_PPA_col2 = dense_rank(desc(off_standard_downs_ppa)),
      Rank_Off_Standard_Down_Success_Rt_col2 = dense_rank(desc(off_standard_downs_success_rate)),
      Rank_Off_Standard_Down_Explosiveness_col2 = dense_rank(desc(off_standard_downs_explosiveness)),
      Rank_Off_Pass_Down_PPA_col2 = dense_rank(desc(off_passing_downs_ppa)),
      Rank_Off_Pass_Down_Success_Rt_col2 = dense_rank(desc(off_passing_downs_success_rate)),
      Rank_Off_Pass_Down_Explosiveness_col2 = dense_rank(desc(off_passing_downs_explosiveness)),
      Rank_Off_Rush_Play_PPA_col2 = dense_rank(desc(off_rushing_plays_ppa)),
      Rank_Off_Rush_Play_Success_Rt_col2 = dense_rank(desc(off_rushing_plays_success_rate)),
      Rank_Off_Rush_Play_Explosiveness_col2 = dense_rank(desc(off_rushing_plays_explosiveness)),
      Rank_Off_Pass_Play_PPA_col2 = dense_rank(desc(off_passing_plays_ppa)),
      Rank_Off_Pass_Play_Success_Rt_col2 = dense_rank(desc(off_passing_plays_success_rate)),
      Rank_Off_Pass_Play_Explosiveness_col2 = dense_rank(desc(off_passing_plays_explosiveness)),
      Rank_Def_PPA_col2 = dense_rank(def_ppa),
      Rank_Def_Success_Rt_col2 = dense_rank(def_success_rate),
      Rank_Def_Explosiveness_col2 = dense_rank(def_explosiveness),
      Rank_Def_Pwr_Success_col2 = dense_rank(def_power_success),
      Rank_Def_Stuff_Rt_col2 = dense_rank(desc(def_stuff_rate)),
      Rank_Def_Pts_Per_Opp_col2 = dense_rank(def_pts_per_opp),
      Rank_Def_Havoc_Total_col2 = dense_rank(desc(def_havoc_total)),
      Rank_Def_Havoc_Front_Seven_col2 = dense_rank(desc(def_havoc_front_seven)),
      Rank_Def_Havoc_DB_col2 = dense_rank(desc(def_havoc_db)),
      Rank_Def_Standard_Down_PPA_col2 = dense_rank(def_standard_downs_ppa),
      Rank_Def_Standard_Down_Success_Rt_col2 = dense_rank(def_standard_downs_success_rate),
      Rank_Def_Standard_Down_Explosiveness_col2 = dense_rank(def_standard_downs_explosiveness),
      Rank_Def_Pass_Down_PPA_col2 = dense_rank(def_passing_downs_ppa),
      Rank_Def_Pass_Down_Success_Rt_col2 = dense_rank(def_passing_downs_success_rate),
      Rank_Def_Pass_Down_Explosiveness_col2 = dense_rank(def_passing_downs_explosiveness),
      Rank_Def_Rush_Play_PPA_col2 = dense_rank(def_rushing_plays_ppa),
      Rank_Def_Rush_Play_Success_Rt_col2 = dense_rank(def_rushing_plays_success_rate),
      Rank_Def_Rush_Play_Explosiveness_col2 = dense_rank(def_rushing_plays_explosiveness),
      Rank_Def_Pass_Play_PPA_col2 = dense_rank(def_passing_plays_ppa),
      Rank_Def_Pass_Play_Success_Rt_col2 = dense_rank(def_passing_plays_success_rate),
      Rank_Def_Pass_Play_Explosiveness_col2 = dense_rank(def_passing_plays_explosiveness))
} else if (as.numeric(week) == 4) {
  ##### Week 4 Variable Ranks #####
  # PY2 weighted 1x, PY1 weighted 2x, current weighted 2x
  ## PY2 ranks
  VoA_Variables <- VoA_Variables |>
    mutate(Rank_Wins_PY2 = dense_rank(desc(Wins_PY2)),
           Rank_Losses_PY2 = dense_rank(Losses_PY2),
           Rank_Comp_Pct_PY2 = dense_rank(desc(completion_pct_PY2)),
           Rank_Pass_YPA_PY2 = dense_rank(desc(pass_ypa_PY2)),
      Rank_Pass_YPR_PY2 = dense_rank(desc(pass_ypr_PY2)),
      Rank_Int_Pct_PY2 = dense_rank(int_pct_PY2),
      Rank_Rush_YPC_PY2 = dense_rank(desc(rush_ypc_PY2)),
      Rank_Turnovers_pg_PY2 = dense_rank(turnovers_pg_PY2),
      Rank_Third_Conv_Rate_PY2 = dense_rank(desc(third_conv_rate_PY2)),
      Rank_Fourth_Conv_Rate_PY2 = dense_rank(desc(fourth_conv_rate_PY2)),
      Rank_Penalty_Yds_pg_PY2 = dense_rank(penalty_yds_pg_PY2),
      Rank_Yds_Per_Penalty_PY2 = dense_rank(yards_per_penalty_PY2),
      Rank_Kick_Return_Avg_PY2 = dense_rank(desc(kick_return_avg_PY2)),
      Rank_Punt_Return_Avg_PY2 = dense_rank(desc(punt_return_avg_PY2)),
      Rank_Total_Yds_pg_PY2 = dense_rank(desc(total_yds_pg_PY2)),
      Rank_Pass_Yds_pg_PY2 = dense_rank(desc(pass_yds_pg_PY2)),
      Rank_Rush_Yds_pg_PY2 = dense_rank(desc(rush_yds_pg_PY2)),
      Rank_First_Downs_pg_PY2 = dense_rank(desc(first_downs_pg_PY2)),
      Rank_Off_YPP_PY2 = dense_rank(desc(off_ypp_PY2)),
      Rank_Def_Ints_pg_PY2 = dense_rank(desc(def_interceptions_pg_PY2)),
      Rank_Off_PPA_PY2 = dense_rank(desc(off_ppa_PY2)),
      Rank_Off_Success_Rt_PY2 = dense_rank(desc(off_success_rate_PY2)),
      Rank_Off_Explosiveness_PY2 = dense_rank(desc(off_explosiveness_PY2)),
      Rank_Off_Pwr_Success_PY2 = dense_rank(desc(off_power_success_PY2)),
      Rank_Off_Stuff_Rt_PY2 = dense_rank(off_stuff_rate_PY2),
      Rank_Off_Line_Yds_PY2 = dense_rank(desc(off_line_yds_PY2)),
      Rank_Off_Second_Lvl_Yds_PY2 = dense_rank(desc(off_second_lvl_yds_PY2)),
      Rank_Off_Open_Field_Yds_PY2 = dense_rank(desc(off_open_field_yds_PY2)),
      Rank_Off_Pts_Per_Opp_PY2 = dense_rank(desc(off_pts_per_opp_PY2)),
      Rank_Off_Field_Pos_Avg_Predicted_Pts_PY2 = dense_rank(desc(off_field_pos_avg_predicted_points_PY2)),
      Rank_Off_Havoc_Total_PY2 = dense_rank(off_havoc_total_PY2),
      Rank_Off_Havoc_Front_PY2 = dense_rank(off_havoc_front_seven_PY2),
      Rank_Off_Havoc_DB_PY2 = dense_rank(off_havoc_db_PY2),
      Rank_Off_Standard_Down_PPA_PY2 = dense_rank(desc(off_standard_downs_ppa_PY2)),
      Rank_Off_Standard_Down_Success_Rt_PY2 = dense_rank(desc(off_standard_downs_success_rate_PY2)),
      Rank_Off_Standard_Down_Explosiveness_PY2 = dense_rank(desc(off_standard_downs_explosiveness_PY2)),
      Rank_Off_Pass_Down_PPA_PY2 = dense_rank(desc(off_passing_downs_ppa_PY2)),
      Rank_Off_Pass_Down_Success_Rt_PY2 = dense_rank(desc(off_passing_downs_success_rate_PY2)),
      Rank_Off_Pass_Down_Explosiveness_PY2 = dense_rank(desc(off_passing_downs_explosiveness_PY2)),
      Rank_Off_Rush_Play_PPA_PY2 = dense_rank(desc(off_rushing_plays_ppa_PY2)),
      Rank_Off_Rush_Play_Success_Rt_PY2 = dense_rank(desc(off_rushing_plays_success_rate_PY2)),
      Rank_Off_Rush_Play_Explosiveness_PY2 = dense_rank(desc(off_rushing_plays_explosiveness_PY2)),
      Rank_Off_Pass_Play_PPA_PY2 = dense_rank(desc(off_passing_plays_ppa_PY2)),
      Rank_Off_Pass_Play_Success_Rt_PY2 = dense_rank(desc(off_passing_plays_success_rate_PY2)),
      Rank_Off_Pass_Play_Explosiveness_PY2 = dense_rank(desc(off_passing_plays_explosiveness_PY2)),
      Rank_Def_PPA_PY2 = dense_rank(def_ppa_PY2),
      Rank_Def_Success_Rt_PY2 = dense_rank(def_success_rate_PY2),
      Rank_Def_Explosiveness_PY2 = dense_rank(def_explosiveness_PY2),
      Rank_Def_Pwr_Success_PY2 = dense_rank(def_power_success_PY2),
      Rank_Def_Stuff_Rt_PY2 = dense_rank(desc(def_stuff_rate_PY2)),
      Rank_Def_Line_Yds_PY2 = dense_rank(def_line_yds_PY2),
      Rank_Def_Second_Lvl_Yds_PY2 = dense_rank(def_second_lvl_yds_PY2),
      Rank_Def_Open_Field_Yds_PY2 = dense_rank(def_open_field_yds_PY2),
      Rank_Def_Pts_Per_Opp_PY2 = dense_rank(def_pts_per_opp_PY2),
      Rank_Def_Field_Pos_Avg_Predicted_Pts_PY2 = dense_rank(def_field_pos_avg_predicted_points_PY2),
      Rank_Def_Havoc_Total_PY2 = dense_rank(desc(def_havoc_total_PY2)),
      Rank_Def_Havoc_Front_Seven_PY2 = dense_rank(desc(def_havoc_front_seven_PY2)),
      Rank_Def_Havoc_DB_PY2 = dense_rank(desc(def_havoc_db_PY2)),
      Rank_Def_Standard_Down_PPA_PY2 = dense_rank(def_standard_downs_ppa_PY2),
      Rank_Def_Standard_Down_Success_Rt_PY2 = dense_rank(def_standard_downs_success_rate_PY2),
      Rank_Def_Standard_Down_Explosiveness_PY2 = dense_rank(def_standard_downs_explosiveness_PY2),
      Rank_Def_Pass_Down_PPA_PY2 = dense_rank(def_passing_downs_ppa_PY2),
      Rank_Def_Pass_Down_Success_Rt_PY2 = dense_rank(def_passing_downs_success_rate_PY2),
      Rank_Def_Pass_Down_Explosiveness_PY2 = dense_rank(def_passing_downs_explosiveness_PY2),
      Rank_Def_Rush_Play_PPA_PY2 = dense_rank(def_rushing_plays_ppa_PY2),
      Rank_Def_Rush_Play_Success_Rt_PY2 = dense_rank(def_rushing_plays_success_rate_PY2),
      Rank_Def_Rush_Play_Explosiveness_PY2 = dense_rank(def_rushing_plays_explosiveness_PY2),
      Rank_Def_Pass_Play_PPA_PY2 = dense_rank(def_passing_plays_ppa_PY2),
      Rank_Def_Pass_Play_Success_Rt_PY2 = dense_rank(def_passing_plays_success_rate_PY2),
      Rank_Def_Pass_Play_Explosiveness_PY2 = dense_rank(def_passing_plays_explosiveness_PY2),
      Rank_Recruit_Pts_PY2 = dense_rank(desc(recruit_pts_PY2)),
      Rank_Talent_PY2 = dense_rank(desc(talent_PY2)),
      Rank_SP_Rating_PY2 = dense_rank(desc(sp_rating_PY2)),
      Rank_SP_Off_Rating_PY2 = dense_rank(desc(sp_offense_rating_PY2)),
      Rank_SP_Def_Rating_PY2 = dense_rank(sp_defense_rating_PY2),
      Rank_SP_SpecialTeams_Rating_PY2 = dense_rank(desc(sp_special_teams_rating_PY2)),
      Rank_FPI_PY2 = dense_rank(desc(FPI_PY2)),
      Rank_SRS_rating_PY2 = dense_rank(desc(SRS_rating_PY2)),
      ## PY2 extra weighted variables
      Rank_Off_YPP_PY2_col2 = dense_rank(desc(off_ypp_PY2)),
      Rank_Off_PPA_PY2_col2 = dense_rank(desc(off_ppa_PY2)),
      Rank_Off_Success_Rt_PY2_col2 = dense_rank(desc(off_success_rate_PY2)),
      Rank_Off_Explosiveness_PY2_col2 = dense_rank(desc(off_explosiveness_PY2)),
      Rank_Off_Pwr_Success_PY2_col2 = dense_rank(desc(off_power_success_PY2)),
      Rank_Off_Stuff_Rt_PY2_col2 = dense_rank(off_stuff_rate_PY2),
      Rank_Off_Pts_Per_Opp_PY2_col2 = dense_rank(desc(off_pts_per_opp_PY2)),
      Rank_Off_Havoc_Total_PY2_col2 = dense_rank(off_havoc_total_PY2),
      Rank_Off_Havoc_Front_PY2_col2 = dense_rank(off_havoc_front_seven_PY2),
      Rank_Off_Havoc_DB_PY2_col2 = dense_rank(off_havoc_db_PY2),
      Rank_Off_Standard_Down_PPA_PY2_col2 = dense_rank(desc(off_standard_downs_ppa_PY2)),
      Rank_Off_Standard_Down_Success_Rt_PY2_col2 = dense_rank(desc(off_standard_downs_success_rate_PY2)),
      Rank_Off_Standard_Down_Explosiveness_PY2_col2 = dense_rank(desc(off_standard_downs_explosiveness_PY2)),
      Rank_Off_Pass_Down_PPA_PY2_col2 = dense_rank(desc(off_passing_downs_ppa_PY2)),
      Rank_Off_Pass_Down_Success_Rt_PY2_col2 = dense_rank(desc(off_passing_downs_success_rate_PY2)),
      Rank_Off_Pass_Down_Explosiveness_PY2_col2 = dense_rank(desc(off_passing_downs_explosiveness_PY2)),
      Rank_Off_Rush_Play_PPA_PY2_col2 = dense_rank(desc(off_rushing_plays_ppa_PY2)),
      Rank_Off_Rush_Play_Success_Rt_PY2_col2 = dense_rank(desc(off_rushing_plays_success_rate_PY2)),
      Rank_Off_Rush_Play_Explosiveness_PY2_col2 = dense_rank(desc(off_rushing_plays_explosiveness_PY2)),
      Rank_Off_Pass_Play_PPA_PY2_col2 = dense_rank(desc(off_passing_plays_ppa_PY2)),
      Rank_Off_Pass_Play_Success_Rt_PY2_col2 = dense_rank(desc(off_passing_plays_success_rate_PY2)),
      Rank_Off_Pass_Play_Explosiveness_PY2_col2 = dense_rank(desc(off_passing_plays_explosiveness_PY2)),
      Rank_Def_PPA_PY2_col2 = dense_rank(def_ppa_PY2),
      Rank_Def_Success_Rt_PY2_col2 = dense_rank(def_success_rate_PY2),
      Rank_Def_Explosiveness_PY2_col2 = dense_rank(def_explosiveness_PY2),
      Rank_Def_Pwr_Success_PY2_col2 = dense_rank(def_power_success_PY2),
      Rank_Def_Stuff_Rt_PY2_col2 = dense_rank(desc(def_stuff_rate_PY2)),
      Rank_Def_Pts_Per_Opp_PY2_col2 = dense_rank(def_pts_per_opp_PY2),
      Rank_Def_Havoc_Total_PY2_col2 = dense_rank(desc(def_havoc_total_PY2)),
      Rank_Def_Havoc_Front_Seven_PY2_col2 = dense_rank(desc(def_havoc_front_seven_PY2)),
      Rank_Def_Havoc_DB_PY2_col2 = dense_rank(desc(def_havoc_db_PY2)),
      Rank_Def_Standard_Down_PPA_PY2_col2 = dense_rank(def_standard_downs_ppa_PY2),
      Rank_Def_Standard_Down_Success_Rt_PY2_col2 = dense_rank(def_standard_downs_success_rate_PY2),
      Rank_Def_Standard_Down_Explosiveness_PY2_col2 = dense_rank(def_standard_downs_explosiveness_PY2),
      Rank_Def_Pass_Down_PPA_PY2_col2 = dense_rank(def_passing_downs_ppa_PY2),
      Rank_Def_Pass_Down_Success_Rt_PY2_col2 = dense_rank(def_passing_downs_success_rate_PY2),
      Rank_Def_Pass_Down_Explosiveness_PY2_col2 = dense_rank(def_passing_downs_explosiveness_PY2),
      Rank_Def_Rush_Play_PPA_PY2_col2 = dense_rank(def_rushing_plays_ppa_PY2),
      Rank_Def_Rush_Play_Success_Rt_PY2_col2 = dense_rank(def_rushing_plays_success_rate_PY2),
      Rank_Def_Rush_Play_Explosiveness_PY2_col2 = dense_rank(def_rushing_plays_explosiveness_PY2),
      Rank_Def_Pass_Play_PPA_PY2_col2 = dense_rank(def_passing_plays_ppa_PY2),
      Rank_Def_Pass_Play_Success_Rt_PY2_col2 = dense_rank(def_passing_plays_success_rate_PY2),
      Rank_Def_Pass_Play_Explosiveness_PY2_col2 = dense_rank(def_passing_plays_explosiveness_PY2),
      Rank_Recruit_Pts_PY2_col2 = dense_rank(desc(recruit_pts_PY2)),
      ## PY1 ranks
      Rank_Wins_PY1 = dense_rank(desc(Wins_PY1)),
      Rank_Losses_PY1 = dense_rank(Losses_PY1),
      Rank_Comp_Pct_PY1 = dense_rank(desc(completion_pct_PY1)),
      Rank_Pass_YPA_PY1 = dense_rank(desc(pass_ypa_PY1)),
      Rank_Pass_YPR_PY1 = dense_rank(desc(pass_ypr_PY1)),
      Rank_Int_Pct_PY1 = dense_rank(int_pct_PY1),
      Rank_Rush_YPC_PY1 = dense_rank(desc(rush_ypc_PY1)),
      Rank_Turnovers_pg_PY1 = dense_rank(turnovers_pg_PY1),
      Rank_Third_Conv_Rate_PY1 = dense_rank(desc(third_conv_rate_PY1)),
      Rank_Fourth_Conv_Rate_PY1 = dense_rank(desc(fourth_conv_rate_PY1)),
      Rank_Penalty_Yds_pg_PY1 = dense_rank(penalty_yds_pg_PY1),
      Rank_Yds_Per_Penalty_PY1 = dense_rank(yards_per_penalty_PY1),
      Rank_Kick_Return_Avg_PY1 = dense_rank(desc(kick_return_avg_PY1)),
      Rank_Punt_Return_Avg_PY1 = dense_rank(desc(punt_return_avg_PY1)),
      Rank_Total_Yds_pg_PY1 = dense_rank(desc(total_yds_pg_PY1)),
      Rank_Pass_Yds_pg_PY1 = dense_rank(desc(pass_yds_pg_PY1)),
      Rank_Rush_Yds_pg_PY1 = dense_rank(desc(rush_yds_pg_PY1)),
      Rank_First_Downs_pg_PY1 = dense_rank(desc(first_downs_pg_PY1)),
      Rank_Off_YPP_PY1 = dense_rank(desc(off_ypp_PY1)),
      Rank_Def_Ints_pg_PY1 = dense_rank(desc(def_interceptions_pg_PY1)),
      Rank_Off_PPA_PY1 = dense_rank(desc(off_ppa_PY1)),
      Rank_Off_Success_Rt_PY1 = dense_rank(desc(off_success_rate_PY1)),
      Rank_Off_Explosiveness_PY1 = dense_rank(desc(off_explosiveness_PY1)),
      Rank_Off_Pwr_Success_PY1 = dense_rank(desc(off_power_success_PY1)),
      Rank_Off_Stuff_Rt_PY1 = dense_rank(off_stuff_rate_PY1),
      Rank_Off_Line_Yds_PY1 = dense_rank(desc(off_line_yds_PY1)),
      Rank_Off_Second_Lvl_Yds_PY1 = dense_rank(desc(off_second_lvl_yds_PY1)),
      Rank_Off_Open_Field_Yds_PY1 = dense_rank(desc(off_open_field_yds_PY1)),
      Rank_Off_Pts_Per_Opp_PY1 = dense_rank(desc(off_pts_per_opp_PY1)),
      Rank_Off_Field_Pos_Avg_Predicted_Pts_PY1 = dense_rank(desc(off_field_pos_avg_predicted_points_PY1)),
      Rank_Off_Havoc_Total_PY1 = dense_rank(off_havoc_total_PY1),
      Rank_Off_Havoc_Front_PY1 = dense_rank(off_havoc_front_seven_PY1),
      Rank_Off_Havoc_DB_PY1 = dense_rank(off_havoc_db_PY1),
      Rank_Off_Standard_Down_PPA_PY1 = dense_rank(desc(off_standard_downs_ppa_PY1)),
      Rank_Off_Standard_Down_Success_Rt_PY1 = dense_rank(desc(off_standard_downs_success_rate_PY1)),
      Rank_Off_Standard_Down_Explosiveness_PY1 = dense_rank(desc(off_standard_downs_explosiveness_PY1)),
      Rank_Off_Pass_Down_PPA_PY1 = dense_rank(desc(off_passing_downs_ppa_PY1)),
      Rank_Off_Pass_Down_Success_Rt_PY1 = dense_rank(desc(off_passing_downs_success_rate_PY1)),
      Rank_Off_Pass_Down_Explosiveness_PY1 = dense_rank(desc(off_passing_downs_explosiveness_PY1)),
      Rank_Off_Rush_Play_PPA_PY1 = dense_rank(desc(off_rushing_plays_ppa_PY1)),
      Rank_Off_Rush_Play_Success_Rt_PY1 = dense_rank(desc(off_rushing_plays_success_rate_PY1)),
      Rank_Off_Rush_Play_Explosiveness_PY1 = dense_rank(desc(off_rushing_plays_explosiveness_PY1)),
      Rank_Off_Pass_Play_PPA_PY1 = dense_rank(desc(off_passing_plays_ppa_PY1)),
      Rank_Off_Pass_Play_Success_Rt_PY1 = dense_rank(desc(off_passing_plays_success_rate_PY1)),
      Rank_Off_Pass_Play_Explosiveness_PY1 = dense_rank(desc(off_passing_plays_explosiveness_PY1)),
      Rank_Def_PPA_PY1 = dense_rank(def_ppa_PY1),
      Rank_Def_Success_Rt_PY1 = dense_rank(def_success_rate_PY1),
      Rank_Def_Explosiveness_PY1 = dense_rank(def_explosiveness_PY1),
      Rank_Def_Pwr_Success_PY1 = dense_rank(def_power_success_PY1),
      Rank_Def_Stuff_Rt_PY1 = dense_rank(desc(def_stuff_rate_PY1)),
      Rank_Def_Line_Yds_PY1 = dense_rank(def_line_yds_PY1),
      Rank_Def_Second_Lvl_Yds_PY1 = dense_rank(def_second_lvl_yds_PY1),
      Rank_Def_Open_Field_Yds_PY1 = dense_rank(def_open_field_yds_PY1),
      Rank_Def_Pts_Per_Opp_PY1 = dense_rank(def_pts_per_opp_PY1),
      Rank_Def_Field_Pos_Avg_Predicted_Pts_PY1 = dense_rank(def_field_pos_avg_predicted_points_PY1),
      Rank_Def_Havoc_Total_PY1 = dense_rank(desc(def_havoc_total_PY1)),
      Rank_Def_Havoc_Front_Seven_PY1 = dense_rank(desc(def_havoc_front_seven_PY1)),
      Rank_Def_Havoc_DB_PY1 = dense_rank(desc(def_havoc_db_PY1)),
      Rank_Def_Standard_Down_PPA_PY1 = dense_rank(def_standard_downs_ppa_PY1),
      Rank_Def_Standard_Down_Success_Rt_PY1 = dense_rank(def_standard_downs_success_rate_PY1),
      Rank_Def_Standard_Down_Explosiveness_PY1 = dense_rank(def_standard_downs_explosiveness_PY1),
      Rank_Def_Pass_Down_PPA_PY1 = dense_rank(def_passing_downs_ppa_PY1),
      Rank_Def_Pass_Down_Success_Rt_PY1 = dense_rank(def_passing_downs_success_rate_PY1),
      Rank_Def_Pass_Down_Explosiveness_PY1 = dense_rank(def_passing_downs_explosiveness_PY1),
      Rank_Def_Rush_Play_PPA_PY1 = dense_rank(def_rushing_plays_ppa_PY1),
      Rank_Def_Rush_Play_Success_Rt_PY1 = dense_rank(def_rushing_plays_success_rate_PY1),
      Rank_Def_Rush_Play_Explosiveness_PY1 = dense_rank(def_rushing_plays_explosiveness_PY1),
      Rank_Def_Pass_Play_PPA_PY1 = dense_rank(def_passing_plays_ppa_PY1),
      Rank_Def_Pass_Play_Success_Rt_PY1 = dense_rank(def_passing_plays_success_rate_PY1),
      Rank_Def_Pass_Play_Explosiveness_PY1 = dense_rank(def_passing_plays_explosiveness_PY1),
      Rank_Recruit_Pts_PY1 = dense_rank(desc(recruit_pts_PY1)),
      Rank_Talent_PY1 = dense_rank(desc(talent_PY1)),
      Rank_SP_Rating_PY1 = dense_rank(desc(sp_rating_PY1)),
      Rank_SP_Off_Rating_PY1 = dense_rank(desc(sp_offense_rating_PY1)),
      Rank_SP_Def_Rating_PY1 = dense_rank(sp_defense_rating_PY1),
      Rank_SP_SpecialTeams_Rating_PY1 = dense_rank(desc(sp_special_teams_rating_PY1)),
      Rank_FPI_PY1 = dense_rank(desc(FPI_PY1)),
      Rank_SRS_rating_PY1 = dense_rank(desc(SRS_rating_PY1)),
      ## PY1 weighted 2 times
      Rank_Wins_PY1_col2 = dense_rank(desc(Wins_PY1)),
      Rank_Losses_PY1_col2 = dense_rank(Losses_PY1),
      Rank_Comp_Pct_PY1_col2 = dense_rank(desc(completion_pct_PY1)),
      Rank_Pass_YPA_PY1_col2 = dense_rank(desc(pass_ypa_PY1)),
      Rank_Pass_YPR_PY1_col2 = dense_rank(desc(pass_ypr_PY1)),
      Rank_Int_Pct_PY1_col2 = dense_rank(int_pct_PY1),
      Rank_Rush_YPC_PY1_col2 = dense_rank(desc(rush_ypc_PY1)),
      Rank_Turnovers_pg_PY1_col2 = dense_rank(turnovers_pg_PY1),
      Rank_Third_Conv_Rate_PY1_col2 = dense_rank(desc(third_conv_rate_PY1)),
      Rank_Fourth_Conv_Rate_PY1_col2 = dense_rank(desc(fourth_conv_rate_PY1)),
      Rank_Penalty_Yds_pg_PY1_col2 = dense_rank(penalty_yds_pg_PY1),
      Rank_Yds_Per_Penalty_PY1_col2 = dense_rank(yards_per_penalty_PY1),
      Rank_Kick_Return_Avg_PY1_col2 = dense_rank(desc(kick_return_avg_PY1)),
      Rank_Punt_Return_Avg_PY1_col2 = dense_rank(desc(punt_return_avg_PY1)),
      Rank_Total_Yds_pg_PY1_col2 = dense_rank(desc(total_yds_pg_PY1)),
      Rank_Pass_Yds_pg_PY1_col2 = dense_rank(desc(pass_yds_pg_PY1)),
      Rank_Rush_Yds_pg_PY1_col2 = dense_rank(desc(rush_yds_pg_PY1)),
      Rank_First_Downs_pg_PY1_col2 = dense_rank(desc(first_downs_pg_PY1)),
      Rank_Off_YPP_PY1_col2 = dense_rank(desc(off_ypp_PY1)),
      Rank_Def_Ints_pg_PY1_col2 = dense_rank(desc(def_interceptions_pg_PY1)),
      Rank_Off_PPA_PY1_col2 = dense_rank(desc(off_ppa_PY1)),
      Rank_Off_Success_Rt_PY1_col2 = dense_rank(desc(off_success_rate_PY1)),
      Rank_Off_Explosiveness_PY1_col2 = dense_rank(desc(off_explosiveness_PY1)),
      Rank_Off_Pwr_Success_PY1_col2 = dense_rank(desc(off_power_success_PY1)),
      Rank_Off_Stuff_Rt_PY1_col2 = dense_rank(off_stuff_rate_PY1),
      Rank_Off_Line_Yds_PY1_col2 = dense_rank(desc(off_line_yds_PY1)),
      Rank_Off_Second_Lvl_Yds_PY1_col2 = dense_rank(desc(off_second_lvl_yds_PY1)),
      Rank_Off_Open_Field_Yds_PY1_col2 = dense_rank(desc(off_open_field_yds_PY1)),
      Rank_Off_Pts_Per_Opp_PY1_col2 = dense_rank(desc(off_pts_per_opp_PY1)),
      Rank_Off_Field_Pos_Avg_Predicted_Pts_PY1_col2 = dense_rank(desc(off_field_pos_avg_predicted_points_PY1)),
      Rank_Off_Havoc_Total_PY1_col2 = dense_rank(off_havoc_total_PY1),
      Rank_Off_Havoc_Front_PY1_col2 = dense_rank(off_havoc_front_seven_PY1),
      Rank_Off_Havoc_DB_PY1_col2 = dense_rank(off_havoc_db_PY1),
      Rank_Off_Standard_Down_PPA_PY1_col2 = dense_rank(desc(off_standard_downs_ppa_PY1)),
      Rank_Off_Standard_Down_Success_Rt_PY1_col2 = dense_rank(desc(off_standard_downs_success_rate_PY1)),
      Rank_Off_Standard_Down_Explosiveness_PY1_col2 = dense_rank(desc(off_standard_downs_explosiveness_PY1)),
      Rank_Off_Pass_Down_PPA_PY1_col2 = dense_rank(desc(off_passing_downs_ppa_PY1)),
      Rank_Off_Pass_Down_Success_Rt_PY1_col2 = dense_rank(desc(off_passing_downs_success_rate_PY1)),
      Rank_Off_Pass_Down_Explosiveness_PY1_col2 = dense_rank(desc(off_passing_downs_explosiveness_PY1)),
      Rank_Off_Rush_Play_PPA_PY1_col2 = dense_rank(desc(off_rushing_plays_ppa_PY1)),
      Rank_Off_Rush_Play_Success_Rt_PY1_col2 = dense_rank(desc(off_rushing_plays_success_rate_PY1)),
      Rank_Off_Rush_Play_Explosiveness_PY1_col2 = dense_rank(desc(off_rushing_plays_explosiveness_PY1)),
      Rank_Off_Pass_Play_PPA_PY1_col2 = dense_rank(desc(off_passing_plays_ppa_PY1)),
      Rank_Off_Pass_Play_Success_Rt_PY1_col2 = dense_rank(desc(off_passing_plays_success_rate_PY1)),
      Rank_Off_Pass_Play_Explosiveness_PY1_col2 = dense_rank(desc(off_passing_plays_explosiveness_PY1)),
      Rank_Def_PPA_PY1_col2 = dense_rank(def_ppa_PY1),
      Rank_Def_Success_Rt_PY1_col2 = dense_rank(def_success_rate_PY1),
      Rank_Def_Explosiveness_PY1_col2 = dense_rank(def_explosiveness_PY1),
      Rank_Def_Pwr_Success_PY1_col2 = dense_rank(def_power_success_PY1),
      Rank_Def_Stuff_Rt_PY1_col2 = dense_rank(desc(def_stuff_rate_PY1)),
      Rank_Def_Line_Yds_PY1_col2 = dense_rank(def_line_yds_PY1),
      Rank_Def_Second_Lvl_Yds_PY1_col2 = dense_rank(def_second_lvl_yds_PY1),
      Rank_Def_Open_Field_Yds_PY1_col2 = dense_rank(def_open_field_yds_PY1),
      Rank_Def_Pts_Per_Opp_PY1_col2 = dense_rank(def_pts_per_opp_PY1),
      Rank_Def_Field_Pos_Avg_Predicted_Pts_PY1_col2 = dense_rank(def_field_pos_avg_predicted_points_PY1),
      Rank_Def_Havoc_Total_PY1_col2 = dense_rank(desc(def_havoc_total_PY1)),
      Rank_Def_Havoc_Front_Seven_PY1_col2 = dense_rank(desc(def_havoc_front_seven_PY1)),
      Rank_Def_Havoc_DB_PY1_col2 = dense_rank(desc(def_havoc_db_PY1)),
      Rank_Def_Standard_Down_PPA_PY1_col2 = dense_rank(def_standard_downs_ppa_PY1),
      Rank_Def_Standard_Down_Success_Rt_PY1_col2 = dense_rank(def_standard_downs_success_rate_PY1),
      Rank_Def_Standard_Down_Explosiveness_PY1_col2 = dense_rank(def_standard_downs_explosiveness_PY1),
      Rank_Def_Pass_Down_PPA_PY1_col2 = dense_rank(def_passing_downs_ppa_PY1),
      Rank_Def_Pass_Down_Success_Rt_PY1_col2 = dense_rank(def_passing_downs_success_rate_PY1),
      Rank_Def_Pass_Down_Explosiveness_PY1_col2 = dense_rank(def_passing_downs_explosiveness_PY1),
      Rank_Def_Rush_Play_PPA_PY1_col2 = dense_rank(def_rushing_plays_ppa_PY1),
      Rank_Def_Rush_Play_Success_Rt_PY1_col2 = dense_rank(def_rushing_plays_success_rate_PY1),
      Rank_Def_Rush_Play_Explosiveness_PY1_col2 = dense_rank(def_rushing_plays_explosiveness_PY1),
      Rank_Def_Pass_Play_PPA_PY1_col2 = dense_rank(def_passing_plays_ppa_PY1),
      Rank_Def_Pass_Play_Success_Rt_PY1_col2 = dense_rank(def_passing_plays_success_rate_PY1),
      Rank_Def_Pass_Play_Explosiveness_PY1_col2 = dense_rank(def_passing_plays_explosiveness_PY1),
      Rank_Recruit_Pts_PY1_col2 = dense_rank(desc(recruit_pts_PY1)),
      Rank_Talent_PY1_col2 = dense_rank(desc(talent_PY1)),
      ## incoming recruiting class, weighted once
      ## PY3 recruiting points also included, weighted 3x
      # only PY3 variable still included
      Rank_Recruit_Pts = dense_rank(desc(recruit_pts)),
      Rank_Recruit_Pts_PY3 = dense_rank(desc(recruit_pts_PY3)),
      Rank_Recruit_Pts_PY3_col2 = Rank_Recruit_Pts_PY3,
      Rank_Recruit_Pts_PY3_col3 = Rank_Recruit_Pts_PY3,
      ## Extra weighted variables, weighted 2x (2 more times)
      Rank_Off_YPP_PY1_col3 = dense_rank(desc(off_ypp_PY1)),
      Rank_Off_PPA_PY1_col3 = dense_rank(desc(off_ppa_PY1)),
      Rank_Off_Success_Rt_PY1_col3 = dense_rank(desc(off_success_rate_PY1)),
      Rank_Off_Explosiveness_PY1_col3 = dense_rank(desc(off_explosiveness_PY1)),
      Rank_Off_Pwr_Success_PY1_col3 = dense_rank(desc(off_power_success_PY1)),
      Rank_Off_Stuff_Rt_PY1_col3 = dense_rank(off_stuff_rate_PY1),
      Rank_Off_Pts_Per_Opp_PY1_col3 = dense_rank(desc(off_pts_per_opp_PY1)),
      Rank_Off_Havoc_Total_PY1_col3 = dense_rank(off_havoc_total_PY1),
      Rank_Off_Havoc_Front_PY1_col3 = dense_rank(off_havoc_front_seven_PY1),
      Rank_Off_Havoc_DB_PY1_col3 = dense_rank(off_havoc_db_PY1),
      Rank_Off_Standard_Down_PPA_PY1_col3 = dense_rank(desc(off_standard_downs_ppa_PY1)),
      Rank_Off_Standard_Down_Success_Rt_PY1_col3 = dense_rank(desc(off_standard_downs_success_rate_PY1)),
      Rank_Off_Standard_Down_Explosiveness_PY1_col3 = dense_rank(desc(off_standard_downs_explosiveness_PY1)),
      Rank_Off_Pass_Down_PPA_PY1_col3 = dense_rank(desc(off_passing_downs_ppa_PY1)),
      Rank_Off_Pass_Down_Success_Rt_PY1_col3 = dense_rank(desc(off_passing_downs_success_rate_PY1)),
      Rank_Off_Pass_Down_Explosiveness_PY1_col3 = dense_rank(desc(off_passing_downs_explosiveness_PY1)),
      Rank_Off_Rush_Play_PPA_PY1_col3 = dense_rank(desc(off_rushing_plays_ppa_PY1)),
      Rank_Off_Rush_Play_Success_Rt_PY1_col3 = dense_rank(desc(off_rushing_plays_success_rate_PY1)),
      Rank_Off_Rush_Play_Explosiveness_PY1_col3 = dense_rank(desc(off_rushing_plays_explosiveness_PY1)),
      Rank_Off_Pass_Play_PPA_PY1_col3 = dense_rank(desc(off_passing_plays_ppa_PY1)),
      Rank_Off_Pass_Play_Success_Rt_PY1_col3 = dense_rank(desc(off_passing_plays_success_rate_PY1)),
      Rank_Off_Pass_Play_Explosiveness_PY1_col3 = dense_rank(desc(off_passing_plays_explosiveness_PY1)),
      Rank_Def_PPA_PY1_col3 = dense_rank(def_ppa_PY1),
      Rank_Def_Success_Rt_PY1_col3 = dense_rank(def_success_rate_PY1),
      Rank_Def_Explosiveness_PY1_col3 = dense_rank(def_explosiveness_PY1),
      Rank_Def_Pwr_Success_PY1_col3 = dense_rank(def_power_success_PY1),
      Rank_Def_Stuff_Rt_PY1_col3 = dense_rank(desc(def_stuff_rate_PY1)),
      Rank_Def_Pts_Per_Opp_PY1_col3 = dense_rank(def_pts_per_opp_PY1),
      Rank_Def_Havoc_Total_PY1_col3 = dense_rank(desc(def_havoc_total_PY1)),
      Rank_Def_Havoc_Front_Seven_PY1_col3 = dense_rank(desc(def_havoc_front_seven_PY1)),
      Rank_Def_Havoc_DB_PY1_col3 = dense_rank(desc(def_havoc_db_PY1)),
      Rank_Def_Standard_Down_PPA_PY1_col3 = dense_rank(def_standard_downs_ppa_PY1),
      Rank_Def_Standard_Down_Success_Rt_PY1_col3 = dense_rank(def_standard_downs_success_rate_PY1),
      Rank_Def_Standard_Down_Explosiveness_PY1_col3 = dense_rank(def_standard_downs_explosiveness_PY1),
      Rank_Def_Pass_Down_PPA_PY1_col3 = dense_rank(def_passing_downs_ppa_PY1),
      Rank_Def_Pass_Down_Success_Rt_PY1_col3 = dense_rank(def_passing_downs_success_rate_PY1),
      Rank_Def_Pass_Down_Explosiveness_PY1_col3 = dense_rank(def_passing_downs_explosiveness_PY1),
      Rank_Def_Rush_Play_PPA_PY1_col3 = dense_rank(def_rushing_plays_ppa_PY1),
      Rank_Def_Rush_Play_Success_Rt_PY1_col3 = dense_rank(def_rushing_plays_success_rate_PY1),
      Rank_Def_Rush_Play_Explosiveness_PY1_col3 = dense_rank(def_rushing_plays_explosiveness_PY1),
      Rank_Def_Pass_Play_PPA_PY1_col3 = dense_rank(def_passing_plays_ppa_PY1),
      Rank_Def_Pass_Play_Success_Rt_PY1_col3 = dense_rank(def_passing_plays_success_rate_PY1),
      Rank_Def_Pass_Play_Explosiveness_PY1_col3 = dense_rank(def_passing_plays_explosiveness_PY1),
      ## Extra weighted variables, weighted 2x (1 more time)
      Rank_Off_YPP_PY1_col4 = dense_rank(desc(off_ypp_PY1)),
      Rank_Off_PPA_PY1_col4 = dense_rank(desc(off_ppa_PY1)),
      Rank_Off_Success_Rt_PY1_col4 = dense_rank(desc(off_success_rate_PY1)),
      Rank_Off_Explosiveness_PY1_col4 = dense_rank(desc(off_explosiveness_PY1)),
      Rank_Off_Pwr_Success_PY1_col4 = dense_rank(desc(off_power_success_PY1)),
      Rank_Off_Stuff_Rt_PY1_col4 = dense_rank(off_stuff_rate_PY1),
      Rank_Off_Pts_Per_Opp_PY1_col4 = dense_rank(desc(off_pts_per_opp_PY1)),
      Rank_Off_Havoc_Total_PY1_col4 = dense_rank(off_havoc_total_PY1),
      Rank_Off_Havoc_Front_PY1_col4 = dense_rank(off_havoc_front_seven_PY1),
      Rank_Off_Havoc_DB_PY1_col4 = dense_rank(off_havoc_db_PY1),
      Rank_Off_Standard_Down_PPA_PY1_col4 = dense_rank(desc(off_standard_downs_ppa_PY1)),
      Rank_Off_Standard_Down_Success_Rt_PY1_col4 = dense_rank(desc(off_standard_downs_success_rate_PY1)),
      Rank_Off_Standard_Down_Explosiveness_PY1_col4 = dense_rank(desc(off_standard_downs_explosiveness_PY1)),
      Rank_Off_Pass_Down_PPA_PY1_col4 = dense_rank(desc(off_passing_downs_ppa_PY1)),
      Rank_Off_Pass_Down_Success_Rt_PY1_col4 = dense_rank(desc(off_passing_downs_success_rate_PY1)),
      Rank_Off_Pass_Down_Explosiveness_PY1_col4 = dense_rank(desc(off_passing_downs_explosiveness_PY1)),
      Rank_Off_Rush_Play_PPA_PY1_col4 = dense_rank(desc(off_rushing_plays_ppa_PY1)),
      Rank_Off_Rush_Play_Success_Rt_PY1_col4 = dense_rank(desc(off_rushing_plays_success_rate_PY1)),
      Rank_Off_Rush_Play_Explosiveness_PY1_col4 = dense_rank(desc(off_rushing_plays_explosiveness_PY1)),
      Rank_Off_Pass_Play_PPA_PY1_col4 = dense_rank(desc(off_passing_plays_ppa_PY1)),
      Rank_Off_Pass_Play_Success_Rt_PY1_col4 = dense_rank(desc(off_passing_plays_success_rate_PY1)),
      Rank_Off_Pass_Play_Explosiveness_PY1_col4 = dense_rank(desc(off_passing_plays_explosiveness_PY1)),
      Rank_Def_PPA_PY1_col4 = dense_rank(def_ppa_PY1),
      Rank_Def_Success_Rt_PY1_col4 = dense_rank(def_success_rate_PY1),
      Rank_Def_Explosiveness_PY1_col4 = dense_rank(def_explosiveness_PY1),
      Rank_Def_Pwr_Success_PY1_col4 = dense_rank(def_power_success_PY1),
      Rank_Def_Stuff_Rt_PY1_col4 = dense_rank(desc(def_stuff_rate_PY1)),
      Rank_Def_Pts_Per_Opp_PY1_col4 = dense_rank(def_pts_per_opp_PY1),
      Rank_Def_Havoc_Total_PY1_col4 = dense_rank(desc(def_havoc_total_PY1)),
      Rank_Def_Havoc_Front_Seven_PY1_col4 = dense_rank(desc(def_havoc_front_seven_PY1)),
      Rank_Def_Havoc_DB_PY1_col4 = dense_rank(desc(def_havoc_db_PY1)),
      Rank_Def_Standard_Down_PPA_PY1_col4 = dense_rank(def_standard_downs_ppa_PY1),
      Rank_Def_Standard_Down_Success_Rt_PY1_col4 = dense_rank(def_standard_downs_success_rate_PY1),
      Rank_Def_Standard_Down_Explosiveness_PY1_col4 = dense_rank(def_standard_downs_explosiveness_PY1),
      Rank_Def_Pass_Down_PPA_PY1_col4 = dense_rank(def_passing_downs_ppa_PY1),
      Rank_Def_Pass_Down_Success_Rt_PY1_col4 = dense_rank(def_passing_downs_success_rate_PY1),
      Rank_Def_Pass_Down_Explosiveness_PY1_col4 = dense_rank(def_passing_downs_explosiveness_PY1),
      Rank_Def_Rush_Play_PPA_PY1_col4 = dense_rank(def_rushing_plays_ppa_PY1),
      Rank_Def_Rush_Play_Success_Rt_PY1_col4 = dense_rank(def_rushing_plays_success_rate_PY1),
      Rank_Def_Rush_Play_Explosiveness_PY1_col4 = dense_rank(def_rushing_plays_explosiveness_PY1),
      Rank_Def_Pass_Play_PPA_PY1_col4 = dense_rank(def_passing_plays_ppa_PY1),
      Rank_Def_Pass_Play_Success_Rt_PY1_col4 = dense_rank(def_passing_plays_success_rate_PY1),
      Rank_Def_Pass_Play_Explosiveness_PY1_col4 = dense_rank(def_passing_plays_explosiveness_PY1),
      ## FPI_SP mean ranks added at the end, weighted once
      Rank_FPI_SP_SRS_PY2_mean = dense_rank(desc(FPI_SP_SRS_PY2_mean)),
      Rank_FPI_SP_SRS_PY1_mean = dense_rank(desc(FPI_SP_SRS_PY1_mean)),
      Rank_AllPY_FPI_SP_SRS_mean = dense_rank(desc(AllPY_FPI_SP_SRS_mean)),
      Rank_WeightedAllPY_FPI_SP_SRS_mean = dense_rank(desc(WeightedAllPY_FPI_SP_SRS_mean)),
      ## Ranking current stats
      Rank_Wins = dense_rank(desc(Wins)),
      Rank_Losses = dense_rank(Losses),
      Rank_Comp_Pct = dense_rank(desc(completion_pct)),
      Rank_Pass_YPA = dense_rank(desc(pass_ypa)),
      Rank_Pass_YPR = dense_rank(desc(pass_ypr)),
      Rank_Int_Pct = dense_rank(int_pct),
      Rank_Rush_YPC = dense_rank(desc(rush_ypc)),
      Rank_Turnovers_pg = dense_rank(turnovers_pg),
      Rank_Third_Conv_Rate = dense_rank(desc(third_conv_rate)),
      Rank_Fourth_Conv_Rate = dense_rank(desc(fourth_conv_rate)),
      Rank_Penalty_Yds_pg = dense_rank(penalty_yds_pg),
      Rank_Yds_Per_Penalty = dense_rank(yards_per_penalty),
      Rank_Kick_Return_Avg = dense_rank(desc(kick_return_avg)),
      Rank_Punt_Return_Avg = dense_rank(desc(punt_return_avg)),
      Rank_Total_Yds_pg = dense_rank(desc(total_yds_pg)),
      Rank_Pass_Yds_pg = dense_rank(desc(pass_yds_pg)),
      Rank_Rush_Yds_pg = dense_rank(desc(rush_yds_pg)),
      Rank_First_Downs_pg = dense_rank(desc(first_downs_pg)),
      Rank_Off_YPP = dense_rank(desc(off_ypp)),
      Rank_Def_Ints_pg = dense_rank(desc(def_interceptions_pg)),
      Rank_Off_PPA = dense_rank(desc(off_ppa)),
      Rank_Off_Success_Rt = dense_rank(desc(off_success_rate)),
      Rank_Off_Explosiveness = dense_rank(desc(off_explosiveness)),
      Rank_Off_Pwr_Success = dense_rank(desc(off_power_success)),
      Rank_Off_Stuff_Rt = dense_rank(off_stuff_rate),
      Rank_Off_Line_Yds = dense_rank(desc(off_line_yds)),
      Rank_Off_Second_Lvl_Yds = dense_rank(desc(off_second_lvl_yds)),
      Rank_Off_Open_Field_Yds = dense_rank(desc(off_open_field_yds)),
      Rank_Off_Pts_Per_Opp = dense_rank(desc(off_pts_per_opp)),
      Rank_Off_Field_Pos_Avg_Predicted_Pts = dense_rank(desc(off_field_pos_avg_predicted_points)),
      Rank_Off_Havoc_Total = dense_rank(off_havoc_total),
      Rank_Off_Havoc_Front = dense_rank(off_havoc_front_seven),
      Rank_Off_Havoc_DB = dense_rank(off_havoc_db),
      Rank_Off_Standard_Down_PPA = dense_rank(desc(off_standard_downs_ppa)),
      Rank_Off_Standard_Down_Success_Rt = dense_rank(desc(off_standard_downs_success_rate)),
      Rank_Off_Standard_Down_Explosiveness = dense_rank(desc(off_standard_downs_explosiveness)),
      Rank_Off_Pass_Down_PPA = dense_rank(desc(off_passing_downs_ppa)),
      Rank_Off_Pass_Down_Success_Rt = dense_rank(desc(off_passing_downs_success_rate)),
      Rank_Off_Pass_Down_Explosiveness = dense_rank(desc(off_passing_downs_explosiveness)),
      Rank_Off_Rush_Play_PPA = dense_rank(desc(off_rushing_plays_ppa)),
      Rank_Off_Rush_Play_Success_Rt = dense_rank(desc(off_rushing_plays_success_rate)),
      Rank_Off_Rush_Play_Explosiveness = dense_rank(desc(off_rushing_plays_explosiveness)),
      Rank_Off_Pass_Play_PPA = dense_rank(desc(off_passing_plays_ppa)),
      Rank_Off_Pass_Play_Success_Rt = dense_rank(desc(off_passing_plays_success_rate)),
      Rank_Off_Pass_Play_Explosiveness = dense_rank(desc(off_passing_plays_explosiveness)),
      Rank_Def_PPA = dense_rank(def_ppa),
      Rank_Def_Success_Rt = dense_rank(def_success_rate),
      Rank_Def_Explosiveness = dense_rank(def_explosiveness),
      Rank_Def_Pwr_Success = dense_rank(def_power_success),
      Rank_Def_Stuff_Rt = dense_rank(desc(def_stuff_rate)),
      Rank_Def_Line_Yds = dense_rank(def_line_yds),
      Rank_Def_Second_Lvl_Yds = dense_rank(def_second_lvl_yds),
      Rank_Def_Open_Field_Yds = dense_rank(def_open_field_yds),
      Rank_Def_Pts_Per_Opp = dense_rank(def_pts_per_opp),
      Rank_Def_Field_Pos_Avg_Predicted_Pts = dense_rank(def_field_pos_avg_predicted_points),
      Rank_Def_Havoc_Total = dense_rank(desc(def_havoc_total)),
      Rank_Def_Havoc_Front_Seven = dense_rank(desc(def_havoc_front_seven)),
      Rank_Def_Havoc_DB = dense_rank(desc(def_havoc_db)),
      Rank_Def_Standard_Down_PPA = dense_rank(def_standard_downs_ppa),
      Rank_Def_Standard_Down_Success_Rt = dense_rank(def_standard_downs_success_rate),
      Rank_Def_Standard_Down_Explosiveness = dense_rank(def_standard_downs_explosiveness),
      Rank_Def_Pass_Down_PPA = dense_rank(def_passing_downs_ppa),
      Rank_Def_Pass_Down_Success_Rt = dense_rank(def_passing_downs_success_rate),
      Rank_Def_Pass_Down_Explosiveness = dense_rank(def_passing_downs_explosiveness),
      Rank_Def_Rush_Play_PPA = dense_rank(def_rushing_plays_ppa),
      Rank_Def_Rush_Play_Success_Rt = dense_rank(def_rushing_plays_success_rate),
      Rank_Def_Rush_Play_Explosiveness = dense_rank(def_rushing_plays_explosiveness),
      Rank_Def_Pass_Play_PPA = dense_rank(def_passing_plays_ppa),
      Rank_Def_Pass_Play_Success_Rt = dense_rank(def_passing_plays_success_rate),
      Rank_Def_Pass_Play_Explosiveness = dense_rank(def_passing_plays_explosiveness),
      Rank_SP_Rating = dense_rank(desc(sp_rating)),
      Rank_SP_Off_Rating = dense_rank(desc(sp_offense_rating)),
      Rank_SP_Def_Rating = dense_rank(sp_defense_rating),
      Rank_SP_SpecialTeams_Rating = dense_rank(desc(sp_special_teams_rating)),
      Rank_FPI = dense_rank(desc(FPI)),
      Rank_SRS_rating = dense_rank(desc(SRS_rating)),
      Rank_FPI_SRS_mean = dense_rank(desc(FPI_SRS_mean)),
      Rank_FPI_SP_mean = dense_rank(desc(FPI_SP_mean)),
      # due to availability issues, SP_Rankings not always included with current data
      ## Current stats weighted 2x
      Rank_Wins_col2 = dense_rank(desc(Wins)),
      Rank_Losses_col2 = dense_rank(Losses),
      Rank_Comp_Pct_col2 = dense_rank(desc(completion_pct)),
      Rank_Pass_YPA_col2 = dense_rank(desc(pass_ypa)),
      Rank_Pass_YPR_col2 = dense_rank(desc(pass_ypr)),
      Rank_Int_Pct_col2 = dense_rank(int_pct),
      Rank_Rush_YPC_col2 = dense_rank(desc(rush_ypc)),
      Rank_Turnovers_pg_col2 = dense_rank(turnovers_pg),
      Rank_Third_Conv_Rate_col2 = dense_rank(desc(third_conv_rate)),
      Rank_Fourth_Conv_Rate_col2 = dense_rank(desc(fourth_conv_rate)),
      Rank_Penalty_Yds_pg_col2 = dense_rank(penalty_yds_pg),
      Rank_Yds_Per_Penalty_col2 = dense_rank(yards_per_penalty),
      Rank_Kick_Return_Avg_col2 = dense_rank(desc(kick_return_avg)),
      Rank_Punt_Return_Avg_col2 = dense_rank(desc(punt_return_avg)),
      Rank_Total_Yds_pg_col2 = dense_rank(desc(total_yds_pg)),
      Rank_Pass_Yds_pg_col2 = dense_rank(desc(pass_yds_pg)),
      Rank_Rush_Yds_pg_col2 = dense_rank(desc(rush_yds_pg)),
      Rank_First_Downs_pg_col2 = dense_rank(desc(first_downs_pg)),
      Rank_Off_YPP_col2 = dense_rank(desc(off_ypp)),
      Rank_Def_Ints_pg_col2 = dense_rank(desc(def_interceptions_pg)),
      Rank_Off_PPA_col2 = dense_rank(desc(off_ppa)),
      Rank_Off_Success_Rt_col2 = dense_rank(desc(off_success_rate)),
      Rank_Off_Explosiveness_col2 = dense_rank(desc(off_explosiveness)),
      Rank_Off_Pwr_Success_col2 = dense_rank(desc(off_power_success)),
      Rank_Off_Stuff_Rt_col2 = dense_rank(off_stuff_rate),
      Rank_Off_Line_Yds_col2 = dense_rank(desc(off_line_yds)),
      Rank_Off_Second_Lvl_Yds_col2 = dense_rank(desc(off_second_lvl_yds)),
      Rank_Off_Open_Field_Yds_col2 = dense_rank(desc(off_open_field_yds)),
      Rank_Off_Pts_Per_Opp_col2 = dense_rank(desc(off_pts_per_opp)),
      Rank_Off_Field_Pos_Avg_Predicted_Pts_col2 = dense_rank(desc(off_field_pos_avg_predicted_points)),
      Rank_Off_Havoc_Total_col2 = dense_rank(off_havoc_total),
      Rank_Off_Havoc_Front_col2 = dense_rank(off_havoc_front_seven),
      Rank_Off_Havoc_DB_col2 = dense_rank(off_havoc_db),
      Rank_Off_Standard_Down_PPA_col2 = dense_rank(desc(off_standard_downs_ppa)),
      Rank_Off_Standard_Down_Success_Rt_col2 = dense_rank(desc(off_standard_downs_success_rate)),
      Rank_Off_Standard_Down_Explosiveness_col2 = dense_rank(desc(off_standard_downs_explosiveness)),
      Rank_Off_Pass_Down_PPA_col2 = dense_rank(desc(off_passing_downs_ppa)),
      Rank_Off_Pass_Down_Success_Rt_col2 = dense_rank(desc(off_passing_downs_success_rate)),
      Rank_Off_Pass_Down_Explosiveness_col2 = dense_rank(desc(off_passing_downs_explosiveness)),
      Rank_Off_Rush_Play_PPA_col2 = dense_rank(desc(off_rushing_plays_ppa)),
      Rank_Off_Rush_Play_Success_Rt_col2 = dense_rank(desc(off_rushing_plays_success_rate)),
      Rank_Off_Rush_Play_Explosiveness_col2 = dense_rank(desc(off_rushing_plays_explosiveness)),
      Rank_Off_Pass_Play_PPA_col2 = dense_rank(desc(off_passing_plays_ppa)),
      Rank_Off_Pass_Play_Success_Rt_col2 = dense_rank(desc(off_passing_plays_success_rate)),
      Rank_Off_Pass_Play_Explosiveness_col2 = dense_rank(desc(off_passing_plays_explosiveness)),
      Rank_Def_PPA_col2 = dense_rank(def_ppa),
      Rank_Def_Success_Rt_col2 = dense_rank(def_success_rate),
      Rank_Def_Explosiveness_col2 = dense_rank(def_explosiveness),
      Rank_Def_Pwr_Success_col2 = dense_rank(def_power_success),
      Rank_Def_Stuff_Rt_col2 = dense_rank(desc(def_stuff_rate)),
      Rank_Def_Line_Yds_col2 = dense_rank(def_line_yds),
      Rank_Def_Second_Lvl_Yds_col2 = dense_rank(def_second_lvl_yds),
      Rank_Def_Open_Field_Yds_col2 = dense_rank(def_open_field_yds),
      Rank_Def_Pts_Per_Opp_col2 = dense_rank(def_pts_per_opp),
      Rank_Def_Field_Pos_Avg_Predicted_Pts_col2 = dense_rank(def_field_pos_avg_predicted_points),
      Rank_Def_Havoc_Total_col2 = dense_rank(desc(def_havoc_total)),
      Rank_Def_Havoc_Front_Seven_col2 = dense_rank(desc(def_havoc_front_seven)),
      Rank_Def_Havoc_DB_col2 = dense_rank(desc(def_havoc_db)),
      Rank_Def_Standard_Down_PPA_col2 = dense_rank(def_standard_downs_ppa),
      Rank_Def_Standard_Down_Success_Rt_col2 = dense_rank(def_standard_downs_success_rate),
      Rank_Def_Standard_Down_Explosiveness_col2 = dense_rank(def_standard_downs_explosiveness),
      Rank_Def_Pass_Down_PPA_col2 = dense_rank(def_passing_downs_ppa),
      Rank_Def_Pass_Down_Success_Rt_col2 = dense_rank(def_passing_downs_success_rate),
      Rank_Def_Pass_Down_Explosiveness_col2 = dense_rank(def_passing_downs_explosiveness),
      Rank_Def_Rush_Play_PPA_col2 = dense_rank(def_rushing_plays_ppa),
      Rank_Def_Rush_Play_Success_Rt_col2 = dense_rank(def_rushing_plays_success_rate),
      Rank_Def_Rush_Play_Explosiveness_col2 = dense_rank(def_rushing_plays_explosiveness),
      Rank_Def_Pass_Play_PPA_col2 = dense_rank(def_passing_plays_ppa),
      Rank_Def_Pass_Play_Success_Rt_col2 = dense_rank(def_passing_plays_success_rate),
      Rank_Def_Pass_Play_Explosiveness_col2 = dense_rank(def_passing_plays_explosiveness),
      ## Extra weighted variables for current year (weighted 2x)
      Rank_Wins_col3 = dense_rank(desc(Wins)),
      Rank_Losses_col3 = dense_rank(Losses),
      Rank_Off_YPP_col3 = dense_rank(desc(off_ypp)),
      Rank_Off_PPA_col3 = dense_rank(desc(off_ppa)),
      Rank_Off_Success_Rt_col3 = dense_rank(desc(off_success_rate)),
      Rank_Off_Explosiveness_col3 = dense_rank(desc(off_explosiveness)),
      Rank_Off_Pwr_Success_col3 = dense_rank(desc(off_power_success)),
      Rank_Off_Stuff_Rt_col3 = dense_rank(off_stuff_rate),
      Rank_Off_Pts_Per_Opp_col3 = dense_rank(desc(off_pts_per_opp)),
      Rank_Off_Havoc_Total_col3 = dense_rank(off_havoc_total),
      Rank_Off_Havoc_Front_col3 = dense_rank(off_havoc_front_seven),
      Rank_Off_Havoc_DB_col3 = dense_rank(off_havoc_db),
      Rank_Off_Standard_Down_PPA_col3 = dense_rank(desc(off_standard_downs_ppa)),
      Rank_Off_Standard_Down_Success_Rt_col3 = dense_rank(desc(off_standard_downs_success_rate)),
      Rank_Off_Standard_Down_Explosiveness_col3 = dense_rank(desc(off_standard_downs_explosiveness)),
      Rank_Off_Pass_Down_PPA_col3 = dense_rank(desc(off_passing_downs_ppa)),
      Rank_Off_Pass_Down_Success_Rt_col3 = dense_rank(desc(off_passing_downs_success_rate)),
      Rank_Off_Pass_Down_Explosiveness_col3 = dense_rank(desc(off_passing_downs_explosiveness)),
      Rank_Off_Rush_Play_PPA_col3 = dense_rank(desc(off_rushing_plays_ppa)),
      Rank_Off_Rush_Play_Success_Rt_col3 = dense_rank(desc(off_rushing_plays_success_rate)),
      Rank_Off_Rush_Play_Explosiveness_col3 = dense_rank(desc(off_rushing_plays_explosiveness)),
      Rank_Off_Pass_Play_PPA_col3 = dense_rank(desc(off_passing_plays_ppa)),
      Rank_Off_Pass_Play_Success_Rt_col3 = dense_rank(desc(off_passing_plays_success_rate)),
      Rank_Off_Pass_Play_Explosiveness_col3 = dense_rank(desc(off_passing_plays_explosiveness)),
      Rank_Def_PPA_col3 = dense_rank(def_ppa),
      Rank_Def_Success_Rt_col3 = dense_rank(def_success_rate),
      Rank_Def_Explosiveness_col3 = dense_rank(def_explosiveness),
      Rank_Def_Pwr_Success_col3 = dense_rank(def_power_success),
      Rank_Def_Stuff_Rt_col3 = dense_rank(desc(def_stuff_rate)),
      Rank_Def_Pts_Per_Opp_col3 = dense_rank(def_pts_per_opp),
      Rank_Def_Havoc_Total_col3 = dense_rank(desc(def_havoc_total)),
      Rank_Def_Havoc_Front_Seven_col3 = dense_rank(desc(def_havoc_front_seven)),
      Rank_Def_Havoc_DB_col3 = dense_rank(desc(def_havoc_db)),
      Rank_Def_Standard_Down_PPA_col3 = dense_rank(def_standard_downs_ppa),
      Rank_Def_Standard_Down_Success_Rt_col3 = dense_rank(def_standard_downs_success_rate),
      Rank_Def_Standard_Down_Explosiveness_col3 = dense_rank(def_standard_downs_explosiveness),
      Rank_Def_Pass_Down_PPA_col3 = dense_rank(def_passing_downs_ppa),
      Rank_Def_Pass_Down_Success_Rt_col3 = dense_rank(def_passing_downs_success_rate),
      Rank_Def_Pass_Down_Explosiveness_col3 = dense_rank(def_passing_downs_explosiveness),
      Rank_Def_Rush_Play_PPA_col3 = dense_rank(def_rushing_plays_ppa),
      Rank_Def_Rush_Play_Success_Rt_col3 = dense_rank(def_rushing_plays_success_rate),
      Rank_Def_Rush_Play_Explosiveness_col3 = dense_rank(def_rushing_plays_explosiveness),
      Rank_Def_Pass_Play_PPA_col3 = dense_rank(def_passing_plays_ppa),
      Rank_Def_Pass_Play_Success_Rt_col3 = dense_rank(def_passing_plays_success_rate),
      Rank_Def_Pass_Play_Explosiveness_col3 = dense_rank(def_passing_plays_explosiveness),
      ## Extra weighted variables for current year (weighted 2x)
      Rank_Wins_col4 = dense_rank(desc(Wins)),
      Rank_Losses_col4 = dense_rank(Losses),
      Rank_Off_YPP_col4 = dense_rank(desc(off_ypp)),
      Rank_Off_PPA_col4 = dense_rank(desc(off_ppa)),
      Rank_Off_Success_Rt_col4 = dense_rank(desc(off_success_rate)),
      Rank_Off_Explosiveness_col4 = dense_rank(desc(off_explosiveness)),
      Rank_Off_Pwr_Success_col4 = dense_rank(desc(off_power_success)),
      Rank_Off_Stuff_Rt_col4 = dense_rank(off_stuff_rate),
      Rank_Off_Pts_Per_Opp_col4 = dense_rank(desc(off_pts_per_opp)),
      Rank_Off_Havoc_Total_col4 = dense_rank(off_havoc_total),
      Rank_Off_Havoc_Front_col4 = dense_rank(off_havoc_front_seven),
      Rank_Off_Havoc_DB_col4 = dense_rank(off_havoc_db),
      Rank_Off_Standard_Down_PPA_col4 = dense_rank(desc(off_standard_downs_ppa)),
      Rank_Off_Standard_Down_Success_Rt_col4 = dense_rank(desc(off_standard_downs_success_rate)),
      Rank_Off_Standard_Down_Explosiveness_col4 = dense_rank(desc(off_standard_downs_explosiveness)),
      Rank_Off_Pass_Down_PPA_col4 = dense_rank(desc(off_passing_downs_ppa)),
      Rank_Off_Pass_Down_Success_Rt_col4 = dense_rank(desc(off_passing_downs_success_rate)),
      Rank_Off_Pass_Down_Explosiveness_col4 = dense_rank(desc(off_passing_downs_explosiveness)),
      Rank_Off_Rush_Play_PPA_col4 = dense_rank(desc(off_rushing_plays_ppa)),
      Rank_Off_Rush_Play_Success_Rt_col4 = dense_rank(desc(off_rushing_plays_success_rate)),
      Rank_Off_Rush_Play_Explosiveness_col4 = dense_rank(desc(off_rushing_plays_explosiveness)),
      Rank_Off_Pass_Play_PPA_col4 = dense_rank(desc(off_passing_plays_ppa)),
      Rank_Off_Pass_Play_Success_Rt_col4 = dense_rank(desc(off_passing_plays_success_rate)),
      Rank_Off_Pass_Play_Explosiveness_col4 = dense_rank(desc(off_passing_plays_explosiveness)),
      Rank_Def_PPA_col4 = dense_rank(def_ppa),
      Rank_Def_Success_Rt_col4 = dense_rank(def_success_rate),
      Rank_Def_Explosiveness_col4 = dense_rank(def_explosiveness),
      Rank_Def_Pwr_Success_col4 = dense_rank(def_power_success),
      Rank_Def_Stuff_Rt_col4 = dense_rank(desc(def_stuff_rate)),
      Rank_Def_Pts_Per_Opp_col4 = dense_rank(def_pts_per_opp),
      Rank_Def_Havoc_Total_col4 = dense_rank(desc(def_havoc_total)),
      Rank_Def_Havoc_Front_Seven_col4 = dense_rank(desc(def_havoc_front_seven)),
      Rank_Def_Havoc_DB_col4 = dense_rank(desc(def_havoc_db)),
      Rank_Def_Standard_Down_PPA_col4 = dense_rank(def_standard_downs_ppa),
      Rank_Def_Standard_Down_Success_Rt_col4 = dense_rank(def_standard_downs_success_rate),
      Rank_Def_Standard_Down_Explosiveness_col4 = dense_rank(def_standard_downs_explosiveness),
      Rank_Def_Pass_Down_PPA_col4 = dense_rank(def_passing_downs_ppa),
      Rank_Def_Pass_Down_Success_Rt_col4 = dense_rank(def_passing_downs_success_rate),
      Rank_Def_Pass_Down_Explosiveness_col4 = dense_rank(def_passing_downs_explosiveness),
      Rank_Def_Rush_Play_PPA_col4 = dense_rank(def_rushing_plays_ppa),
      Rank_Def_Rush_Play_Success_Rt_col4 = dense_rank(def_rushing_plays_success_rate),
      Rank_Def_Rush_Play_Explosiveness_col4 = dense_rank(def_rushing_plays_explosiveness),
      Rank_Def_Pass_Play_PPA_col4 = dense_rank(def_passing_plays_ppa),
      Rank_Def_Pass_Play_Success_Rt_col4 = dense_rank(def_passing_plays_success_rate),
      Rank_Def_Pass_Play_Explosiveness_col4 = dense_rank(def_passing_plays_explosiveness))
} else if (as.numeric(week) <= 6) {
  ##### Weeks 5-6 Variable Ranks #####
  # PY1 weighted 1x, current weighted 2x
  VoA_Variables <- VoA_Variables |>
    ## PY1 ranks
    mutate(Rank_Wins_PY1 = dense_rank(desc(Wins_PY1)),
      Rank_Losses_PY1 = dense_rank(Losses_PY1),
      Rank_Comp_Pct_PY1 = dense_rank(desc(completion_pct_PY1)),
      Rank_Pass_YPA_PY1 = dense_rank(desc(pass_ypa_PY1)),
      Rank_Pass_YPR_PY1 = dense_rank(desc(pass_ypr_PY1)),
      Rank_Int_Pct_PY1 = dense_rank(int_pct_PY1),
      Rank_Rush_YPC_PY1 = dense_rank(desc(rush_ypc_PY1)),
      Rank_Turnovers_pg_PY1 = dense_rank(turnovers_pg_PY1),
      Rank_Third_Conv_Rate_PY1 = dense_rank(desc(third_conv_rate_PY1)),
      Rank_Fourth_Conv_Rate_PY1 = dense_rank(desc(fourth_conv_rate_PY1)),
      Rank_Penalty_Yds_pg_PY1 = dense_rank(penalty_yds_pg_PY1),
      Rank_Yds_Per_Penalty_PY1 = dense_rank(yards_per_penalty_PY1),
      Rank_Kick_Return_Avg_PY1 = dense_rank(desc(kick_return_avg_PY1)),
      Rank_Punt_Return_Avg_PY1 = dense_rank(desc(punt_return_avg_PY1)),
      Rank_Total_Yds_pg_PY1 = dense_rank(desc(total_yds_pg_PY1)),
      Rank_Pass_Yds_pg_PY1 = dense_rank(desc(pass_yds_pg_PY1)),
      Rank_Rush_Yds_pg_PY1 = dense_rank(desc(rush_yds_pg_PY1)),
      Rank_First_Downs_pg_PY1 = dense_rank(desc(first_downs_pg_PY1)),
      Rank_Off_YPP_PY1 = dense_rank(desc(off_ypp_PY1)),
      Rank_Def_Ints_pg_PY1 = dense_rank(desc(def_interceptions_pg_PY1)),
      Rank_Off_PPA_PY1 = dense_rank(desc(off_ppa_PY1)),
      Rank_Off_Success_Rt_PY1 = dense_rank(desc(off_success_rate_PY1)),
      Rank_Off_Explosiveness_PY1 = dense_rank(desc(off_explosiveness_PY1)),
      Rank_Off_Pwr_Success_PY1 = dense_rank(desc(off_power_success_PY1)),
      Rank_Off_Stuff_Rt_PY1 = dense_rank(off_stuff_rate_PY1),
      Rank_Off_Line_Yds_PY1 = dense_rank(desc(off_line_yds_PY1)),
      Rank_Off_Second_Lvl_Yds_PY1 = dense_rank(desc(off_second_lvl_yds_PY1)),
      Rank_Off_Open_Field_Yds_PY1 = dense_rank(desc(off_open_field_yds_PY1)),
      Rank_Off_Pts_Per_Opp_PY1 = dense_rank(desc(off_pts_per_opp_PY1)),
      Rank_Off_Field_Pos_Avg_Predicted_Pts_PY1 = dense_rank(desc(off_field_pos_avg_predicted_points_PY1)),
      Rank_Off_Havoc_Total_PY1 = dense_rank(off_havoc_total_PY1),
      Rank_Off_Havoc_Front_PY1 = dense_rank(off_havoc_front_seven_PY1),
      Rank_Off_Havoc_DB_PY1 = dense_rank(off_havoc_db_PY1),
      Rank_Off_Standard_Down_PPA_PY1 = dense_rank(desc(off_standard_downs_ppa_PY1)),
      Rank_Off_Standard_Down_Success_Rt_PY1 = dense_rank(desc(off_standard_downs_success_rate_PY1)),
      Rank_Off_Standard_Down_Explosiveness_PY1 = dense_rank(desc(off_standard_downs_explosiveness_PY1)),
      Rank_Off_Pass_Down_PPA_PY1 = dense_rank(desc(off_passing_downs_ppa_PY1)),
      Rank_Off_Pass_Down_Success_Rt_PY1 = dense_rank(desc(off_passing_downs_success_rate_PY1)),
      Rank_Off_Pass_Down_Explosiveness_PY1 = dense_rank(desc(off_passing_downs_explosiveness_PY1)),
      Rank_Off_Rush_Play_PPA_PY1 = dense_rank(desc(off_rushing_plays_ppa_PY1)),
      Rank_Off_Rush_Play_Success_Rt_PY1 = dense_rank(desc(off_rushing_plays_success_rate_PY1)),
      Rank_Off_Rush_Play_Explosiveness_PY1 = dense_rank(desc(off_rushing_plays_explosiveness_PY1)),
      Rank_Off_Pass_Play_PPA_PY1 = dense_rank(desc(off_passing_plays_ppa_PY1)),
      Rank_Off_Pass_Play_Success_Rt_PY1 = dense_rank(desc(off_passing_plays_success_rate_PY1)),
      Rank_Off_Pass_Play_Explosiveness_PY1 = dense_rank(desc(off_passing_plays_explosiveness_PY1)),
      Rank_Def_PPA_PY1 = dense_rank(def_ppa_PY1),
      Rank_Def_Success_Rt_PY1 = dense_rank(def_success_rate_PY1),
      Rank_Def_Explosiveness_PY1 = dense_rank(def_explosiveness_PY1),
      Rank_Def_Pwr_Success_PY1 = dense_rank(def_power_success_PY1),
      Rank_Def_Stuff_Rt_PY1 = dense_rank(desc(def_stuff_rate_PY1)),
      Rank_Def_Line_Yds_PY1 = dense_rank(def_line_yds_PY1),
      Rank_Def_Second_Lvl_Yds_PY1 = dense_rank(def_second_lvl_yds_PY1),
      Rank_Def_Open_Field_Yds_PY1 = dense_rank(def_open_field_yds_PY1),
      Rank_Def_Pts_Per_Opp_PY1 = dense_rank(def_pts_per_opp_PY1),
      Rank_Def_Field_Pos_Avg_Predicted_Pts_PY1 = dense_rank(def_field_pos_avg_predicted_points_PY1),
      Rank_Def_Havoc_Total_PY1 = dense_rank(desc(def_havoc_total_PY1)),
      Rank_Def_Havoc_Front_Seven_PY1 = dense_rank(desc(def_havoc_front_seven_PY1)),
      Rank_Def_Havoc_DB_PY1 = dense_rank(desc(def_havoc_db_PY1)),
      Rank_Def_Standard_Down_PPA_PY1 = dense_rank(def_standard_downs_ppa_PY1),
      Rank_Def_Standard_Down_Success_Rt_PY1 = dense_rank(def_standard_downs_success_rate_PY1),
      Rank_Def_Standard_Down_Explosiveness_PY1 = dense_rank(def_standard_downs_explosiveness_PY1),
      Rank_Def_Pass_Down_PPA_PY1 = dense_rank(def_passing_downs_ppa_PY1),
      Rank_Def_Pass_Down_Success_Rt_PY1 = dense_rank(def_passing_downs_success_rate_PY1),
      Rank_Def_Pass_Down_Explosiveness_PY1 = dense_rank(def_passing_downs_explosiveness_PY1),
      Rank_Def_Rush_Play_PPA_PY1 = dense_rank(def_rushing_plays_ppa_PY1),
      Rank_Def_Rush_Play_Success_Rt_PY1 = dense_rank(def_rushing_plays_success_rate_PY1),
      Rank_Def_Rush_Play_Explosiveness_PY1 = dense_rank(def_rushing_plays_explosiveness_PY1),
      Rank_Def_Pass_Play_PPA_PY1 = dense_rank(def_passing_plays_ppa_PY1),
      Rank_Def_Pass_Play_Success_Rt_PY1 = dense_rank(def_passing_plays_success_rate_PY1),
      Rank_Def_Pass_Play_Explosiveness_PY1 = dense_rank(def_passing_plays_explosiveness_PY1),
      Rank_Recruit_Pts_PY1 = dense_rank(desc(recruit_pts_PY1)),
      Rank_Talent_PY1 = dense_rank(desc(talent_PY1)),
      Rank_SP_Rating_PY1 = dense_rank(desc(sp_rating_PY1)),
      Rank_SP_Off_Rating_PY1 = dense_rank(desc(sp_offense_rating_PY1)),
      Rank_SP_Def_Rating_PY1 = dense_rank(sp_defense_rating_PY1),
      Rank_SP_SpecialTeams_Rating_PY1 = dense_rank(desc(sp_special_teams_rating_PY1)),
      Rank_FPI_PY1 = dense_rank(desc(FPI_PY1)),
      Rank_SRS_rating_PY1 = dense_rank(desc(SRS_rating_PY1)),
      ## incoming recruiting class, weighted once
      ## recruiting for PY2 and PY3 also included
      Rank_Recruit_Pts = dense_rank(desc(recruit_pts)),
      Rank_Recruit_Pts_PY2 = dense_rank(desc(recruit_pts_PY2)),
      Rank_Recruit_Pts_PY2_col2 = dense_rank(desc(recruit_pts_PY2)),
      Rank_Recruit_Pts_PY3 = dense_rank(desc(recruit_pts_PY3)),
      Rank_Recruit_Pts_PY3_col2 = dense_rank(desc(recruit_pts_PY3)),
      Rank_Recruit_Pts_PY3_col3 = dense_rank(desc(recruit_pts_PY3)),
      ## Extra weighted variables, weighted 2x (2 more times)
      Rank_Off_YPP_PY1_col2 = dense_rank(desc(off_ypp_PY1)),
      Rank_Off_PPA_PY1_col2 = dense_rank(desc(off_ppa_PY1)),
      Rank_Off_Success_Rt_PY1_col2 = dense_rank(desc(off_success_rate_PY1)),
      Rank_Off_Explosiveness_PY1_col2 = dense_rank(desc(off_explosiveness_PY1)),
      Rank_Off_Pwr_Success_PY1_col2 = dense_rank(desc(off_power_success_PY1)),
      Rank_Off_Stuff_Rt_PY1_col2 = dense_rank(off_stuff_rate_PY1),
      Rank_Off_Pts_Per_Opp_PY1_col2 = dense_rank(desc(off_pts_per_opp_PY1)),
      Rank_Off_Havoc_Total_PY1_col2 = dense_rank(off_havoc_total_PY1),
      Rank_Off_Havoc_Front_PY1_col2 = dense_rank(off_havoc_front_seven_PY1),
      Rank_Off_Havoc_DB_PY1_col2 = dense_rank(off_havoc_db_PY1),
      Rank_Off_Standard_Down_PPA_PY1_col2 = dense_rank(desc(off_standard_downs_ppa_PY1)),
      Rank_Off_Standard_Down_Success_Rt_PY1_col2 = dense_rank(desc(off_standard_downs_success_rate_PY1)),
      Rank_Off_Standard_Down_Explosiveness_PY1_col2 = dense_rank(desc(off_standard_downs_explosiveness_PY1)),
      Rank_Off_Pass_Down_PPA_PY1_col2 = dense_rank(desc(off_passing_downs_ppa_PY1)),
      Rank_Off_Pass_Down_Success_Rt_PY1_col2 = dense_rank(desc(off_passing_downs_success_rate_PY1)),
      Rank_Off_Pass_Down_Explosiveness_PY1_col2 = dense_rank(desc(off_passing_downs_explosiveness_PY1)),
      Rank_Off_Rush_Play_PPA_PY1_col2 = dense_rank(desc(off_rushing_plays_ppa_PY1)),
      Rank_Off_Rush_Play_Success_Rt_PY1_col2 = dense_rank(desc(off_rushing_plays_success_rate_PY1)),
      Rank_Off_Rush_Play_Explosiveness_PY1_col2 = dense_rank(desc(off_rushing_plays_explosiveness_PY1)),
      Rank_Off_Pass_Play_PPA_PY1_col2 = dense_rank(desc(off_passing_plays_ppa_PY1)),
      Rank_Off_Pass_Play_Success_Rt_PY1_col2 = dense_rank(desc(off_passing_plays_success_rate_PY1)),
      Rank_Off_Pass_Play_Explosiveness_PY1_col2 = dense_rank(desc(off_passing_plays_explosiveness_PY1)),
      Rank_Def_PPA_PY1_col2 = dense_rank(def_ppa_PY1),
      Rank_Def_Success_Rt_PY1_col2 = dense_rank(def_success_rate_PY1),
      Rank_Def_Explosiveness_PY1_col2 = dense_rank(def_explosiveness_PY1),
      Rank_Def_Pwr_Success_PY1_col2 = dense_rank(def_power_success_PY1),
      Rank_Def_Stuff_Rt_PY1_col2 = dense_rank(desc(def_stuff_rate_PY1)),
      Rank_Def_Pts_Per_Opp_PY1_col2 = dense_rank(def_pts_per_opp_PY1),
      Rank_Def_Havoc_Total_PY1_col2 = dense_rank(desc(def_havoc_total_PY1)),
      Rank_Def_Havoc_Front_Seven_PY1_col2 = dense_rank(desc(def_havoc_front_seven_PY1)),
      Rank_Def_Havoc_DB_PY1_col2 = dense_rank(desc(def_havoc_db_PY1)),
      Rank_Def_Standard_Down_PPA_PY1_col2 = dense_rank(def_standard_downs_ppa_PY1),
      Rank_Def_Standard_Down_Success_Rt_PY1_col2 = dense_rank(def_standard_downs_success_rate_PY1),
      Rank_Def_Standard_Down_Explosiveness_PY1_col2 = dense_rank(def_standard_downs_explosiveness_PY1),
      Rank_Def_Pass_Down_PPA_PY1_col2 = dense_rank(def_passing_downs_ppa_PY1),
      Rank_Def_Pass_Down_Success_Rt_PY1_col2 = dense_rank(def_passing_downs_success_rate_PY1),
      Rank_Def_Pass_Down_Explosiveness_PY1_col2 = dense_rank(def_passing_downs_explosiveness_PY1),
      Rank_Def_Rush_Play_PPA_PY1_col2 = dense_rank(def_rushing_plays_ppa_PY1),
      Rank_Def_Rush_Play_Success_Rt_PY1_col2 = dense_rank(def_rushing_plays_success_rate_PY1),
      Rank_Def_Rush_Play_Explosiveness_PY1_col2 = dense_rank(def_rushing_plays_explosiveness_PY1),
      Rank_Def_Pass_Play_PPA_PY1_col2 = dense_rank(def_passing_plays_ppa_PY1),
      Rank_Def_Pass_Play_Success_Rt_PY1_col2 = dense_rank(def_passing_plays_success_rate_PY1),
      Rank_Def_Pass_Play_Explosiveness_PY1_col2 = dense_rank(def_passing_plays_explosiveness_PY1),
      ## FPI_SP_SRS mean ranks added at the end, weighted once
      Rank_FPI_SP_SRS_PY1_mean = dense_rank(desc(FPI_SP_SRS_PY1_mean)),
      ## Ranking current stats
      Rank_Wins = dense_rank(desc(Wins)),
      Rank_Losses = dense_rank(Losses),
      Rank_Comp_Pct = dense_rank(desc(completion_pct)),
      Rank_Pass_YPA = dense_rank(desc(pass_ypa)),
      Rank_Pass_YPR = dense_rank(desc(pass_ypr)),
      Rank_Int_Pct = dense_rank(int_pct),
      Rank_Rush_YPC = dense_rank(desc(rush_ypc)),
      Rank_Turnovers_pg = dense_rank(turnovers_pg),
      Rank_Third_Conv_Rate = dense_rank(desc(third_conv_rate)),
      Rank_Fourth_Conv_Rate = dense_rank(desc(fourth_conv_rate)),
      Rank_Penalty_Yds_pg = dense_rank(penalty_yds_pg),
      Rank_Yds_Per_Penalty = dense_rank(yards_per_penalty),
      Rank_Kick_Return_Avg = dense_rank(desc(kick_return_avg)),
      Rank_Punt_Return_Avg = dense_rank(desc(punt_return_avg)),
      Rank_Total_Yds_pg = dense_rank(desc(total_yds_pg)),
      Rank_Pass_Yds_pg = dense_rank(desc(pass_yds_pg)),
      Rank_Rush_Yds_pg = dense_rank(desc(rush_yds_pg)),
      Rank_First_Downs_pg = dense_rank(desc(first_downs_pg)),
      Rank_Off_YPP = dense_rank(desc(off_ypp)),
      Rank_Def_Ints_pg = dense_rank(desc(def_interceptions_pg)),
      Rank_Off_PPA = dense_rank(desc(off_ppa)),
      Rank_Off_Success_Rt = dense_rank(desc(off_success_rate)),
      Rank_Off_Explosiveness = dense_rank(desc(off_explosiveness)),
      Rank_Off_Pwr_Success = dense_rank(desc(off_power_success)),
      Rank_Off_Stuff_Rt = dense_rank(off_stuff_rate),
      Rank_Off_Line_Yds = dense_rank(desc(off_line_yds)),
      Rank_Off_Second_Lvl_Yds = dense_rank(desc(off_second_lvl_yds)),
      Rank_Off_Open_Field_Yds = dense_rank(desc(off_open_field_yds)),
      Rank_Off_Pts_Per_Opp = dense_rank(desc(off_pts_per_opp)),
      Rank_Off_Field_Pos_Avg_Predicted_Pts = dense_rank(desc(off_field_pos_avg_predicted_points)),
      Rank_Off_Havoc_Total = dense_rank(off_havoc_total),
      Rank_Off_Havoc_Front = dense_rank(off_havoc_front_seven),
      Rank_Off_Havoc_DB = dense_rank(off_havoc_db),
      Rank_Off_Standard_Down_PPA = dense_rank(desc(off_standard_downs_ppa)),
      Rank_Off_Standard_Down_Success_Rt = dense_rank(desc(off_standard_downs_success_rate)),
      Rank_Off_Standard_Down_Explosiveness = dense_rank(desc(off_standard_downs_explosiveness)),
      Rank_Off_Pass_Down_PPA = dense_rank(desc(off_passing_downs_ppa)),
      Rank_Off_Pass_Down_Success_Rt = dense_rank(desc(off_passing_downs_success_rate)),
      Rank_Off_Pass_Down_Explosiveness = dense_rank(desc(off_passing_downs_explosiveness)),
      Rank_Off_Rush_Play_PPA = dense_rank(desc(off_rushing_plays_ppa)),
      Rank_Off_Rush_Play_Success_Rt = dense_rank(desc(off_rushing_plays_success_rate)),
      Rank_Off_Rush_Play_Explosiveness = dense_rank(desc(off_rushing_plays_explosiveness)),
      Rank_Off_Pass_Play_PPA = dense_rank(desc(off_passing_plays_ppa)),
      Rank_Off_Pass_Play_Success_Rt = dense_rank(desc(off_passing_plays_success_rate)),
      Rank_Off_Pass_Play_Explosiveness = dense_rank(desc(off_passing_plays_explosiveness)),
      Rank_Def_PPA = dense_rank(def_ppa),
      Rank_Def_Success_Rt = dense_rank(def_success_rate),
      Rank_Def_Explosiveness = dense_rank(def_explosiveness),
      Rank_Def_Pwr_Success = dense_rank(def_power_success),
      Rank_Def_Stuff_Rt = dense_rank(desc(def_stuff_rate)),
      Rank_Def_Line_Yds = dense_rank(def_line_yds),
      Rank_Def_Second_Lvl_Yds = dense_rank(def_second_lvl_yds),
      Rank_Def_Open_Field_Yds = dense_rank(def_open_field_yds),
      Rank_Def_Pts_Per_Opp = dense_rank(def_pts_per_opp),
      Rank_Def_Field_Pos_Avg_Predicted_Pts = dense_rank(def_field_pos_avg_predicted_points),
      Rank_Def_Havoc_Total = dense_rank(desc(def_havoc_total)),
      Rank_Def_Havoc_Front_Seven = dense_rank(desc(def_havoc_front_seven)),
      Rank_Def_Havoc_DB = dense_rank(desc(def_havoc_db)),
      Rank_Def_Standard_Down_PPA = dense_rank(def_standard_downs_ppa),
      Rank_Def_Standard_Down_Success_Rt = dense_rank(def_standard_downs_success_rate),
      Rank_Def_Standard_Down_Explosiveness = dense_rank(def_standard_downs_explosiveness),
      Rank_Def_Pass_Down_PPA = dense_rank(def_passing_downs_ppa),
      Rank_Def_Pass_Down_Success_Rt = dense_rank(def_passing_downs_success_rate),
      Rank_Def_Pass_Down_Explosiveness = dense_rank(def_passing_downs_explosiveness),
      Rank_Def_Rush_Play_PPA = dense_rank(def_rushing_plays_ppa),
      Rank_Def_Rush_Play_Success_Rt = dense_rank(def_rushing_plays_success_rate),
      Rank_Def_Rush_Play_Explosiveness = dense_rank(def_rushing_plays_explosiveness),
      Rank_Def_Pass_Play_PPA = dense_rank(def_passing_plays_ppa),
      Rank_Def_Pass_Play_Success_Rt = dense_rank(def_passing_plays_success_rate),
      Rank_Def_Pass_Play_Explosiveness = dense_rank(def_passing_plays_explosiveness),
      Rank_SP_Rating = dense_rank(desc(sp_rating)),
      Rank_SP_Off_Rating = dense_rank(desc(sp_offense_rating)),
      Rank_SP_Def_Rating = dense_rank(sp_defense_rating),
      Rank_SP_SpecialTeams_Rating = dense_rank(desc(sp_special_teams_rating)),
      Rank_FPI = dense_rank(desc(FPI)),
      Rank_SRS_rating = dense_rank(desc(SRS_rating)),
      Rank_FPI_SRS_mean = dense_rank(desc(FPI_SRS_mean)),
      Rank_FPI_SP_mean = dense_rank(desc(FPI_SP_mean)),
      # due to availability issues, SP_Rankings not always included with current data
      ## Current stats weighted 2x
      Rank_Wins_col2 = dense_rank(desc(Wins)),
      Rank_Losses_col2 = dense_rank(Losses),
      Rank_Comp_Pct_col2 = dense_rank(desc(completion_pct)),
      Rank_Pass_YPA_col2 = dense_rank(desc(pass_ypa)),
      Rank_Pass_YPR_col2 = dense_rank(desc(pass_ypr)),
      Rank_Int_Pct_col2 = dense_rank(int_pct),
      Rank_Rush_YPC_col2 = dense_rank(desc(rush_ypc)),
      Rank_Turnovers_pg_col2 = dense_rank(turnovers_pg),
      Rank_Third_Conv_Rate_col2 = dense_rank(desc(third_conv_rate)),
      Rank_Fourth_Conv_Rate_col2 = dense_rank(desc(fourth_conv_rate)),
      Rank_Penalty_Yds_pg_col2 = dense_rank(penalty_yds_pg),
      Rank_Yds_Per_Penalty_col2 = dense_rank(yards_per_penalty),
      Rank_Kick_Return_Avg_col2 = dense_rank(desc(kick_return_avg)),
      Rank_Punt_Return_Avg_col2 = dense_rank(desc(punt_return_avg)),
      Rank_Total_Yds_pg_col2 = dense_rank(desc(total_yds_pg)),
      Rank_Pass_Yds_pg_col2 = dense_rank(desc(pass_yds_pg)),
      Rank_Rush_Yds_pg_col2 = dense_rank(desc(rush_yds_pg)),
      Rank_First_Downs_pg_col2 = dense_rank(desc(first_downs_pg)),
      Rank_Off_YPP_col2 = dense_rank(desc(off_ypp)),
      Rank_Def_Ints_pg_col2 = dense_rank(desc(def_interceptions_pg)),
      Rank_Off_PPA_col2 = dense_rank(desc(off_ppa)),
      Rank_Off_Success_Rt_col2 = dense_rank(desc(off_success_rate)),
      Rank_Off_Explosiveness_col2 = dense_rank(desc(off_explosiveness)),
      Rank_Off_Pwr_Success_col2 = dense_rank(desc(off_power_success)),
      Rank_Off_Stuff_Rt_col2 = dense_rank(off_stuff_rate),
      Rank_Off_Line_Yds_col2 = dense_rank(desc(off_line_yds)),
      Rank_Off_Second_Lvl_Yds_col2 = dense_rank(desc(off_second_lvl_yds)),
      Rank_Off_Open_Field_Yds_col2 = dense_rank(desc(off_open_field_yds)),
      Rank_Off_Pts_Per_Opp_col2 = dense_rank(desc(off_pts_per_opp)),
      Rank_Off_Field_Pos_Avg_Predicted_Pts_col2 = dense_rank(desc(off_field_pos_avg_predicted_points)),
      Rank_Off_Havoc_Total_col2 = dense_rank(off_havoc_total),
      Rank_Off_Havoc_Front_col2 = dense_rank(off_havoc_front_seven),
      Rank_Off_Havoc_DB_col2 = dense_rank(off_havoc_db),
      Rank_Off_Standard_Down_PPA_col2 = dense_rank(desc(off_standard_downs_ppa)),
      Rank_Off_Standard_Down_Success_Rt_col2 = dense_rank(desc(off_standard_downs_success_rate)),
      Rank_Off_Standard_Down_Explosiveness_col2 = dense_rank(desc(off_standard_downs_explosiveness)),
      Rank_Off_Pass_Down_PPA_col2 = dense_rank(desc(off_passing_downs_ppa)),
      Rank_Off_Pass_Down_Success_Rt_col2 = dense_rank(desc(off_passing_downs_success_rate)),
      Rank_Off_Pass_Down_Explosiveness_col2 = dense_rank(desc(off_passing_downs_explosiveness)),
      Rank_Off_Rush_Play_PPA_col2 = dense_rank(desc(off_rushing_plays_ppa)),
      Rank_Off_Rush_Play_Success_Rt_col2 = dense_rank(desc(off_rushing_plays_success_rate)),
      Rank_Off_Rush_Play_Explosiveness_col2 = dense_rank(desc(off_rushing_plays_explosiveness)),
      Rank_Off_Pass_Play_PPA_col2 = dense_rank(desc(off_passing_plays_ppa)),
      Rank_Off_Pass_Play_Success_Rt_col2 = dense_rank(desc(off_passing_plays_success_rate)),
      Rank_Off_Pass_Play_Explosiveness_col2 = dense_rank(desc(off_passing_plays_explosiveness)),
      Rank_Def_PPA_col2 = dense_rank(def_ppa),
      Rank_Def_Success_Rt_col2 = dense_rank(def_success_rate),
      Rank_Def_Explosiveness_col2 = dense_rank(def_explosiveness),
      Rank_Def_Pwr_Success_col2 = dense_rank(def_power_success),
      Rank_Def_Stuff_Rt_col2 = dense_rank(desc(def_stuff_rate)),
      Rank_Def_Line_Yds_col2 = dense_rank(def_line_yds),
      Rank_Def_Second_Lvl_Yds_col2 = dense_rank(def_second_lvl_yds),
      Rank_Def_Open_Field_Yds_col2 = dense_rank(def_open_field_yds),
      Rank_Def_Pts_Per_Opp_col2 = dense_rank(def_pts_per_opp),
      Rank_Def_Field_Pos_Avg_Predicted_Pts_col2 = dense_rank(def_field_pos_avg_predicted_points),
      Rank_Def_Havoc_Total_col2 = dense_rank(desc(def_havoc_total)),
      Rank_Def_Havoc_Front_Seven_col2 = dense_rank(desc(def_havoc_front_seven)),
      Rank_Def_Havoc_DB_col2 = dense_rank(desc(def_havoc_db)),
      Rank_Def_Standard_Down_PPA_col2 = dense_rank(def_standard_downs_ppa),
      Rank_Def_Standard_Down_Success_Rt_col2 = dense_rank(def_standard_downs_success_rate),
      Rank_Def_Standard_Down_Explosiveness_col2 = dense_rank(def_standard_downs_explosiveness),
      Rank_Def_Pass_Down_PPA_col2 = dense_rank(def_passing_downs_ppa),
      Rank_Def_Pass_Down_Success_Rt_col2 = dense_rank(def_passing_downs_success_rate),
      Rank_Def_Pass_Down_Explosiveness_col2 = dense_rank(def_passing_downs_explosiveness),
      Rank_Def_Rush_Play_PPA_col2 = dense_rank(def_rushing_plays_ppa),
      Rank_Def_Rush_Play_Success_Rt_col2 = dense_rank(def_rushing_plays_success_rate),
      Rank_Def_Rush_Play_Explosiveness_col2 = dense_rank(def_rushing_plays_explosiveness),
      Rank_Def_Pass_Play_PPA_col2 = dense_rank(def_passing_plays_ppa),
      Rank_Def_Pass_Play_Success_Rt_col2 = dense_rank(def_passing_plays_success_rate),
      Rank_Def_Pass_Play_Explosiveness_col2 = dense_rank(def_passing_plays_explosiveness),
      ## Extra weighted variables for current year (weighted 2x)
      Rank_Wins_col3 = dense_rank(desc(Wins)),
      Rank_Losses_col3 = dense_rank(Losses),
      Rank_Off_YPP_col3 = dense_rank(desc(off_ypp)),
      Rank_Off_PPA_col3 = dense_rank(desc(off_ppa)),
      Rank_Off_Success_Rt_col3 = dense_rank(desc(off_success_rate)),
      Rank_Off_Explosiveness_col3 = dense_rank(desc(off_explosiveness)),
      Rank_Off_Pwr_Success_col3 = dense_rank(desc(off_power_success)),
      Rank_Off_Stuff_Rt_col3 = dense_rank(off_stuff_rate),
      Rank_Off_Pts_Per_Opp_col3 = dense_rank(desc(off_pts_per_opp)),
      Rank_Off_Havoc_Total_col3 = dense_rank(off_havoc_total),
      Rank_Off_Havoc_Front_col3 = dense_rank(off_havoc_front_seven),
      Rank_Off_Havoc_DB_col3 = dense_rank(off_havoc_db),
      Rank_Off_Standard_Down_PPA_col3 = dense_rank(desc(off_standard_downs_ppa)),
      Rank_Off_Standard_Down_Success_Rt_col3 = dense_rank(desc(off_standard_downs_success_rate)),
      Rank_Off_Standard_Down_Explosiveness_col3 = dense_rank(desc(off_standard_downs_explosiveness)),
      Rank_Off_Pass_Down_PPA_col3 = dense_rank(desc(off_passing_downs_ppa)),
      Rank_Off_Pass_Down_Success_Rt_col3 = dense_rank(desc(off_passing_downs_success_rate)),
      Rank_Off_Pass_Down_Explosiveness_col3 = dense_rank(desc(off_passing_downs_explosiveness)),
      Rank_Off_Rush_Play_PPA_col3 = dense_rank(desc(off_rushing_plays_ppa)),
      Rank_Off_Rush_Play_Success_Rt_col3 = dense_rank(desc(off_rushing_plays_success_rate)),
      Rank_Off_Rush_Play_Explosiveness_col3 = dense_rank(desc(off_rushing_plays_explosiveness)),
      Rank_Off_Pass_Play_PPA_col3 = dense_rank(desc(off_passing_plays_ppa)),
      Rank_Off_Pass_Play_Success_Rt_col3 = dense_rank(desc(off_passing_plays_success_rate)),
      Rank_Off_Pass_Play_Explosiveness_col3 = dense_rank(desc(off_passing_plays_explosiveness)),
      Rank_Def_PPA_col3 = dense_rank(def_ppa),
      Rank_Def_Success_Rt_col3 = dense_rank(def_success_rate),
      Rank_Def_Explosiveness_col3 = dense_rank(def_explosiveness),
      Rank_Def_Pwr_Success_col3 = dense_rank(def_power_success),
      Rank_Def_Stuff_Rt_col3 = dense_rank(desc(def_stuff_rate)),
      Rank_Def_Pts_Per_Opp_col3 = dense_rank(def_pts_per_opp),
      Rank_Def_Havoc_Total_col3 = dense_rank(desc(def_havoc_total)),
      Rank_Def_Havoc_Front_Seven_col3 = dense_rank(desc(def_havoc_front_seven)),
      Rank_Def_Havoc_DB_col3 = dense_rank(desc(def_havoc_db)),
      Rank_Def_Standard_Down_PPA_col3 = dense_rank(def_standard_downs_ppa),
      Rank_Def_Standard_Down_Success_Rt_col3 = dense_rank(def_standard_downs_success_rate),
      Rank_Def_Standard_Down_Explosiveness_col3 = dense_rank(def_standard_downs_explosiveness),
      Rank_Def_Pass_Down_PPA_col3 = dense_rank(def_passing_downs_ppa),
      Rank_Def_Pass_Down_Success_Rt_col3 = dense_rank(def_passing_downs_success_rate),
      Rank_Def_Pass_Down_Explosiveness_col3 = dense_rank(def_passing_downs_explosiveness),
      Rank_Def_Rush_Play_PPA_col3 = dense_rank(def_rushing_plays_ppa),
      Rank_Def_Rush_Play_Success_Rt_col3 = dense_rank(def_rushing_plays_success_rate),
      Rank_Def_Rush_Play_Explosiveness_col3 = dense_rank(def_rushing_plays_explosiveness),
      Rank_Def_Pass_Play_PPA_col3 = dense_rank(def_passing_plays_ppa),
      Rank_Def_Pass_Play_Success_Rt_col3 = dense_rank(def_passing_plays_success_rate),
      Rank_Def_Pass_Play_Explosiveness_col3 = dense_rank(def_passing_plays_explosiveness),
      ## Extra weighted variables for current year (weighted 2x)
      Rank_Wins_col4 = dense_rank(desc(Wins)),
      Rank_Losses_col4 = dense_rank(Losses),
      Rank_Off_YPP_col4 = dense_rank(desc(off_ypp)),
      Rank_Off_PPA_col4 = dense_rank(desc(off_ppa)),
      Rank_Off_Success_Rt_col4 = dense_rank(desc(off_success_rate)),
      Rank_Off_Explosiveness_col4 = dense_rank(desc(off_explosiveness)),
      Rank_Off_Pwr_Success_col4 = dense_rank(desc(off_power_success)),
      Rank_Off_Stuff_Rt_col4 = dense_rank(off_stuff_rate),
      Rank_Off_Pts_Per_Opp_col4 = dense_rank(desc(off_pts_per_opp)),
      Rank_Off_Havoc_Total_col4 = dense_rank(off_havoc_total),
      Rank_Off_Havoc_Front_col4 = dense_rank(off_havoc_front_seven),
      Rank_Off_Havoc_DB_col4 = dense_rank(off_havoc_db),
      Rank_Off_Standard_Down_PPA_col4 = dense_rank(desc(off_standard_downs_ppa)),
      Rank_Off_Standard_Down_Success_Rt_col4 = dense_rank(desc(off_standard_downs_success_rate)),
      Rank_Off_Standard_Down_Explosiveness_col4 = dense_rank(desc(off_standard_downs_explosiveness)),
      Rank_Off_Pass_Down_PPA_col4 = dense_rank(desc(off_passing_downs_ppa)),
      Rank_Off_Pass_Down_Success_Rt_col4 = dense_rank(desc(off_passing_downs_success_rate)),
      Rank_Off_Pass_Down_Explosiveness_col4 = dense_rank(desc(off_passing_downs_explosiveness)),
      Rank_Off_Rush_Play_PPA_col4 = dense_rank(desc(off_rushing_plays_ppa)),
      Rank_Off_Rush_Play_Success_Rt_col4 = dense_rank(desc(off_rushing_plays_success_rate)),
      Rank_Off_Rush_Play_Explosiveness_col4 = dense_rank(desc(off_rushing_plays_explosiveness)),
      Rank_Off_Pass_Play_PPA_col4 = dense_rank(desc(off_passing_plays_ppa)),
      Rank_Off_Pass_Play_Success_Rt_col4 = dense_rank(desc(off_passing_plays_success_rate)),
      Rank_Off_Pass_Play_Explosiveness_col4 = dense_rank(desc(off_passing_plays_explosiveness)),
      Rank_Def_PPA_col4 = dense_rank(def_ppa),
      Rank_Def_Success_Rt_col4 = dense_rank(def_success_rate),
      Rank_Def_Explosiveness_col4 = dense_rank(def_explosiveness),
      Rank_Def_Pwr_Success_col4 = dense_rank(def_power_success),
      Rank_Def_Stuff_Rt_col4 = dense_rank(desc(def_stuff_rate)),
      Rank_Def_Pts_Per_Opp_col4 = dense_rank(def_pts_per_opp),
      Rank_Def_Havoc_Total_col4 = dense_rank(desc(def_havoc_total)),
      Rank_Def_Havoc_Front_Seven_col4 = dense_rank(desc(def_havoc_front_seven)),
      Rank_Def_Havoc_DB_col4 = dense_rank(desc(def_havoc_db)),
      Rank_Def_Standard_Down_PPA_col4 = dense_rank(def_standard_downs_ppa),
      Rank_Def_Standard_Down_Success_Rt_col4 = dense_rank(def_standard_downs_success_rate),
      Rank_Def_Standard_Down_Explosiveness_col4 = dense_rank(def_standard_downs_explosiveness),
      Rank_Def_Pass_Down_PPA_col4 = dense_rank(def_passing_downs_ppa),
      Rank_Def_Pass_Down_Success_Rt_col4 = dense_rank(def_passing_downs_success_rate),
      Rank_Def_Pass_Down_Explosiveness_col4 = dense_rank(def_passing_downs_explosiveness),
      Rank_Def_Rush_Play_PPA_col4 = dense_rank(def_rushing_plays_ppa),
      Rank_Def_Rush_Play_Success_Rt_col4 = dense_rank(def_rushing_plays_success_rate),
      Rank_Def_Rush_Play_Explosiveness_col4 = dense_rank(def_rushing_plays_explosiveness),
      Rank_Def_Pass_Play_PPA_col4 = dense_rank(def_passing_plays_ppa),
      Rank_Def_Pass_Play_Success_Rt_col4 = dense_rank(def_passing_plays_success_rate),
      Rank_Def_Pass_Play_Explosiveness_col4 = dense_rank(def_passing_plays_explosiveness))
} else {
  ##### Week 7-End of Season Variable Ranks #####
  ## Recruiting points no longer included
  # current will be only data source used, everything weighted "1x" (aside from special variables, and recruiting)
  ## Ranking current stats
  VoA_Variables <- VoA_Variables |>
    mutate(Rank_Wins = dense_rank(desc(Wins)),
           Rank_Losses = dense_rank(Losses),
           Rank_Comp_Pct = dense_rank(desc(completion_pct)),
      Rank_Pass_YPA = dense_rank(desc(pass_ypa)),
      Rank_Pass_YPR = dense_rank(desc(pass_ypr)),
      Rank_Int_Pct = dense_rank(int_pct),
      Rank_Rush_YPC = dense_rank(desc(rush_ypc)),
      Rank_Turnovers_pg = dense_rank(turnovers_pg),
      Rank_Third_Conv_Rate = dense_rank(desc(third_conv_rate)),
      Rank_Fourth_Conv_Rate = dense_rank(desc(fourth_conv_rate)),
      Rank_Penalty_Yds_pg = dense_rank(penalty_yds_pg),
      Rank_Yds_Per_Penalty = dense_rank(yards_per_penalty),
      Rank_Kick_Return_Avg = dense_rank(desc(kick_return_avg)),
      Rank_Punt_Return_Avg = dense_rank(desc(punt_return_avg)),
      Rank_Total_Yds_pg = dense_rank(desc(total_yds_pg)),
      Rank_Pass_Yds_pg = dense_rank(desc(pass_yds_pg)),
      Rank_Rush_Yds_pg = dense_rank(desc(rush_yds_pg)),
      Rank_First_Downs_pg = dense_rank(desc(first_downs_pg)),
      Rank_Off_YPP = dense_rank(desc(off_ypp)),
      Rank_Def_Ints_pg = dense_rank(desc(def_interceptions_pg)),
      Rank_Off_PPA = dense_rank(desc(off_ppa)),
      Rank_Off_Success_Rt = dense_rank(desc(off_success_rate)),
      Rank_Off_Explosiveness = dense_rank(desc(off_explosiveness)),
      Rank_Off_Pwr_Success = dense_rank(desc(off_power_success)),
      Rank_Off_Stuff_Rt = dense_rank(off_stuff_rate),
      Rank_Off_Line_Yds = dense_rank(desc(off_line_yds)),
      Rank_Off_Second_Lvl_Yds = dense_rank(desc(off_second_lvl_yds)),
      Rank_Off_Open_Field_Yds = dense_rank(desc(off_open_field_yds)),
      Rank_Off_Pts_Per_Opp = dense_rank(desc(off_pts_per_opp)),
      Rank_Off_Field_Pos_Avg_Predicted_Pts = dense_rank(desc(off_field_pos_avg_predicted_points)),
      Rank_Off_Havoc_Total = dense_rank(off_havoc_total),
      Rank_Off_Havoc_Front = dense_rank(off_havoc_front_seven),
      Rank_Off_Havoc_DB = dense_rank(off_havoc_db),
      Rank_Off_Standard_Down_PPA = dense_rank(desc(off_standard_downs_ppa)),
      Rank_Off_Standard_Down_Success_Rt = dense_rank(desc(off_standard_downs_success_rate)),
      Rank_Off_Standard_Down_Explosiveness = dense_rank(desc(off_standard_downs_explosiveness)),
      Rank_Off_Pass_Down_PPA = dense_rank(desc(off_passing_downs_ppa)),
      Rank_Off_Pass_Down_Success_Rt = dense_rank(desc(off_passing_downs_success_rate)),
      Rank_Off_Pass_Down_Explosiveness = dense_rank(desc(off_passing_downs_explosiveness)),
      Rank_Off_Rush_Play_PPA = dense_rank(desc(off_rushing_plays_ppa)),
      Rank_Off_Rush_Play_Success_Rt = dense_rank(desc(off_rushing_plays_success_rate)),
      Rank_Off_Rush_Play_Explosiveness = dense_rank(desc(off_rushing_plays_explosiveness)),
      Rank_Off_Pass_Play_PPA = dense_rank(desc(off_passing_plays_ppa)),
      Rank_Off_Pass_Play_Success_Rt = dense_rank(desc(off_passing_plays_success_rate)),
      Rank_Off_Pass_Play_Explosiveness = dense_rank(desc(off_passing_plays_explosiveness)),
      Rank_Def_PPA = dense_rank(def_ppa),
      Rank_Def_Success_Rt = dense_rank(def_success_rate),
      Rank_Def_Explosiveness = dense_rank(def_explosiveness),
      Rank_Def_Pwr_Success = dense_rank(def_power_success),
      Rank_Def_Stuff_Rt = dense_rank(desc(def_stuff_rate)),
      Rank_Def_Line_Yds = dense_rank(def_line_yds),
      Rank_Def_Second_Lvl_Yds = dense_rank(def_second_lvl_yds),
      Rank_Def_Open_Field_Yds = dense_rank(def_open_field_yds),
      Rank_Def_Pts_Per_Opp = dense_rank(def_pts_per_opp),
      Rank_Def_Field_Pos_Avg_Predicted_Pts = dense_rank(def_field_pos_avg_predicted_points),
      Rank_Def_Havoc_Total = dense_rank(desc(def_havoc_total)),
      Rank_Def_Havoc_Front_Seven = dense_rank(desc(def_havoc_front_seven)),
      Rank_Def_Havoc_DB = dense_rank(desc(def_havoc_db)),
      Rank_Def_Standard_Down_PPA = dense_rank(def_standard_downs_ppa),
      Rank_Def_Standard_Down_Success_Rt = dense_rank(def_standard_downs_success_rate),
      Rank_Def_Standard_Down_Explosiveness = dense_rank(def_standard_downs_explosiveness),
      Rank_Def_Pass_Down_PPA = dense_rank(def_passing_downs_ppa),
      Rank_Def_Pass_Down_Success_Rt = dense_rank(def_passing_downs_success_rate),
      Rank_Def_Pass_Down_Explosiveness = dense_rank(def_passing_downs_explosiveness),
      Rank_Def_Rush_Play_PPA = dense_rank(def_rushing_plays_ppa),
      Rank_Def_Rush_Play_Success_Rt = dense_rank(def_rushing_plays_success_rate),
      Rank_Def_Rush_Play_Explosiveness = dense_rank(def_rushing_plays_explosiveness),
      Rank_Def_Pass_Play_PPA = dense_rank(def_passing_plays_ppa),
      Rank_Def_Pass_Play_Success_Rt = dense_rank(def_passing_plays_success_rate),
      Rank_Def_Pass_Play_Explosiveness = dense_rank(def_passing_plays_explosiveness),
      Rank_SP_Rating = dense_rank(desc(sp_rating)),
      Rank_SP_Off_Rating = dense_rank(desc(sp_offense_rating)),
      Rank_SP_Def_Rating = dense_rank(sp_defense_rating),
      Rank_SP_SpecialTeams_Rating = dense_rank(desc(sp_special_teams_rating)),
      Rank_FPI = dense_rank(desc(FPI)),
      Rank_SRS_rating = dense_rank(desc(SRS_rating)),
      Rank_FPI_SRS_mean = dense_rank(desc(FPI_SRS_mean)),
      Rank_FPI_SP_mean = dense_rank(desc(FPI_SP_mean)),
      ## Extra weighted variables for current year (weighted 2x)
      Rank_Wins_col2 = dense_rank(desc(Wins)),
      Rank_Losses_col2 = dense_rank(Losses),
      Rank_Off_YPP_col2 = dense_rank(desc(off_ypp)),
      Rank_Off_PPA_col2 = dense_rank(desc(off_ppa)),
      Rank_Off_Success_Rt_col2 = dense_rank(desc(off_success_rate)),
      Rank_Off_Explosiveness_col2 = dense_rank(desc(off_explosiveness)),
      Rank_Off_Pwr_Success_col2 = dense_rank(desc(off_power_success)),
      Rank_Off_Stuff_Rt_col2 = dense_rank(off_stuff_rate),
      Rank_Off_Pts_Per_Opp_col2 = dense_rank(desc(off_pts_per_opp)),
      Rank_Off_Havoc_Total_col2 = dense_rank(off_havoc_total),
      Rank_Off_Havoc_Front_col2 = dense_rank(off_havoc_front_seven),
      Rank_Off_Havoc_DB_col2 = dense_rank(off_havoc_db),
      Rank_Off_Standard_Down_PPA_col2 = dense_rank(desc(off_standard_downs_ppa)),
      Rank_Off_Standard_Down_Success_Rt_col2 = dense_rank(desc(off_standard_downs_success_rate)),
      Rank_Off_Standard_Down_Explosiveness_col2 = dense_rank(desc(off_standard_downs_explosiveness)),
      Rank_Off_Pass_Down_PPA_col2 = dense_rank(desc(off_passing_downs_ppa)),
      Rank_Off_Pass_Down_Success_Rt_col2 = dense_rank(desc(off_passing_downs_success_rate)),
      Rank_Off_Pass_Down_Explosiveness_col2 = dense_rank(desc(off_passing_downs_explosiveness)),
      Rank_Off_Rush_Play_PPA_col2 = dense_rank(desc(off_rushing_plays_ppa)),
      Rank_Off_Rush_Play_Success_Rt_col2 = dense_rank(desc(off_rushing_plays_success_rate)),
      Rank_Off_Rush_Play_Explosiveness_col2 = dense_rank(desc(off_rushing_plays_explosiveness)),
      Rank_Off_Pass_Play_PPA_col2 = dense_rank(desc(off_passing_plays_ppa)),
      Rank_Off_Pass_Play_Success_Rt_col2 = dense_rank(desc(off_passing_plays_success_rate)),
      Rank_Off_Pass_Play_Explosiveness_col2 = dense_rank(desc(off_passing_plays_explosiveness)),
      Rank_Def_PPA_col2 = dense_rank(def_ppa),
      Rank_Def_Success_Rt_col2 = dense_rank(def_success_rate),
      Rank_Def_Explosiveness_col2 = dense_rank(def_explosiveness),
      Rank_Def_Pwr_Success_col2 = dense_rank(def_power_success),
      Rank_Def_Stuff_Rt_col2 = dense_rank(desc(def_stuff_rate)),
      Rank_Def_Pts_Per_Opp_col2 = dense_rank(def_pts_per_opp),
      Rank_Def_Havoc_Total_col2 = dense_rank(desc(def_havoc_total)),
      Rank_Def_Havoc_Front_Seven_col2 = dense_rank(desc(def_havoc_front_seven)),
      Rank_Def_Havoc_DB_col2 = dense_rank(desc(def_havoc_db)),
      Rank_Def_Standard_Down_PPA_col2 = dense_rank(def_standard_downs_ppa),
      Rank_Def_Standard_Down_Success_Rt_col2 = dense_rank(def_standard_downs_success_rate),
      Rank_Def_Standard_Down_Explosiveness_col2 = dense_rank(def_standard_downs_explosiveness),
      Rank_Def_Pass_Down_PPA_col2 = dense_rank(def_passing_downs_ppa),
      Rank_Def_Pass_Down_Success_Rt_col2 = dense_rank(def_passing_downs_success_rate),
      Rank_Def_Pass_Down_Explosiveness_col2 = dense_rank(def_passing_downs_explosiveness),
      Rank_Def_Rush_Play_PPA_col2 = dense_rank(def_rushing_plays_ppa),
      Rank_Def_Rush_Play_Success_Rt_col2 = dense_rank(def_rushing_plays_success_rate),
      Rank_Def_Rush_Play_Explosiveness_col2 = dense_rank(def_rushing_plays_explosiveness),
      Rank_Def_Pass_Play_PPA_col2 = dense_rank(def_passing_plays_ppa),
      Rank_Def_Pass_Play_Success_Rt_col2 = dense_rank(def_passing_plays_success_rate),
      Rank_Def_Pass_Play_Explosiveness_col2 = dense_rank(def_passing_plays_explosiveness))
}
 ## end of if statement




##### calculating the mean stat ranking, VoA_Output #####
## Rank variables start at 248 for Week 0
## Rank variables start at 321 for Week 1
## it will be a different number for the other weeks
## script wouldn't run properly without a real number in there so I'll have to come back
# and edit the number in during the season as I figure out how big VoA_Variables gets
if (as.numeric(week) == 0) {
  ## correcting "season" column to reflect the season for which these rankings are being produced
  VoA_Variables$season = rep(as.numeric(year), nrow(VoA_Variables))
  ## Append new column of Model output, which is the mean of all variables in VoARanks
  VoA_Variables <- VoA_Variables |>
    mutate(VoA_Output = (rowMeans(VoA_Variables[,248:ncol(VoA_Variables)])))
  ## Append column of VoA Final Rankings
  # VoA_Variables <- VoA_Variables |>
  #   mutate(VoA_Ranking = dense_rank(VoA_Output))
} else if (as.numeric(week) == 1) {
  ## Append new column of Model output, which is the mean of all variables in VoARanks
  VoA_Variables <- VoA_Variables |>
    mutate(VoA_Output = (rowMeans(VoA_Variables[,321:ncol(VoA_Variables)])))
  ## Append column of VoA Final Rankings
  # VoA_Variables <- VoA_Variables |>
  #   mutate(VoA_Ranking = dense_rank(VoA_Output))
} else if (as.numeric(week) <= 4) {
  ## Append new column of Model output, which is the mean of all variables in VoARanks
  VoA_Variables <- VoA_Variables |>
    mutate(VoA_Output = (rowMeans(VoA_Variables[,248:ncol(VoA_Variables)])))
  ## Append column of VoA Final Rankings
  # VoA_Variables <- VoA_Variables |>
  #   mutate(VoA_Ranking = dense_rank(VoA_Output))
} else if (as.numeric(week) == 5) {
  ## Append new column of Model output, which is the mean of all variables in VoARanks
  VoA_Variables <- VoA_Variables |>
    mutate(VoA_Output = (rowMeans(VoA_Variables[,157:ncol(VoA_Variables)])))
  ## Append column of VoA Final Rankings
  # VoA_Variables <- VoA_Variables |>
  #   mutate(VoA_Ranking = dense_rank(VoA_Output))
} else {
  ## Append new column of Model output, which is the mean of all variables in VoARanks
  VoA_Variables <- VoA_Variables |>
    mutate(VoA_Output = (rowMeans(VoA_Variables[,78:ncol(VoA_Variables)])))
  ## Append column of VoA Final Rankings
  # VoA_Variables <- VoA_Variables |>
  #   mutate(VoA_Ranking = dense_rank(VoA_Output))
}
## End of if statement

##### Using Intial VoA Rankings to add in conference strength metric #####
Conference_Outputs <- VoA_Variables |>
  group_by(conference) |>
  summarize(Rk_mean = mean(VoA_Output), Rk_median = median(VoA_Output)) |>
  mutate(Conf_Rk = dense_rank(Rk_mean))

VoA_Variables <- VoA_Variables |>
  select(-VoA_Output) |>
  mutate(Conference_Strength = case_when(conference == "ACC" ~ Conference_Outputs$Rk_mean[Conference_Outputs$conference == "ACC"],
                                         conference == "American Athletic" ~ Conference_Outputs$Rk_mean[Conference_Outputs$conference == "American Athletic"],
                                         conference == "Big 12" ~ Conference_Outputs$Rk_mean[Conference_Outputs$conference == "Big 12"],
                                         conference == "Big Ten" ~ Conference_Outputs$Rk_mean[Conference_Outputs$conference == "Big Ten"],
                                         conference == "Conference USA" ~ Conference_Outputs$Rk_mean[Conference_Outputs$conference == "Conference USA"],
                                         conference == "FBS Independents" ~ Conference_Outputs$Rk_mean[Conference_Outputs$conference == "FBS Independents"],
                                         conference == "Mid-American" ~ Conference_Outputs$Rk_mean[Conference_Outputs$conference == "Mid-American"],
                                         conference == "Mountain West" ~ Conference_Outputs$Rk_mean[Conference_Outputs$conference == "Mountain West"],
                                         conference == "Pac-12" ~ Conference_Outputs$Rk_mean[Conference_Outputs$conference == "Pac-12"],
                                         conference == "SEC" ~ Conference_Outputs$Rk_mean[Conference_Outputs$conference == "SEC"],
                                         conference == "Sun Belt" ~ Conference_Outputs$Rk_mean[Conference_Outputs$conference == "Sun Belt"],)) |>
  mutate(Conference_Strength_col2 = Conference_Strength,
         Conference_Strength_col3 = Conference_Strength,
         Conference_Strength_col4 = Conference_Strength,
         Conference_Strength_col5 = Conference_Strength,
         Conference_Strength_col6 = Conference_Strength,
         Conference_Strength_col7 = Conference_Strength,
         Conference_Strength_col8 = Conference_Strength,
         Conference_Strength_col9 = Conference_Strength,
         Conference_Strength_col10 = Conference_Strength)

##### Re running rowMeans function to get VoA Output #####
## script wouldn't run properly without a real number in the later weeks so I'll have to come back
# and edit the number in during the season as I figure out how big VoA_Variables gets
if (as.numeric(week) == 0) {
  ## Append new column of Model output, which is the mean of all variables in VoARanks
  VoA_Variables <- VoA_Variables |>
    mutate(VoA_Output = (rowMeans(VoA_Variables[,248:ncol(VoA_Variables)])))
  ## Append column of VoA Final Rankings
  # VoA_Variables <- VoA_Variables |>
  #   mutate(VoA_Ranking = dense_rank(VoA_Output))
} else if (as.numeric(week) == 1) {
  ## Append new column of Model output, which is the mean of all variables in VoARanks
  VoA_Variables <- VoA_Variables |>
    mutate(VoA_Output = (rowMeans(VoA_Variables[,316:ncol(VoA_Variables)])))
  ## Append column of VoA Final Rankings
  # VoA_Variables <- VoA_Variables |>
  #   mutate(VoA_Ranking = dense_rank(VoA_Output))
} else if (as.numeric(week) <= 4) {
  ## Append new column of Model output, which is the mean of all variables in VoARanks
  VoA_Variables <- VoA_Variables |>
    mutate(VoA_Output = (rowMeans(VoA_Variables[,237:ncol(VoA_Variables)])))
  ## Append column of VoA Final Rankings
  # VoA_Variables <- VoA_Variables |>
  #   mutate(VoA_Ranking = dense_rank(VoA_Output))
} else if (as.numeric(week) == 5) {
  ## Append new column of Model output, which is the mean of all variables in VoARanks
  VoA_Variables <- VoA_Variables |>
    mutate(VoA_Output = (rowMeans(VoA_Variables[,157:ncol(VoA_Variables)])))
  ## Append column of VoA Final Rankings
  # VoA_Variables <- VoA_Variables |>
  #   mutate(VoA_Ranking = dense_rank(VoA_Output))
} else {
  ## Append new column of Model output, which is the mean of all variables in VoARanks
  VoA_Variables <- VoA_Variables |>
    mutate(VoA_Output = (rowMeans(VoA_Variables[,78:ncol(VoA_Variables)])))
  ## Append column of VoA Final Rankings
  # VoA_Variables <- VoA_Variables |>
  #   mutate(VoA_Ranking = dense_rank(VoA_Output))
}
## End of if statement

##### using R's linear model function to create FPI/SP+ like metric #####
# includes PPA, success rate, explosiveness, VoA_Output, VoA's Conference_Strength, and pts_per_opp (offense and defense where applicable)
## For current season starting in week 1 and beyond, I would have preferred to use
# a mean of FPI and SP+ as the "predicted" variable in the lm() model
# due to availability issues, SP_Rankings not included with current data
# so this is not possible. as such, I will use just FPI
## starting in 2023 season, planning to use SRS in addition to FPI
# previous season's SRS will be used if current season SRS is not available
## starting in 2023 season, variables weighted by year in lm() model
# will be weighted just like in the variable ranks for the VoA Output
# only exceptions will be VoA Output weighted ^5, ConferenceStrength weighted ^2
# Once only current stats are used, VoA Output weighted ^3, ConferenceStrength weighted ^2
## if Week = 0
# PY3 weighted 1x, PY2 weighted 2x, PY1 weighted 3x
### if Week = 1
# PY3 weighted 1x, PY2 weighted 2x, PY1 weighted 3x, current weighted 1x
### if week <= 4
# PY2 weighted 1x, PY1 weighted 2x, current weighted 2x
### if week <= 6
# PY1 weighted 1x, current weighted 2x
### if week > 6
# current will be only data source used, everything weighted "1x" (aside from special variables)
set.seed(386)
if (as.numeric(week) == 0) {
  model <- lm(AllPY_FPI_SP_SRS_mean ~ off_ppa_PY1^3 + off_ppa_PY2^2 + def_ppa_PY1^3 + def_ppa_PY2^2 + off_ppa_PY3 + def_ppa_PY3 + VoA_Output^5 + Conference_Strength^2 + off_ypp_PY3 + off_ypp_PY2^2 + off_ypp_PY1^3 + off_success_rate_PY3 + off_success_rate_PY2^2 + off_success_rate_PY1^3 + def_success_rate_PY3 + def_success_rate_PY2^2 + def_success_rate_PY1^3 + off_explosiveness_PY3 + off_explosiveness_PY2^2 + off_explosiveness_PY1^3 + def_explosiveness_PY3 + def_explosiveness_PY2^2 + def_explosiveness_PY1^3 + off_pts_per_opp_PY3 + off_pts_per_opp_PY2^2 + off_pts_per_opp_PY1^3 + def_pts_per_opp_PY3 + def_pts_per_opp_PY2^2 + def_pts_per_opp_PY1^3, data = VoA_Variables)
  ## summary(model)
  VoA_Variables <- VoA_Variables |>
    mutate(VoA_Rating = predict(model),
           VoA_Ranking = dense_rank(desc(VoA_Rating))) 
} else if (as.numeric(week) == 1) {
  model <- lm(FPI_SRS_mean ~ off_ppa + off_ppa_PY1^3 + off_ppa_PY2^2 + def_ppa + def_ppa_PY1^3 + def_ppa_PY2^2 + off_ppa_PY3 + def_ppa_PY3 + VoA_Output^5 + Conference_Strength^2 + off_ypp_PY3 + off_ypp_PY2^2 + off_ypp_PY1^3 + off_ypp + off_success_rate_PY3 + off_success_rate + off_success_rate_PY2^2 + off_success_rate_PY1^3 + def_success_rate_PY3 + def_success_rate_PY2^2 + def_success_rate_PY1^3 + def_success_rate + off_explosiveness_PY3 + off_explosiveness_PY2^2 + off_explosiveness_PY1^3 + off_explosiveness + def_explosiveness_PY3 + def_explosiveness_PY2^2 + def_explosiveness_PY1^3 + def_explosiveness + off_pts_per_opp_PY3 + off_pts_per_opp_PY2^2 + off_pts_per_opp_PY1^3 + off_pts_per_opp + def_pts_per_opp_PY3 + def_pts_per_opp_PY2^2 + def_pts_per_opp_PY1^3 + def_pts_per_opp, data = VoA_Variables)
  ## summary(model)
  VoA_Variables <- VoA_Variables |>
    mutate(VoA_Rating = predict(model),
           VoA_Ranking = dense_rank(desc(VoA_Rating)))
} else if (as.numeric(week) <= 4) {
  model <- lm(FPI_SP_SRS_mean ~ off_ppa^2 + off_ppa_PY1^2 + off_ppa_PY2 + def_ppa^2 + def_ppa_PY1^2 + def_ppa_PY2 + VoA_Output^5 + Conference_Strength^2 + off_ypp_PY2 + off_ypp_PY1^2 + off_ypp^2 + off_success_rate^2 + off_success_rate_PY2 + off_success_rate_PY1^2 + def_success_rate_PY2 + def_success_rate_PY1^2 + def_success_rate^2 + off_explosiveness_PY2 + off_explosiveness_PY1^2 + off_explosiveness^2 + def_explosiveness_PY2 + def_explosiveness_PY1^2 + def_explosiveness^2 + off_pts_per_opp_PY2 + off_pts_per_opp_PY1^2 + off_pts_per_opp^2 + def_pts_per_opp_PY2 + def_pts_per_opp_PY1^2 + def_pts_per_opp^2, data = VoA_Variables)
  ## summary(model)
  VoA_Variables <- VoA_Variables |>
    mutate(VoA_Rating = predict(model),
           VoA_Ranking = dense_rank(desc(VoA_Rating)))
} else if (as.numeric(week) <= 6) {
  model <- lm(FPI_SRS_mean ~ off_ppa^2 + off_ppa_PY1 + def_ppa^2 + def_ppa_PY1 + VoA_Output^5 + Conference_Strength^2 + off_ypp_PY1 + off_ypp^2 + off_success_rate^2 + off_success_rate_PY1 + def_success_rate_PY1 + def_success_rate^2 + off_explosiveness_PY1 + off_explosiveness^2 + def_explosiveness_PY1 + def_explosiveness^2 + off_pts_per_opp_PY1 + off_pts_per_opp^2 + def_pts_per_opp_PY1 + def_pts_per_opp^2, data = VoA_Variables)
  ## summary(model)
  VoA_Variables <- VoA_Variables |>
    mutate(VoA_Rating = predict(model),
           VoA_Ranking = dense_rank(desc(VoA_Rating)))
} else {
  model <- lm(FPI_SRS_mean ~ off_ppa + def_ppa + VoA_Output^3 + Conference_Strength^2 + off_ypp + off_success_rate + def_success_rate + off_explosiveness + def_explosiveness + off_pts_per_opp + def_pts_per_opp, data = VoA_Variables)
  ## summary(model)
  VoA_Variables <- VoA_Variables |>
    mutate(VoA_Rating = predict(model),
           VoA_Ranking = dense_rank(desc(VoA_Rating)))
}

             
## creating data frame with just team, VoA ranking, and VoA output
## Create Vector of teamname, VoA Ranking
FinalTable <- VoA_Variables |>
  select(team, conference, CFB_Week, VoA_Output, VoA_Ranking, VoA_Rating) |>
  arrange(VoA_Ranking)
FinalVoATop25 <- FinalTable |> filter(VoA_Ranking < 26)
## tail(FinalVoATop25)
## cols_hide() appears to be working
# 2 dfs created below originally created to avoid excess columns in gt tables
# Final_gt_table <- FinalTable |>
#   select(team, VoA_Ranking, VoA_Rating)
# Final_gt_Top25 <- FinalVoATop25 |>
#   select(team, VoA_Ranking, VoA_Rating)

##### Creating Top 25 and Full Tables Arranged by VoA Rating #####
if (as.numeric(week) == 0) {
  ## Top 25 Table
  # adding title and subtitle
  VoATop25Table <- FinalVoATop25 |>
    gt() |> # use 'gt' to make an awesome table...
    gt_theme_espn() |>
    tab_header(
      title = paste(year, preseason_text, VoA_Top25_text), # ...with this title
      subtitle = "Supremely Excellent Yet Salaciously Godlike And Infallibly Magnificent Vortex of Accuracy")  |>  # and this subtitle
    ## tab_style(style = cell_fill("bisque"),
    ##           locations = cells_body()) |>  # add fill color to table
    fmt_number( # A column (numeric data)
      columns = c(VoA_Rating), # What column variable? FinalVoATop25$VoA_Rating
      decimals = 5 # With four decimal places
    ) |> 
    fmt_number( # Another column (also numeric data)
      columns = c(VoA_Ranking), # What column variable? FinalVoATop25$VoA_Ranking
      decimals = 0 # I want this column to have zero decimal places
    ) |>
    data_color( # Update cell colors, testing different color palettes
      columns = c(VoA_Rating),
      fn = scales::col_numeric( # <- bc it's numeric
        palette = brewer.pal(9, "Reds"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    cols_label(VoA_Rating = "VoA Rating", VoA_Ranking = "VoA Ranking") |> # Update labels
    cols_move_to_end(columns = "VoA_Rating") |>
    cols_hide(c(conference, CFB_Week, VoA_Output)) |>
    tab_footnote(
      footnote = "Table by @gshelor, Data from CFB Data API, ESPN.com, and ESPN's Bill Connelly via cfbfastR, FCS data mostly from stats.ncaa.org"
    )
  
  ## Full 130 teams table
  # adding title and subtitle
  VoA_Full_Table <- FinalTable |>
    gt() |> # use 'gt' to make an awesome table...
    gt_theme_538() |>
    tab_header(
      title = paste(year, preseason_text, VoA_text), # ...with this title
      subtitle = "Supremely Excellent Yet Salaciously Godlike And Infallibly Magnificent Vortex of Accuracy")  |>  # and this subtitle
    ##tab_style(style = cell_fill("bisque"),
    ##        locations = cells_body()) |>  # add fill color to table
    fmt_number( # A column (numeric data)
      columns = c(VoA_Rating), # What column variable? FinalVoATop25$VoA_Rating
      decimals = 5 # With four decimal places
    ) |> 
    fmt_number( # Another column (also numeric data)
      columns = c(VoA_Ranking), # What column variable? FinalVoATop25$VoA_Ranking
      decimals = 0 # I want this column to have zero decimal places
    ) |> 
    data_color( # Update cell colors, testing different color palettes
      columns = c(VoA_Rating), # ...for dose column
      fn = scales::col_numeric( # <- bc it's numeric
        palette = brewer.pal(9, "RdBu"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = TRUE
      )
    ) |>
    cols_label(VoA_Rating = "VoA Rating", VoA_Ranking = "VoA Ranking") |> # Update labels
    cols_move_to_end(columns = "VoA_Rating") |>
    cols_hide(c(conference, CFB_Week, VoA_Output)) |>
    tab_footnote(
      footnote = "Table by @gshelor, Data from CFB Data API, ESPN.com, and ESPN's Bill Connelly via cfbfastR, FCS data mostly from stats.ncaa.org"
    )
} else if (as.numeric(week) > 15) {
  ## Top 25 Table
  # adding title and subtitle
  VoATop25Table <- FinalVoATop25 |>
    gt() |> # use 'gt' to make an awesome table...
    gt_theme_espn() |>
    tab_header(
      title = paste(year, Postseason_text, VoA_Top25_text), # ...with this title
      subtitle = "Supremely Excellent Yet Salaciously Godlike And Infallibly Magnificent Vortex of Accuracy")  |>  # and this subtitle
    ## tab_style(style = cell_fill("bisque"),
    ##           locations = cells_body()) |>  # add fill color to table
    fmt_number( # A column (numeric data)
      columns = c(VoA_Rating), # What column variable? FinalVoATop25$VoA_Rating
      decimals = 5 # With four decimal places
    ) |> 
    fmt_number( # Another column (also numeric data)
      columns = c(VoA_Ranking), # What column variable? FinalVoATop25$VoA_Ranking
      decimals = 0 # I want this column to have zero decimal places
    ) |>
    data_color( # Update cell colors, testing different color palettes
      columns = c(VoA_Rating), # ...for dose column
      fn = scales::col_numeric( # <- bc it's numeric
        palette = brewer.pal(9, "Reds"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    cols_label(VoA_Rating = "VoA Rating", VoA_Ranking = "VoA Ranking") |> # Update labels
    cols_move_to_end(columns = "VoA_Rating") |>
    cols_hide(c(conference, CFB_Week, VoA_Output)) |>
    tab_footnote(
      footnote = "Table by @gshelor, Data from CFB Data API, ESPN.com, and ESPN's Bill Connelly via cfbfastR, FCS data mostly from stats.ncaa.org"
    )
  
  ## Full 130 teams table
  # adding title and subtitle
  VoA_Full_Table <- FinalTable |>
    gt() |> # use 'gt' to make an awesome table...
    gt_theme_538() |>
    tab_header(
      title = paste(year, Postseason_text, VoA_text), # ...with this title
      subtitle = "Supremely Excellent Yet Salaciously Godlike And Infallibly Magnificent Vortex of Accuracy")  |>  # and this subtitle
    ##tab_style(style = cell_fill("bisque"),
    ##        locations = cells_body()) |>  # add fill color to table
    fmt_number( # A column (numeric data)
      columns = c(VoA_Rating), # What column variable? FinalVoATop25$VoA_Rating
      decimals = 5 # With four decimal places
    ) |> 
    fmt_number( # Another column (also numeric data)
      columns = c(VoA_Ranking), # What column variable? FinalVoATop25$VoA_Ranking
      decimals = 0 # I want this column to have zero decimal places
    ) |> 
    data_color( # Update cell colors, testing different color palettes
      columns = c(VoA_Rating), # ...for dose column
      fn = scales::col_numeric( # <- bc it's numeric
        palette = brewer.pal(9, "RdBu"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = TRUE
      )
    ) |>
    cols_label(VoA_Rating = "VoA Rating", VoA_Ranking = "VoA Ranking") |> # Update labels
    cols_move_to_end(columns = "VoA_Rating") |>
    cols_hide(c(conference, CFB_Week, VoA_Output)) |>
    tab_footnote(
      footnote = "Table by @gshelor, Data from CFB Data API, ESPN.com, and ESPN's Bill Connelly via cfbfastR, FCS data mostly from stats.ncaa.org"
    )
} else {
  ## Top 25 Table
  # adding title and subtitle
  VoATop25Table <- FinalVoATop25 |>
    gt() |> # use 'gt' to make an awesome table...
    gt_theme_espn() |>
    tab_header(
      title = paste(year, week_text, week, VoA_Top25_text), # ...with this title
      subtitle = "Supremely Excellent Yet Salaciously Godlike And Infallibly Magnificent Vortex of Accuracy")  |>  # and this subtitle
    ## tab_style(style = cell_fill("bisque"),
    ##           locations = cells_body()) |>  # add fill color to table
    fmt_number( # A column (numeric data)
      columns = c(VoA_Rating), # What column variable? FinalVoATop25$VoA_Rating
      decimals = 5 # With four decimal places
    ) |> 
    fmt_number( # Another column (also numeric data)
      columns = c(VoA_Ranking), # What column variable? FinalVoATop25$VoA_Ranking
      decimals = 0 # I want this column to have zero decimal places
    ) |>
    data_color( # Update cell colors, testing different color palettes
      columns = c(VoA_Rating), # ...for dose column
      fn = scales::col_numeric( # <- bc it's numeric
        palette = brewer.pal(9, "Reds"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    cols_label(VoA_Rating = "VoA Rating", VoA_Ranking = "VoA Ranking") |> # Update labels
    cols_move_to_end(columns = "VoA_Rating") |>
    cols_hide(c(conference, CFB_Week, VoA_Output)) |>
    tab_footnote(
      footnote = "Table by @gshelor, Data from CFB Data API, ESPN.com, and ESPN's Bill Connelly via cfbfastR, FCS data mostly from stats.ncaa.org"
    )
  
  ## Full 130 teams table
  # adding title and subtitle
  VoA_Full_Table <- FinalTable |>
    gt() |> # use 'gt' to make an awesome table...
    gt_theme_538() |>
    tab_header(
      title = paste(year, week_text, week, VoA_text), # ...with this title
      subtitle = "Supremely Excellent Yet Salaciously Godlike And Infallibly Magnificent Vortex of Accuracy")  |>  # and this subtitle
    ##tab_style(style = cell_fill("bisque"),
    ##        locations = cells_body()) |>  # add fill color to table
    fmt_number( # A column (numeric data)
      columns = c(VoA_Rating), # What column variable? FinalVoATop25$VoA_Rating
      decimals = 5 # With four decimal places
    ) |> 
    fmt_number( # Another column (also numeric data)
      columns = c(VoA_Ranking), # What column variable? FinalVoATop25$VoA_Ranking
      decimals = 0 # I want this column to have zero decimal places
    ) |> 
    data_color( # Update cell colors, testing different color palettes
      columns = c(VoA_Rating), # ...for dose column
      fn = scales::col_numeric( # <- bc it's numeric
        palette = brewer.pal(9, "RdBu"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = TRUE
      )
    ) |>
    cols_label(VoA_Rating = "VoA Rating", VoA_Ranking = "VoA Ranking") |> # Update labels
    cols_move_to_end(columns = "VoA_Rating") |>
    cols_hide(c(conference, CFB_Week, VoA_Output)) |>
    tab_footnote(
      footnote = "Table by @gshelor, Data from CFB Data API, ESPN.com, and ESPN's Bill Connelly via cfbfastR, FCS data mostly from stats.ncaa.org"
    )
}

##### Resume VoA #####
## determining mean VoA Rating of top 12 teams in VoA
# choosing top 12 because of future playoff expansion which seems likely if not already certain
# it really should only be 8 max but whatever, I'm gonna be just fine
## Resume VoA only created after week 5 (Week 6 - end of season)
if (as.numeric(week) > 5) {
  Top12 <- VoA_Variables |>
    filter(VoA_Ranking <= 12) |>
    select(season, team, FPI, VoA_Rating, VoA_Ranking)
  Top12_mean <- mean(Top12$VoA_Rating)
  
  
  ## pulling in completed games
  completed_games <- cfbd_game_info(as.numeric(year)) |>
    filter(home_team %in% VoA_Variables$team | away_team %in% VoA_Variables$team) |>
    select(game_id, season, week, neutral_site, completed, home_team, home_points, away_team, away_points) |>
    filter(completed == TRUE)
  
  ## using SRS ratings for FCS teams instead of the randomly sampled VoA rating based on
  # bottom half of VoA ratings as done during 2022 CFB season
  `%nin%` = Negate(`%in%`)
  FCS <- cfbd_ratings_srs(year = as.numeric(year)) |>
    filter(team %nin% VoA_Variables$team) |>
    filter(team %in% completed_games$home_team | team %in% completed_games$away_team)
  
  ## creating separate data frame for each team's schedule
  ## using this data frame to calculate how a team did relative to what VoA would 
  # currently predict
  ## Air Force
  AirForce <- completed_games |>
    filter(home_team == "Air Force" | away_team == "Air Force") |>
    mutate(team = "Air Force",
           team_opp = case_when(home_team == "Air Force" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Air Force"])
  
  ## creating df of VoA/SRS ratings of opponents
  AirForceFBSOpps <- VoA_Variables |>
    filter(team %in% AirForce$team_opp) |>
    select(team, VoA_Rating)
  AirForceFCSOpps <- FCS |>
    filter(team %in% AirForce$team_opp) |>
    select(team, rating)
  colnames(AirForceFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  AirForceOpps <- rbind(AirForceFBSOpps, AirForceFCSOpps)
  colnames(AirForceOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  AirForce <- full_join(AirForce, AirForceOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  AirForce <- AirForce |>
    mutate(actual_diff = case_when(home_team == "Air Force" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Air Force" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Air Force" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Air Force" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  AirForce_losses <- AirForce |>
    filter(home_team == "Air Force" & home_points < away_points | away_team == "Air Force" & away_points < home_points)
  ## storing overall team Resume Score as vector
  AirForce_resume <- sum(AirForce$Resume_Score) - (7 * nrow(AirForce_losses))
  
  
  
  ## Akron
  ## creating separate data frame for each team's schedule
  ## using this data frame to calculate how a team did relative to what VoA would 
  # currently predict
  Akron <- completed_games |>
    filter(home_team == "Akron" | away_team == "Akron") |>
    mutate(team = "Akron",
           team_opp = case_when(home_team == "Akron" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Akron"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  AkronFBSOpps <- VoA_Variables |>
    filter(team %in% Akron$team_opp) |>
    select(team, VoA_Rating)
  AkronFCSOpps <- FCS |>
    filter(team %in% Akron$team_opp) |>
    select(team, rating)
  colnames(AkronFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  AkronOpps <- rbind(AkronFBSOpps, AkronFCSOpps)
  colnames(AkronOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  Akron <- full_join(Akron, AkronOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  Akron <- Akron |>
    mutate(actual_diff = case_when(home_team == "Akron" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Akron" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Akron" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Akron" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  Akron_losses <- Akron |>
    filter(home_team == "Akron" & home_points < away_points | away_team == "Akron" & away_points < home_points)
  ## storing overall team Resume Score as vector
  Akron_resume <- sum(Akron$Resume_Score) - (7 * nrow(Akron_losses))
  
  ## Alabama
  ## creating separate data frame for each team's schedule
  ## using this data frame to calculate how a team did relative to what VoA would 
  # currently predict
  Alabama <- completed_games |>
    filter(home_team == "Alabama" | away_team == "Alabama") |>
    mutate(team = "Alabama",
           team_opp = case_when(home_team == "Alabama" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Alabama"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  AlabamaFBSOpps <- VoA_Variables |>
    filter(team %in% Alabama$team_opp) |>
    select(team, VoA_Rating)
  AlabamaFCSOpps <- FCS |>
    filter(team %in% Alabama$team_opp) |>
    select(team, rating)
  colnames(AlabamaFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  AlabamaOpps <- rbind(AlabamaFBSOpps, AlabamaFCSOpps)
  colnames(AlabamaOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  Alabama <- full_join(Alabama, AlabamaOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  Alabama <- Alabama |>
    mutate(actual_diff = case_when(home_team == "Alabama" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Alabama" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Alabama" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Alabama" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  Alabama_losses <- Alabama |>
    filter(home_team == "Alabama" & home_points < away_points | away_team == "Alabama" & away_points < home_points)
  ## storing overall team Resume Score as vector
  Alabama_resume <- sum(Alabama$Resume_Score) - (7 * nrow(Alabama_losses))
  
  
  ## AppalachianSt
  ## creating separate data frame for each team's schedule
  ## using this data frame to calculate how a team did relative to what VoA would 
  # currently predict
  AppalachianSt <- completed_games |>
    filter(home_team == "Appalachian State" | away_team == "Appalachian State") |>
    mutate(team = "Appalachian State",
           team_opp = case_when(home_team == "Appalachian State" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Appalachian State"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  AppalachianStFBSOpps <- VoA_Variables |>
    filter(team %in% AppalachianSt$team_opp) |>
    select(team, VoA_Rating)
  AppalachianStFCSOpps <- FCS |>
    filter(team %in% AppalachianSt$team_opp) |>
    select(team, rating)
  colnames(AppalachianStFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  AppalachianStOpps <- rbind(AppalachianStFBSOpps, AppalachianStFCSOpps)
  colnames(AppalachianStOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  AppalachianSt <- full_join(AppalachianSt, AppalachianStOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  AppalachianSt <- AppalachianSt |>
    mutate(actual_diff = case_when(home_team == "Appalachian State" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Appalachian State" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Appalachian State" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Appalachian State" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  AppalachianSt_losses <- AppalachianSt |>
    filter(home_team == "Appalachian State" & home_points < away_points | away_team == "Appalachian State" & away_points < home_points)
  ## storing overall team Resume Score as vector
  AppalachianSt_resume <- sum(AppalachianSt$Resume_Score) - (7 * nrow(AppalachianSt_losses))
  
  
  ## Arizona
  ## creating separate data frame for each team's schedule
  ## using this data frame to calculate how a team did relative to what VoA would 
  # currently predict
  Arizona <- completed_games |>
    filter(home_team == "Arizona" | away_team == "Arizona") |>
    mutate(team = "Arizona",
           team_opp = case_when(home_team == "Arizona" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Arizona"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  ArizonaFBSOpps <- VoA_Variables |>
    filter(team %in% Arizona$team_opp) |>
    select(team, VoA_Rating)
  ArizonaFCSOpps <- FCS |>
    filter(team %in% Arizona$team_opp) |>
    select(team, rating)
  colnames(ArizonaFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  ArizonaOpps <- rbind(ArizonaFBSOpps, ArizonaFCSOpps)
  colnames(ArizonaOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  Arizona <- full_join(Arizona, ArizonaOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  Arizona <- Arizona |>
    mutate(actual_diff = case_when(home_team == "Arizona" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Arizona" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Arizona" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Arizona" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  Arizona_losses <- Arizona |>
    filter(home_team == "Arizona" & home_points < away_points | away_team == "Arizona" & away_points < home_points)
  ## storing overall team Resume Score as vector
  Arizona_resume <- sum(Arizona$Resume_Score) - (7 * nrow(Arizona_losses))
  
  
  ## ArizonaSt
  ## creating separate data frame for each team's schedule
  ## using this data frame to calculate how a team did relative to what VoA would 
  # currently predict
  ArizonaSt <- completed_games |>
    filter(home_team == "Arizona State" | away_team == "Arizona State") |>
    mutate(team = "Arizona State",
           team_opp = case_when(home_team == "Arizona State" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Arizona State"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  ArizonaStFBSOpps <- VoA_Variables |>
    filter(team %in% ArizonaSt$team_opp) |>
    select(team, VoA_Rating)
  ArizonaStFCSOpps <- FCS |>
    filter(team %in% ArizonaSt$team_opp) |>
    select(team, rating)
  colnames(ArizonaStFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  ArizonaStOpps <- rbind(ArizonaStFBSOpps, ArizonaStFCSOpps)
  colnames(ArizonaStOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  ArizonaSt <- full_join(ArizonaSt, ArizonaStOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  ArizonaSt <- ArizonaSt |>
    mutate(actual_diff = case_when(home_team == "Arizona State" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Arizona State" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Arizona State" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Arizona State" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  ArizonaSt_losses <- ArizonaSt |>
    filter(home_team == "Arizona State" & home_points < away_points | away_team == "Arizona State" & away_points < home_points)
  ## storing overall team Resume Score as vector
  ArizonaSt_resume <- sum(ArizonaSt$Resume_Score) - (7 * nrow(ArizonaSt_losses))
  
  
  ## Arkansas
  ## creating separate data frame for each team's schedule
  ## using this data frame to calculate how a team did relative to what VoA would 
  # currently predict
  Arkansas <- completed_games |>
    filter(home_team == "Arkansas" | away_team == "Arkansas") |>
    mutate(team = "Arkansas",
           team_opp = case_when(home_team == "Arkansas" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Arkansas"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  ArkansasFBSOpps <- VoA_Variables |>
    filter(team %in% Arkansas$team_opp) |>
    select(team, VoA_Rating)
  ArkansasFCSOpps <- FCS |>
    filter(team %in% Arkansas$team_opp) |>
    select(team, rating)
  colnames(ArkansasFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  ArkansasOpps <- rbind(ArkansasFBSOpps, ArkansasFCSOpps)
  colnames(ArkansasOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  Arkansas <- full_join(Arkansas, ArkansasOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  Arkansas <- Arkansas |>
    mutate(actual_diff = case_when(home_team == "Arkansas" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Arkansas" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Arkansas" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Arkansas" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  Arkansas_losses <- Arkansas |>
    filter(home_team == "Arkansas" & home_points < away_points | away_team == "Arkansas" & away_points < home_points)
  ## storing overall team Resume Score as vector
  Arkansas_resume <- sum(Arkansas$Resume_Score) - (7 * nrow(Arkansas_losses))
  
  
  ## ArkansasSt
  ## creating separate data frame for each team's schedule
  ## using this data frame to calculate how a team did relative to what VoA would 
  # currently predict
  ArkansasSt <- completed_games |>
    filter(home_team == "Arkansas State" | away_team == "Arkansas State") |>
    mutate(team = "Arkansas State",
           team_opp = case_when(home_team == "Arkansas State" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Arkansas State"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  ArkansasStFBSOpps <- VoA_Variables |>
    filter(team %in% ArkansasSt$team_opp) |>
    select(team, VoA_Rating)
  ArkansasStFCSOpps <- FCS |>
    filter(team %in% ArkansasSt$team_opp) |>
    select(team, rating)
  colnames(ArkansasStFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  ArkansasStOpps <- rbind(ArkansasStFBSOpps, ArkansasStFCSOpps)
  colnames(ArkansasStOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  ArkansasSt <- full_join(ArkansasSt, ArkansasStOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  ArkansasSt <- ArkansasSt |>
    mutate(actual_diff = case_when(home_team == "Arkansas State" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Arkansas State" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Arkansas State" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Arkansas State" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  ArkansasSt_losses <- ArkansasSt |>
    filter(home_team == "Arkansas State" & home_points < away_points | away_team == "Arkansas State" & away_points < home_points)
  ## storing overall team Resume Score as vector
  ArkansasSt_resume <- sum(ArkansasSt$Resume_Score) - (7 * nrow(ArkansasSt_losses))
  
  ## Army
  ## creating separate data frame for each team's schedule
  ## using this data frame to calculate how a team did relative to what VoA would 
  # currently predict
  Army <- completed_games |>
    filter(home_team == "Army" | away_team == "Army") |>
    mutate(team = "Army",
           team_opp = case_when(home_team == "Army" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Army"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  ArmyFBSOpps <- VoA_Variables |>
    filter(team %in% Army$team_opp) |>
    select(team, VoA_Rating)
  ArmyFCSOpps <- FCS |>
    filter(team %in% Army$team_opp) |>
    select(team, rating)
  colnames(ArmyFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  ArmyOpps <- rbind(ArmyFBSOpps, ArmyFCSOpps)
  colnames(ArmyOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  Army <- full_join(Army, ArmyOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  Army <- Army |>
    mutate(actual_diff = case_when(home_team == "Army" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Army" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Army" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Army" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  Army_losses <- Army |>
    filter(home_team == "Army" & home_points < away_points | away_team == "Army" & away_points < home_points)
  ## storing overall team Resume Score as vector
  Army_resume <- sum(Army$Resume_Score) - (7 * nrow(Army_losses))
  
  ## Auburn
  ## creating separate data frame for each team's schedule
  ## using this data frame to calculate how a team did relative to what VoA would 
  # currently predict
  Auburn <- completed_games |>
    filter(home_team == "Auburn" | away_team == "Auburn") |>
    mutate(team = "Auburn",
           team_opp = case_when(home_team == "Auburn" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Auburn"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  AuburnFBSOpps <- VoA_Variables |>
    filter(team %in% Auburn$team_opp) |>
    select(team, VoA_Rating)
  AuburnFCSOpps <- FCS |>
    filter(team %in% Auburn$team_opp) |>
    select(team, rating)
  colnames(AuburnFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  AuburnOpps <- rbind(AuburnFBSOpps, AuburnFCSOpps)
  colnames(AuburnOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  Auburn <- full_join(Auburn, AuburnOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  Auburn <- Auburn |>
    mutate(actual_diff = case_when(home_team == "Auburn" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Auburn" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Auburn" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Auburn" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  Auburn_losses <- Auburn |>
    filter(home_team == "Auburn" & home_points < away_points | away_team == "Auburn" & away_points < home_points)
  ## storing overall team Resume Score as vector
  Auburn_resume <- sum(Auburn$Resume_Score) - (7 * nrow(Auburn_losses))
  
  
  ## BallSt
  BallSt <- completed_games |>
    filter(home_team == "Ball State" | away_team == "Ball State") |>
    mutate(team = "Ball State",
           team_opp = case_when(home_team == "Ball State" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Ball State"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  BallStFBSOpps <- VoA_Variables |>
    filter(team %in% BallSt$team_opp) |>
    select(team, VoA_Rating)
  BallStFCSOpps <- FCS |>
    filter(team %in% BallSt$team_opp) |>
    select(team, rating)
  colnames(BallStFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  BallStOpps <- rbind(BallStFBSOpps, BallStFCSOpps)
  colnames(BallStOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  BallSt <- full_join(BallSt, BallStOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  BallSt <- BallSt |>
    mutate(actual_diff = case_when(home_team == "Ball State" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Ball State" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Ball State" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Ball State" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  BallSt_losses <- BallSt |>
    filter(home_team == "Ball State" & home_points < away_points | away_team == "Ball State" & away_points < home_points)
  ## storing overall team Resume Score as vector
  BallSt_resume <- sum(BallSt$Resume_Score) - (7 * nrow(BallSt_losses))
  
  
  ## Baylor
  Baylor <- completed_games |>
    filter(home_team == "Baylor" | away_team == "Baylor") |>
    mutate(team = "Baylor",
           team_opp = case_when(home_team == "Baylor" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Baylor"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  BaylorFBSOpps <- VoA_Variables |>
    filter(team %in% Baylor$team_opp) |>
    select(team, VoA_Rating)
  BaylorFCSOpps <- FCS |>
    filter(team %in% Baylor$team_opp) |>
    select(team, rating)
  colnames(BaylorFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  BaylorOpps <- rbind(BaylorFBSOpps, BaylorFCSOpps)
  colnames(BaylorOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  Baylor <- full_join(Baylor, BaylorOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  Baylor <- Baylor |>
    mutate(actual_diff = case_when(home_team == "Baylor" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Baylor" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Baylor" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Baylor" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  Baylor_losses <- Baylor |>
    filter(home_team == "Baylor" & home_points < away_points | away_team == "Baylor" & away_points < home_points)
  ## storing overall team Resume Score as vector
  Baylor_resume <- sum(Baylor$Resume_Score) - (7 * nrow(Baylor_losses))
  
  
  ## BoiseSt
  BoiseSt <- completed_games |>
    filter(home_team == "Boise State" | away_team == "Boise State") |>
    mutate(team = "Boise State",
           team_opp = case_when(home_team == "Boise State" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Boise State"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  BoiseStFBSOpps <- VoA_Variables |>
    filter(team %in% BoiseSt$team_opp) |>
    select(team, VoA_Rating)
  BoiseStFCSOpps <- FCS |>
    filter(team %in% BoiseSt$team_opp) |>
    select(team, rating)
  colnames(BoiseStFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  BoiseStOpps <- rbind(BoiseStFBSOpps, BoiseStFCSOpps)
  colnames(BoiseStOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  BoiseSt <- full_join(BoiseSt, BoiseStOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  BoiseSt <- BoiseSt |>
    mutate(actual_diff = case_when(home_team == "Boise State" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Boise State" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Boise State" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Boise State" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  BoiseSt_losses <- BoiseSt |>
    filter(home_team == "Boise State" & home_points < away_points | away_team == "Boise State" & away_points < home_points)
  ## storing overall team Resume Score as vector
  BoiseSt_resume <- sum(BoiseSt$Resume_Score) - (7 * nrow(BoiseSt_losses))
  
  
  ## BC
  BC <- completed_games |>
    filter(home_team == "Boston College" | away_team == "Boston College") |>
    mutate(team = "Boston College",
           team_opp = case_when(home_team == "Boston College" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Boston College"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  BCFBSOpps <- VoA_Variables |>
    filter(team %in% BC$team_opp) |>
    select(team, VoA_Rating)
  BCFCSOpps <- FCS |>
    filter(team %in% BC$team_opp) |>
    select(team, rating)
  colnames(BCFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  BCOpps <- rbind(BCFBSOpps, BCFCSOpps)
  colnames(BCOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  BC <- full_join(BC, BCOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  BC <- BC |>
    mutate(actual_diff = case_when(home_team == "Boston College" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Boston College" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Boston College" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Boston College" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  BC_losses <- BC |>
    filter(home_team == "Boston College" & home_points < away_points | away_team == "Boston College" & away_points < home_points)
  ## storing overall team Resume Score as vector
  BC_resume <- sum(BC$Resume_Score) - (7 * nrow(BC_losses))
  
  
  ## BowlingGreen
  BowlingGreen <- completed_games |>
    filter(home_team == "Bowling Green" | away_team == "Bowling Green") |>
    mutate(team = "Bowling Green",
           team_opp = case_when(home_team == "Bowling Green" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Bowling Green"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  BowlingGreenFBSOpps <- VoA_Variables |>
    filter(team %in% BowlingGreen$team_opp) |>
    select(team, VoA_Rating)
  BowlingGreenFCSOpps <- FCS |>
    filter(team %in% BowlingGreen$team_opp) |>
    select(team, rating)
  colnames(BowlingGreenFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  BowlingGreenOpps <- rbind(BowlingGreenFBSOpps, BowlingGreenFCSOpps)
  colnames(BowlingGreenOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  BowlingGreen <- full_join(BowlingGreen, BowlingGreenOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  BowlingGreen <- BowlingGreen |>
    mutate(actual_diff = case_when(home_team == "Bowling Green" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Bowling Green" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Bowling Green" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Bowling Green" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  BowlingGreen_losses <- BowlingGreen |>
    filter(home_team == "Bowling Green" & home_points < away_points | away_team == "Bowling Green" & away_points < home_points)
  ## storing overall team Resume Score as vector
  BowlingGreen_resume <- sum(BowlingGreen$Resume_Score) - (7 * nrow(BowlingGreen_losses))
  
  
  ## Buffalo
  Buffalo <- completed_games |>
    filter(home_team == "Buffalo" | away_team == "Buffalo") |>
    mutate(team = "Buffalo",
           team_opp = case_when(home_team == "Buffalo" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Buffalo"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  BuffaloFBSOpps <- VoA_Variables |>
    filter(team %in% Buffalo$team_opp) |>
    select(team, VoA_Rating)
  BuffaloFCSOpps <- FCS |>
    filter(team %in% Buffalo$team_opp) |>
    select(team, rating)
  colnames(BuffaloFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  BuffaloOpps <- rbind(BuffaloFBSOpps, BuffaloFCSOpps)
  colnames(BuffaloOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  Buffalo <- full_join(Buffalo, BuffaloOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  Buffalo <- Buffalo |>
    mutate(actual_diff = case_when(home_team == "Buffalo" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Buffalo" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Buffalo" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Buffalo" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  Buffalo_losses <- Buffalo |>
    filter(home_team == "Buffalo" & home_points < away_points | away_team == "Buffalo" & away_points < home_points)
  ## storing overall team Resume Score as vector
  Buffalo_resume <- sum(Buffalo$Resume_Score) - (7 * nrow(Buffalo_losses))
  
  
  ## BYU
  BYU <- completed_games |>
    filter(home_team == "BYU" | away_team == "BYU") |>
    mutate(team = "BYU",
           team_opp = case_when(home_team == "BYU" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "BYU"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  BYUFBSOpps <- VoA_Variables |>
    filter(team %in% BYU$team_opp) |>
    select(team, VoA_Rating)
  BYUFCSOpps <- FCS |>
    filter(team %in% BYU$team_opp) |>
    select(team, rating)
  colnames(BYUFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  BYUOpps <- rbind(BYUFBSOpps, BYUFCSOpps)
  colnames(BYUOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  BYU <- full_join(BYU, BYUOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  BYU <- BYU |>
    mutate(actual_diff = case_when(home_team == "BYU" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "BYU" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "BYU" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "BYU" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  BYU_losses <- BYU |>
    filter(home_team == "BYU" & home_points < away_points | away_team == "BYU" & away_points < home_points)
  ## storing overall team Resume Score as vector
  BYU_resume <- sum(BYU$Resume_Score) - (7 * nrow(BYU_losses))
  
  
  ## California
  California <- completed_games |>
    filter(home_team == "California" | away_team == "California") |>
    mutate(team = "California",
           team_opp = case_when(home_team == "California" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "California"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  CaliforniaFBSOpps <- VoA_Variables |>
    filter(team %in% California$team_opp) |>
    select(team, VoA_Rating)
  CaliforniaFCSOpps <- FCS |>
    filter(team %in% California$team_opp) |>
    select(team, rating)
  colnames(CaliforniaFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  CaliforniaOpps <- rbind(CaliforniaFBSOpps, CaliforniaFCSOpps)
  colnames(CaliforniaOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  California <- full_join(California, CaliforniaOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  California <- California |>
    mutate(actual_diff = case_when(home_team == "California" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "California" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "California" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "California" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  California_losses <- California |>
    filter(home_team == "California" & home_points < away_points | away_team == "California" & away_points < home_points)
  ## storing overall team Resume Score as vector
  California_resume <- sum(California$Resume_Score) - (7 * nrow(California_losses))
  
  
  ## CMU
  CMU <- completed_games |>
    filter(home_team == "Central Michigan" | away_team == "Central Michigan") |>
    mutate(team = "Central Michigan",
           team_opp = case_when(home_team == "Central Michigan" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Central Michigan"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  CMUFBSOpps <- VoA_Variables |>
    filter(team %in% CMU$team_opp) |>
    select(team, VoA_Rating)
  CMUFCSOpps <- FCS |>
    filter(team %in% CMU$team_opp) |>
    select(team, rating)
  colnames(CMUFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  CMUOpps <- rbind(CMUFBSOpps, CMUFCSOpps)
  colnames(CMUOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  CMU <- full_join(CMU, CMUOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  CMU <- CMU |>
    mutate(actual_diff = case_when(home_team == "Central Michigan" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Central Michigan" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Central Michigan" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Central Michigan" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  CMU_losses <- CMU |>
    filter(home_team == "Central Michigan" & home_points < away_points | away_team == "Central Michigan" & away_points < home_points)
  ## storing overall team Resume Score as vector
  CMU_resume <- sum(CMU$Resume_Score) - (7 * nrow(CMU_losses))
  
  
  ## Charlotte
  Charlotte <- completed_games |>
    filter(home_team == "Charlotte" | away_team == "Charlotte") |>
    mutate(team = "Charlotte",
           team_opp = case_when(home_team == "Charlotte" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Charlotte"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  CharlotteFBSOpps <- VoA_Variables |>
    filter(team %in% Charlotte$team_opp) |>
    select(team, VoA_Rating)
  CharlotteFCSOpps <- FCS |>
    filter(team %in% Charlotte$team_opp) |>
    select(team, rating)
  colnames(CharlotteFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  CharlotteOpps <- rbind(CharlotteFBSOpps, CharlotteFCSOpps)
  colnames(CharlotteOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  Charlotte <- full_join(Charlotte, CharlotteOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  Charlotte <- Charlotte |>
    mutate(actual_diff = case_when(home_team == "Charlotte" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Charlotte" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Charlotte" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Charlotte" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  Charlotte_losses <- Charlotte |>
    filter(home_team == "Charlotte" & home_points < away_points | away_team == "Charlotte" & away_points < home_points)
  ## storing overall team Resume Score as vector
  Charlotte_resume <- sum(Charlotte$Resume_Score) - (7 * nrow(Charlotte_losses))
  
  
  ## Cincinnati
  Cincinnati <- completed_games |>
    filter(home_team == "Cincinnati" | away_team == "Cincinnati") |>
    mutate(team = "Cincinnati",
           team_opp = case_when(home_team == "Cincinnati" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Cincinnati"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  CincinnatiFBSOpps <- VoA_Variables |>
    filter(team %in% Cincinnati$team_opp) |>
    select(team, VoA_Rating)
  CincinnatiFCSOpps <- FCS |>
    filter(team %in% Cincinnati$team_opp) |>
    select(team, rating)
  colnames(CincinnatiFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  CincinnatiOpps <- rbind(CincinnatiFBSOpps, CincinnatiFCSOpps)
  colnames(CincinnatiOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  Cincinnati <- full_join(Cincinnati, CincinnatiOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  Cincinnati <- Cincinnati |>
    mutate(actual_diff = case_when(home_team == "Cincinnati" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Cincinnati" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Cincinnati" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Cincinnati" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  Cincinnati_losses <- Cincinnati |>
    filter(home_team == "Cincinnati" & home_points < away_points | away_team == "Cincinnati" & away_points < home_points)
  ## storing overall team Resume Score as vector
  Cincinnati_resume <- sum(Cincinnati$Resume_Score) - (7 * nrow(Cincinnati_losses))
  
  
  ## Clemson
  Clemson <- completed_games |>
    filter(home_team == "Clemson" | away_team == "Clemson") |>
    mutate(team = "Clemson",
           team_opp = case_when(home_team == "Clemson" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Clemson"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  ClemsonFBSOpps <- VoA_Variables |>
    filter(team %in% Clemson$team_opp) |>
    select(team, VoA_Rating)
  ClemsonFCSOpps <- FCS |>
    filter(team %in% Clemson$team_opp) |>
    select(team, rating)
  colnames(ClemsonFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  ClemsonOpps <- rbind(ClemsonFBSOpps, ClemsonFCSOpps)
  colnames(ClemsonOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  Clemson <- full_join(Clemson, ClemsonOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  Clemson <- Clemson |>
    mutate(actual_diff = case_when(home_team == "Clemson" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Clemson" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Clemson" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Clemson" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  Clemson_losses <- Clemson |>
    filter(home_team == "Clemson" & home_points < away_points | away_team == "Clemson" & away_points < home_points)
  ## storing overall team Resume Score as vector
  Clemson_resume <- sum(Clemson$Resume_Score) - (7 * nrow(Clemson_losses))
  
  
  ## CoastalCarolina
  CoastalCarolina <- completed_games |>
    filter(home_team == "Coastal Carolina" | away_team == "Coastal Carolina") |>
    mutate(team = "Coastal Carolina",
           team_opp = case_when(home_team == "Coastal Carolina" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Coastal Carolina"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  CoastalCarolinaFBSOpps <- VoA_Variables |>
    filter(team %in% CoastalCarolina$team_opp) |>
    select(team, VoA_Rating)
  CoastalCarolinaFCSOpps <- FCS |>
    filter(team %in% CoastalCarolina$team_opp) |>
    select(team, rating)
  colnames(CoastalCarolinaFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  CoastalCarolinaOpps <- rbind(CoastalCarolinaFBSOpps, CoastalCarolinaFCSOpps)
  colnames(CoastalCarolinaOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  CoastalCarolina <- full_join(CoastalCarolina, CoastalCarolinaOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  CoastalCarolina <- CoastalCarolina |>
    mutate(actual_diff = case_when(home_team == "Coastal Carolina" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Coastal Carolina" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Coastal Carolina" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Coastal Carolina" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  CoastalCarolina_losses <- CoastalCarolina |>
    filter(home_team == "Coastal Carolina" & home_points < away_points | away_team == "Coastal Carolina" & away_points < home_points)
  ## storing overall team Resume Score as vector
  CoastalCarolina_resume <- sum(CoastalCarolina$Resume_Score) - (7 * nrow(CoastalCarolina_losses))
  
  
  ## Colorado
  Colorado <- completed_games |>
    filter(home_team == "Colorado" | away_team == "Colorado") |>
    mutate(team = "Colorado",
           team_opp = case_when(home_team == "Colorado" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Colorado"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  ColoradoFBSOpps <- VoA_Variables |>
    filter(team %in% Colorado$team_opp) |>
    select(team, VoA_Rating)
  ColoradoFCSOpps <- FCS |>
    filter(team %in% Colorado$team_opp) |>
    select(team, rating)
  colnames(ColoradoFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  ColoradoOpps <- rbind(ColoradoFBSOpps, ColoradoFCSOpps)
  colnames(ColoradoOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  Colorado <- full_join(Colorado, ColoradoOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  Colorado <- Colorado |>
    mutate(actual_diff = case_when(home_team == "Colorado" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Colorado" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Colorado" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Colorado" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  Colorado_losses <- Colorado |>
    filter(home_team == "Colorado" & home_points < away_points | away_team == "Colorado" & away_points < home_points)
  ## storing overall team Resume Score as vector
  Colorado_resume <- sum(Colorado$Resume_Score) - (7 * nrow(Colorado_losses))
  
  
  ## ColoradoSt
  ColoradoSt <- completed_games |>
    filter(home_team == "Colorado State" | away_team == "Colorado State") |>
    mutate(team = "Colorado State",
           team_opp = case_when(home_team == "Colorado State" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Colorado State"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  ColoradoStFBSOpps <- VoA_Variables |>
    filter(team %in% ColoradoSt$team_opp) |>
    select(team, VoA_Rating)
  ColoradoStFCSOpps <- FCS |>
    filter(team %in% ColoradoSt$team_opp) |>
    select(team, rating)
  colnames(ColoradoStFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  ColoradoStOpps <- rbind(ColoradoStFBSOpps, ColoradoStFCSOpps)
  colnames(ColoradoStOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  ColoradoSt <- full_join(ColoradoSt, ColoradoStOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  ColoradoSt <- ColoradoSt |>
    mutate(actual_diff = case_when(home_team == "Colorado State" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Colorado State" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Colorado State" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Colorado State" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  ColoradoSt_losses <- ColoradoSt |>
    filter(home_team == "Colorado State" & home_points < away_points | away_team == "Colorado State" & away_points < home_points)
  ## storing overall team Resume Score as vector
  ColoradoSt_resume <- sum(ColoradoSt$Resume_Score) - (7 * nrow(ColoradoSt_losses))
  
  
  ## UConn
  Connecticut <- completed_games |>
    filter(home_team == "Connecticut" | away_team == "Connecticut") |>
    mutate(team = "Connecticut",
           team_opp = case_when(home_team == "Connecticut" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Connecticut"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  ConnecticutFBSOpps <- VoA_Variables |>
    filter(team %in% Connecticut$team_opp) |>
    select(team, VoA_Rating)
  ConnecticutFCSOpps <- FCS |>
    filter(team %in% Connecticut$team_opp) |>
    select(team, rating)
  colnames(ConnecticutFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  ConnecticutOpps <- rbind(ConnecticutFBSOpps, ConnecticutFCSOpps)
  colnames(ConnecticutOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  Connecticut <- full_join(Connecticut, ConnecticutOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  Connecticut <- Connecticut |>
    mutate(actual_diff = case_when(home_team == "Connecticut" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Connecticut" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Connecticut" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Connecticut" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  Connecticut_losses <- Connecticut |>
    filter(home_team == "Connecticut" & home_points < away_points | away_team == "Connecticut" & away_points < home_points)
  ## storing overall team Resume Score as vector
  Connecticut_resume <- sum(Connecticut$Resume_Score) - (7 * nrow(Connecticut_losses))
  
  
  ## Duke
  Duke <- completed_games |>
    filter(home_team == "Duke" | away_team == "Duke") |>
    mutate(team = "Duke",
           team_opp = case_when(home_team == "Duke" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Duke"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  DukeFBSOpps <- VoA_Variables |>
    filter(team %in% Duke$team_opp) |>
    select(team, VoA_Rating)
  DukeFCSOpps <- FCS |>
    filter(team %in% Duke$team_opp) |>
    select(team, rating)
  colnames(DukeFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  DukeOpps <- rbind(DukeFBSOpps, DukeFCSOpps)
  colnames(DukeOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  Duke <- full_join(Duke, DukeOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  Duke <- Duke |>
    mutate(actual_diff = case_when(home_team == "Duke" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Duke" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Duke" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Duke" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  Duke_losses <- Duke |>
    filter(home_team == "Duke" & home_points < away_points | away_team == "Duke" & away_points < home_points)
  ## storing overall team Resume Score as vector
  Duke_resume <- sum(Duke$Resume_Score) - (7 * nrow(Duke_losses))
  
  
  ## EastCarolina
  EastCarolina <- completed_games |>
    filter(home_team == "East Carolina" | away_team == "East Carolina") |>
    mutate(team = "East Carolina",
           team_opp = case_when(home_team == "East Carolina" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "East Carolina"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  EastCarolinaFBSOpps <- VoA_Variables |>
    filter(team %in% EastCarolina$team_opp) |>
    select(team, VoA_Rating)
  EastCarolinaFCSOpps <- FCS |>
    filter(team %in% EastCarolina$team_opp) |>
    select(team, rating)
  colnames(EastCarolinaFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  EastCarolinaOpps <- rbind(EastCarolinaFBSOpps, EastCarolinaFCSOpps)
  colnames(EastCarolinaOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  EastCarolina <- full_join(EastCarolina, EastCarolinaOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  EastCarolina <- EastCarolina |>
    mutate(actual_diff = case_when(home_team == "East Carolina" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "East Carolina" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "East Carolina" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "East Carolina" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  EastCarolina_losses <- EastCarolina |>
    filter(home_team == "East Carolina" & home_points < away_points | away_team == "East Carolina" & away_points < home_points)
  ## storing overall team Resume Score as vector
  EastCarolina_resume <- sum(EastCarolina$Resume_Score) - (7 * nrow(EastCarolina_losses))
  
  
  ## EMU
  EMU <- completed_games |>
    filter(home_team == "Eastern Michigan" | away_team == "Eastern Michigan") |>
    mutate(team = "Eastern Michigan",
           team_opp = case_when(home_team == "Eastern Michigan" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Eastern Michigan"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  EMUFBSOpps <- VoA_Variables |>
    filter(team %in% EMU$team_opp) |>
    select(team, VoA_Rating)
  EMUFCSOpps <- FCS |>
    filter(team %in% EMU$team_opp) |>
    select(team, rating)
  colnames(EMUFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  EMUOpps <- rbind(EMUFBSOpps, EMUFCSOpps)
  colnames(EMUOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  EMU <- full_join(EMU, EMUOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  EMU <- EMU |>
    mutate(actual_diff = case_when(home_team == "Eastern Michigan" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Eastern Michigan" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Eastern Michigan" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Eastern Michigan" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  EMU_losses <- EMU |>
    filter(home_team == "Eastern Michigan" & home_points < away_points | away_team == "Eastern Michigan" & away_points < home_points)
  ## storing overall team Resume Score as vector
  EMU_resume <- sum(EMU$Resume_Score) - (7 * nrow(EMU_losses))
  
  
  ## Florida
  Florida <- completed_games |>
    filter(home_team == "Florida" | away_team == "Florida") |>
    mutate(team = "Florida",
           team_opp = case_when(home_team == "Florida" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Florida"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  FloridaFBSOpps <- VoA_Variables |>
    filter(team %in% Florida$team_opp) |>
    select(team, VoA_Rating)
  FloridaFCSOpps <- FCS |>
    filter(team %in% Florida$team_opp) |>
    select(team, rating)
  colnames(FloridaFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  FloridaOpps <- rbind(FloridaFBSOpps, FloridaFCSOpps)
  colnames(FloridaOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  Florida <- full_join(Florida, FloridaOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  Florida <- Florida |>
    mutate(actual_diff = case_when(home_team == "Florida" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Florida" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Florida" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Florida" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  Florida_losses <- Florida |>
    filter(home_team == "Florida" & home_points < away_points | away_team == "Florida" & away_points < home_points)
  ## storing overall team Resume Score as vector
  Florida_resume <- sum(Florida$Resume_Score) - (7 * nrow(Florida_losses))
  
  
  ## FAU
  FAU <- completed_games |>
    filter(home_team == "Florida Atlantic" | away_team == "Florida Atlantic") |>
    mutate(team = "Florida Atlantic",
           team_opp = case_when(home_team == "Florida Atlantic" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Florida Atlantic"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  FAUFBSOpps <- VoA_Variables |>
    filter(team %in% FAU$team_opp) |>
    select(team, VoA_Rating)
  FAUFCSOpps <- FCS |>
    filter(team %in% FAU$team_opp) |>
    select(team, rating)
  colnames(FAUFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  FAUOpps <- rbind(FAUFBSOpps, FAUFCSOpps)
  colnames(FAUOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  FAU <- full_join(FAU, FAUOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  FAU <- FAU |>
    mutate(actual_diff = case_when(home_team == "Florida Atlantic" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Florida Atlantic" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Florida Atlantic" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Florida Atlantic" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  FAU_losses <- FAU |>
    filter(home_team == "Florida Atlantic" & home_points < away_points | away_team == "Florida Atlantic" & away_points < home_points)
  ## storing overall team Resume Score as vector
  FAU_resume <- sum(FAU$Resume_Score) - (7 * nrow(FAU_losses))
  
  
  ## FIU
  FIU <- completed_games |>
    filter(home_team == "Florida International" | away_team == "Florida International") |>
    mutate(team = "Florida International",
           team_opp = case_when(home_team == "Florida International" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Florida International"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  FIUFBSOpps <- VoA_Variables |>
    filter(team %in% FIU$team_opp) |>
    select(team, VoA_Rating)
  FIUFCSOpps <- FCS |>
    filter(team %in% FIU$team_opp) |>
    select(team, rating)
  colnames(FIUFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  FIUOpps <- rbind(FIUFBSOpps, FIUFCSOpps)
  colnames(FIUOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  FIU <- full_join(FIU, FIUOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  FIU <- FIU |>
    mutate(actual_diff = case_when(home_team == "Florida International" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Florida International" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Florida International" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Florida International" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  FIU_losses <- FIU |>
    filter(home_team == "Florida International" & home_points < away_points | away_team == "Florida International" & away_points < home_points)
  ## storing overall team Resume Score as vector
  FIU_resume <- sum(FIU$Resume_Score) - (7 * nrow(FIU_losses))
  
  
  ## FloridaSt
  FloridaSt <- completed_games |>
    filter(home_team == "Florida State" | away_team == "Florida State") |>
    mutate(team = "Florida State",
           team_opp = case_when(home_team == "Florida State" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Florida State"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  FloridaStFBSOpps <- VoA_Variables |>
    filter(team %in% FloridaSt$team_opp) |>
    select(team, VoA_Rating)
  FloridaStFCSOpps <- FCS |>
    filter(team %in% FloridaSt$team_opp) |>
    select(team, rating)
  colnames(FloridaStFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  FloridaStOpps <- rbind(FloridaStFBSOpps, FloridaStFCSOpps)
  colnames(FloridaStOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  FloridaSt <- full_join(FloridaSt, FloridaStOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  FloridaSt <- FloridaSt |>
    mutate(actual_diff = case_when(home_team == "Florida State" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Florida State" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Florida State" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Florida State" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  FloridaSt_losses <- FloridaSt |>
    filter(home_team == "Florida State" & home_points < away_points | away_team == "Florida State" & away_points < home_points)
  ## storing overall team Resume Score as vector
  FloridaSt_resume <- sum(FloridaSt$Resume_Score) - (7 * nrow(FloridaSt_losses))
  
  
  ## FresnoSt
  FresnoSt <- completed_games |>
    filter(home_team == "Fresno State" | away_team == "Fresno State") |>
    mutate(team = "Fresno State",
           team_opp = case_when(home_team == "Fresno State" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Fresno State"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  FresnoStFBSOpps <- VoA_Variables |>
    filter(team %in% FresnoSt$team_opp) |>
    select(team, VoA_Rating)
  FresnoStFCSOpps <- FCS |>
    filter(team %in% FresnoSt$team_opp) |>
    select(team, rating)
  colnames(FresnoStFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  FresnoStOpps <- rbind(FresnoStFBSOpps, FresnoStFCSOpps)
  colnames(FresnoStOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  FresnoSt <- full_join(FresnoSt, FresnoStOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  FresnoSt <- FresnoSt |>
    mutate(actual_diff = case_when(home_team == "Fresno State" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Fresno State" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Fresno State" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Fresno State" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  FresnoSt_losses <- FresnoSt |>
    filter(home_team == "Fresno State" & home_points < away_points | away_team == "Fresno State" & away_points < home_points)
  ## storing overall team Resume Score as vector
  FresnoSt_resume <- sum(FresnoSt$Resume_Score) - (7 * nrow(FresnoSt_losses))
  
  
  ## Georgia
  Georgia <- completed_games |>
    filter(home_team == "Georgia" | away_team == "Georgia") |>
    mutate(team = "Georgia",
           team_opp = case_when(home_team == "Georgia" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Georgia"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  GeorgiaFBSOpps <- VoA_Variables |>
    filter(team %in% Georgia$team_opp) |>
    select(team, VoA_Rating)
  GeorgiaFCSOpps <- FCS |>
    filter(team %in% Georgia$team_opp) |>
    select(team, rating)
  colnames(GeorgiaFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  GeorgiaOpps <- rbind(GeorgiaFBSOpps, GeorgiaFCSOpps)
  colnames(GeorgiaOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  Georgia <- full_join(Georgia, GeorgiaOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  Georgia <- Georgia |>
    mutate(actual_diff = case_when(home_team == "Georgia" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Georgia" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Georgia" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Georgia" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  Georgia_losses <- Georgia |>
    filter(home_team == "Georgia" & home_points < away_points | away_team == "Georgia" & away_points < home_points)
  ## storing overall team Resume Score as vector
  Georgia_resume <- sum(Georgia$Resume_Score) - (7 * nrow(Georgia_losses))
  
  
  ## GeorgiaSouthern
  GeorgiaSouthern <- completed_games |>
    filter(home_team == "Georgia Southern" | away_team == "Georgia Southern") |>
    mutate(team = "Georgia Southern",
           team_opp = case_when(home_team == "Georgia Southern" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Georgia Southern"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  GeorgiaSouthernFBSOpps <- VoA_Variables |>
    filter(team %in% GeorgiaSouthern$team_opp) |>
    select(team, VoA_Rating)
  GeorgiaSouthernFCSOpps <- FCS |>
    filter(team %in% GeorgiaSouthern$team_opp) |>
    select(team, rating)
  colnames(GeorgiaSouthernFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  GeorgiaSouthernOpps <- rbind(GeorgiaSouthernFBSOpps, GeorgiaSouthernFCSOpps)
  colnames(GeorgiaSouthernOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  GeorgiaSouthern <- full_join(GeorgiaSouthern, GeorgiaSouthernOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  GeorgiaSouthern <- GeorgiaSouthern |>
    mutate(actual_diff = case_when(home_team == "Georgia Southern" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Georgia Southern" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Georgia Southern" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Georgia Southern" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  GeorgiaSouthern_losses <- GeorgiaSouthern |>
    filter(home_team == "Georgia Southern" & home_points < away_points | away_team == "Georgia Southern" & away_points < home_points)
  ## storing overall team Resume Score as vector
  GeorgiaSouthern_resume <- sum(GeorgiaSouthern$Resume_Score) - (7 * nrow(GeorgiaSouthern_losses))
  
  
  ## GeorgiaSt
  GeorgiaSt <- completed_games |>
    filter(home_team == "Georgia State" | away_team == "Georgia State") |>
    mutate(team = "Georgia State",
           team_opp = case_when(home_team == "Georgia State" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Georgia State"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  GeorgiaStFBSOpps <- VoA_Variables |>
    filter(team %in% GeorgiaSt$team_opp) |>
    select(team, VoA_Rating)
  GeorgiaStFCSOpps <- FCS |>
    filter(team %in% GeorgiaSt$team_opp) |>
    select(team, rating)
  colnames(GeorgiaStFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  GeorgiaStOpps <- rbind(GeorgiaStFBSOpps, GeorgiaStFCSOpps)
  colnames(GeorgiaStOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  GeorgiaSt <- full_join(GeorgiaSt, GeorgiaStOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  GeorgiaSt <- GeorgiaSt |>
    mutate(actual_diff = case_when(home_team == "Georgia State" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Georgia State" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Georgia State" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Georgia State" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  GeorgiaSt_losses <- GeorgiaSt |>
    filter(home_team == "Georgia State" & home_points < away_points | away_team == "Georgia State" & away_points < home_points)
  ## storing overall team Resume Score as vector
  GeorgiaSt_resume <- sum(GeorgiaSt$Resume_Score) - (7 * nrow(GeorgiaSt_losses))
  
  
  ## GeorgiaTech
  GeorgiaTech <- completed_games |>
    filter(home_team == "Georgia Tech" | away_team == "Georgia Tech") |>
    mutate(team = "Georgia Tech",
           team_opp = case_when(home_team == "Georgia Tech" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Georgia Tech"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  GeorgiaTechFBSOpps <- VoA_Variables |>
    filter(team %in% GeorgiaTech$team_opp) |>
    select(team, VoA_Rating)
  GeorgiaTechFCSOpps <- FCS |>
    filter(team %in% GeorgiaTech$team_opp) |>
    select(team, rating)
  colnames(GeorgiaTechFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  GeorgiaTechOpps <- rbind(GeorgiaTechFBSOpps, GeorgiaTechFCSOpps)
  colnames(GeorgiaTechOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  GeorgiaTech <- full_join(GeorgiaTech, GeorgiaTechOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  GeorgiaTech <- GeorgiaTech |>
    mutate(actual_diff = case_when(home_team == "Georgia Tech" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Georgia Tech" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Georgia Tech" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Georgia Tech" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  GeorgiaTech_losses <- GeorgiaTech |>
    filter(home_team == "Georgia Tech" & home_points < away_points | away_team == "Georgia Tech" & away_points < home_points)
  ## storing overall team Resume Score as vector
  GeorgiaTech_resume <- sum(GeorgiaTech$Resume_Score) - (7 * nrow(GeorgiaTech_losses))
  
  
  ## Hawaii
  Hawaii <- completed_games |>
    filter(home_team == "Hawai'i" | away_team == "Hawai'i") |>
    mutate(team = "Hawai'i",
           team_opp = case_when(home_team == "Hawai'i" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Hawai'i"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  HawaiiFBSOpps <- VoA_Variables |>
    filter(team %in% Hawaii$team_opp) |>
    select(team, VoA_Rating)
  HawaiiFCSOpps <- FCS |>
    filter(team %in% Hawaii$team_opp) |>
    select(team, rating)
  colnames(HawaiiFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  HawaiiOpps <- rbind(HawaiiFBSOpps, HawaiiFCSOpps)
  colnames(HawaiiOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  Hawaii <- full_join(Hawaii, HawaiiOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  Hawaii <- Hawaii |>
    mutate(actual_diff = case_when(home_team == "Hawai'i" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Hawai'i" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Hawai'i" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Hawai'i" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  Hawaii_losses <- Hawaii |>
    filter(home_team == "Hawai'i" & home_points < away_points | away_team == "Hawai'i" & away_points < home_points)
  ## storing overall team Resume Score as vector
  Hawaii_resume <- sum(Hawaii$Resume_Score) - (7 * nrow(Hawaii_losses))
  
  
  ## Houston
  Houston <- completed_games |>
    filter(home_team == "Houston" | away_team == "Houston") |>
    mutate(team = "Houston",
           team_opp = case_when(home_team == "Houston" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Houston"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  HoustonFBSOpps <- VoA_Variables |>
    filter(team %in% Houston$team_opp) |>
    select(team, VoA_Rating)
  HoustonFCSOpps <- FCS |>
    filter(team %in% Houston$team_opp) |>
    select(team, rating)
  colnames(HoustonFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  HoustonOpps <- rbind(HoustonFBSOpps, HoustonFCSOpps)
  colnames(HoustonOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  Houston <- full_join(Houston, HoustonOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  Houston <- Houston |>
    mutate(actual_diff = case_when(home_team == "Houston" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Houston" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Houston" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Houston" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  Houston_losses <- Houston |>
    filter(home_team == "Houston" & home_points < away_points | away_team == "Houston" & away_points < home_points)
  ## storing overall team Resume Score as vector
  Houston_resume <- sum(Houston$Resume_Score) - (7 * nrow(Houston_losses))
  
  
  ## Illinois
  Illinois <- completed_games |>
    filter(home_team == "Illinois" | away_team == "Illinois") |>
    mutate(team = "Illinois",
           team_opp = case_when(home_team == "Illinois" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Illinois"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  IllinoisFBSOpps <- VoA_Variables |>
    filter(team %in% Illinois$team_opp) |>
    select(team, VoA_Rating)
  IllinoisFCSOpps <- FCS |>
    filter(team %in% Illinois$team_opp) |>
    select(team, rating)
  colnames(IllinoisFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  IllinoisOpps <- rbind(IllinoisFBSOpps, IllinoisFCSOpps)
  colnames(IllinoisOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  Illinois <- full_join(Illinois, IllinoisOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  Illinois <- Illinois |>
    mutate(actual_diff = case_when(home_team == "Illinois" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Illinois" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Illinois" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Illinois" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  Illinois_losses <- Illinois |>
    filter(home_team == "Illinois" & home_points < away_points | away_team == "Illinois" & away_points < home_points)
  ## storing overall team Resume Score as vector
  Illinois_resume <- sum(Illinois$Resume_Score) - (7 * nrow(Illinois_losses))
  
  
  ## Indiana
  Indiana <- completed_games |>
    filter(home_team == "Indiana" | away_team == "Indiana") |>
    mutate(team = "Indiana",
           team_opp = case_when(home_team == "Indiana" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Indiana"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  IndianaFBSOpps <- VoA_Variables |>
    filter(team %in% Indiana$team_opp) |>
    select(team, VoA_Rating)
  IndianaFCSOpps <- FCS |>
    filter(team %in% Indiana$team_opp) |>
    select(team, rating)
  colnames(IndianaFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  IndianaOpps <- rbind(IndianaFBSOpps, IndianaFCSOpps)
  colnames(IndianaOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  Indiana <- full_join(Indiana, IndianaOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  Indiana <- Indiana |>
    mutate(actual_diff = case_when(home_team == "Indiana" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Indiana" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Indiana" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Indiana" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  Indiana_losses <- Indiana |>
    filter(home_team == "Indiana" & home_points < away_points | away_team == "Indiana" & away_points < home_points)
  ## storing overall team Resume Score as vector
  Indiana_resume <- sum(Indiana$Resume_Score) - (7 * nrow(Indiana_losses))
  
  
  ## Iowa
  Iowa <- completed_games |>
    filter(home_team == "Iowa" | away_team == "Iowa") |>
    mutate(team = "Iowa",
           team_opp = case_when(home_team == "Iowa" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Iowa"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  IowaFBSOpps <- VoA_Variables |>
    filter(team %in% Iowa$team_opp) |>
    select(team, VoA_Rating)
  IowaFCSOpps <- FCS |>
    filter(team %in% Iowa$team_opp) |>
    select(team, rating)
  colnames(IowaFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  IowaOpps <- rbind(IowaFBSOpps, IowaFCSOpps)
  colnames(IowaOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  Iowa <- full_join(Iowa, IowaOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  Iowa <- Iowa |>
    mutate(actual_diff = case_when(home_team == "Iowa" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Iowa" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Iowa" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Iowa" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  Iowa_losses <- Iowa |>
    filter(home_team == "Iowa" & home_points < away_points | away_team == "Iowa" & away_points < home_points)
  ## storing overall team Resume Score as vector
  Iowa_resume <- sum(Iowa$Resume_Score) - (7 * nrow(Iowa_losses))
  
  
  ## IowaSt
  IowaSt <- completed_games |>
    filter(home_team == "Iowa State" | away_team == "Iowa State") |>
    mutate(team = "Iowa State",
           team_opp = case_when(home_team == "Iowa State" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Iowa State"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  IowaStFBSOpps <- VoA_Variables |>
    filter(team %in% IowaSt$team_opp) |>
    select(team, VoA_Rating)
  IowaStFCSOpps <- FCS |>
    filter(team %in% IowaSt$team_opp) |>
    select(team, rating)
  colnames(IowaStFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  IowaStOpps <- rbind(IowaStFBSOpps, IowaStFCSOpps)
  colnames(IowaStOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  IowaSt <- full_join(IowaSt, IowaStOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  IowaSt <- IowaSt |>
    mutate(actual_diff = case_when(home_team == "Iowa State" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Iowa State" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Iowa State" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Iowa State" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  IowaSt_losses <- IowaSt |>
    filter(home_team == "Iowa State" & home_points < away_points | away_team == "Iowa State" & away_points < home_points)
  ## storing overall team Resume Score as vector
  IowaSt_resume <- sum(IowaSt$Resume_Score) - (7 * nrow(IowaSt_losses))
  
  
  ## JamesMadison
  JamesMadison <- completed_games |>
    filter(home_team == "James Madison" | away_team == "James Madison") |>
    mutate(team = "James Madison",
           team_opp = case_when(home_team == "James Madison" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "James Madison"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  JamesMadisonFBSOpps <- VoA_Variables |>
    filter(team %in% JamesMadison$team_opp) |>
    select(team, VoA_Rating)
  JamesMadisonFCSOpps <- FCS |>
    filter(team %in% JamesMadison$team_opp) |>
    select(team, rating)
  colnames(JamesMadisonFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  JamesMadisonOpps <- rbind(JamesMadisonFBSOpps, JamesMadisonFCSOpps)
  colnames(JamesMadisonOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  JamesMadison <- full_join(JamesMadison, JamesMadisonOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  JamesMadison <- JamesMadison |>
    mutate(actual_diff = case_when(home_team == "James Madison" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "James Madison" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "James Madison" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "James Madison" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  JamesMadison_losses <- JamesMadison |>
    filter(home_team == "James Madison" & home_points < away_points | away_team == "James Madison" & away_points < home_points)
  ## storing overall team Resume Score as vector
  JamesMadison_resume <- sum(JamesMadison$Resume_Score) - (7 * nrow(JamesMadison_losses))
  
  
  ## Kansas
  Kansas <- completed_games |>
    filter(home_team == "Kansas" | away_team == "Kansas") |>
    mutate(team = "Kansas",
           team_opp = case_when(home_team == "Kansas" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Kansas"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  KansasFBSOpps <- VoA_Variables |>
    filter(team %in% Kansas$team_opp) |>
    select(team, VoA_Rating)
  KansasFCSOpps <- FCS |>
    filter(team %in% Kansas$team_opp) |>
    select(team, rating)
  colnames(KansasFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  KansasOpps <- rbind(KansasFBSOpps, KansasFCSOpps)
  colnames(KansasOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  Kansas <- full_join(Kansas, KansasOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  Kansas <- Kansas |>
    mutate(actual_diff = case_when(home_team == "Kansas" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Kansas" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Kansas" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Kansas" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  Kansas_losses <- Kansas |>
    filter(home_team == "Kansas" & home_points < away_points | away_team == "Kansas" & away_points < home_points)
  ## storing overall team Resume Score as vector
  Kansas_resume <- sum(Kansas$Resume_Score) - (7 * nrow(Kansas_losses))
  
  
  ## KansasSt
  KansasSt <- completed_games |>
    filter(home_team == "Kansas State" | away_team == "Kansas State") |>
    mutate(team = "Kansas State",
           team_opp = case_when(home_team == "Kansas State" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Kansas State"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  KansasStFBSOpps <- VoA_Variables |>
    filter(team %in% KansasSt$team_opp) |>
    select(team, VoA_Rating)
  KansasStFCSOpps <- FCS |>
    filter(team %in% KansasSt$team_opp) |>
    select(team, rating)
  colnames(KansasStFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  KansasStOpps <- rbind(KansasStFBSOpps, KansasStFCSOpps)
  colnames(KansasStOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  KansasSt <- full_join(KansasSt, KansasStOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  KansasSt <- KansasSt |>
    mutate(actual_diff = case_when(home_team == "Kansas State" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Kansas State" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Kansas State" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Kansas State" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  KansasSt_losses <- KansasSt |>
    filter(home_team == "Kansas State" & home_points < away_points | away_team == "Kansas State" & away_points < home_points)
  ## storing overall team Resume Score as vector
  KansasSt_resume <- sum(KansasSt$Resume_Score) - (7 * nrow(KansasSt_losses))
  
  
  ## KentSt
  KentSt <- completed_games |>
    filter(home_team == "Kent State" | away_team == "Kent State") |>
    mutate(team = "Kent State",
           team_opp = case_when(home_team == "Kent State" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Kent State"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  KentStFBSOpps <- VoA_Variables |>
    filter(team %in% KentSt$team_opp) |>
    select(team, VoA_Rating)
  KentStFCSOpps <- FCS |>
    filter(team %in% KentSt$team_opp) |>
    select(team, rating)
  colnames(KentStFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  KentStOpps <- rbind(KentStFBSOpps, KentStFCSOpps)
  colnames(KentStOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  KentSt <- full_join(KentSt, KentStOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  KentSt <- KentSt |>
    mutate(actual_diff = case_when(home_team == "Kent State" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Kent State" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Kent State" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Kent State" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  KentSt_losses <- KentSt |>
    filter(home_team == "Kent State" & home_points < away_points | away_team == "Kent State" & away_points < home_points)
  ## storing overall team Resume Score as vector
  KentSt_resume <- sum(KentSt$Resume_Score) - (7 * nrow(KentSt_losses))
  
  
  ## Kentucky
  Kentucky <- completed_games |>
    filter(home_team == "Kentucky" | away_team == "Kentucky") |>
    mutate(team = "Kentucky",
           team_opp = case_when(home_team == "Kentucky" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Kentucky"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  KentuckyFBSOpps <- VoA_Variables |>
    filter(team %in% Kentucky$team_opp) |>
    select(team, VoA_Rating)
  KentuckyFCSOpps <- FCS |>
    filter(team %in% Kentucky$team_opp) |>
    select(team, rating)
  colnames(KentuckyFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  KentuckyOpps <- rbind(KentuckyFBSOpps, KentuckyFCSOpps)
  colnames(KentuckyOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  Kentucky <- full_join(Kentucky, KentuckyOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  Kentucky <- Kentucky |>
    mutate(actual_diff = case_when(home_team == "Kentucky" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Kentucky" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Kentucky" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Kentucky" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  Kentucky_losses <- Kentucky |>
    filter(home_team == "Kentucky" & home_points < away_points | away_team == "Kentucky" & away_points < home_points)
  ## storing overall team Resume Score as vector
  Kentucky_resume <- sum(Kentucky$Resume_Score) - (7 * nrow(Kentucky_losses))
  
  
  ## Liberty
  Liberty <- completed_games |>
    filter(home_team == "Liberty" | away_team == "Liberty") |>
    mutate(team = "Liberty",
           team_opp = case_when(home_team == "Liberty" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Liberty"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  LibertyFBSOpps <- VoA_Variables |>
    filter(team %in% Liberty$team_opp) |>
    select(team, VoA_Rating)
  LibertyFCSOpps <- FCS |>
    filter(team %in% Liberty$team_opp) |>
    select(team, rating)
  colnames(LibertyFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  LibertyOpps <- rbind(LibertyFBSOpps, LibertyFCSOpps)
  colnames(LibertyOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  Liberty <- full_join(Liberty, LibertyOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  Liberty <- Liberty |>
    mutate(actual_diff = case_when(home_team == "Liberty" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Liberty" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Liberty" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Liberty" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  Liberty_losses <- Liberty |>
    filter(home_team == "Liberty" & home_points < away_points | away_team == "Liberty" & away_points < home_points)
  ## storing overall team Resume Score as vector
  Liberty_resume <- sum(Liberty$Resume_Score) - (7 * nrow(Liberty_losses))
  
  
  ## Louisiana
  Louisiana <- completed_games |>
    filter(home_team == "Louisiana" | away_team == "Louisiana") |>
    mutate(team = "Louisiana",
           team_opp = case_when(home_team == "Louisiana" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Louisiana"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  LouisianaFBSOpps <- VoA_Variables |>
    filter(team %in% Louisiana$team_opp) |>
    select(team, VoA_Rating)
  LouisianaFCSOpps <- FCS |>
    filter(team %in% Louisiana$team_opp) |>
    select(team, rating)
  colnames(LouisianaFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  LouisianaOpps <- rbind(LouisianaFBSOpps, LouisianaFCSOpps)
  colnames(LouisianaOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  Louisiana <- full_join(Louisiana, LouisianaOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  Louisiana <- Louisiana |>
    mutate(actual_diff = case_when(home_team == "Louisiana" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Louisiana" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Louisiana" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Louisiana" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  Louisiana_losses <- Louisiana |>
    filter(home_team == "Louisiana" & home_points < away_points | away_team == "Louisiana" & away_points < home_points)
  ## storing overall team Resume Score as vector
  Louisiana_resume <- sum(Louisiana$Resume_Score) - (7 * nrow(Louisiana_losses))
  
  
  ## ULM
  ULM <- completed_games |>
    filter(home_team == "Louisiana Monroe" | away_team == "Louisiana Monroe") |>
    mutate(team = "Louisiana Monroe",
           team_opp = case_when(home_team == "Louisiana Monroe" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Louisiana Monroe"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  ULMFBSOpps <- VoA_Variables |>
    filter(team %in% ULM$team_opp) |>
    select(team, VoA_Rating)
  ULMFCSOpps <- FCS |>
    filter(team %in% ULM$team_opp) |>
    select(team, rating)
  colnames(ULMFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  ULMOpps <- rbind(ULMFBSOpps, ULMFCSOpps)
  colnames(ULMOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  ULM <- full_join(ULM, ULMOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  ULM <- ULM |>
    mutate(actual_diff = case_when(home_team == "Louisiana Monroe" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Louisiana Monroe" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Louisiana Monroe" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Louisiana Monroe" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  ULM_losses <- ULM |>
    filter(home_team == "Louisiana Monroe" & home_points < away_points | away_team == "Louisiana Monroe" & away_points < home_points)
  ## storing overall team Resume Score as vector
  ULM_resume <- sum(ULM$Resume_Score) - (7 * nrow(ULM_losses))
  
  
  ## LouisianaTech
  LouisianaTech <- completed_games |>
    filter(home_team == "Louisiana Tech" | away_team == "Louisiana Tech") |>
    mutate(team = "Louisiana Tech",
           team_opp = case_when(home_team == "Louisiana Tech" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Louisiana Tech"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  LouisianaTechFBSOpps <- VoA_Variables |>
    filter(team %in% LouisianaTech$team_opp) |>
    select(team, VoA_Rating)
  LouisianaTechFCSOpps <- FCS |>
    filter(team %in% LouisianaTech$team_opp) |>
    select(team, rating)
  colnames(LouisianaTechFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  LouisianaTechOpps <- rbind(LouisianaTechFBSOpps, LouisianaTechFCSOpps)
  colnames(LouisianaTechOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  LouisianaTech <- full_join(LouisianaTech, LouisianaTechOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  LouisianaTech <- LouisianaTech |>
    mutate(actual_diff = case_when(home_team == "Louisiana Tech" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Louisiana Tech" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Louisiana Tech" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Louisiana Tech" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  LouisianaTech_losses <- LouisianaTech |>
    filter(home_team == "Louisiana Tech" & home_points < away_points | away_team == "Louisiana Tech" & away_points < home_points)
  ## storing overall team Resume Score as vector
  LouisianaTech_resume <- sum(LouisianaTech$Resume_Score) - (7 * nrow(LouisianaTech_losses))
  
  
  ## Louisville
  Louisville <- completed_games |>
    filter(home_team == "Louisville" | away_team == "Louisville") |>
    mutate(team = "Louisville",
           team_opp = case_when(home_team == "Louisville" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Louisville"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  LouisvilleFBSOpps <- VoA_Variables |>
    filter(team %in% Louisville$team_opp) |>
    select(team, VoA_Rating)
  LouisvilleFCSOpps <- FCS |>
    filter(team %in% Louisville$team_opp) |>
    select(team, rating)
  colnames(LouisvilleFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  LouisvilleOpps <- rbind(LouisvilleFBSOpps, LouisvilleFCSOpps)
  colnames(LouisvilleOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  Louisville <- full_join(Louisville, LouisvilleOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  Louisville <- Louisville |>
    mutate(actual_diff = case_when(home_team == "Louisville" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Louisville" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Louisville" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Louisville" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  Louisville_losses <- Louisville |>
    filter(home_team == "Louisville" & home_points < away_points | away_team == "Louisville" & away_points < home_points)
  ## storing overall team Resume Score as vector
  Louisville_resume <- sum(Louisville$Resume_Score) - (7 * nrow(Louisville_losses))
  
  
  ## LSU
  LSU <- completed_games |>
    filter(home_team == "LSU" | away_team == "LSU") |>
    mutate(team = "LSU",
           team_opp = case_when(home_team == "LSU" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "LSU"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  LSUFBSOpps <- VoA_Variables |>
    filter(team %in% LSU$team_opp) |>
    select(team, VoA_Rating)
  LSUFCSOpps <- FCS |>
    filter(team %in% LSU$team_opp) |>
    select(team, rating)
  colnames(LSUFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  LSUOpps <- rbind(LSUFBSOpps, LSUFCSOpps)
  colnames(LSUOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  LSU <- full_join(LSU, LSUOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  LSU <- LSU |>
    mutate(actual_diff = case_when(home_team == "LSU" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "LSU" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "LSU" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "LSU" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  LSU_losses <- LSU |>
    filter(home_team == "LSU" & home_points < away_points | away_team == "LSU" & away_points < home_points)
  ## storing overall team Resume Score as vector
  LSU_resume <- sum(LSU$Resume_Score) - (7 * nrow(LSU_losses))
  
  
  ## Marshall
  Marshall <- completed_games |>
    filter(home_team == "Marshall" | away_team == "Marshall") |>
    mutate(team = "Marshall",
           team_opp = case_when(home_team == "Marshall" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Marshall"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  MarshallFBSOpps <- VoA_Variables |>
    filter(team %in% Marshall$team_opp) |>
    select(team, VoA_Rating)
  MarshallFCSOpps <- FCS |>
    filter(team %in% Marshall$team_opp) |>
    select(team, rating)
  colnames(MarshallFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  MarshallOpps <- rbind(MarshallFBSOpps, MarshallFCSOpps)
  colnames(MarshallOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  Marshall <- full_join(Marshall, MarshallOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  Marshall <- Marshall |>
    mutate(actual_diff = case_when(home_team == "Marshall" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Marshall" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Marshall" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Marshall" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  Marshall_losses <- Marshall |>
    filter(home_team == "Marshall" & home_points < away_points | away_team == "Marshall" & away_points < home_points)
  ## storing overall team Resume Score as vector
  Marshall_resume <- sum(Marshall$Resume_Score) - (7 * nrow(Marshall_losses))
  
  
  ## Maryland
  Maryland <- completed_games |>
    filter(home_team == "Maryland" | away_team == "Maryland") |>
    mutate(team = "Maryland",
           team_opp = case_when(home_team == "Maryland" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Maryland"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  MarylandFBSOpps <- VoA_Variables |>
    filter(team %in% Maryland$team_opp) |>
    select(team, VoA_Rating)
  MarylandFCSOpps <- FCS |>
    filter(team %in% Maryland$team_opp) |>
    select(team, rating)
  colnames(MarylandFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  MarylandOpps <- rbind(MarylandFBSOpps, MarylandFCSOpps)
  colnames(MarylandOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  Maryland <- full_join(Maryland, MarylandOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  Maryland <- Maryland |>
    mutate(actual_diff = case_when(home_team == "Maryland" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Maryland" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Maryland" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Maryland" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  Maryland_losses <- Maryland |>
    filter(home_team == "Maryland" & home_points < away_points | away_team == "Maryland" & away_points < home_points)
  ## storing overall team Resume Score as vector
  Maryland_resume <- sum(Maryland$Resume_Score) - (7 * nrow(Maryland_losses))
  
  
  ## Memphis
  Memphis <- completed_games |>
    filter(home_team == "Memphis" | away_team == "Memphis") |>
    mutate(team = "Memphis",
           team_opp = case_when(home_team == "Memphis" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Memphis"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  MemphisFBSOpps <- VoA_Variables |>
    filter(team %in% Memphis$team_opp) |>
    select(team, VoA_Rating)
  MemphisFCSOpps <- FCS |>
    filter(team %in% Memphis$team_opp) |>
    select(team, rating)
  colnames(MemphisFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  MemphisOpps <- rbind(MemphisFBSOpps, MemphisFCSOpps)
  colnames(MemphisOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  Memphis <- full_join(Memphis, MemphisOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  Memphis <- Memphis |>
    mutate(actual_diff = case_when(home_team == "Memphis" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Memphis" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Memphis" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Memphis" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  Memphis_losses <- Memphis |>
    filter(home_team == "Memphis" & home_points < away_points | away_team == "Memphis" & away_points < home_points)
  ## storing overall team Resume Score as vector
  Memphis_resume <- sum(Memphis$Resume_Score) - (7 * nrow(Memphis_losses))
  
  
  ## Miami
  Miami <- completed_games |>
    filter(home_team == "Miami" | away_team == "Miami") |>
    mutate(team = "Miami",
           team_opp = case_when(home_team == "Miami" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Miami"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  MiamiFBSOpps <- VoA_Variables |>
    filter(team %in% Miami$team_opp) |>
    select(team, VoA_Rating)
  MiamiFCSOpps <- FCS |>
    filter(team %in% Miami$team_opp) |>
    select(team, rating)
  colnames(MiamiFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  MiamiOpps <- rbind(MiamiFBSOpps, MiamiFCSOpps)
  colnames(MiamiOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  Miami <- full_join(Miami, MiamiOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  Miami <- Miami |>
    mutate(actual_diff = case_when(home_team == "Miami" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Miami" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Miami" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Miami" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  Miami_losses <- Miami |>
    filter(home_team == "Miami" & home_points < away_points | away_team == "Miami" & away_points < home_points)
  ## storing overall team Resume Score as vector
  Miami_resume <- sum(Miami$Resume_Score) - (7 * nrow(Miami_losses))
  
  
  ## MiamiOH
  MiamiOH <- completed_games |>
    filter(home_team == "Miami (OH)" | away_team == "Miami (OH)") |>
    mutate(team = "Miami (OH)",
           team_opp = case_when(home_team == "Miami (OH)" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Miami (OH)"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  MiamiOHFBSOpps <- VoA_Variables |>
    filter(team %in% MiamiOH$team_opp) |>
    select(team, VoA_Rating)
  MiamiOHFCSOpps <- FCS |>
    filter(team %in% MiamiOH$team_opp) |>
    select(team, rating)
  colnames(MiamiOHFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  MiamiOHOpps <- rbind(MiamiOHFBSOpps, MiamiOHFCSOpps)
  colnames(MiamiOHOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  MiamiOH <- full_join(MiamiOH, MiamiOHOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  MiamiOH <- MiamiOH |>
    mutate(actual_diff = case_when(home_team == "Miami (OH)" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Miami (OH)" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Miami (OH)" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Miami (OH)" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  MiamiOH_losses <- MiamiOH |>
    filter(home_team == "Miami (OH)" & home_points < away_points | away_team == "Miami (OH)" & away_points < home_points)
  ## storing overall team Resume Score as vector
  MiamiOH_resume <- sum(MiamiOH$Resume_Score) - (7 * nrow(MiamiOH_losses))
  
  
  ## Michigan
  Michigan <- completed_games |>
    filter(home_team == "Michigan" | away_team == "Michigan") |>
    mutate(team = "Michigan",
           team_opp = case_when(home_team == "Michigan" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Michigan"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  MichiganFBSOpps <- VoA_Variables |>
    filter(team %in% Michigan$team_opp) |>
    select(team, VoA_Rating)
  MichiganFCSOpps <- FCS |>
    filter(team %in% Michigan$team_opp) |>
    select(team, rating)
  colnames(MichiganFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  MichiganOpps <- rbind(MichiganFBSOpps, MichiganFCSOpps)
  colnames(MichiganOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  Michigan <- full_join(Michigan, MichiganOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  Michigan <- Michigan |>
    mutate(actual_diff = case_when(home_team == "Michigan" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Michigan" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Michigan" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Michigan" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  Michigan_losses <- Michigan |>
    filter(home_team == "Michigan" & home_points < away_points | away_team == "Michigan" & away_points < home_points)
  ## storing overall team Resume Score as vector
  Michigan_resume <- sum(Michigan$Resume_Score) - (7 * nrow(Michigan_losses))
  
  
  ## MichiganSt
  MichiganSt <- completed_games |>
    filter(home_team == "Michigan State" | away_team == "Michigan State") |>
    mutate(team = "Michigan State",
           team_opp = case_when(home_team == "Michigan State" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Michigan State"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  MichiganStFBSOpps <- VoA_Variables |>
    filter(team %in% MichiganSt$team_opp) |>
    select(team, VoA_Rating)
  MichiganStFCSOpps <- FCS |>
    filter(team %in% MichiganSt$team_opp) |>
    select(team, rating)
  colnames(MichiganStFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  MichiganStOpps <- rbind(MichiganStFBSOpps, MichiganStFCSOpps)
  colnames(MichiganStOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  MichiganSt <- full_join(MichiganSt, MichiganStOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  MichiganSt <- MichiganSt |>
    mutate(actual_diff = case_when(home_team == "Michigan State" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Michigan State" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Michigan State" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Michigan State" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  MichiganSt_losses <- MichiganSt |>
    filter(home_team == "Michigan State" & home_points < away_points | away_team == "Michigan State" & away_points < home_points)
  ## storing overall team Resume Score as vector
  MichiganSt_resume <- sum(MichiganSt$Resume_Score) - (7 * nrow(MichiganSt_losses))
  
  
  ## MiddleTennessee
  MiddleTennessee <- completed_games |>
    filter(home_team == "Middle Tennessee" | away_team == "Middle Tennessee") |>
    mutate(team = "Middle Tennessee",
           team_opp = case_when(home_team == "Middle Tennessee" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Middle Tennessee"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  MiddleTennesseeFBSOpps <- VoA_Variables |>
    filter(team %in% MiddleTennessee$team_opp) |>
    select(team, VoA_Rating)
  MiddleTennesseeFCSOpps <- FCS |>
    filter(team %in% MiddleTennessee$team_opp) |>
    select(team, rating)
  colnames(MiddleTennesseeFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  MiddleTennesseeOpps <- rbind(MiddleTennesseeFBSOpps, MiddleTennesseeFCSOpps)
  colnames(MiddleTennesseeOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  MiddleTennessee <- full_join(MiddleTennessee, MiddleTennesseeOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  MiddleTennessee <- MiddleTennessee |>
    mutate(actual_diff = case_when(home_team == "Middle Tennessee" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Middle Tennessee" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Middle Tennessee" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Middle Tennessee" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  MiddleTennessee_losses <- MiddleTennessee |>
    filter(home_team == "Middle Tennessee" & home_points < away_points | away_team == "Middle Tennessee" & away_points < home_points)
  ## storing overall team Resume Score as vector
  MiddleTennessee_resume <- sum(MiddleTennessee$Resume_Score) - (7 * nrow(MiddleTennessee_losses))
  
  
  ## Minnesota
  Minnesota <- completed_games |>
    filter(home_team == "Minnesota" | away_team == "Minnesota") |>
    mutate(team = "Minnesota",
           team_opp = case_when(home_team == "Minnesota" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Minnesota"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  MinnesotaFBSOpps <- VoA_Variables |>
    filter(team %in% Minnesota$team_opp) |>
    select(team, VoA_Rating)
  MinnesotaFCSOpps <- FCS |>
    filter(team %in% Minnesota$team_opp) |>
    select(team, rating)
  colnames(MinnesotaFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  MinnesotaOpps <- rbind(MinnesotaFBSOpps, MinnesotaFCSOpps)
  colnames(MinnesotaOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  Minnesota <- full_join(Minnesota, MinnesotaOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  Minnesota <- Minnesota |>
    mutate(actual_diff = case_when(home_team == "Minnesota" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Minnesota" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Minnesota" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Minnesota" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  Minnesota_losses <- Minnesota |>
    filter(home_team == "Minnesota" & home_points < away_points | away_team == "Minnesota" & away_points < home_points)
  ## storing overall team Resume Score as vector
  Minnesota_resume <- sum(Minnesota$Resume_Score) - (7 * nrow(Minnesota_losses))
  
  
  ## MissSt
  MissSt <- completed_games |>
    filter(home_team == "Mississippi State" | away_team == "Mississippi State") |>
    mutate(team = "Mississippi State",
           team_opp = case_when(home_team == "Mississippi State" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Mississippi State"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  MissStFBSOpps <- VoA_Variables |>
    filter(team %in% MissSt$team_opp) |>
    select(team, VoA_Rating)
  MissStFCSOpps <- FCS |>
    filter(team %in% MissSt$team_opp) |>
    select(team, rating)
  colnames(MissStFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  MissStOpps <- rbind(MissStFBSOpps, MissStFCSOpps)
  colnames(MissStOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  MissSt <- full_join(MissSt, MissStOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  MissSt <- MissSt |>
    mutate(actual_diff = case_when(home_team == "Mississippi State" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Mississippi State" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Mississippi State" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Mississippi State" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  MissSt_losses <- MissSt |>
    filter(home_team == "Mississippi State" & home_points < away_points | away_team == "Mississippi State" & away_points < home_points)
  ## storing overall team Resume Score as vector
  MissSt_resume <- sum(MissSt$Resume_Score) - (7 * nrow(MissSt_losses))
  
  
  ## Missouri
  Missouri <- completed_games |>
    filter(home_team == "Missouri" | away_team == "Missouri") |>
    mutate(team = "Missouri",
           team_opp = case_when(home_team == "Missouri" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Missouri"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  MissouriFBSOpps <- VoA_Variables |>
    filter(team %in% Missouri$team_opp) |>
    select(team, VoA_Rating)
  MissouriFCSOpps <- FCS |>
    filter(team %in% Missouri$team_opp) |>
    select(team, rating)
  colnames(MissouriFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  MissouriOpps <- rbind(MissouriFBSOpps, MissouriFCSOpps)
  colnames(MissouriOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  Missouri <- full_join(Missouri, MissouriOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  Missouri <- Missouri |>
    mutate(actual_diff = case_when(home_team == "Missouri" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Missouri" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Missouri" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Missouri" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  Missouri_losses <- Missouri |>
    filter(home_team == "Missouri" & home_points < away_points | away_team == "Missouri" & away_points < home_points)
  ## storing overall team Resume Score as vector
  Missouri_resume <- sum(Missouri$Resume_Score) - (7 * nrow(Missouri_losses))
  
  
  ## Navy
  Navy <- completed_games |>
    filter(home_team == "Navy" | away_team == "Navy") |>
    mutate(team = "Navy",
           team_opp = case_when(home_team == "Navy" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Navy"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  NavyFBSOpps <- VoA_Variables |>
    filter(team %in% Navy$team_opp) |>
    select(team, VoA_Rating)
  NavyFCSOpps <- FCS |>
    filter(team %in% Navy$team_opp) |>
    select(team, rating)
  colnames(NavyFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  NavyOpps <- rbind(NavyFBSOpps, NavyFCSOpps)
  colnames(NavyOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  Navy <- full_join(Navy, NavyOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  Navy <- Navy |>
    mutate(actual_diff = case_when(home_team == "Navy" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Navy" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Navy" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Navy" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  Navy_losses <- Navy |>
    filter(home_team == "Navy" & home_points < away_points | away_team == "Navy" & away_points < home_points)
  ## storing overall team Resume Score as vector
  Navy_resume <- sum(Navy$Resume_Score) - (7 * nrow(Navy_losses))
  
  
  ## NCSt
  NCSt <- completed_games |>
    filter(home_team == "NC State" | away_team == "NC State") |>
    mutate(team = "NC State",
           team_opp = case_when(home_team == "NC State" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "NC State"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  NCStFBSOpps <- VoA_Variables |>
    filter(team %in% NCSt$team_opp) |>
    select(team, VoA_Rating)
  NCStFCSOpps <- FCS |>
    filter(team %in% NCSt$team_opp) |>
    select(team, rating)
  colnames(NCStFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  NCStOpps <- rbind(NCStFBSOpps, NCStFCSOpps)
  colnames(NCStOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  NCSt <- full_join(NCSt, NCStOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  NCSt <- NCSt |>
    mutate(actual_diff = case_when(home_team == "NC State" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "NC State" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "NC State" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "NC State" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  NCSt_losses <- NCSt |>
    filter(home_team == "NC State" & home_points < away_points | away_team == "NC State" & away_points < home_points)
  ## storing overall team Resume Score as vector
  NCSt_resume <- sum(NCSt$Resume_Score) - (7 * nrow(NCSt_losses))
  
  
  ## Nebraska
  Nebraska <- completed_games |>
    filter(home_team == "Nebraska" | away_team == "Nebraska") |>
    mutate(team = "Nebraska",
           team_opp = case_when(home_team == "Nebraska" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Nebraska"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  NebraskaFBSOpps <- VoA_Variables |>
    filter(team %in% Nebraska$team_opp) |>
    select(team, VoA_Rating)
  NebraskaFCSOpps <- FCS |>
    filter(team %in% Nebraska$team_opp) |>
    select(team, rating)
  colnames(NebraskaFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  NebraskaOpps <- rbind(NebraskaFBSOpps, NebraskaFCSOpps)
  colnames(NebraskaOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  Nebraska <- full_join(Nebraska, NebraskaOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  Nebraska <- Nebraska |>
    mutate(actual_diff = case_when(home_team == "Nebraska" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Nebraska" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Nebraska" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Nebraska" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  Nebraska_losses <- Nebraska |>
    filter(home_team == "Nebraska" & home_points < away_points | away_team == "Nebraska" & away_points < home_points)
  ## storing overall team Resume Score as vector
  Nebraska_resume <- sum(Nebraska$Resume_Score) - (7 * nrow(Nebraska_losses))
  
  
  ## Nevada
  Nevada <- completed_games |>
    filter(home_team == "Nevada" | away_team == "Nevada") |>
    mutate(team = "Nevada",
           team_opp = case_when(home_team == "Nevada" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Nevada"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  NevadaFBSOpps <- VoA_Variables |>
    filter(team %in% Nevada$team_opp) |>
    select(team, VoA_Rating)
  NevadaFCSOpps <- FCS |>
    filter(team %in% Nevada$team_opp) |>
    select(team, rating)
  colnames(NevadaFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  NevadaOpps <- rbind(NevadaFBSOpps, NevadaFCSOpps)
  colnames(NevadaOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  Nevada <- full_join(Nevada, NevadaOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  Nevada <- Nevada |>
    mutate(actual_diff = case_when(home_team == "Nevada" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Nevada" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Nevada" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Nevada" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  Nevada_losses <- Nevada |>
    filter(home_team == "Nevada" & home_points < away_points | away_team == "Nevada" & away_points < home_points)
  ## storing overall team Resume Score as vector
  Nevada_resume <- sum(Nevada$Resume_Score) - (7 * nrow(Nevada_losses))
  
  
  ## NewMexico
  NewMexico <- completed_games |>
    filter(home_team == "New Mexico" | away_team == "New Mexico") |>
    mutate(team = "New Mexico",
           team_opp = case_when(home_team == "New Mexico" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "New Mexico"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  NewMexicoFBSOpps <- VoA_Variables |>
    filter(team %in% NewMexico$team_opp) |>
    select(team, VoA_Rating)
  NewMexicoFCSOpps <- FCS |>
    filter(team %in% NewMexico$team_opp) |>
    select(team, rating)
  colnames(NewMexicoFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  NewMexicoOpps <- rbind(NewMexicoFBSOpps, NewMexicoFCSOpps)
  colnames(NewMexicoOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  NewMexico <- full_join(NewMexico, NewMexicoOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  NewMexico <- NewMexico |>
    mutate(actual_diff = case_when(home_team == "New Mexico" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "New Mexico" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "New Mexico" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "New Mexico" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  NewMexico_losses <- NewMexico |>
    filter(home_team == "New Mexico" & home_points < away_points | away_team == "New Mexico" & away_points < home_points)
  ## storing overall team Resume Score as vector
  NewMexico_resume <- sum(NewMexico$Resume_Score) - (7 * nrow(NewMexico_losses))
  
  
  ## NewMexicoSt
  NewMexicoSt <- completed_games |>
    filter(home_team == "New Mexico State" | away_team == "New Mexico State") |>
    mutate(team = "New Mexico State",
           team_opp = case_when(home_team == "New Mexico State" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "New Mexico State"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  NewMexicoStFBSOpps <- VoA_Variables |>
    filter(team %in% NewMexicoSt$team_opp) |>
    select(team, VoA_Rating)
  NewMexicoStFCSOpps <- FCS |>
    filter(team %in% NewMexicoSt$team_opp) |>
    select(team, rating)
  colnames(NewMexicoStFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  NewMexicoStOpps <- rbind(NewMexicoStFBSOpps, NewMexicoStFCSOpps)
  colnames(NewMexicoStOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  NewMexicoSt <- full_join(NewMexicoSt, NewMexicoStOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  NewMexicoSt <- NewMexicoSt |>
    mutate(actual_diff = case_when(home_team == "New Mexico State" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "New Mexico State" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "New Mexico State" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "New Mexico State" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  NewMexicoSt_losses <- NewMexicoSt |>
    filter(home_team == "New Mexico State" & home_points < away_points | away_team == "New Mexico State" & away_points < home_points)
  ## storing overall team Resume Score as vector
  NewMexicoSt_resume <- sum(NewMexicoSt$Resume_Score) - (7 * nrow(NewMexicoSt_losses))
  
  
  ## NorthCarolina
  NorthCarolina <- completed_games |>
    filter(home_team == "North Carolina" | away_team == "North Carolina") |>
    mutate(team = "North Carolina",
           team_opp = case_when(home_team == "North Carolina" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "North Carolina"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  NorthCarolinaFBSOpps <- VoA_Variables |>
    filter(team %in% NorthCarolina$team_opp) |>
    select(team, VoA_Rating)
  NorthCarolinaFCSOpps <- FCS |>
    filter(team %in% NorthCarolina$team_opp) |>
    select(team, rating)
  colnames(NorthCarolinaFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  NorthCarolinaOpps <- rbind(NorthCarolinaFBSOpps, NorthCarolinaFCSOpps)
  colnames(NorthCarolinaOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  NorthCarolina <- full_join(NorthCarolina, NorthCarolinaOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  NorthCarolina <- NorthCarolina |>
    mutate(actual_diff = case_when(home_team == "North Carolina" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "North Carolina" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "North Carolina" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "North Carolina" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  NorthCarolina_losses <- NorthCarolina |>
    filter(home_team == "North Carolina" & home_points < away_points | away_team == "North Carolina" & away_points < home_points)
  ## storing overall team Resume Score as vector
  NorthCarolina_resume <- sum(NorthCarolina$Resume_Score) - (7 * nrow(NorthCarolina_losses))
  
  
  ## NorthTexas
  NorthTexas <- completed_games |>
    filter(home_team == "North Texas" | away_team == "North Texas") |>
    mutate(team = "North Texas",
           team_opp = case_when(home_team == "North Texas" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "North Texas"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  NorthTexasFBSOpps <- VoA_Variables |>
    filter(team %in% NorthTexas$team_opp) |>
    select(team, VoA_Rating)
  NorthTexasFCSOpps <- FCS |>
    filter(team %in% NorthTexas$team_opp) |>
    select(team, rating)
  colnames(NorthTexasFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  NorthTexasOpps <- rbind(NorthTexasFBSOpps, NorthTexasFCSOpps)
  colnames(NorthTexasOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  NorthTexas <- full_join(NorthTexas, NorthTexasOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  NorthTexas <- NorthTexas |>
    mutate(actual_diff = case_when(home_team == "North Texas" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "North Texas" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "North Texas" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "North Texas" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  NorthTexas_losses <- NorthTexas |>
    filter(home_team == "North Texas" & home_points < away_points | away_team == "North Texas" & away_points < home_points)
  ## storing overall team Resume Score as vector
  NorthTexas_resume <- sum(NorthTexas$Resume_Score) - (7 * nrow(NorthTexas_losses))
  
  
  ## NorthernIllinois
  NorthernIllinois <- completed_games |>
    filter(home_team == "Northern Illinois" | away_team == "Northern Illinois") |>
    mutate(team = "Northern Illinois",
           team_opp = case_when(home_team == "Northern Illinois" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Northern Illinois"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  NorthernIllinoisFBSOpps <- VoA_Variables |>
    filter(team %in% NorthernIllinois$team_opp) |>
    select(team, VoA_Rating)
  NorthernIllinoisFCSOpps <- FCS |>
    filter(team %in% NorthernIllinois$team_opp) |>
    select(team, rating)
  colnames(NorthernIllinoisFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  NorthernIllinoisOpps <- rbind(NorthernIllinoisFBSOpps, NorthernIllinoisFCSOpps)
  colnames(NorthernIllinoisOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  NorthernIllinois <- full_join(NorthernIllinois, NorthernIllinoisOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  NorthernIllinois <- NorthernIllinois |>
    mutate(actual_diff = case_when(home_team == "Northern Illinois" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Northern Illinois" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Northern Illinois" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Northern Illinois" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  NorthernIllinois_losses <- NorthernIllinois |>
    filter(home_team == "Northern Illinois" & home_points < away_points | away_team == "Northern Illinois" & away_points < home_points)
  ## storing overall team Resume Score as vector
  NorthernIllinois_resume <- sum(NorthernIllinois$Resume_Score) - (7 * nrow(NorthernIllinois_losses))
  
  
  ## Northwestern
  Northwestern <- completed_games |>
    filter(home_team == "Northwestern" | away_team == "Northwestern") |>
    mutate(team = "Northwestern",
           team_opp = case_when(home_team == "Northwestern" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Northwestern"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  NorthwesternFBSOpps <- VoA_Variables |>
    filter(team %in% Northwestern$team_opp) |>
    select(team, VoA_Rating)
  NorthwesternFCSOpps <- FCS |>
    filter(team %in% Northwestern$team_opp) |>
    select(team, rating)
  colnames(NorthwesternFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  NorthwesternOpps <- rbind(NorthwesternFBSOpps, NorthwesternFCSOpps)
  colnames(NorthwesternOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  Northwestern <- full_join(Northwestern, NorthwesternOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  Northwestern <- Northwestern |>
    mutate(actual_diff = case_when(home_team == "Northwestern" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Northwestern" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Northwestern" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Northwestern" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  Northwestern_losses <- Northwestern |>
    filter(home_team == "Northwestern" & home_points < away_points | away_team == "Northwestern" & away_points < home_points)
  ## storing overall team Resume Score as vector
  Northwestern_resume <- sum(Northwestern$Resume_Score) - (7 * nrow(Northwestern_losses))
  
  
  ## NotreDame
  NotreDame <- completed_games |>
    filter(home_team == "Notre Dame" | away_team == "Notre Dame") |>
    mutate(team = "Notre Dame",
           team_opp = case_when(home_team == "Notre Dame" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Notre Dame"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  NotreDameFBSOpps <- VoA_Variables |>
    filter(team %in% NotreDame$team_opp) |>
    select(team, VoA_Rating)
  NotreDameFCSOpps <- FCS |>
    filter(team %in% NotreDame$team_opp) |>
    select(team, rating)
  colnames(NotreDameFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  NotreDameOpps <- rbind(NotreDameFBSOpps, NotreDameFCSOpps)
  colnames(NotreDameOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  NotreDame <- full_join(NotreDame, NotreDameOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  NotreDame <- NotreDame |>
    mutate(actual_diff = case_when(home_team == "Notre Dame" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Notre Dame" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Notre Dame" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Notre Dame" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  NotreDame_losses <- NotreDame |>
    filter(home_team == "Notre Dame" & home_points < away_points | away_team == "Notre Dame" & away_points < home_points)
  ## storing overall team Resume Score as vector
  NotreDame_resume <- sum(NotreDame$Resume_Score) - (7 * nrow(NotreDame_losses))
  
  
  ## Ohio
  Ohio <- completed_games |>
    filter(home_team == "Ohio" | away_team == "Ohio") |>
    mutate(team = "Ohio",
           team_opp = case_when(home_team == "Ohio" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Ohio"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  OhioFBSOpps <- VoA_Variables |>
    filter(team %in% Ohio$team_opp) |>
    select(team, VoA_Rating)
  OhioFCSOpps <- FCS |>
    filter(team %in% Ohio$team_opp) |>
    select(team, rating)
  colnames(OhioFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  OhioOpps <- rbind(OhioFBSOpps, OhioFCSOpps)
  colnames(OhioOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  Ohio <- full_join(Ohio, OhioOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  Ohio <- Ohio |>
    mutate(actual_diff = case_when(home_team == "Ohio" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Ohio" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Ohio" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Ohio" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  Ohio_losses <- Ohio |>
    filter(home_team == "Ohio" & home_points < away_points | away_team == "Ohio" & away_points < home_points)
  ## storing overall team Resume Score as vector
  Ohio_resume <- sum(Ohio$Resume_Score) - (7 * nrow(Ohio_losses))
  
  
  ## OhioSt
  OhioSt <- completed_games |>
    filter(home_team == "Ohio State" | away_team == "Ohio State") |>
    mutate(team = "Ohio State",
           team_opp = case_when(home_team == "Ohio State" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Ohio State"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  OhioStFBSOpps <- VoA_Variables |>
    filter(team %in% OhioSt$team_opp) |>
    select(team, VoA_Rating)
  OhioStFCSOpps <- FCS |>
    filter(team %in% OhioSt$team_opp) |>
    select(team, rating)
  colnames(OhioStFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  OhioStOpps <- rbind(OhioStFBSOpps, OhioStFCSOpps)
  colnames(OhioStOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  OhioSt <- full_join(OhioSt, OhioStOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  OhioSt <- OhioSt |>
    mutate(actual_diff = case_when(home_team == "Ohio State" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Ohio State" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Ohio State" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Ohio State" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  OhioSt_losses <- OhioSt |>
    filter(home_team == "Ohio State" & home_points < away_points | away_team == "Ohio State" & away_points < home_points)
  ## storing overall team Resume Score as vector
  OhioSt_resume <- sum(OhioSt$Resume_Score) - (7 * nrow(OhioSt_losses))
  
  
  ## Oklahoma
  Oklahoma <- completed_games |>
    filter(home_team == "Oklahoma" | away_team == "Oklahoma") |>
    mutate(team = "Oklahoma",
           team_opp = case_when(home_team == "Oklahoma" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Oklahoma"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  OklahomaFBSOpps <- VoA_Variables |>
    filter(team %in% Oklahoma$team_opp) |>
    select(team, VoA_Rating)
  OklahomaFCSOpps <- FCS |>
    filter(team %in% Oklahoma$team_opp) |>
    select(team, rating)
  colnames(OklahomaFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  OklahomaOpps <- rbind(OklahomaFBSOpps, OklahomaFCSOpps)
  colnames(OklahomaOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  Oklahoma <- full_join(Oklahoma, OklahomaOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  Oklahoma <- Oklahoma |>
    mutate(actual_diff = case_when(home_team == "Oklahoma" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Oklahoma" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Oklahoma" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Oklahoma" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  Oklahoma_losses <- Oklahoma |>
    filter(home_team == "Oklahoma" & home_points < away_points | away_team == "Oklahoma" & away_points < home_points)
  ## storing overall team Resume Score as vector
  Oklahoma_resume <- sum(Oklahoma$Resume_Score) - (7 * nrow(Oklahoma_losses))
  
  
  ## OklahomaSt
  OklahomaSt <- completed_games |>
    filter(home_team == "Oklahoma State" | away_team == "Oklahoma State") |>
    mutate(team = "Oklahoma State",
           team_opp = case_when(home_team == "Oklahoma State" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Oklahoma State"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  OklahomaStFBSOpps <- VoA_Variables |>
    filter(team %in% OklahomaSt$team_opp) |>
    select(team, VoA_Rating)
  OklahomaStFCSOpps <- FCS |>
    filter(team %in% OklahomaSt$team_opp) |>
    select(team, rating)
  colnames(OklahomaStFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  OklahomaStOpps <- rbind(OklahomaStFBSOpps, OklahomaStFCSOpps)
  colnames(OklahomaStOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  OklahomaSt <- full_join(OklahomaSt, OklahomaStOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  OklahomaSt <- OklahomaSt |>
    mutate(actual_diff = case_when(home_team == "Oklahoma State" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Oklahoma State" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Oklahoma State" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Oklahoma State" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  OklahomaSt_losses <- OklahomaSt |>
    filter(home_team == "Oklahoma State" & home_points < away_points | away_team == "Oklahoma State" & away_points < home_points)
  ## storing overall team Resume Score as vector
  OklahomaSt_resume <- sum(OklahomaSt$Resume_Score) - (7 * nrow(OklahomaSt_losses))
  
  
  ## OldDominion
  OldDominion <- completed_games |>
    filter(home_team == "Old Dominion" | away_team == "Old Dominion") |>
    mutate(team = "Old Dominion",
           team_opp = case_when(home_team == "Old Dominion" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Old Dominion"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  OldDominionFBSOpps <- VoA_Variables |>
    filter(team %in% OldDominion$team_opp) |>
    select(team, VoA_Rating)
  OldDominionFCSOpps <- FCS |>
    filter(team %in% OldDominion$team_opp) |>
    select(team, rating)
  colnames(OldDominionFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  OldDominionOpps <- rbind(OldDominionFBSOpps, OldDominionFCSOpps)
  colnames(OldDominionOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  OldDominion <- full_join(OldDominion, OldDominionOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  OldDominion <- OldDominion |>
    mutate(actual_diff = case_when(home_team == "Old Dominion" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Old Dominion" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Old Dominion" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Old Dominion" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  OldDominion_losses <- OldDominion |>
    filter(home_team == "Old Dominion" & home_points < away_points | away_team == "Old Dominion" & away_points < home_points)
  ## storing overall team Resume Score as vector
  OldDominion_resume <- sum(OldDominion$Resume_Score) - (7 * nrow(OldDominion_losses))
  
  
  ## OleMiss
  OleMiss <- completed_games |>
    filter(home_team == "Ole Miss" | away_team == "Ole Miss") |>
    mutate(team = "Ole Miss",
           team_opp = case_when(home_team == "Ole Miss" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Ole Miss"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  OleMissFBSOpps <- VoA_Variables |>
    filter(team %in% OleMiss$team_opp) |>
    select(team, VoA_Rating)
  OleMissFCSOpps <- FCS |>
    filter(team %in% OleMiss$team_opp) |>
    select(team, rating)
  colnames(OleMissFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  OleMissOpps <- rbind(OleMissFBSOpps, OleMissFCSOpps)
  colnames(OleMissOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  OleMiss <- full_join(OleMiss, OleMissOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  OleMiss <- OleMiss |>
    mutate(actual_diff = case_when(home_team == "Ole Miss" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Ole Miss" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Ole Miss" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Ole Miss" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  OleMiss_losses <- OleMiss |>
    filter(home_team == "Ole Miss" & home_points < away_points | away_team == "Ole Miss" & away_points < home_points)
  ## storing overall team Resume Score as vector
  OleMiss_resume <- sum(OleMiss$Resume_Score) - (7 * nrow(OleMiss_losses))
  
  
  ## Oregon
  Oregon <- completed_games |>
    filter(home_team == "Oregon" | away_team == "Oregon") |>
    mutate(team = "Oregon",
           team_opp = case_when(home_team == "Oregon" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Oregon"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  OregonFBSOpps <- VoA_Variables |>
    filter(team %in% Oregon$team_opp) |>
    select(team, VoA_Rating)
  OregonFCSOpps <- FCS |>
    filter(team %in% Oregon$team_opp) |>
    select(team, rating)
  colnames(OregonFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  OregonOpps <- rbind(OregonFBSOpps, OregonFCSOpps)
  colnames(OregonOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  Oregon <- full_join(Oregon, OregonOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  Oregon <- Oregon |>
    mutate(actual_diff = case_when(home_team == "Oregon" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Oregon" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Oregon" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Oregon" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  Oregon_losses <- Oregon |>
    filter(home_team == "Oregon" & home_points < away_points | away_team == "Oregon" & away_points < home_points)
  ## storing overall team Resume Score as vector
  Oregon_resume <- sum(Oregon$Resume_Score) - (7 * nrow(Oregon_losses))
  
  
  ## OregonSt
  OregonSt <- completed_games |>
    filter(home_team == "Oregon State" | away_team == "Oregon State") |>
    mutate(team = "Oregon State",
           team_opp = case_when(home_team == "Oregon State" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Oregon State"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  OregonStFBSOpps <- VoA_Variables |>
    filter(team %in% OregonSt$team_opp) |>
    select(team, VoA_Rating)
  OregonStFCSOpps <- FCS |>
    filter(team %in% OregonSt$team_opp) |>
    select(team, rating)
  colnames(OregonStFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  OregonStOpps <- rbind(OregonStFBSOpps, OregonStFCSOpps)
  colnames(OregonStOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  OregonSt <- full_join(OregonSt, OregonStOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  OregonSt <- OregonSt |>
    mutate(actual_diff = case_when(home_team == "Oregon State" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Oregon State" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Oregon State" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Oregon State" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  OregonSt_losses <- OregonSt |>
    filter(home_team == "Oregon State" & home_points < away_points | away_team == "Oregon State" & away_points < home_points)
  ## storing overall team Resume Score as vector
  OregonSt_resume <- sum(OregonSt$Resume_Score) - (7 * nrow(OregonSt_losses))
  
  
  ## PennSt
  PennSt <- completed_games |>
    filter(home_team == "Penn State" | away_team == "Penn State") |>
    mutate(team = "Penn State",
           team_opp = case_when(home_team == "Penn State" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Penn State"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  PennStFBSOpps <- VoA_Variables |>
    filter(team %in% PennSt$team_opp) |>
    select(team, VoA_Rating)
  PennStFCSOpps <- FCS |>
    filter(team %in% PennSt$team_opp) |>
    select(team, rating)
  colnames(PennStFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  PennStOpps <- rbind(PennStFBSOpps, PennStFCSOpps)
  colnames(PennStOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  PennSt <- full_join(PennSt, PennStOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  PennSt <- PennSt |>
    mutate(actual_diff = case_when(home_team == "Penn State" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Penn State" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Penn State" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Penn State" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  PennSt_losses <- PennSt |>
    filter(home_team == "Penn State" & home_points < away_points | away_team == "Penn State" & away_points < home_points)
  ## storing overall team Resume Score as vector
  PennSt_resume <- sum(PennSt$Resume_Score) - (7 * nrow(PennSt_losses))
  
  
  ## Pittsburgh
  Pittsburgh <- completed_games |>
    filter(home_team == "Pittsburgh" | away_team == "Pittsburgh") |>
    mutate(team = "Pittsburgh",
           team_opp = case_when(home_team == "Pittsburgh" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Pittsburgh"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  PittsburghFBSOpps <- VoA_Variables |>
    filter(team %in% Pittsburgh$team_opp) |>
    select(team, VoA_Rating)
  PittsburghFCSOpps <- FCS |>
    filter(team %in% Pittsburgh$team_opp) |>
    select(team, rating)
  colnames(PittsburghFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  PittsburghOpps <- rbind(PittsburghFBSOpps, PittsburghFCSOpps)
  colnames(PittsburghOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  Pittsburgh <- full_join(Pittsburgh, PittsburghOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  Pittsburgh <- Pittsburgh |>
    mutate(actual_diff = case_when(home_team == "Pittsburgh" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Pittsburgh" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Pittsburgh" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Pittsburgh" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  Pittsburgh_losses <- Pittsburgh |>
    filter(home_team == "Pittsburgh" & home_points < away_points | away_team == "Pittsburgh" & away_points < home_points)
  ## storing overall team Resume Score as vector
  Pittsburgh_resume <- sum(Pittsburgh$Resume_Score) - (7 * nrow(Pittsburgh_losses))
  
  
  ## Purdue
  Purdue <- completed_games |>
    filter(home_team == "Purdue" | away_team == "Purdue") |>
    mutate(team = "Purdue",
           team_opp = case_when(home_team == "Purdue" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Purdue"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  PurdueFBSOpps <- VoA_Variables |>
    filter(team %in% Purdue$team_opp) |>
    select(team, VoA_Rating)
  PurdueFCSOpps <- FCS |>
    filter(team %in% Purdue$team_opp) |>
    select(team, rating)
  colnames(PurdueFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  PurdueOpps <- rbind(PurdueFBSOpps, PurdueFCSOpps)
  colnames(PurdueOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  Purdue <- full_join(Purdue, PurdueOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  Purdue <- Purdue |>
    mutate(actual_diff = case_when(home_team == "Purdue" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Purdue" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Purdue" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Purdue" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  Purdue_losses <- Purdue |>
    filter(home_team == "Purdue" & home_points < away_points | away_team == "Purdue" & away_points < home_points)
  ## storing overall team Resume Score as vector
  Purdue_resume <- sum(Purdue$Resume_Score) - (7 * nrow(Purdue_losses))
  
  
  ## Rice
  Rice <- completed_games |>
    filter(home_team == "Rice" | away_team == "Rice") |>
    mutate(team = "Rice",
           team_opp = case_when(home_team == "Rice" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Rice"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  RiceFBSOpps <- VoA_Variables |>
    filter(team %in% Rice$team_opp) |>
    select(team, VoA_Rating)
  RiceFCSOpps <- FCS |>
    filter(team %in% Rice$team_opp) |>
    select(team, rating)
  colnames(RiceFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  RiceOpps <- rbind(RiceFBSOpps, RiceFCSOpps)
  colnames(RiceOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  Rice <- full_join(Rice, RiceOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  Rice <- Rice |>
    mutate(actual_diff = case_when(home_team == "Rice" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Rice" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Rice" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Rice" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  Rice_losses <- Rice |>
    filter(home_team == "Rice" & home_points < away_points | away_team == "Rice" & away_points < home_points)
  ## storing overall team Resume Score as vector
  Rice_resume <- sum(Rice$Resume_Score) - (7 * nrow(Rice_losses))
  
  
  ## Rutgers
  Rutgers <- completed_games |>
    filter(home_team == "Rutgers" | away_team == "Rutgers") |>
    mutate(team = "Rutgers",
           team_opp = case_when(home_team == "Rutgers" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Rutgers"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  RutgersFBSOpps <- VoA_Variables |>
    filter(team %in% Rutgers$team_opp) |>
    select(team, VoA_Rating)
  RutgersFCSOpps <- FCS |>
    filter(team %in% Rutgers$team_opp) |>
    select(team, rating)
  colnames(RutgersFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  RutgersOpps <- rbind(RutgersFBSOpps, RutgersFCSOpps)
  colnames(RutgersOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  Rutgers <- full_join(Rutgers, RutgersOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  Rutgers <- Rutgers |>
    mutate(actual_diff = case_when(home_team == "Rutgers" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Rutgers" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Rutgers" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Rutgers" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  Rutgers_losses <- Rutgers |>
    filter(home_team == "Rutgers" & home_points < away_points | away_team == "Rutgers" & away_points < home_points)
  ## storing overall team Resume Score as vector
  Rutgers_resume <- sum(Rutgers$Resume_Score) - (7 * nrow(Rutgers_losses))
  
  
  ## SanDiegoSt
  SanDiegoSt <- completed_games |>
    filter(home_team == "San Diego State" | away_team == "San Diego State") |>
    mutate(team = "San Diego State",
           team_opp = case_when(home_team == "San Diego State" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "San Diego State"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  SanDiegoStFBSOpps <- VoA_Variables |>
    filter(team %in% SanDiegoSt$team_opp) |>
    select(team, VoA_Rating)
  SanDiegoStFCSOpps <- FCS |>
    filter(team %in% SanDiegoSt$team_opp) |>
    select(team, rating)
  colnames(SanDiegoStFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  SanDiegoStOpps <- rbind(SanDiegoStFBSOpps, SanDiegoStFCSOpps)
  colnames(SanDiegoStOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  SanDiegoSt <- full_join(SanDiegoSt, SanDiegoStOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  SanDiegoSt <- SanDiegoSt |>
    mutate(actual_diff = case_when(home_team == "San Diego State" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "San Diego State" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "San Diego State" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "San Diego State" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  SanDiegoSt_losses <- SanDiegoSt |>
    filter(home_team == "San Diego State" & home_points < away_points | away_team == "San Diego State" & away_points < home_points)
  ## storing overall team Resume Score as vector
  SanDiegoSt_resume <- sum(SanDiegoSt$Resume_Score) - (7 * nrow(SanDiegoSt_losses))
  
  
  ## SanJoseSt
  SanJoseSt <- completed_games |>
    filter(home_team == "San José State" | away_team == "San José State") |>
    mutate(team = "San José State",
           team_opp = case_when(home_team == "San José State" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "San José State"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  SanJoseStFBSOpps <- VoA_Variables |>
    filter(team %in% SanJoseSt$team_opp) |>
    select(team, VoA_Rating)
  SanJoseStFCSOpps <- FCS |>
    filter(team %in% SanJoseSt$team_opp) |>
    select(team, rating)
  colnames(SanJoseStFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  SanJoseStOpps <- rbind(SanJoseStFBSOpps, SanJoseStFCSOpps)
  colnames(SanJoseStOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  SanJoseSt <- full_join(SanJoseSt, SanJoseStOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  SanJoseSt <- SanJoseSt |>
    mutate(actual_diff = case_when(home_team == "San José State" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "San José State" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "San José State" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "San José State" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  SanJoseSt_losses <- SanJoseSt |>
    filter(home_team == "San José State" & home_points < away_points | away_team == "San José State" & away_points < home_points)
  ## storing overall team Resume Score as vector
  SanJoseSt_resume <- sum(SanJoseSt$Resume_Score) - (7 * nrow(SanJoseSt_losses))
  
  
  ## SMU
  SMU <- completed_games |>
    filter(home_team == "SMU" | away_team == "SMU") |>
    mutate(team = "SMU",
           team_opp = case_when(home_team == "SMU" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "SMU"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  SMUFBSOpps <- VoA_Variables |>
    filter(team %in% SMU$team_opp) |>
    select(team, VoA_Rating)
  SMUFCSOpps <- FCS |>
    filter(team %in% SMU$team_opp) |>
    select(team, rating)
  colnames(SMUFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  SMUOpps <- rbind(SMUFBSOpps, SMUFCSOpps)
  colnames(SMUOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  SMU <- full_join(SMU, SMUOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  SMU <- SMU |>
    mutate(actual_diff = case_when(home_team == "SMU" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "SMU" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "SMU" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "SMU" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  SMU_losses <- SMU |>
    filter(home_team == "SMU" & home_points < away_points | away_team == "SMU" & away_points < home_points)
  ## storing overall team Resume Score as vector
  SMU_resume <- sum(SMU$Resume_Score) - (7 * nrow(SMU_losses))
  
  
  ## SouthAlabama
  SouthAlabama <- completed_games |>
    filter(home_team == "South Alabama" | away_team == "South Alabama") |>
    mutate(team = "South Alabama",
           team_opp = case_when(home_team == "South Alabama" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "South Alabama"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  SouthAlabamaFBSOpps <- VoA_Variables |>
    filter(team %in% SouthAlabama$team_opp) |>
    select(team, VoA_Rating)
  SouthAlabamaFCSOpps <- FCS |>
    filter(team %in% SouthAlabama$team_opp) |>
    select(team, rating)
  colnames(SouthAlabamaFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  SouthAlabamaOpps <- rbind(SouthAlabamaFBSOpps, SouthAlabamaFCSOpps)
  colnames(SouthAlabamaOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  SouthAlabama <- full_join(SouthAlabama, SouthAlabamaOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  SouthAlabama <- SouthAlabama |>
    mutate(actual_diff = case_when(home_team == "South Alabama" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "South Alabama" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "South Alabama" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "South Alabama" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  SouthAlabama_losses <- SouthAlabama |>
    filter(home_team == "South Alabama" & home_points < away_points | away_team == "South Alabama" & away_points < home_points)
  ## storing overall team Resume Score as vector
  SouthAlabama_resume <- sum(SouthAlabama$Resume_Score) - (7 * nrow(SouthAlabama_losses))
  
  
  ## SouthCarolina
  SouthCarolina <- completed_games |>
    filter(home_team == "South Carolina" | away_team == "South Carolina") |>
    mutate(team = "South Carolina",
           team_opp = case_when(home_team == "South Carolina" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "South Carolina"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  SouthCarolinaFBSOpps <- VoA_Variables |>
    filter(team %in% SouthCarolina$team_opp) |>
    select(team, VoA_Rating)
  SouthCarolinaFCSOpps <- FCS |>
    filter(team %in% SouthCarolina$team_opp) |>
    select(team, rating)
  colnames(SouthCarolinaFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  SouthCarolinaOpps <- rbind(SouthCarolinaFBSOpps, SouthCarolinaFCSOpps)
  colnames(SouthCarolinaOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  SouthCarolina <- full_join(SouthCarolina, SouthCarolinaOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  SouthCarolina <- SouthCarolina |>
    mutate(actual_diff = case_when(home_team == "South Carolina" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "South Carolina" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "South Carolina" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "South Carolina" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  SouthCarolina_losses <- SouthCarolina |>
    filter(home_team == "South Carolina" & home_points < away_points | away_team == "South Carolina" & away_points < home_points)
  ## storing overall team Resume Score as vector
  SouthCarolina_resume <- sum(SouthCarolina$Resume_Score) - (7 * nrow(SouthCarolina_losses))
  
  
  ## SouthFlorida
  SouthFlorida <- completed_games |>
    filter(home_team == "South Florida" | away_team == "South Florida") |>
    mutate(team = "South Florida",
           team_opp = case_when(home_team == "South Florida" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "South Florida"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  SouthFloridaFBSOpps <- VoA_Variables |>
    filter(team %in% SouthFlorida$team_opp) |>
    select(team, VoA_Rating)
  SouthFloridaFCSOpps <- FCS |>
    filter(team %in% SouthFlorida$team_opp) |>
    select(team, rating)
  colnames(SouthFloridaFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  SouthFloridaOpps <- rbind(SouthFloridaFBSOpps, SouthFloridaFCSOpps)
  colnames(SouthFloridaOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  SouthFlorida <- full_join(SouthFlorida, SouthFloridaOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  SouthFlorida <- SouthFlorida |>
    mutate(actual_diff = case_when(home_team == "South Florida" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "South Florida" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "South Florida" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "South Florida" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  SouthFlorida_losses <- SouthFlorida |>
    filter(home_team == "South Florida" & home_points < away_points | away_team == "South Florida" & away_points < home_points)
  ## storing overall team Resume Score as vector
  SouthFlorida_resume <- sum(SouthFlorida$Resume_Score) - (7 * nrow(SouthFlorida_losses))
  
  
  ## SouthernMiss
  SouthernMiss <- completed_games |>
    filter(home_team == "Southern Mississippi" | away_team == "Southern Mississippi") |>
    mutate(team = "Southern Mississippi",
           team_opp = case_when(home_team == "Southern Mississippi" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Southern Mississippi"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  SouthernMissFBSOpps <- VoA_Variables |>
    filter(team %in% SouthernMiss$team_opp) |>
    select(team, VoA_Rating)
  SouthernMissFCSOpps <- FCS |>
    filter(team %in% SouthernMiss$team_opp) |>
    select(team, rating)
  colnames(SouthernMissFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  SouthernMissOpps <- rbind(SouthernMissFBSOpps, SouthernMissFCSOpps)
  colnames(SouthernMissOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  SouthernMiss <- full_join(SouthernMiss, SouthernMissOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  SouthernMiss <- SouthernMiss |>
    mutate(actual_diff = case_when(home_team == "Southern Mississippi" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Southern Mississippi" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Southern Mississippi" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Southern Mississippi" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  SouthernMiss_losses <- SouthernMiss |>
    filter(home_team == "Southern Mississippi" & home_points < away_points | away_team == "Southern Mississippi" & away_points < home_points)
  ## storing overall team Resume Score as vector
  SouthernMiss_resume <- sum(SouthernMiss$Resume_Score) - (7 * nrow(SouthernMiss_losses))
  
  
  ## Stanford
  Stanford <- completed_games |>
    filter(home_team == "Stanford" | away_team == "Stanford") |>
    mutate(team = "Stanford",
           team_opp = case_when(home_team == "Stanford" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Stanford"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  StanfordFBSOpps <- VoA_Variables |>
    filter(team %in% Stanford$team_opp) |>
    select(team, VoA_Rating)
  StanfordFCSOpps <- FCS |>
    filter(team %in% Stanford$team_opp) |>
    select(team, rating)
  colnames(StanfordFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  StanfordOpps <- rbind(StanfordFBSOpps, StanfordFCSOpps)
  colnames(StanfordOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  Stanford <- full_join(Stanford, StanfordOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  Stanford <- Stanford |>
    mutate(actual_diff = case_when(home_team == "Stanford" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Stanford" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Stanford" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Stanford" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  Stanford_losses <- Stanford |>
    filter(home_team == "Stanford" & home_points < away_points | away_team == "Stanford" & away_points < home_points)
  ## storing overall team Resume Score as vector
  Stanford_resume <- sum(Stanford$Resume_Score) - (7 * nrow(Stanford_losses))
  
  
  ## Syracuse
  Syracuse <- completed_games |>
    filter(home_team == "Syracuse" | away_team == "Syracuse") |>
    mutate(team = "Syracuse",
           team_opp = case_when(home_team == "Syracuse" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Syracuse"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  SyracuseFBSOpps <- VoA_Variables |>
    filter(team %in% Syracuse$team_opp) |>
    select(team, VoA_Rating)
  SyracuseFCSOpps <- FCS |>
    filter(team %in% Syracuse$team_opp) |>
    select(team, rating)
  colnames(SyracuseFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  SyracuseOpps <- rbind(SyracuseFBSOpps, SyracuseFCSOpps)
  colnames(SyracuseOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  Syracuse <- full_join(Syracuse, SyracuseOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  Syracuse <- Syracuse |>
    mutate(actual_diff = case_when(home_team == "Syracuse" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Syracuse" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Syracuse" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Syracuse" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  Syracuse_losses <- Syracuse |>
    filter(home_team == "Syracuse" & home_points < away_points | away_team == "Syracuse" & away_points < home_points)
  ## storing overall team Resume Score as vector
  Syracuse_resume <- sum(Syracuse$Resume_Score) - (7 * nrow(Syracuse_losses))
  
  
  ## TCU
  TCU <- completed_games |>
    filter(home_team == "TCU" | away_team == "TCU") |>
    mutate(team = "TCU",
           team_opp = case_when(home_team == "TCU" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "TCU"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  TCUFBSOpps <- VoA_Variables |>
    filter(team %in% TCU$team_opp) |>
    select(team, VoA_Rating)
  TCUFCSOpps <- FCS |>
    filter(team %in% TCU$team_opp) |>
    select(team, rating)
  colnames(TCUFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  TCUOpps <- rbind(TCUFBSOpps, TCUFCSOpps)
  colnames(TCUOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  TCU <- full_join(TCU, TCUOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  TCU <- TCU |>
    mutate(actual_diff = case_when(home_team == "TCU" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "TCU" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "TCU" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "TCU" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  TCU_losses <- TCU |>
    filter(home_team == "TCU" & home_points < away_points | away_team == "TCU" & away_points < home_points)
  ## storing overall team Resume Score as vector
  TCU_resume <- sum(TCU$Resume_Score) - (7 * nrow(TCU_losses))
  
  
  ## Temple
  Temple <- completed_games |>
    filter(home_team == "Temple" | away_team == "Temple") |>
    mutate(team = "Temple",
           team_opp = case_when(home_team == "Temple" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Temple"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  TempleFBSOpps <- VoA_Variables |>
    filter(team %in% Temple$team_opp) |>
    select(team, VoA_Rating)
  TempleFCSOpps <- FCS |>
    filter(team %in% Temple$team_opp) |>
    select(team, rating)
  colnames(TempleFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  TempleOpps <- rbind(TempleFBSOpps, TempleFCSOpps)
  colnames(TempleOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  Temple <- full_join(Temple, TempleOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  Temple <- Temple |>
    mutate(actual_diff = case_when(home_team == "Temple" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Temple" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Temple" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Temple" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  Temple_losses <- Temple |>
    filter(home_team == "Temple" & home_points < away_points | away_team == "Temple" & away_points < home_points)
  ## storing overall team Resume Score as vector
  Temple_resume <- sum(Temple$Resume_Score) - (7 * nrow(Temple_losses))
  
  
  ## Tennessee
  Tennessee <- completed_games |>
    filter(home_team == "Tennessee" | away_team == "Tennessee") |>
    mutate(team = "Tennessee",
           team_opp = case_when(home_team == "Tennessee" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Tennessee"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  TennesseeFBSOpps <- VoA_Variables |>
    filter(team %in% Tennessee$team_opp) |>
    select(team, VoA_Rating)
  TennesseeFCSOpps <- FCS |>
    filter(team %in% Tennessee$team_opp) |>
    select(team, rating)
  colnames(TennesseeFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  TennesseeOpps <- rbind(TennesseeFBSOpps, TennesseeFCSOpps)
  colnames(TennesseeOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  Tennessee <- full_join(Tennessee, TennesseeOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  Tennessee <- Tennessee |>
    mutate(actual_diff = case_when(home_team == "Tennessee" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Tennessee" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Tennessee" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Tennessee" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  Tennessee_losses <- Tennessee |>
    filter(home_team == "Tennessee" & home_points < away_points | away_team == "Tennessee" & away_points < home_points)
  ## storing overall team Resume Score as vector
  Tennessee_resume <- sum(Tennessee$Resume_Score) - (7 * nrow(Tennessee_losses))
  
  
  ## Texas
  Texas <- completed_games |>
    filter(home_team == "Texas" | away_team == "Texas") |>
    mutate(team = "Texas",
           team_opp = case_when(home_team == "Texas" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Texas"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  TexasFBSOpps <- VoA_Variables |>
    filter(team %in% Texas$team_opp) |>
    select(team, VoA_Rating)
  TexasFCSOpps <- FCS |>
    filter(team %in% Texas$team_opp) |>
    select(team, rating)
  colnames(TexasFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  TexasOpps <- rbind(TexasFBSOpps, TexasFCSOpps)
  colnames(TexasOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  Texas <- full_join(Texas, TexasOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  Texas <- Texas |>
    mutate(actual_diff = case_when(home_team == "Texas" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Texas" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Texas" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Texas" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  Texas_losses <- Texas |>
    filter(home_team == "Texas" & home_points < away_points | away_team == "Texas" & away_points < home_points)
  ## storing overall team Resume Score as vector
  Texas_resume <- sum(Texas$Resume_Score) - (7 * nrow(Texas_losses))
  
  
  ## TexasAM
  TexasAM <- completed_games |>
    filter(home_team == "Texas A&M" | away_team == "Texas A&M") |>
    mutate(team = "Texas A&M",
           team_opp = case_when(home_team == "Texas A&M" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Texas A&M"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  TexasAMFBSOpps <- VoA_Variables |>
    filter(team %in% TexasAM$team_opp) |>
    select(team, VoA_Rating)
  TexasAMFCSOpps <- FCS |>
    filter(team %in% TexasAM$team_opp) |>
    select(team, rating)
  colnames(TexasAMFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  TexasAMOpps <- rbind(TexasAMFBSOpps, TexasAMFCSOpps)
  colnames(TexasAMOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  TexasAM <- full_join(TexasAM, TexasAMOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  TexasAM <- TexasAM |>
    mutate(actual_diff = case_when(home_team == "Texas A&M" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Texas A&M" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Texas A&M" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Texas A&M" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  TexasAM_losses <- TexasAM |>
    filter(home_team == "Texas A&M" & home_points < away_points | away_team == "Texas A&M" & away_points < home_points)
  ## storing overall team Resume Score as vector
  TexasAM_resume <- sum(TexasAM$Resume_Score) - (7 * nrow(TexasAM_losses))
  
  
  ## TexasSt
  TexasSt <- completed_games |>
    filter(home_team == "Texas State" | away_team == "Texas State") |>
    mutate(team = "Texas State",
           team_opp = case_when(home_team == "Texas State" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Texas State"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  TexasStFBSOpps <- VoA_Variables |>
    filter(team %in% TexasSt$team_opp) |>
    select(team, VoA_Rating)
  TexasStFCSOpps <- FCS |>
    filter(team %in% TexasSt$team_opp) |>
    select(team, rating)
  colnames(TexasStFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  TexasStOpps <- rbind(TexasStFBSOpps, TexasStFCSOpps)
  colnames(TexasStOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  TexasSt <- full_join(TexasSt, TexasStOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  TexasSt <- TexasSt |>
    mutate(actual_diff = case_when(home_team == "Texas State" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Texas State" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Texas State" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Texas State" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  TexasSt_losses <- TexasSt |>
    filter(home_team == "Texas State" & home_points < away_points | away_team == "Texas State" & away_points < home_points)
  ## storing overall team Resume Score as vector
  TexasSt_resume <- sum(TexasSt$Resume_Score) - (7 * nrow(TexasSt_losses))
  
  
  ## TexasTech
  TexasTech <- completed_games |>
    filter(home_team == "Texas Tech" | away_team == "Texas Tech") |>
    mutate(team = "Texas Tech",
           team_opp = case_when(home_team == "Texas Tech" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Texas Tech"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  TexasTechFBSOpps <- VoA_Variables |>
    filter(team %in% TexasTech$team_opp) |>
    select(team, VoA_Rating)
  TexasTechFCSOpps <- FCS |>
    filter(team %in% TexasTech$team_opp) |>
    select(team, rating)
  colnames(TexasTechFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  TexasTechOpps <- rbind(TexasTechFBSOpps, TexasTechFCSOpps)
  colnames(TexasTechOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  TexasTech <- full_join(TexasTech, TexasTechOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  TexasTech <- TexasTech |>
    mutate(actual_diff = case_when(home_team == "Texas Tech" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Texas Tech" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Texas Tech" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Texas Tech" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  TexasTech_losses <- TexasTech |>
    filter(home_team == "Texas Tech" & home_points < away_points | away_team == "Texas Tech" & away_points < home_points)
  ## storing overall team Resume Score as vector
  TexasTech_resume <- sum(TexasTech$Resume_Score) - (7 * nrow(TexasTech_losses))
  
  
  ## Toledo
  Toledo <- completed_games |>
    filter(home_team == "Toledo" | away_team == "Toledo") |>
    mutate(team = "Toledo",
           team_opp = case_when(home_team == "Toledo" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Toledo"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  ToledoFBSOpps <- VoA_Variables |>
    filter(team %in% Toledo$team_opp) |>
    select(team, VoA_Rating)
  ToledoFCSOpps <- FCS |>
    filter(team %in% Toledo$team_opp) |>
    select(team, rating)
  colnames(ToledoFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  ToledoOpps <- rbind(ToledoFBSOpps, ToledoFCSOpps)
  colnames(ToledoOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  Toledo <- full_join(Toledo, ToledoOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  Toledo <- Toledo |>
    mutate(actual_diff = case_when(home_team == "Toledo" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Toledo" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Toledo" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Toledo" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  Toledo_losses <- Toledo |>
    filter(home_team == "Toledo" & home_points < away_points | away_team == "Toledo" & away_points < home_points)
  ## storing overall team Resume Score as vector
  Toledo_resume <- sum(Toledo$Resume_Score) - (7 * nrow(Toledo_losses))
  
  
  ## Troy
  Troy <- completed_games |>
    filter(home_team == "Troy" | away_team == "Troy") |>
    mutate(team = "Troy",
           team_opp = case_when(home_team == "Troy" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Troy"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  TroyFBSOpps <- VoA_Variables |>
    filter(team %in% Troy$team_opp) |>
    select(team, VoA_Rating)
  TroyFCSOpps <- FCS |>
    filter(team %in% Troy$team_opp) |>
    select(team, rating)
  colnames(TroyFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  TroyOpps <- rbind(TroyFBSOpps, TroyFCSOpps)
  colnames(TroyOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  Troy <- full_join(Troy, TroyOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  Troy <- Troy |>
    mutate(actual_diff = case_when(home_team == "Troy" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Troy" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Troy" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Troy" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  Troy_losses <- Troy |>
    filter(home_team == "Troy" & home_points < away_points | away_team == "Troy" & away_points < home_points)
  ## storing overall team Resume Score as vector
  Troy_resume <- sum(Troy$Resume_Score) - (7 * nrow(Troy_losses))
  
  
  ## Tulane
  Tulane <- completed_games |>
    filter(home_team == "Tulane" | away_team == "Tulane") |>
    mutate(team = "Tulane",
           team_opp = case_when(home_team == "Tulane" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Tulane"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  TulaneFBSOpps <- VoA_Variables |>
    filter(team %in% Tulane$team_opp) |>
    select(team, VoA_Rating)
  TulaneFCSOpps <- FCS |>
    filter(team %in% Tulane$team_opp) |>
    select(team, rating)
  colnames(TulaneFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  TulaneOpps <- rbind(TulaneFBSOpps, TulaneFCSOpps)
  colnames(TulaneOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  Tulane <- full_join(Tulane, TulaneOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  Tulane <- Tulane |>
    mutate(actual_diff = case_when(home_team == "Tulane" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Tulane" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Tulane" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Tulane" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  Tulane_losses <- Tulane |>
    filter(home_team == "Tulane" & home_points < away_points | away_team == "Tulane" & away_points < home_points)
  ## storing overall team Resume Score as vector
  Tulane_resume <- sum(Tulane$Resume_Score) - (7 * nrow(Tulane_losses))
  
  
  ## Tulsa
  Tulsa <- completed_games |>
    filter(home_team == "Tulsa" | away_team == "Tulsa") |>
    mutate(team = "Tulsa",
           team_opp = case_when(home_team == "Tulsa" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Tulsa"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  TulsaFBSOpps <- VoA_Variables |>
    filter(team %in% Tulsa$team_opp) |>
    select(team, VoA_Rating)
  TulsaFCSOpps <- FCS |>
    filter(team %in% Tulsa$team_opp) |>
    select(team, rating)
  colnames(TulsaFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  TulsaOpps <- rbind(TulsaFBSOpps, TulsaFCSOpps)
  colnames(TulsaOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  Tulsa <- full_join(Tulsa, TulsaOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  Tulsa <- Tulsa |>
    mutate(actual_diff = case_when(home_team == "Tulsa" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Tulsa" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Tulsa" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Tulsa" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  Tulsa_losses <- Tulsa |>
    filter(home_team == "Tulsa" & home_points < away_points | away_team == "Tulsa" & away_points < home_points)
  ## storing overall team Resume Score as vector
  Tulsa_resume <- sum(Tulsa$Resume_Score) - (7 * nrow(Tulsa_losses))
  
  
  ## UAB
  UAB <- completed_games |>
    filter(home_team == "UAB" | away_team == "UAB") |>
    mutate(team = "UAB",
           team_opp = case_when(home_team == "UAB" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "UAB"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  UABFBSOpps <- VoA_Variables |>
    filter(team %in% UAB$team_opp) |>
    select(team, VoA_Rating)
  UABFCSOpps <- FCS |>
    filter(team %in% UAB$team_opp) |>
    select(team, rating)
  colnames(UABFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  UABOpps <- rbind(UABFBSOpps, UABFCSOpps)
  colnames(UABOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  UAB <- full_join(UAB, UABOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  UAB <- UAB |>
    mutate(actual_diff = case_when(home_team == "UAB" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "UAB" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "UAB" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "UAB" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  UAB_losses <- UAB |>
    filter(home_team == "UAB" & home_points < away_points | away_team == "UAB" & away_points < home_points)
  ## storing overall team Resume Score as vector
  UAB_resume <- sum(UAB$Resume_Score) - (7 * nrow(UAB_losses))
  
  
  ## UCF
  UCF <- completed_games |>
    filter(home_team == "UCF" | away_team == "UCF") |>
    mutate(team = "UCF",
           team_opp = case_when(home_team == "UCF" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "UCF"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  UCFFBSOpps <- VoA_Variables |>
    filter(team %in% UCF$team_opp) |>
    select(team, VoA_Rating)
  UCFFCSOpps <- FCS |>
    filter(team %in% UCF$team_opp) |>
    select(team, rating)
  colnames(UCFFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  UCFOpps <- rbind(UCFFBSOpps, UCFFCSOpps)
  colnames(UCFOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  UCF <- full_join(UCF, UCFOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  UCF <- UCF |>
    mutate(actual_diff = case_when(home_team == "UCF" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "UCF" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "UCF" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "UCF" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  UCF_losses <- UCF |>
    filter(home_team == "UCF" & home_points < away_points | away_team == "UCF" & away_points < home_points)
  ## storing overall team Resume Score as vector
  UCF_resume <- sum(UCF$Resume_Score) - (7 * nrow(UCF_losses))
  
  
  ## UCLA
  UCLA <- completed_games |>
    filter(home_team == "UCLA" | away_team == "UCLA") |>
    mutate(team = "UCLA",
           team_opp = case_when(home_team == "UCLA" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "UCLA"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  UCLAFBSOpps <- VoA_Variables |>
    filter(team %in% UCLA$team_opp) |>
    select(team, VoA_Rating)
  UCLAFCSOpps <- FCS |>
    filter(team %in% UCLA$team_opp) |>
    select(team, rating)
  colnames(UCLAFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  UCLAOpps <- rbind(UCLAFBSOpps, UCLAFCSOpps)
  colnames(UCLAOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  UCLA <- full_join(UCLA, UCLAOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  UCLA <- UCLA |>
    mutate(actual_diff = case_when(home_team == "UCLA" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "UCLA" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "UCLA" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "UCLA" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  UCLA_losses <- UCLA |>
    filter(home_team == "UCLA" & home_points < away_points | away_team == "UCLA" & away_points < home_points)
  ## storing overall team Resume Score as vector
  UCLA_resume <- sum(UCLA$Resume_Score) - (7 * nrow(UCLA_losses))
  
  
  ## UMass
  UMass <- completed_games |>
    filter(home_team == "UMass" | away_team == "UMass") |>
    mutate(team = "UMass",
           team_opp = case_when(home_team == "UMass" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "UMass"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  UMassFBSOpps <- VoA_Variables |>
    filter(team %in% UMass$team_opp) |>
    select(team, VoA_Rating)
  UMassFCSOpps <- FCS |>
    filter(team %in% UMass$team_opp) |>
    select(team, rating)
  colnames(UMassFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  UMassOpps <- rbind(UMassFBSOpps, UMassFCSOpps)
  colnames(UMassOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  UMass <- full_join(UMass, UMassOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  UMass <- UMass |>
    mutate(actual_diff = case_when(home_team == "UMass" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "UMass" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "UMass" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "UMass" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  UMass_losses <- UMass |>
    filter(home_team == "UMass" & home_points < away_points | away_team == "UMass" & away_points < home_points)
  ## storing overall team Resume Score as vector
  UMass_resume <- sum(UMass$Resume_Score) - (7 * nrow(UMass_losses))
  
  
  ## UNLV
  UNLV <- completed_games |>
    filter(home_team == "UNLV" | away_team == "UNLV") |>
    mutate(team = "UNLV",
           team_opp = case_when(home_team == "UNLV" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "UNLV"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  UNLVFBSOpps <- VoA_Variables |>
    filter(team %in% UNLV$team_opp) |>
    select(team, VoA_Rating)
  UNLVFCSOpps <- FCS |>
    filter(team %in% UNLV$team_opp) |>
    select(team, rating)
  colnames(UNLVFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  UNLVOpps <- rbind(UNLVFBSOpps, UNLVFCSOpps)
  colnames(UNLVOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  UNLV <- full_join(UNLV, UNLVOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  UNLV <- UNLV |>
    mutate(actual_diff = case_when(home_team == "UNLV" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "UNLV" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "UNLV" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "UNLV" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  UNLV_losses <- UNLV |>
    filter(home_team == "UNLV" & home_points < away_points | away_team == "UNLV" & away_points < home_points)
  ## storing overall team Resume Score as vector
  UNLV_resume <- sum(UNLV$Resume_Score) - (7 * nrow(UNLV_losses))
  
  
  ## USC
  USC <- completed_games |>
    filter(home_team == "USC" | away_team == "USC") |>
    mutate(team = "USC",
           team_opp = case_when(home_team == "USC" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "USC"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  USCFBSOpps <- VoA_Variables |>
    filter(team %in% USC$team_opp) |>
    select(team, VoA_Rating)
  USCFCSOpps <- FCS |>
    filter(team %in% USC$team_opp) |>
    select(team, rating)
  colnames(USCFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  USCOpps <- rbind(USCFBSOpps, USCFCSOpps)
  colnames(USCOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  USC <- full_join(USC, USCOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  USC <- USC |>
    mutate(actual_diff = case_when(home_team == "USC" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "USC" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "USC" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "USC" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  USC_losses <- USC |>
    filter(home_team == "USC" & home_points < away_points | away_team == "USC" & away_points < home_points)
  ## storing overall team Resume Score as vector
  USC_resume <- sum(USC$Resume_Score) - (7 * nrow(USC_losses))
  
  
  ## UTSA
  UTSA <- completed_games |>
    filter(home_team == "UT San Antonio" | away_team == "UT San Antonio") |>
    mutate(team = "UT San Antonio",
           team_opp = case_when(home_team == "UT San Antonio" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "UT San Antonio"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  UTSAFBSOpps <- VoA_Variables |>
    filter(team %in% UTSA$team_opp) |>
    select(team, VoA_Rating)
  UTSAFCSOpps <- FCS |>
    filter(team %in% UTSA$team_opp) |>
    select(team, rating)
  colnames(UTSAFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  UTSAOpps <- rbind(UTSAFBSOpps, UTSAFCSOpps)
  colnames(UTSAOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  UTSA <- full_join(UTSA, UTSAOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  UTSA <- UTSA |>
    mutate(actual_diff = case_when(home_team == "UT San Antonio" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "UT San Antonio" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "UT San Antonio" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "UT San Antonio" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  UTSA_losses <- UTSA |>
    filter(home_team == "UT San Antonio" & home_points < away_points | away_team == "UT San Antonio" & away_points < home_points)
  ## storing overall team Resume Score as vector
  UTSA_resume <- sum(UTSA$Resume_Score) - (7 * nrow(UTSA_losses))
  
  
  ## Utah
  Utah <- completed_games |>
    filter(home_team == "Utah" | away_team == "Utah") |>
    mutate(team = "Utah",
           team_opp = case_when(home_team == "Utah" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Utah"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  UtahFBSOpps <- VoA_Variables |>
    filter(team %in% Utah$team_opp) |>
    select(team, VoA_Rating)
  UtahFCSOpps <- FCS |>
    filter(team %in% Utah$team_opp) |>
    select(team, rating)
  colnames(UtahFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  UtahOpps <- rbind(UtahFBSOpps, UtahFCSOpps)
  colnames(UtahOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  Utah <- full_join(Utah, UtahOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  Utah <- Utah |>
    mutate(actual_diff = case_when(home_team == "Utah" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Utah" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Utah" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Utah" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  Utah_losses <- Utah |>
    filter(home_team == "Utah" & home_points < away_points | away_team == "Utah" & away_points < home_points)
  ## storing overall team Resume Score as vector
  Utah_resume <- sum(Utah$Resume_Score) - (7 * nrow(Utah_losses))
  
  
  ## UtahSt
  UtahSt <- completed_games |>
    filter(home_team == "Utah State" | away_team == "Utah State") |>
    mutate(team = "Utah State",
           team_opp = case_when(home_team == "Utah State" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Utah State"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  UtahStFBSOpps <- VoA_Variables |>
    filter(team %in% UtahSt$team_opp) |>
    select(team, VoA_Rating)
  UtahStFCSOpps <- FCS |>
    filter(team %in% UtahSt$team_opp) |>
    select(team, rating)
  colnames(UtahStFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  UtahStOpps <- rbind(UtahStFBSOpps, UtahStFCSOpps)
  colnames(UtahStOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  UtahSt <- full_join(UtahSt, UtahStOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  UtahSt <- UtahSt |>
    mutate(actual_diff = case_when(home_team == "Utah State" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Utah State" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Utah State" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Utah State" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  UtahSt_losses <- UtahSt |>
    filter(home_team == "Utah State" & home_points < away_points | away_team == "Utah State" & away_points < home_points)
  ## storing overall team Resume Score as vector
  UtahSt_resume <- sum(UtahSt$Resume_Score) - (7 * nrow(UtahSt_losses))
  
  
  ## UTEP
  UTEP <- completed_games |>
    filter(home_team == "UTEP" | away_team == "UTEP") |>
    mutate(team = "UTEP",
           team_opp = case_when(home_team == "UTEP" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "UTEP"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  UTEPFBSOpps <- VoA_Variables |>
    filter(team %in% UTEP$team_opp) |>
    select(team, VoA_Rating)
  UTEPFCSOpps <- FCS |>
    filter(team %in% UTEP$team_opp) |>
    select(team, rating)
  colnames(UTEPFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  UTEPOpps <- rbind(UTEPFBSOpps, UTEPFCSOpps)
  colnames(UTEPOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  UTEP <- full_join(UTEP, UTEPOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  UTEP <- UTEP |>
    mutate(actual_diff = case_when(home_team == "UTEP" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "UTEP" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "UTEP" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "UTEP" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  UTEP_losses <- UTEP |>
    filter(home_team == "UTEP" & home_points < away_points | away_team == "UTEP" & away_points < home_points)
  ## storing overall team Resume Score as vector
  UTEP_resume <- sum(UTEP$Resume_Score) - (7 * nrow(UTEP_losses))
  
  
  ## Vanderbilt
  Vanderbilt <- completed_games |>
    filter(home_team == "Vanderbilt" | away_team == "Vanderbilt") |>
    mutate(team = "Vanderbilt",
           team_opp = case_when(home_team == "Vanderbilt" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Vanderbilt"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  VanderbiltFBSOpps <- VoA_Variables |>
    filter(team %in% Vanderbilt$team_opp) |>
    select(team, VoA_Rating)
  VanderbiltFCSOpps <- FCS |>
    filter(team %in% Vanderbilt$team_opp) |>
    select(team, rating)
  colnames(VanderbiltFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  VanderbiltOpps <- rbind(VanderbiltFBSOpps, VanderbiltFCSOpps)
  colnames(VanderbiltOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  Vanderbilt <- full_join(Vanderbilt, VanderbiltOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  Vanderbilt <- Vanderbilt |>
    mutate(actual_diff = case_when(home_team == "Vanderbilt" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Vanderbilt" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Vanderbilt" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Vanderbilt" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  Vanderbilt_losses <- Vanderbilt |>
    filter(home_team == "Vanderbilt" & home_points < away_points | away_team == "Vanderbilt" & away_points < home_points)
  ## storing overall team Resume Score as vector
  Vanderbilt_resume <- sum(Vanderbilt$Resume_Score) - (7 * nrow(Vanderbilt_losses))
  
  
  ## Virginia
  Virginia <- completed_games |>
    filter(home_team == "Virginia" | away_team == "Virginia") |>
    mutate(team = "Virginia",
           team_opp = case_when(home_team == "Virginia" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Virginia"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  VirginiaFBSOpps <- VoA_Variables |>
    filter(team %in% Virginia$team_opp) |>
    select(team, VoA_Rating)
  VirginiaFCSOpps <- FCS |>
    filter(team %in% Virginia$team_opp) |>
    select(team, rating)
  colnames(VirginiaFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  VirginiaOpps <- rbind(VirginiaFBSOpps, VirginiaFCSOpps)
  colnames(VirginiaOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  Virginia <- full_join(Virginia, VirginiaOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  Virginia <- Virginia |>
    mutate(actual_diff = case_when(home_team == "Virginia" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Virginia" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Virginia" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Virginia" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  Virginia_losses <- Virginia |>
    filter(home_team == "Virginia" & home_points < away_points | away_team == "Virginia" & away_points < home_points)
  ## storing overall team Resume Score as vector
  Virginia_resume <- sum(Virginia$Resume_Score) - (7 * nrow(Virginia_losses))
  
  
  ## VirginiaTech
  VirginiaTech <- completed_games |>
    filter(home_team == "Virginia Tech" | away_team == "Virginia Tech") |>
    mutate(team = "Virginia Tech",
           team_opp = case_when(home_team == "Virginia Tech" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Virginia Tech"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  VirginiaTechFBSOpps <- VoA_Variables |>
    filter(team %in% VirginiaTech$team_opp) |>
    select(team, VoA_Rating)
  VirginiaTechFCSOpps <- FCS |>
    filter(team %in% VirginiaTech$team_opp) |>
    select(team, rating)
  colnames(VirginiaTechFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  VirginiaTechOpps <- rbind(VirginiaTechFBSOpps, VirginiaTechFCSOpps)
  colnames(VirginiaTechOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  VirginiaTech <- full_join(VirginiaTech, VirginiaTechOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  VirginiaTech <- VirginiaTech |>
    mutate(actual_diff = case_when(home_team == "Virginia Tech" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Virginia Tech" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Virginia Tech" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Virginia Tech" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  VirginiaTech_losses <- VirginiaTech |>
    filter(home_team == "Virginia Tech" & home_points < away_points | away_team == "Virginia Tech" & away_points < home_points)
  ## storing overall team Resume Score as vector
  VirginiaTech_resume <- sum(VirginiaTech$Resume_Score) - (7 * nrow(VirginiaTech_losses))
  
  
  ## WakeForest
  WakeForest <- completed_games |>
    filter(home_team == "Wake Forest" | away_team == "Wake Forest") |>
    mutate(team = "Wake Forest",
           team_opp = case_when(home_team == "Wake Forest" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Wake Forest"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  WakeForestFBSOpps <- VoA_Variables |>
    filter(team %in% WakeForest$team_opp) |>
    select(team, VoA_Rating)
  WakeForestFCSOpps <- FCS |>
    filter(team %in% WakeForest$team_opp) |>
    select(team, rating)
  colnames(WakeForestFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  WakeForestOpps <- rbind(WakeForestFBSOpps, WakeForestFCSOpps)
  colnames(WakeForestOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  WakeForest <- full_join(WakeForest, WakeForestOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  WakeForest <- WakeForest |>
    mutate(actual_diff = case_when(home_team == "Wake Forest" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Wake Forest" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Wake Forest" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Wake Forest" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  WakeForest_losses <- WakeForest |>
    filter(home_team == "Wake Forest" & home_points < away_points | away_team == "Wake Forest" & away_points < home_points)
  ## storing overall team Resume Score as vector
  WakeForest_resume <- sum(WakeForest$Resume_Score) - (7 * nrow(WakeForest_losses))
  
  
  ## Washington
  Washington <- completed_games |>
    filter(home_team == "Washington" | away_team == "Washington") |>
    mutate(team = "Washington",
           team_opp = case_when(home_team == "Washington" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Washington"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  WashingtonFBSOpps <- VoA_Variables |>
    filter(team %in% Washington$team_opp) |>
    select(team, VoA_Rating)
  WashingtonFCSOpps <- FCS |>
    filter(team %in% Washington$team_opp) |>
    select(team, rating)
  colnames(WashingtonFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  WashingtonOpps <- rbind(WashingtonFBSOpps, WashingtonFCSOpps)
  colnames(WashingtonOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  Washington <- full_join(Washington, WashingtonOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  Washington <- Washington |>
    mutate(actual_diff = case_when(home_team == "Washington" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Washington" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Washington" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Washington" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  Washington_losses <- Washington |>
    filter(home_team == "Washington" & home_points < away_points | away_team == "Washington" & away_points < home_points)
  ## storing overall team Resume Score as vector
  Washington_resume <- sum(Washington$Resume_Score) - (7 * nrow(Washington_losses))
  
  
  ## WashingtonSt
  WashingtonSt <- completed_games |>
    filter(home_team == "Washington State" | away_team == "Washington State") |>
    mutate(team = "Washington State",
           team_opp = case_when(home_team == "Washington State" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Washington State"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  WashingtonStFBSOpps <- VoA_Variables |>
    filter(team %in% WashingtonSt$team_opp) |>
    select(team, VoA_Rating)
  WashingtonStFCSOpps <- FCS |>
    filter(team %in% WashingtonSt$team_opp) |>
    select(team, rating)
  colnames(WashingtonStFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  WashingtonStOpps <- rbind(WashingtonStFBSOpps, WashingtonStFCSOpps)
  colnames(WashingtonStOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  WashingtonSt <- full_join(WashingtonSt, WashingtonStOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  WashingtonSt <- WashingtonSt |>
    mutate(actual_diff = case_when(home_team == "Washington State" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Washington State" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Washington State" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Washington State" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  WashingtonSt_losses <- WashingtonSt |>
    filter(home_team == "Washington State" & home_points < away_points | away_team == "Washington State" & away_points < home_points)
  ## storing overall team Resume Score as vector
  WashingtonSt_resume <- sum(WashingtonSt$Resume_Score) - (7 * nrow(WashingtonSt_losses))
  
  
  ## WestVirginia
  WestVirginia <- completed_games |>
    filter(home_team == "West Virginia" | away_team == "West Virginia") |>
    mutate(team = "West Virginia",
           team_opp = case_when(home_team == "West Virginia" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "West Virginia"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  WestVirginiaFBSOpps <- VoA_Variables |>
    filter(team %in% WestVirginia$team_opp) |>
    select(team, VoA_Rating)
  WestVirginiaFCSOpps <- FCS |>
    filter(team %in% WestVirginia$team_opp) |>
    select(team, rating)
  colnames(WestVirginiaFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  WestVirginiaOpps <- rbind(WestVirginiaFBSOpps, WestVirginiaFCSOpps)
  colnames(WestVirginiaOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  WestVirginia <- full_join(WestVirginia, WestVirginiaOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  WestVirginia <- WestVirginia |>
    mutate(actual_diff = case_when(home_team == "West Virginia" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "West Virginia" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "West Virginia" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "West Virginia" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  WestVirginia_losses <- WestVirginia |>
    filter(home_team == "West Virginia" & home_points < away_points | away_team == "West Virginia" & away_points < home_points)
  ## storing overall team Resume Score as vector
  WestVirginia_resume <- sum(WestVirginia$Resume_Score) - (7 * nrow(WestVirginia_losses))
  
  
  ## WesternKentucky
  WesternKentucky <- completed_games |>
    filter(home_team == "Western Kentucky" | away_team == "Western Kentucky") |>
    mutate(team = "Western Kentucky",
           team_opp = case_when(home_team == "Western Kentucky" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Western Kentucky"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  WesternKentuckyFBSOpps <- VoA_Variables |>
    filter(team %in% WesternKentucky$team_opp) |>
    select(team, VoA_Rating)
  WesternKentuckyFCSOpps <- FCS |>
    filter(team %in% WesternKentucky$team_opp) |>
    select(team, rating)
  colnames(WesternKentuckyFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  WesternKentuckyOpps <- rbind(WesternKentuckyFBSOpps, WesternKentuckyFCSOpps)
  colnames(WesternKentuckyOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  WesternKentucky <- full_join(WesternKentucky, WesternKentuckyOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  WesternKentucky <- WesternKentucky |>
    mutate(actual_diff = case_when(home_team == "Western Kentucky" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Western Kentucky" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Western Kentucky" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Western Kentucky" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  WesternKentucky_losses <- WesternKentucky |>
    filter(home_team == "Western Kentucky" & home_points < away_points | away_team == "Western Kentucky" & away_points < home_points)
  ## storing overall team Resume Score as vector
  WesternKentucky_resume <- sum(WesternKentucky$Resume_Score) - (7 * nrow(WesternKentucky_losses))
  
  
  ## WesternMichigan
  WesternMichigan <- completed_games |>
    filter(home_team == "Western Michigan" | away_team == "Western Michigan") |>
    mutate(team = "Western Michigan",
           team_opp = case_when(home_team == "Western Michigan" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Western Michigan"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  WesternMichiganFBSOpps <- VoA_Variables |>
    filter(team %in% WesternMichigan$team_opp) |>
    select(team, VoA_Rating)
  WesternMichiganFCSOpps <- FCS |>
    filter(team %in% WesternMichigan$team_opp) |>
    select(team, rating)
  colnames(WesternMichiganFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  WesternMichiganOpps <- rbind(WesternMichiganFBSOpps, WesternMichiganFCSOpps)
  colnames(WesternMichiganOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  WesternMichigan <- full_join(WesternMichigan, WesternMichiganOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  WesternMichigan <- WesternMichigan |>
    mutate(actual_diff = case_when(home_team == "Western Michigan" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Western Michigan" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Western Michigan" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Western Michigan" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  WesternMichigan_losses <- WesternMichigan |>
    filter(home_team == "Western Michigan" & home_points < away_points | away_team == "Western Michigan" & away_points < home_points)
  ## storing overall team Resume Score as vector
  WesternMichigan_resume <- sum(WesternMichigan$Resume_Score) - (7 * nrow(WesternMichigan_losses))
  
  
  ## Wisconsin
  Wisconsin <- completed_games |>
    filter(home_team == "Wisconsin" | away_team == "Wisconsin") |>
    mutate(team = "Wisconsin",
           team_opp = case_when(home_team == "Wisconsin" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Wisconsin"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  WisconsinFBSOpps <- VoA_Variables |>
    filter(team %in% Wisconsin$team_opp) |>
    select(team, VoA_Rating)
  WisconsinFCSOpps <- FCS |>
    filter(team %in% Wisconsin$team_opp) |>
    select(team, rating)
  colnames(WisconsinFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  WisconsinOpps <- rbind(WisconsinFBSOpps, WisconsinFCSOpps)
  colnames(WisconsinOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  Wisconsin <- full_join(Wisconsin, WisconsinOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  Wisconsin <- Wisconsin |>
    mutate(actual_diff = case_when(home_team == "Wisconsin" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Wisconsin" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Wisconsin" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Wisconsin" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  Wisconsin_losses <- Wisconsin |>
    filter(home_team == "Wisconsin" & home_points < away_points | away_team == "Wisconsin" & away_points < home_points)
  ## storing overall team Resume Score as vector
  Wisconsin_resume <- sum(Wisconsin$Resume_Score) - (7 * nrow(Wisconsin_losses))
  
  
  ## Wyoming
  Wyoming <- completed_games |>
    filter(home_team == "Wyoming" | away_team == "Wyoming") |>
    mutate(team = "Wyoming",
           team_opp = case_when(home_team == "Wyoming" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Wyoming"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  WyomingFBSOpps <- VoA_Variables |>
    filter(team %in% Wyoming$team_opp) |>
    select(team, VoA_Rating)
  WyomingFCSOpps <- FCS |>
    filter(team %in% Wyoming$team_opp) |>
    select(team, rating)
  colnames(WyomingFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  WyomingOpps <- rbind(WyomingFBSOpps, WyomingFCSOpps)
  colnames(WyomingOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  Wyoming <- full_join(Wyoming, WyomingOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  Wyoming <- Wyoming |>
    mutate(actual_diff = case_when(home_team == "Wyoming" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Wyoming" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Wyoming" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Wyoming" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  Wyoming_losses <- Wyoming |>
    filter(home_team == "Wyoming" & home_points < away_points | away_team == "Wyoming" & away_points < home_points)
  ## storing overall team Resume Score as vector
  Wyoming_resume <- sum(Wyoming$Resume_Score) - (7 * nrow(Wyoming_losses))
  
  ## Sam Houston State
  SamHoustonSt <- completed_games |>
    filter(home_team == "Sam Houston State" | away_team == "Sam Houston State") |>
    mutate(team = "Sam Houston State",
           team_opp = case_when(home_team == "Sam Houston State" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Sam Houston State"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  SamHoustonStFBSOpps <- VoA_Variables |>
    filter(team %in% SamHoustonSt$team_opp) |>
    select(team, VoA_Rating)
  SamHoustonStFCSOpps <- FCS |>
    filter(team %in% SamHoustonSt$team_opp) |>
    select(team, rating)
  colnames(SamHoustonStFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  SamHoustonStOpps <- rbind(SamHoustonStFBSOpps, SamHoustonStFCSOpps)
  colnames(SamHoustonStOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  SamHoustonSt <- full_join(SamHoustonSt, SamHoustonStOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  SamHoustonSt <- SamHoustonSt |>
    mutate(actual_diff = case_when(home_team == "Sam Houston State" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Sam Houston State" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Sam Houston State" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Sam Houston State" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  SamHoustonSt_losses <- SamHoustonSt |>
    filter(home_team == "Sam Houston State" & home_points < away_points | away_team == "Sam Houston State" & away_points < home_points)
  ## storing overall team Resume Score as vector
  SamHoustonSt_resume <- sum(SamHoustonSt$Resume_Score) - (7 * nrow(SamHoustonSt_losses))
  
  ## Jacksonville State
  JacksonvilleSt <- completed_games |>
    filter(home_team == "Jacksonville State" | away_team == "Jacksonville State") |>
    mutate(team = "Jacksonville State",
           team_opp = case_when(home_team == "Jacksonville State" ~ away_team,
                                TRUE ~ home_team),
           team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Jacksonville State"]) |>
    filter(completed == TRUE)
  
  ## creating df of VoA/SRS ratings of opponents
  JacksonvilleStFBSOpps <- VoA_Variables |>
    filter(team %in% JacksonvilleSt$team_opp) |>
    select(team, VoA_Rating)
  JacksonvilleStFCSOpps <- FCS |>
    filter(team %in% JacksonvilleSt$team_opp) |>
    select(team, rating)
  colnames(JacksonvilleStFCSOpps) <- c("team", "VoA_Rating")
  ## combining FBS VoA ratings and FCS SRS ratings into 1 df
  JacksonvilleStOpps <- rbind(JacksonvilleStFBSOpps, JacksonvilleStFCSOpps)
  colnames(JacksonvilleStOpps) <- c("team_opp", "opp_VoA_rating")
  
  ## adding opponent ratings to main team df
  JacksonvilleSt <- full_join(JacksonvilleSt, JacksonvilleStOpps, by = "team_opp")
  ## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
  JacksonvilleSt <- JacksonvilleSt |>
    mutate(actual_diff = case_when(home_team == "Jacksonville State" ~ home_points - away_points,
                                   TRUE ~ away_points - home_points),
           projected_diff = case_when(home_team == "Jacksonville State" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                      away_team == "Jacksonville State" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                      TRUE ~ team_VoA_rating - opp_VoA_rating),
           Top12_proj = case_when(home_team == "Jacksonville State" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                  TRUE ~ Top12_mean - opp_VoA_rating),
           Resume_Score = actual_diff - Top12_proj)
  
  ## determining number of losses
  JacksonvilleSt_losses <- JacksonvilleSt |>
    filter(home_team == "Jacksonville State" & home_points < away_points | away_team == "Jacksonville State" & away_points < home_points)
  ## storing overall team Resume Score as vector
  JacksonvilleSt_resume <- sum(JacksonvilleSt$Resume_Score) - (7 * nrow(JacksonvilleSt_losses))
  
  ##### Creating Data frame of just teams and Resume VoA Scores #####
  ResumeVoA <- VoA_Variables |>
    select(team) |>
    mutate(Resume_VoA=case_when(team == "Air Force" ~ AirForce_resume,
                                team == "Akron" ~ Akron_resume,
                                team == "Alabama" ~ Alabama_resume,
                                team == "Appalachian State" ~ AppalachianSt_resume,
                                team == "Arizona" ~ Arizona_resume,
                                team == "Arizona State" ~ ArizonaSt_resume,
                                team == "Arkansas" ~ Arkansas_resume,
                                team == "Arkansas State" ~ ArkansasSt_resume,
                                team == "Army" ~ Army_resume,
                                team == "Auburn" ~ Auburn_resume,
                                team == "Ball State" ~ BallSt_resume,
                                team == "Baylor" ~ Baylor_resume,
                                team == "Boise State" ~ BoiseSt_resume,
                                team == "Boston College" ~ BC_resume,
                                team == "Bowling Green" ~ BowlingGreen_resume,
                                team == "Buffalo" ~ Buffalo_resume,
                                team == "BYU" ~ BYU_resume,
                                team == "California" ~ California_resume,
                                team == "Central Michigan" ~ CMU_resume,
                                team == "Charlotte" ~ Charlotte_resume,
                                team == "Cincinnati" ~ Cincinnati_resume,
                                team == "Clemson" ~ Clemson_resume,
                                team == "Coastal Carolina" ~ CoastalCarolina_resume,
                                team == "Colorado" ~ Colorado_resume,
                                team == "Colorado State" ~ ColoradoSt_resume,
                                team == "Connecticut" ~ Connecticut_resume,
                                team == "Duke" ~ Duke_resume,
                                team == "East Carolina" ~ EastCarolina_resume,
                                team == "Eastern Michigan" ~ EMU_resume,
                                team == "Florida" ~ Florida_resume,
                                team == "Florida Atlantic" ~ FAU_resume,
                                team == "Florida International" ~ FIU_resume,
                                team == "Florida State" ~ FloridaSt_resume,
                                team == "Fresno State" ~ FresnoSt_resume,
                                team == "Georgia" ~ Georgia_resume,
                                team == "Georgia Southern" ~ GeorgiaSouthern_resume,
                                team == "Georgia State" ~ GeorgiaSt_resume,
                                team == "Georgia Tech" ~ GeorgiaTech_resume,
                                team == "Hawai'i" ~ Hawaii_resume,
                                team == "Houston" ~ Houston_resume,
                                team == "Illinois" ~ Illinois_resume,
                                team == "Indiana" ~ Indiana_resume,
                                team == "Iowa" ~ Iowa_resume,
                                team == "Iowa State" ~ IowaSt_resume,
                                team == "James Madison" ~ JamesMadison_resume,
                                team == "Kansas" ~ Kansas_resume,
                                team == "Kansas State" ~ KansasSt_resume,
                                team == "Kent State" ~ KentSt_resume,
                                team == "Kentucky" ~ Kentucky_resume,
                                team == "Liberty" ~ Liberty_resume,
                                team == "Louisiana" ~ Louisiana_resume,
                                team == "Louisiana Monroe" ~ ULM_resume,
                                team == "Louisiana Tech" ~ LouisianaTech_resume,
                                team == "Louisville" ~ Louisville_resume,
                                team == "LSU" ~ LSU_resume,
                                team == "Marshall" ~ Marshall_resume,
                                team == "Maryland" ~ Maryland_resume,
                                team == "Memphis" ~ Memphis_resume,
                                team == "Miami" ~ Miami_resume,
                                team == "Miami (OH)" ~ MiamiOH_resume,
                                team == "Michigan" ~ Michigan_resume,
                                team == "Michigan State" ~ MichiganSt_resume,
                                team == "Middle Tennessee" ~ MiddleTennessee_resume,
                                team == "Minnesota" ~ Minnesota_resume,
                                team == "Mississippi State" ~ MissSt_resume,
                                team == "Missouri" ~ Missouri_resume,
                                team == "Navy" ~ Navy_resume,
                                team == "NC State" ~ NCSt_resume,
                                team == "Nebraska" ~ Nebraska_resume,
                                team == "Nevada" ~ Nevada_resume,
                                team == "New Mexico" ~ NewMexico_resume,
                                team == "New Mexico State" ~ NewMexicoSt_resume,
                                team == "North Carolina" ~ NorthCarolina_resume,
                                team == "North Texas" ~ NorthTexas_resume,
                                team == "Northern Illinois" ~ NorthernIllinois_resume,
                                team == "Northwestern" ~ Northwestern_resume,
                                team == "Notre Dame" ~ NotreDame_resume,
                                team == "Ohio" ~ Ohio_resume,
                                team == "Ohio State" ~ OhioSt_resume,
                                team == "Oklahoma" ~ Oklahoma_resume,
                                team == "Oklahoma State" ~ OklahomaSt_resume,
                                team == "Old Dominion" ~ OldDominion_resume,
                                team == "Ole Miss" ~ OleMiss_resume,
                                team == "Oregon" ~ Oregon_resume,
                                team == "Oregon State" ~ OregonSt_resume,
                                team == "Penn State" ~ PennSt_resume,
                                team == "Pittsburgh" ~ Pittsburgh_resume,
                                team == "Purdue" ~ Purdue_resume,
                                team == "Rice" ~ Rice_resume,
                                team == "Rutgers" ~ Rutgers_resume,
                                team == "San Diego State" ~ SanDiegoSt_resume,
                                team == "San José State" ~ SanJoseSt_resume,
                                team == "SMU" ~ SMU_resume,
                                team == "South Alabama" ~ SouthAlabama_resume,
                                team == "South Carolina" ~ SouthCarolina_resume,
                                team == "South Florida" ~ SouthFlorida_resume,
                                team == "Southern Mississippi" ~ SouthernMiss_resume,
                                team == "Stanford" ~ Stanford_resume,
                                team == "Syracuse" ~ Syracuse_resume,
                                team == "TCU" ~ TCU_resume,
                                team == "Temple" ~ Temple_resume,
                                team == "Tennessee" ~ Tennessee_resume,
                                team == "Texas" ~ Texas_resume,
                                team == "Texas A&M" ~ TexasAM_resume,
                                team == "Texas State" ~ TexasSt_resume,
                                team == "Texas Tech" ~ TexasTech_resume,
                                team == "Toledo" ~ Toledo_resume,
                                team == "Troy" ~ Troy_resume,
                                team == "Tulane" ~ Tulane_resume,
                                team == "Tulsa" ~ Tulsa_resume,
                                team == "UAB" ~ UAB_resume,
                                team == "UCF" ~ UCF_resume,
                                team == "UCLA" ~ UCLA_resume,
                                team == "UMass" ~ UMass_resume,
                                team == "UNLV" ~ UNLV_resume,
                                team == "USC" ~ USC_resume,
                                team == "UT San Antonio" ~ UTSA_resume,
                                team == "Utah" ~ Utah_resume,
                                team == "Utah State" ~ UtahSt_resume,
                                team == "UTEP" ~ UTEP_resume,
                                team == "Vanderbilt" ~ Vanderbilt_resume,
                                team == "Virginia" ~ Virginia_resume,
                                team == "Virginia Tech" ~ VirginiaTech_resume,
                                team == "Wake Forest" ~ WakeForest_resume,
                                team == "Washington" ~ Washington_resume,
                                team == "Washington State" ~ WashingtonSt_resume,
                                team == "West Virginia" ~ WestVirginia_resume,
                                team == "Western Kentucky" ~ WesternKentucky_resume,
                                team == "Western Michigan" ~ WesternMichigan_resume,
                                team == "Wisconsin" ~ Wisconsin_resume,
                                team == "Wyoming" ~ Wyoming_resume,
                                team == "Sam Houston State" ~ SamHoustonSt_resume,
                                team == "Jacksonville State" ~ JacksonvilleSt_resume),
           Resume_VoA_Rank = dense_rank(desc(Resume_VoA))) |>
    arrange(Resume_VoA_Rank)
  
  ## joining VoA_Variables and ResumeVoA
  VoA_Variables <- full_join(VoA_Variables, ResumeVoA, by = "team")
  
  ## filtering resume top 25 out for table
  ResumeVoATop25 <- VoA_Variables |>
    select(team, Resume_VoA, Resume_VoA_Rank) |>
    filter(Resume_VoA_Rank < 26) |>
    arrange(Resume_VoA_Rank)
  
  FinalResumeTable <- VoA_Variables |>
    select(team, Resume_VoA, Resume_VoA_Rank) |>
    arrange(Resume_VoA_Rank)
} else {
  print("no Resume VoA until Week 6!")
}

##### Creating Top 25 and Full Tables Arranged by Resume VoA #####
if (as.numeric(week) > 5) {
  ## Top 25 Table
  # adding title and subtitle
  ResumeVoATop25Table <- ResumeVoATop25 |>
    gt() |> # use 'gt' to make an awesome table...
    gt_theme_espn() |>
    tab_header(
      title = paste(year, week_text, week, resume_text, VoA_Top25_text), # ...with this title
      subtitle = "Supremely Excellent Yet Salaciously Godlike And Infallibly Magnificent Vortex of Accuracy")  |>  # and this subtitle
    ## tab_style(style = cell_fill("bisque"),
    ##           locations = cells_body()) |>  # add fill color to table
    fmt_number( # A column (numeric data)
      columns = c(Resume_VoA), # What column variable? FinalVoATop25$VoA_Rating
      decimals = 3 # With four decimal places
    ) |> 
    fmt_number( # Another column (also numeric data)
      columns = c(Resume_VoA_Rank), # What column variable? FinalVoATop25$VoA_Ranking
      decimals = 0 # I want this column to have zero decimal places
    ) |>
    data_color( # Update cell colors, testing different color palettes
      columns = c(Resume_VoA), # ...for dose column
      fn = scales::col_numeric( # <- bc it's numeric
        palette = brewer.pal(9, "Reds"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    cols_label(Resume_VoA = "Resume VoA Rating", Resume_VoA_Rank = "Resume VoA Ranking") |> # Update labels
    cols_move_to_end(columns = "Resume_VoA") |>
    # cols_hide(c(conference, CFB_Week, VoA_Output)) |>
    tab_footnote(
      footnote = "Table by @gshelor, Data from CFB Data API, ESPN.com, and ESPN's Bill Connelly via cfbfastR, FCS data mostly from stats.ncaa.org"
    )
  
  ## Full 130 teams table
  # adding title and subtitle
  Resume_VoA_Table <- FinalResumeTable |>
    gt() |> # use 'gt' to make an awesome table...
    gt_theme_538() |>
    tab_header(
      title = paste(year, week_text, week, resume_text, VoA_text), # ...with this title
      subtitle = "Supremely Excellent Yet Salaciously Godlike And Infallibly Magnificent Vortex of Accuracy")  |>  # and this subtitle
    ##tab_style(style = cell_fill("bisque"),
    ##        locations = cells_body()) |>  # add fill color to table
    fmt_number( # A column (numeric data)
      columns = c(Resume_VoA), # What column variable? FinalVoATop25$VoA_Rating
      decimals = 3 # With four decimal places
    ) |> 
    fmt_number( # Another column (also numeric data)
      columns = c(Resume_VoA_Rank), # What column variable? FinalVoATop25$VoA_Ranking
      decimals = 0 # I want this column to have zero decimal places
    ) |> 
    data_color( # Update cell colors, testing different color palettes
      columns = c(Resume_VoA), # ...for dose column
      fn = scales::col_numeric( # <- bc it's numeric
        palette = brewer.pal(9, "RdBu"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = TRUE
      )
    ) |>
    cols_label(Resume_VoA = "Resume VoA Rating", Resume_VoA_Rank = "Resume VoA Rank") |> # Update labels
    cols_move_to_end(columns = "Resume_VoA") |>
    # cols_hide(c(conference, CFB_Week, VoA_Output)) |>
    tab_footnote(
      footnote = "Table by @gshelor, Data from CFB Data API, ESPN.com, and ESPN's Bill Connelly via cfbfastR, FCS data mostly from stats.ncaa.org"
    )
} else if (as.numeric(week) > 15) {
  ## Top 25 Table
  # adding title and subtitle
  ResumeVoATop25Table <- ResumeVoATop25 |>
    gt() |> # use 'gt' to make an awesome table...
    gt_theme_espn() |>
    tab_header(
      title = paste(year, Postseason_text, resume_text, VoA_Top25_text), # ...with this title
      subtitle = "Supremely Excellent Yet Salaciously Godlike And Infallibly Magnificent Vortex of Accuracy")  |>  # and this subtitle
    ## tab_style(style = cell_fill("bisque"),
    ##           locations = cells_body()) |>  # add fill color to table
    fmt_number( # A column (numeric data)
      columns = c(Resume_VoA), # What column variable? FinalVoATop25$VoA_Rating
      decimals = 3 # With four decimal places
    ) |> 
    fmt_number( # Another column (also numeric data)
      columns = c(Resume_VoA_Rank), # What column variable? FinalVoATop25$VoA_Ranking
      decimals = 0 # I want this column to have zero decimal places
    ) |>
    data_color( # Update cell colors, testing different color palettes
      columns = c(Resume_VoA), # ...for dose column
      fn = scales::col_numeric( # <- bc it's numeric
        palette = brewer.pal(9, "Reds"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    cols_label(Resume_VoA = "Resume VoA Rating", Resume_VoA_Rank = "Resume VoA Ranking") |> # Update labels
    cols_move_to_end(columns = "Resume_VoA") |>
    # cols_hide(c(conference, CFB_Week, VoA_Output)) |>
    tab_footnote(
      footnote = "Table by @gshelor, Data from CFB Data API, ESPN.com, and ESPN's Bill Connelly via cfbfastR, FCS data mostly from stats.ncaa.org"
    )
  
  ## Full 130 teams table
  # adding title and subtitle
  Resume_VoA_Table <- FinalResumeTable |>
    gt() |> # use 'gt' to make an awesome table...
    gt_theme_538() |>
    tab_header(
      title = paste(year, Postseason_text, resume_text, VoA_text), # ...with this title
      subtitle = "Supremely Excellent Yet Salaciously Godlike And Infallibly Magnificent Vortex of Accuracy")  |>  # and this subtitle
    ##tab_style(style = cell_fill("bisque"),
    ##        locations = cells_body()) |>  # add fill color to table
    fmt_number( # A column (numeric data)
      columns = c(Resume_VoA), # What column variable? FinalVoATop25$VoA_Rating
      decimals = 3 # With four decimal places
    ) |> 
    fmt_number( # Another column (also numeric data)
      columns = c(Resume_VoA_Rank), # What column variable? FinalVoATop25$VoA_Ranking
      decimals = 0 # I want this column to have zero decimal places
    ) |> 
    data_color( # Update cell colors, testing different color palettes
      columns = c(Resume_VoA), # ...for dose column
      fn = scales::col_numeric( # <- bc it's numeric
        palette = brewer.pal(9, "RdBu"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = TRUE
      )
    ) |>
    cols_label(Resume_VoA = "Resume VoA Rating", Resume_VoA_Rank = "Resume VoA Rank") |> # Update labels
    cols_move_to_end(columns = "Resume_VoA") |>
    # cols_hide(c(conference, CFB_Week, VoA_Output)) |>
    tab_footnote(
      footnote = "Table by @gshelor, Data from CFB Data API, ESPN.com, and ESPN's Bill Connelly via cfbfastR, FCS data mostly from stats.ncaa.org"
    )
} else {
  print("No Resume VoA until Week 6!")
}




##### Saving tables and final VoA_Variables csv #####
## viewing and saving the gt tables outside the if statement so that I can see them in the RStudio viewer
VoATop25Table
VoATop25Table |>
  gtsave(
    top25_file_pathway, expand = 5,
    path = output_dir
  )
VoA_Full_Table
VoA_Full_Table |>
  gtsave(
    fulltable_file_pathway, expand = 5,
    path = output_dir
  )

## Resume VoA not produced until Week 6, so not saved until Week 6
if (as.numeric(week) > 5) {
  ## Resume tables
  ResumeVoATop25Table
  ResumeVoATop25Table |>
    gtsave(
      resumetop25_file_pathway, expand = 5,
      path = output_dir
    )
  Resume_VoA_Table
  Resume_VoA_Table |>
    gtsave(
      resumefulltable_file_pathway, expand = 5,
      path = output_dir
    )
}

## Exporting final dataframe as csv
write_csv(VoA_Variables, file_pathway)

##### Making the Unintelligible Charts #####
## Tracks VoA Ratings and Rankings by week
## now reading in and merging VoA rating and ranking data up to current week
if (as.numeric(week) == 2) {
  Week0_VoA <- read_csv(here("Data", "VoA2023", "2023Week0_VoA.csv")) |>
    select(team, conference, CFB_Week, VoA_Output, VoA_Ranking, VoA_Rating)
  Week1_VoA <- read_csv(here("Data", "VoA2023", "2023Week1_VoA.csv")) |>
    select(team, conference, CFB_Week, VoA_Output, VoA_Ranking, VoA_Rating)
  Full_Ratings_Rks <- rbind(Week0_VoA, Week1_VoA)
  Full_Ratings_Rks <- rbind(Full_Ratings_Rks, FinalTable)
  write_csv(Full_Ratings_Rks, paste(data_dir, "/TrackingChartCSVs", "/", year, week_text, "0_2Ratings_Rks.csv", sep = ""))
} else if (as.numeric(week) == 3) {
  Full_Ratings_Rks <- read_csv(here("Data", "VoA2023", "TrackingChartCSVs", paste(year, week_text, "0_2Ratings_Rks.csv", sep = ""))) |>
    select(team, conference, CFB_Week, VoA_Output, VoA_Ranking, VoA_Rating)
  Full_Ratings_Rks <- rbind(Full_Ratings_Rks, FinalTable)
  write_csv(Full_Ratings_Rks, paste(data_dir, "/TrackingChartCSVs", "/", year, week_text, "0_3Ratings_Rks.csv", sep = ""))
} else if (as.numeric(week) == 4) {
  Full_Ratings_Rks <- read_csv(here("Data", "VoA2023", "TrackingChartCSVs", paste(year, week_text, "0_3Ratings_Rks.csv", sep = ""))) |>
    select(team, conference, CFB_Week, VoA_Output, VoA_Ranking, VoA_Rating)
  Full_Ratings_Rks <- rbind(Full_Ratings_Rks, FinalTable)
  write_csv(Full_Ratings_Rks, paste(data_dir, "/TrackingChartCSVs", "/", year, week_text, "0_4Ratings_Rks.csv", sep = ""))
} else if (as.numeric(week) == 5) {
  Full_Ratings_Rks <- read_csv(here("Data", "VoA2023", "TrackingChartCSVs", paste(year, week_text, "0_4Ratings_Rks.csv", sep = ""))) |>
    select(team, conference, CFB_Week, VoA_Output, VoA_Ranking, VoA_Rating)
  Full_Ratings_Rks <- rbind(Full_Ratings_Rks, FinalTable)
  write_csv(Full_Ratings_Rks, paste(data_dir, "/TrackingChartCSVs", "/", year, week_text, "0_5Ratings_Rks.csv", sep = ""))
} else if (as.numeric(week) == 6) {
  Full_Ratings_Rks <- read_csv(here("Data", "VoA2023", "TrackingChartCSVs", paste(year, week_text, "0_5Ratings_Rks.csv", sep = ""))) |>
    select(team, conference, CFB_Week, VoA_Output, VoA_Ranking, VoA_Rating)
  Full_Ratings_Rks <- rbind(Full_Ratings_Rks, FinalTable)
  write_csv(Full_Ratings_Rks, paste(data_dir, "/TrackingChartCSVs", "/", year, week_text, "0_6Ratings_Rks.csv", sep = ""))
} else if (as.numeric(week) == 7) {
  Full_Ratings_Rks <- read_csv(here("Data", "VoA2023", "TrackingChartCSVs", paste(year, week_text, "0_6Ratings_Rks.csv", sep = ""))) |>
    select(team, conference, CFB_Week, VoA_Output, VoA_Ranking, VoA_Rating)
  Full_Ratings_Rks <- rbind(Full_Ratings_Rks, FinalTable)
  write_csv(Full_Ratings_Rks, paste(data_dir, "/TrackingChartCSVs", "/", year, week_text, "0_7Ratings_Rks.csv", sep = ""))
} else if (as.numeric(week) == 8) {
  Full_Ratings_Rks <- read_csv(here("Data", "VoA2023", "TrackingChartCSVs", paste(year, week_text, "0_7Ratings_Rks.csv", sep = ""))) |>
    select(team, conference, CFB_Week, VoA_Output, VoA_Ranking, VoA_Rating)
  Full_Ratings_Rks <- rbind(Full_Ratings_Rks, FinalTable)
  write_csv(Full_Ratings_Rks, paste(data_dir, "/TrackingChartCSVs", "/", year, week_text, "0_8Ratings_Rks.csv", sep = ""))
} else if (as.numeric(week) == 9) {
  Full_Ratings_Rks <- read_csv(here("Data", "VoA2023", "TrackingChartCSVs", paste(year, week_text, "0_8Ratings_Rks.csv", sep = ""))) |>
    select(team, conference, CFB_Week, VoA_Output, VoA_Ranking, VoA_Rating)
  Full_Ratings_Rks <- rbind(Full_Ratings_Rks, FinalTable)
  write_csv(Full_Ratings_Rks, paste(data_dir, "/TrackingChartCSVs", "/", year, week_text, "0_9Ratings_Rks.csv", sep = ""))
} else if (as.numeric(week) == 10) {
  Full_Ratings_Rks <- read_csv(here("Data", "VoA2023", "TrackingChartCSVs", paste(year, week_text, "0_9Ratings_Rks.csv", sep = ""))) |>
    select(team, conference, CFB_Week, VoA_Output, VoA_Ranking, VoA_Rating)
  Full_Ratings_Rks <- rbind(Full_Ratings_Rks, FinalTable)
  write_csv(Full_Ratings_Rks, paste(data_dir, "/TrackingChartCSVs", "/", year, week_text, "0_10Ratings_Rks.csv", sep = ""))
} else if (as.numeric(week) == 11) {
  Full_Ratings_Rks <- read_csv(here("Data", "VoA2023", "TrackingChartCSVs", paste(year, week_text, "0_10Ratings_Rks.csv", sep = ""))) |>
    select(team, conference, CFB_Week, VoA_Output, VoA_Ranking, VoA_Rating)
  Full_Ratings_Rks <- rbind(Full_Ratings_Rks, FinalTable)
  write_csv(Full_Ratings_Rks, paste(data_dir, "/TrackingChartCSVs", "/", year, week_text, "0_11Ratings_Rks.csv", sep = ""))
} else if (as.numeric(week) == 12) {
  Full_Ratings_Rks <- read_csv(here("Data", "VoA2023", "TrackingChartCSVs", paste(year, week_text, "0_11Ratings_Rks.csv", sep = ""))) |>
    select(team, conference, CFB_Week, VoA_Output, VoA_Ranking, VoA_Rating)
  Full_Ratings_Rks <- rbind(Full_Ratings_Rks, FinalTable)
  write_csv(Full_Ratings_Rks, paste(data_dir, "/TrackingChartCSVs", "/", year, week_text, "0_12Ratings_Rks.csv", sep = ""))
} else if (as.numeric(week) == 13) {
  Full_Ratings_Rks <- read_csv(here("Data", "VoA2023", "TrackingChartCSVs", paste(year, week_text, "0_12Ratings_Rks.csv", sep = ""))) |>
    select(team, conference, CFB_Week, VoA_Output, VoA_Ranking, VoA_Rating)
  Full_Ratings_Rks <- rbind(Full_Ratings_Rks, FinalTable)
  write_csv(Full_Ratings_Rks, paste(data_dir, "/TrackingChartCSVs", "/", year, week_text, "0_13Ratings_Rks.csv", sep = ""))
} else if (as.numeric(week) == 14) {
  Full_Ratings_Rks <- read_csv(here("Data", "VoA2023", "TrackingChartCSVs", paste(year, week_text, "0_13Ratings_Rks.csv", sep = ""))) |>
    select(team, conference, CFB_Week, VoA_Output, VoA_Ranking, VoA_Rating)
  Full_Ratings_Rks <- rbind(Full_Ratings_Rks, FinalTable)
  write_csv(Full_Ratings_Rks, paste(data_dir, "/TrackingChartCSVs", "/", year, week_text, "0_14Ratings_Rks.csv", sep = ""))
} else if (as.numeric(week) == 15) {
  Full_Ratings_Rks <- read_csv(here("Data", "VoA2023", "TrackingChartCSVs", paste(year, week_text, "0_14Ratings_Rks.csv", sep = ""))) |>
    select(team, conference, CFB_Week, VoA_Output, VoA_Ranking, VoA_Rating)
  Full_Ratings_Rks <- rbind(Full_Ratings_Rks, FinalTable)
  write_csv(Full_Ratings_Rks, paste(data_dir, "TrackingChartCSVs", "/", year, week_text, "0_15Ratings_Rks.csv", sep = ""))
} else if (as.numeric(week) == 16) {
  Full_Ratings_Rks <- read_csv(here("Data", "VoA2023", "TrackingChartCSVs", paste(year, week_text, "0_15Ratings_Rks.csv", sep = ""))) |>
    select(team, conference, CFB_Week, VoA_Output, VoA_Ranking, VoA_Rating)
  Full_Ratings_Rks <- rbind(Full_Ratings_Rks, FinalTable)
  ## no need to write out a new tracking csv since "week 16" is the postseason VoA
  ## write_csv(Full_Ratings_Rks, paste(data_dir, "TrackingChartCSVs", "/", year, week_text, "0_16Ratings_Rks.csv", sep = ""))
} else {
  print("No charts until Week 2!")
}
## end of if statement

## Filtering by conference for unintelligible charts
if (as.numeric(week) >= 2) {
  ## Subsetting by team, each conference (including independents) gets separate charts
  AAC_Ratings_Rks <- Full_Ratings_Rks |> filter(conference == "American Athletic")
  ACC_Ratings_Rks <- Full_Ratings_Rks |> filter(conference == "ACC")
  Big12_Ratings_Rks <- Full_Ratings_Rks |> filter(conference == "Big 12")
  Big10_Ratings_Rks <- Full_Ratings_Rks |> filter(conference == "Big Ten")
  CUSA_Ratings_Rks <- Full_Ratings_Rks |> filter(conference == "Conference USA")
  Indy_Ratings_Rks <- Full_Ratings_Rks |> filter(conference == "FBS Independents")
  MAC_Ratings_Rks <- Full_Ratings_Rks |> filter(conference == "Mid-American")
  MWC_Ratings_Rks <- Full_Ratings_Rks |> filter(conference == "Mountain West")
  Pac12_Ratings_Rks <- Full_Ratings_Rks |> filter(conference == "Pac-12")
  SEC_Ratings_Rks <- Full_Ratings_Rks |> filter(conference == "SEC")
  SunBelt_Ratings_Rks <- Full_Ratings_Rks |> filter(conference == "Sun Belt")
  
  ##### Creating Charts #####
  # charting VoA_Rating and VoA_Ranking for each week from week 2 on
  AAC_VoA_Rating_Chart <- ggplot(AAC_Ratings_Rks, aes(x = CFB_Week, y = VoA_Rating, group = team)) +
    theme_bw() +
    geom_line(linewidth = 1.5) +
    geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA Rating") +
    labs(caption = "chart by @gshelor, data from collegefootballdata.com API via cfbfastR and stats.ncaa.org") +
    ggtitle("American Conference Vortex of Accuracy Ratings by Week") +
    expand_limits(y = c(floor(floor(min(AAC_Ratings_Rks$VoA_Rating)) / 10) * 10, ceiling((ceiling(max(AAC_Ratings_Rks$VoA_Rating)) / 10)) * 10)) +
    scale_y_continuous(breaks = seq((floor((floor(min(AAC_Ratings_Rks$VoA_Rating)) / 10)) * 10), (ceiling((ceiling(max(AAC_Ratings_Rks$VoA_Rating)) / 10)) * 10), by = 5)) +
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)) +
    geom_cfb_logos(aes(team = team, width = 0.035)) +
    theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
  AAC_VoA_Rating_Chart
  ggsave(AAC_Output_filename, path = output_dir, width = 50, height = 40, units = 'cm')
  
  AAC_VoA_Ranking_Chart <- ggplot(AAC_Ratings_Rks, aes(x = CFB_Week, y = VoA_Ranking, group = team)) +
    theme_bw() +
    geom_line(linewidth = 1.5) +
    geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA Ranking") +
    labs(caption = "chart by @gshelor, data from collegefootballdata.com API via cfbfastR and stats.ncaa.org") +
    ggtitle("American Conference Vortex of Accuracy Rankings by Week") +
    expand_limits(y = c(0,130)) +
    scale_y_continuous(breaks = c(0,20,40,60,80,100,130)) +
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)) +
    geom_cfb_logos(aes(team = team, width = 0.035)) +
    theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
  AAC_VoA_Ranking_Chart
  ggsave(AAC_Ranking_filename, path = output_dir, width = 50, height = 40, units = 'cm')
  
  ACC_VoA_Rating_Chart <- ggplot(ACC_Ratings_Rks, aes(x = CFB_Week, y = VoA_Rating, group = team)) +
    theme_bw() +
    geom_line(linewidth = 1.5) +
    geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA Rating") +
    labs(caption = "chart by @gshelor, data from collegefootballdata.com API via cfbfastR and stats.ncaa.org") +
    ggtitle("ACC Vortex of Accuracy Ratings by Week") +
    expand_limits(y = c(floor(floor(min(ACC_Ratings_Rks$VoA_Rating)) / 10) * 10, ceiling((ceiling(max(ACC_Ratings_Rks$VoA_Rating)) / 10)) * 10)) +
    scale_y_continuous(breaks = seq((floor((floor(min(ACC_Ratings_Rks$VoA_Rating)) / 10)) * 10), (ceiling((ceiling(max(ACC_Ratings_Rks$VoA_Rating)) / 10)) * 10), by = 5)) +
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)) +
    geom_cfb_logos(aes(team = team, width = 0.035)) +
    theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
  ACC_VoA_Rating_Chart
  ggsave(ACC_Output_filename, path = output_dir, width = 50, height = 40, units = 'cm')
  
  ACC_VoA_Ranking_Chart <- ggplot(ACC_Ratings_Rks, aes(x = CFB_Week, y = VoA_Ranking, group = team)) +
    theme_bw() +
    geom_line(linewidth = 1.5) +
    geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA Ranking") +
    labs(caption = "chart by @gshelor, data from collegefootballdata.com API via cfbfastR and stats.ncaa.org") +
    ggtitle("ACC Vortex of Accuracy Rankings by Week") +
    expand_limits(y = c(0,130)) +
    scale_y_continuous(breaks = c(0,20,40,60,80,100,130)) +
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)) +
    geom_cfb_logos(aes(team = team, width = 0.035)) +
    theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
  ACC_VoA_Ranking_Chart
  ggsave(ACC_Ranking_filename, path = output_dir, width = 50, height = 40, units = 'cm')
  
  Big12_VoA_Rating_Chart <- ggplot(Big12_Ratings_Rks, aes(x = CFB_Week, y = VoA_Rating, group = team)) +
    theme_bw() +
    geom_line(linewidth = 1.5) +
    geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA Rating") +
    labs(caption = "chart by @gshelor, data from collegefootballdata.com API via cfbfastR and stats.ncaa.org") +
    ggtitle("Big 12 Vortex of Accuracy Ratings by Week") +
    expand_limits(y = c(floor(floor(min(Big12_Ratings_Rks$VoA_Rating)) / 10) * 10, ceiling((ceiling(max(Big12_Ratings_Rks$VoA_Rating)) / 10)) * 10)) +
    scale_y_continuous(breaks = seq((floor((floor(min(Big12_Ratings_Rks$VoA_Rating)) / 10)) * 10), (ceiling((ceiling(max(Big12_Ratings_Rks$VoA_Rating)) / 10)) * 10), by = 5)) +
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)) +
    geom_cfb_logos(aes(team = team, width = 0.035)) +
    theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
  Big12_VoA_Rating_Chart
  ggsave(Big12_Output_filename, path = output_dir, width = 50, height = 40, units = 'cm')
  
  Big12_VoA_Ranking_Chart <- ggplot(Big12_Ratings_Rks, aes(x = CFB_Week, y = VoA_Ranking, group = team)) +
    theme_bw() +
    geom_line(linewidth = 1.5) +
    geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA Ranking") +
    labs(caption = "chart by @gshelor, data from collegefootballdata.com API via cfbfastR and stats.ncaa.org") +
    ggtitle("Big 12 Vortex of Accuracy Rankings by Week") +
    expand_limits(y = c(0,130)) +
    scale_y_continuous(breaks = c(0,20,40,60,80,100,130)) +
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)) +
    geom_cfb_logos(aes(team = team, width = 0.035)) +
    theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
  Big12_VoA_Ranking_Chart
  ggsave(Big12_Ranking_filename, path = output_dir, width = 50, height = 40, units = 'cm')
  
  Big10_VoA_Rating_Chart <- ggplot(Big10_Ratings_Rks, aes(x = CFB_Week, y = VoA_Rating, group = team)) +
    theme_bw() +
    geom_line(linewidth = 1.5) +
    geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA Rating") +
    labs(caption = "chart by @gshelor, data from collegefootballdata.com API via cfbfastR and stats.ncaa.org") +
    ggtitle("Big 10 Vortex of Accuracy Ratings by Week") +
    expand_limits(y = c(floor(floor(min(Big10_Ratings_Rks$VoA_Rating)) / 10) * 10, ceiling((ceiling(max(Big10_Ratings_Rks$VoA_Rating)) / 10)) * 10)) +
    scale_y_continuous(breaks = seq((floor((floor(min(Big10_Ratings_Rks$VoA_Rating)) / 10)) * 10), (ceiling((ceiling(max(Big10_Ratings_Rks$VoA_Rating)) / 10)) * 10), by = 5)) +
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)) +
    geom_cfb_logos(aes(team = team, width = 0.035)) +
    theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
  Big10_VoA_Rating_Chart
  ggsave(Big10_Output_filename, path = output_dir, width = 50, height = 40, units = 'cm')
  
  Big10_VoA_Ranking_Chart <- ggplot(Big10_Ratings_Rks, aes(x = CFB_Week, y = VoA_Ranking, group = team)) +
    theme_bw() +
    geom_line(linewidth = 1.5) +
    geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA Rating") +
    labs(caption = "chart by @gshelor, data from collegefootballdata.com API via cfbfastR and stats.ncaa.org") +
    ggtitle("Big 10 Vortex of Accuracy Rankings by Week") +
    expand_limits(y = c(0,130)) +
    scale_y_continuous(breaks = c(0,20,40,60,80,100,130)) +
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)) +
    geom_cfb_logos(aes(team = team, width = 0.035)) +
    theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
  Big10_VoA_Ranking_Chart
  ggsave(Big10_Ranking_filename, path = output_dir, width = 50, height = 40, units = 'cm')
  
  CUSA_VoA_Rating_Chart <- ggplot(CUSA_Ratings_Rks, aes(x = CFB_Week, y = VoA_Rating, group = team)) +
    theme_bw() +
    geom_line(linewidth = 1.5) +
    geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA Rating") +
    labs(caption = "chart by @gshelor, data from collegefootballdata.com API via cfbfastR and stats.ncaa.org") +
    ggtitle("CUSA Vortex of Accuracy Ratings by Week") +
    expand_limits(y = c(floor(floor(min(CUSA_Ratings_Rks$VoA_Rating)) / 10) * 10, ceiling((ceiling(max(CUSA_Ratings_Rks$VoA_Rating)) / 10)) * 10)) +
    scale_y_continuous(breaks = seq((floor((floor(min(CUSA_Ratings_Rks$VoA_Rating)) / 10)) * 10), (ceiling((ceiling(max(CUSA_Ratings_Rks$VoA_Rating)) / 10)) * 10), by = 5)) +
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)) +
    geom_cfb_logos(aes(team = team, width = 0.035)) +
    theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
  CUSA_VoA_Rating_Chart
  ggsave(CUSA_Output_filename, path = output_dir, width = 50, height = 40, units = 'cm')
  
  CUSA_VoA_Ranking_Chart <- ggplot(CUSA_Ratings_Rks, aes(x = CFB_Week, y = VoA_Ranking, group = team)) +
    theme_bw() +
    geom_line(linewidth = 1.5) +
    geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA Ranking") +
    labs(caption = "chart by @gshelor, data from collegefootballdata.com API via cfbfastR and stats.ncaa.org") +
    ggtitle("CUSA Vortex of Accuracy Rankings by Week") +
    expand_limits(y = c(0,130)) +
    scale_y_continuous(breaks = c(0,20,40,60,80,100,130)) +
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)) +
    geom_cfb_logos(aes(team = team, width = 0.035)) +
    theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
  CUSA_VoA_Ranking_Chart
  ggsave(CUSA_Ranking_filename, path = output_dir, width = 50, height = 40, units = 'cm')
  
  Indy_VoA_Rating_Chart <- ggplot(Indy_Ratings_Rks, aes(x = CFB_Week, y = VoA_Rating, group = team)) +
    theme_bw() +
    geom_line(linewidth = 1.5) +
    geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA Rating") +
    labs(caption = "chart by @gshelor, data from collegefootballdata.com API via cfbfastR and stats.ncaa.org") +
    ggtitle("Independents Vortex of Accuracy Ratings by Week") +
    expand_limits(y = c(floor(floor(min(Indy_Ratings_Rks$VoA_Rating)) / 10) * 10, ceiling((ceiling(max(Indy_Ratings_Rks$VoA_Rating)) / 10)) * 10)) +
    scale_y_continuous(breaks = seq((floor((floor(min(Indy_Ratings_Rks$VoA_Rating)) / 10)) * 10), (ceiling((ceiling(max(Indy_Ratings_Rks$VoA_Rating)) / 10)) * 10), by = 5)) +
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)) +
    geom_cfb_logos(aes(team = team, width = 0.035)) +
    theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
  Indy_VoA_Rating_Chart
  ggsave(Indy_Output_filename, path = output_dir, width = 50, height = 40, units = 'cm')
  
  Indy_VoA_Ranking_Chart <- ggplot(Indy_Ratings_Rks, aes(x = CFB_Week, y = VoA_Ranking, group = team)) +
    theme_bw() +
    geom_line(linewidth = 1.5) +
    geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA Ranking") +
    labs(caption = "chart by @gshelor, data from collegefootballdata.com API via cfbfastR and stats.ncaa.org") +
    ggtitle("Independents Vortex of Accuracy Rankings by Week") +
    expand_limits(y = c(0,130)) +
    scale_y_continuous(breaks = c(0,20,40,60,80,100,130)) +
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)) +
    geom_cfb_logos(aes(team = team, width = 0.035)) +
    theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
  Indy_VoA_Ranking_Chart
  ggsave(Indy_Ranking_filename, path = output_dir, width = 50, height = 40, units = 'cm')
  
  MAC_VoA_Rating_Chart <- ggplot(MAC_Ratings_Rks, aes(x = CFB_Week, y = VoA_Rating, group = team)) +
    theme_bw() +
    geom_line(linewidth = 1.5) +
    geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA Rating") +
    labs(caption = "chart by @gshelor, data from collegefootballdata.com API via cfbfastR and stats.ncaa.org") +
    ggtitle("MAC Vortex of Accuracy Ratings by Week") +
    expand_limits(y = c(floor(floor(min(MAC_Ratings_Rks$VoA_Rating)) / 10) * 10, ceiling((ceiling(max(MAC_Ratings_Rks$VoA_Rating)) / 10)) * 10)) +
    scale_y_continuous(breaks = seq((floor((floor(min(MAC_Ratings_Rks$VoA_Rating)) / 10)) * 10), (ceiling((ceiling(max(MAC_Ratings_Rks$VoA_Rating)) / 10)) * 10), by = 5)) +
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)) +
    geom_cfb_logos(aes(team = team, width = 0.035)) +
    theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
  MAC_VoA_Rating_Chart
  ggsave(MAC_Output_filename, path = output_dir, width = 50, height = 40, units = 'cm')
  
  MAC_VoA_Ranking_Chart <- ggplot(MAC_Ratings_Rks, aes(x = CFB_Week, y = VoA_Ranking, group = team)) +
    theme_bw() +
    geom_line(linewidth = 1.5) +
    geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA Ranking") +
    labs(caption = "chart by @gshelor, data from collegefootballdata.com API via cfbfastR and stats.ncaa.org") +
    ggtitle("MAC Vortex of Accuracy Rankings by Week") +
    expand_limits(y = c(0,130)) +
    scale_y_continuous(breaks = c(0,20,40,60,80,100,130)) +
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)) +
    geom_cfb_logos(aes(team = team, width = 0.035)) +
    theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
  MAC_VoA_Ranking_Chart
  ggsave(MAC_Ranking_filename, path = output_dir, width = 50, height = 40, units = 'cm')
  
  MWC_VoA_Rating_Chart <- ggplot(MWC_Ratings_Rks, aes(x = CFB_Week, y = VoA_Rating, group = team)) +
    theme_bw() +
    geom_line(linewidth = 1.5) +
    geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA Rating") +
    labs(caption = "chart by @gshelor, data from collegefootballdata.com API via cfbfastR and stats.ncaa.org") +
    ggtitle("Mountain West Vortex of Accuracy Ratings by Week") +
    expand_limits(y = c(floor(floor(min(MWC_Ratings_Rks$VoA_Rating)) / 10) * 10, ceiling((ceiling(max(MWC_Ratings_Rks$VoA_Rating)) / 10)) * 10)) +
    scale_y_continuous(breaks = seq((floor((floor(min(MWC_Ratings_Rks$VoA_Rating)) / 10)) * 10), (ceiling((ceiling(max(MWC_Ratings_Rks$VoA_Rating)) / 10)) * 10), by = 5)) +
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)) +
    geom_cfb_logos(aes(team = team, width = 0.035)) +
    theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
  MWC_VoA_Rating_Chart
  ggsave(MWC_Output_filename, path = output_dir, width = 50, height = 40, units = 'cm')
  
  MWC_VoA_Ranking_Chart <- ggplot(MWC_Ratings_Rks, aes(x = CFB_Week, y = VoA_Ranking, group = team)) +
    theme_bw() +
    geom_line(linewidth = 1.5) +
    geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA Ranking") +
    labs(caption = "chart by @gshelor, data from collegefootballdata.com API via cfbfastR and stats.ncaa.org") +
    ggtitle("Mountain West Vortex of Accuracy Rankings by Week") +
    expand_limits(y = c(0,130)) +
    scale_y_continuous(breaks = c(0,20,40,60,80,100,130)) +
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)) +
    geom_cfb_logos(aes(team = team, width = 0.035)) +
    theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
  MWC_VoA_Ranking_Chart
  ggsave(MWC_Ranking_filename, path = output_dir, width = 50, height = 40, units = 'cm')
  
  Pac12_VoA_Rating_Chart <- ggplot(Pac12_Ratings_Rks, aes(x = CFB_Week, y = VoA_Rating, group = team)) +
    theme_bw() +
    geom_line(linewidth = 1.5) +
    geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA Rating") +
    labs(caption = "chart by @gshelor, data from collegefootballdata.com API via cfbfastR and stats.ncaa.org") +
    ggtitle("Pac 12 Vortex of Accuracy Ratings by Week") +
    expand_limits(y = c(floor(floor(min(Pac12_Ratings_Rks$VoA_Rating)) / 10) * 10, ceiling((ceiling(max(Pac12_Ratings_Rks$VoA_Rating)) / 10)) * 10)) +
    scale_y_continuous(breaks = seq((floor((floor(min(Pac12_Ratings_Rks$VoA_Rating)) / 10)) * 10), (ceiling((ceiling(max(Pac12_Ratings_Rks$VoA_Rating)) / 10)) * 10), by = 5)) +
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)) +
    geom_cfb_logos(aes(team = team, width = 0.035)) +
    theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
  Pac12_VoA_Rating_Chart
  ggsave(Pac12_Output_filename, path = output_dir, width = 50, height = 40, units = 'cm')
  
  Pac12_VoA_Ranking_Chart <- ggplot(Pac12_Ratings_Rks, aes(x = CFB_Week, y = VoA_Ranking, group = team)) +
    theme_bw() +
    geom_line(linewidth = 1.5) +
    geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA Ranking") +
    labs(caption = "chart by @gshelor, data from collegefootballdata.com API via cfbfastR and stats.ncaa.org") +
    ggtitle("Pac 12 Vortex of Accuracy Rankings by Week") +
    expand_limits(y = c(0,130)) +
    scale_y_continuous(breaks = c(0,20,40,60,80,100,130)) +
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)) +
    geom_cfb_logos(aes(team = team, width = 0.035)) +
    theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
  Pac12_VoA_Ranking_Chart
  ggsave(Pac12_Ranking_filename, path = output_dir, width = 50, height = 40, units = 'cm')
  
  SEC_VoA_Rating_Chart <- ggplot(SEC_Ratings_Rks, aes(x = CFB_Week, y = VoA_Rating, group = team)) +
    theme_bw() +
    geom_line(linewidth = 1.5) +
    geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA Rating") +
    labs(caption = "chart by @gshelor, data from collegefootballdata.com API via cfbfastR and stats.ncaa.org") +
    ggtitle("SEC Vortex of Accuracy Ratings by Week") +
    expand_limits(y = c(floor(floor(min(SEC_Ratings_Rks$VoA_Rating)) / 10) * 10, ceiling((ceiling(max(SEC_Ratings_Rks$VoA_Rating)) / 10)) * 10)) +
    scale_y_continuous(breaks = seq((floor((floor(min(SEC_Ratings_Rks$VoA_Rating)) / 10)) * 10), (ceiling((ceiling(max(SEC_Ratings_Rks$VoA_Rating)) / 10)) * 10), by = 5)) +
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)) +
    geom_cfb_logos(aes(team = team, width = 0.035)) +
    theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
  SEC_VoA_Rating_Chart
  ggsave(SEC_Output_filename, path = output_dir, width = 50, height = 40, units = 'cm')
  
  SEC_VoA_Ranking_Chart <- ggplot(SEC_Ratings_Rks, aes(x = CFB_Week, y = VoA_Ranking, group = team)) +
    theme_bw() +
    geom_line(linewidth = 1.5) +
    geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA Ranking") +
    labs(caption = "chart by @gshelor, data from collegefootballdata.com API via cfbfastR and stats.ncaa.org") +
    ggtitle("SEC Vortex of Accuracy Rankings by Week") +
    expand_limits(y = c(0,130)) +
    scale_y_continuous(breaks = c(0,20,40,60,80,100,130)) +
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)) +
    geom_cfb_logos(aes(team = team, width = 0.035)) +
    theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
  SEC_VoA_Ranking_Chart
  ggsave(SEC_Ranking_filename, path = output_dir, width = 50, height = 40, units = 'cm')
  
  SunBelt_VoA_Rating_Chart <- ggplot(SunBelt_Ratings_Rks, aes(x = CFB_Week, y = VoA_Rating, group = team)) +
    theme_bw() +
    geom_line(linewidth = 1.5) +
    geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA Rating") +
    labs(caption = "chart by @gshelor, data from collegefootballdata.com API via cfbfastR and stats.ncaa.org") +
    ggtitle("Sun Belt Vortex of Accuracy Ratings by Week") +
    expand_limits(y = c(floor(floor(min(SunBelt_Ratings_Rks$VoA_Rating)) / 10) * 10, ceiling((ceiling(max(SunBelt_Ratings_Rks$VoA_Rating)) / 10)) * 10)) +
    scale_y_continuous(breaks = seq((floor((floor(min(SunBelt_Ratings_Rks$VoA_Rating)) / 10)) * 10), (ceiling((ceiling(max(SunBelt_Ratings_Rks$VoA_Rating)) / 10)) * 10), by = 5)) +
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)) +
    geom_cfb_logos(aes(team = team, width = 0.035)) +
    theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
  SunBelt_VoA_Rating_Chart
  ggsave(SunBelt_Output_filename, path = output_dir, width = 50, height = 40, units = 'cm')
  
  SunBelt_VoA_Ranking_Chart <- ggplot(SunBelt_Ratings_Rks, aes(x = CFB_Week, y = VoA_Ranking, group = team)) +
    theme_bw() +
    geom_line(linewidth = 1.5) +
    geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA Ranking") +
    labs(caption = "chart by @gshelor, data from collegefootballdata.com API via cfbfastR and stats.ncaa.org") +
    ggtitle("Sun Belt Vortex of Accuracy Rankings by Week") +
    expand_limits(y = c(0,130)) +
    scale_y_continuous(breaks = c(0,20,40,60,80,100,130)) +
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)) +
    geom_cfb_logos(aes(team = team, width = 0.035)) +
    theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
  SunBelt_VoA_Ranking_Chart
  ggsave(SunBelt_Ranking_filename, path = output_dir, width = 50, height = 40, units = 'cm')
} else {
  print("No charts until Week 2!")
}

## Creating Histograms of VoA Output for all teams, and separate plots for power 5 and group of 5 teams subsetted out
# plots will be made for each week, not just after week 2 like Unintelligble Charts will
## subsetting teams
Power5_VoA <- VoA_Variables |>
  filter(conference == "ACC" | conference == "Big 12" | conference == "Big Ten" | conference == "FBS Independents" | conference == "Pac-12" | conference == "SEC") |>
  filter(team != "Connecticut" & team != "Army" & team != "UMass")

Group5_VoA <- VoA_Variables |>
  filter(conference == "American Athletic" | conference == "Conference USA" | conference == "FBS Independents" | conference == "Mid-American" | conference == "Mountain West" | conference == "Sun Belt") |>
  filter(team != "Notre Dame")

FBS_Rating_histogram <- ggplot(VoA_Variables, aes(VoA_Rating)) +
  geom_histogram(binwidth = 5,
                 col = "black",
                 fill = "orange") +
  scale_x_continuous(breaks = seq(-40,40,5)) +
  scale_y_continuous(breaks = seq(0,50,5)) +
  ggtitle(FBS_hist_title) +
  xlab("VoA Rating") +
  ylab("Frequency") +
  labs(caption = "chart by @gshelor, data from collegefootballdata.com API via cfbfastR and stats.ncaa.org") +
  theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
FBS_Rating_histogram
ggsave(FBS_hist_filename, path = output_dir, width = 50, height = 40, units = 'cm')

Power5_Rating_histogram <- ggplot(Power5_VoA, aes(VoA_Rating)) +
  geom_histogram(binwidth = 5,
                 col = "black",
                 fill = "blue") +
  scale_x_continuous(breaks = seq(-40,40,5)) +
  scale_y_continuous(breaks = seq(0,50,5)) +
  ggtitle(Power5_hist_title) +
  xlab("VoA Rating") +
  ylab("Frequency") +
  labs(caption = "chart by @gshelor, data from collegefootballdata.com API via cfbfastR and stats.ncaa.org") +
  theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
Power5_Rating_histogram
ggsave(Power5_hist_filename, path = output_dir, width = 50, height = 40, units = 'cm')

Group5_Rating_histogram <- ggplot(Group5_VoA, aes(VoA_Rating)) +
  geom_histogram(binwidth = 5,
                 col = "black",
                 fill = "pink") +
  scale_x_continuous(breaks = seq(-40,40,5)) +
  scale_y_continuous(breaks = seq(0,50,5)) +
  ggtitle(Group5_hist_title) +
  xlab("VoA Rating") +
  ylab("Frequency") +
  labs(caption = "chart by @gshelor, data from collegefootballdata.com API via cfbfastR and stats.ncaa.org") +
  theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
Group5_Rating_histogram
ggsave(Group5_hist_filename, path = output_dir, width = 50, height = 40, units = 'cm')

## Creating Scatterplot of VoA_Output vs VoA_Rating
VoA_Output_Rating_plot <- ggplot(VoA_Variables, aes(x = VoA_Output, y = VoA_Rating)) +
  geom_point(size = 5) +
  geom_smooth() +
  scale_x_continuous(breaks = seq(0,130,10)) +
  scale_y_continuous(breaks = seq(-40,40,5)) +
  ggtitle(Output_Rating_Plot_title) +
  xlab("VoA Output") +
  ylab("VoA Rating") +
  labs(caption = "chart by @gshelor, data from collegefootballdata.com API via cfbfastR and stats.ncaa.org") +
  theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
VoA_Output_Rating_plot
ggsave(Output_Rating_Plot_filename, path = output_dir, width = 50, height = 40, units = 'cm')







## End of Script
end_time <- Sys.time()
end_time - start_time

