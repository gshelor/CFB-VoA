## The Vortex of Accuracy, Version 4.0
## Supremely Excellent Yet Salaciously Godlike And Infallibly Magnificent Vortex of Accuracy
## Created by Griffin Shelor
## installing packages
# install.packages(c("devtoools", "tidyverse", "matrixStats", "grid", "gridExtra", "gt", "viridis", "webshot", "writexl", "rvest", "cfbfastR", "espnscrapeR", "openxlsx", "here", "ggsci", "RColorBrewer", "ggpubr", "remotes", "pacman", "gtExtras"))
## Load Packages for Ranking Variables
start_time <- Sys.time()
library(pacman)
pacman::p_load(tidyverse, gt, viridis, webshot, cfbfastR,  here, ggsci, RColorBrewer, 
               ggpubr, gtExtras)
## testing to see if script runs without certain packeges I don't think are being used
# writexl, rvest, openxlsx, tidymodels, ranger, grid, gridExtra, matrixStats, espnscrapeR,

## Creating Week and Year String for Top 25 Table Title, eventually could be used as part of reading in cfbfastR/cfbdata API data
year <- readline(prompt = "What year is it? ")
week <- readline(prompt = "What week is it? ")

## setting strings for table titles, file pathways, unintelligible charts
output_dir <- here("RVoA", "Outputs")
data_dir <- here("Data", paste("VoA", year, sep = ""))
preseason_text <- "Preseason"
VoAString <- "VoA.csv"
week_text <- "Week"
VoA_Top25_text <- "Vortex of Accuracy Top 25"
top25_png <- "VoATop25.png"
fulltable_png <- "VoAFullTable.png"
VoA_text <- "Vortex of Accuracy"
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
fulltable_file_pathway <- paste(year,week_text,week,"_",fulltable_png, sep = "")
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
FBS_hist_filename <- paste(year, "_", week_text, week, "_", FBS_text, Histogram_text, sep = "")
Power5_hist_filename <- paste(year, "_", week_text, week, "_", Power_Five_text, Histogram_text, sep = "")
Group5_hist_filename <- paste(year, "_", week_text, week, "_", Group_Five_text, Histogram_text, sep = "")
Output_Rating_Plot_filename <- paste(year, "_", week_text, week, "_", Output_Rating_Plot_png, sep = "")
## creating string for excel spreadsheet pathway
file_pathway <- paste(data_dir, "/", year, week_text, week,"_", VoAString, sep = "")


##### pulling in data based on week of the season
if (as.numeric(week) == 0) {
  ## reading in data for 3 previous years
  JMU_AllYears <- read_csv(here("Data", "VoA2022", "JamesMadisonPrevYears", "JMU_AllYears.csv"))
  Stats_PY1 <- cfbd_stats_season_team(year = as.integer(year) - 1, start_week = 1, end_week = 15) %>%
    filter(team != "James Madison") %>%
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
  Stats_PY2 <- cfbd_stats_season_team(year = as.integer(year) - 2, start_week = 1, end_week = 15) %>%
    filter(team != "James Madison") %>%
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
  Stats_PY3 <- cfbd_stats_season_team(year = as.integer(year) - 3, start_week = 1, end_week = 15) %>%
    filter(team != "James Madison") %>%
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
  ## filtering out COVID opt-outs from 2019 (PY3) data, will be merged into 2020 (PY2) data
  COVID_Optouts <- Stats_PY3 %>%
    filter(team == "New Mexico State" | team == "Connecticut" | team == "Old Dominion")
  Stats_PY2 <- rbind(Stats_PY2, COVID_Optouts)
  
  ## advanced stats data
  Adv_Stats_PY1 <- cfbd_stats_season_advanced(year = as.integer(year) - 1, excl_garbage_time = FALSE, start_week = 1, end_week = 15) %>%
    filter(team != "James Madison") %>%
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
  Adv_Stats_PY2 <- cfbd_stats_season_advanced(year = as.integer(year) - 2, excl_garbage_time = FALSE, start_week = 1, end_week = 15) %>%
    filter(team != "James Madison") %>%
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
  Adv_Stats_PY3 <- cfbd_stats_season_advanced(year = as.integer(year) - 3, excl_garbage_time = FALSE, start_week = 1, end_week = 15) %>%
    filter(team != "James Madison") %>%
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
  ## filtering out COVID opt-outs from 2019 (PY3) data, will be merged into 2020 (PY2) data
  COVID_Optouts_adv <- Adv_Stats_PY3 %>%
    filter(team == "New Mexico State" | team == "Connecticut" | team == "Old Dominion")
  Adv_Stats_PY2 <- rbind(Adv_Stats_PY2, COVID_Optouts_adv)
  
  ## pulling in SP+ data
  SP_Rankings_PY1 <-cfbd_ratings_sp(year = as.integer(year) - 1) %>%
    filter(team != "James Madison") %>%
    filter(team != "nationalAverages") %>%
    select(team, rating, offense_rating, defense_rating, special_teams_rating)
  colnames(SP_Rankings_PY1) <- c("team", "sp_rating_PY1", "sp_offense_rating_PY1", "sp_defense_rating_PY1", "sp_special_teams_rating_PY1")
  ## Eliminating NAs
  SP_Rankings_PY1[is.na(SP_Rankings_PY1)] = 0
  SP_Rankings_PY2 <-cfbd_ratings_sp(year = as.integer(year) - 2) %>%
    filter(team != "James Madison") %>%
    filter(team != "nationalAverages") %>%
    select(team, rating, offense_rating, defense_rating, special_teams_rating)
  colnames(SP_Rankings_PY2) <- c("team", "sp_rating_PY2", "sp_offense_rating_PY2", "sp_defense_rating_PY2", "sp_special_teams_rating_PY2")
  ## Eliminating NAs
  SP_Rankings_PY2[is.na(SP_Rankings_PY2)] = 0
  SP_Rankings_PY3 <-cfbd_ratings_sp(year = as.integer(year) - 3) %>%
    filter(team != "James Madison") %>%
    filter(team != "nationalAverages") %>%
    select(team, rating, offense_rating, defense_rating, special_teams_rating)
  colnames(SP_Rankings_PY3) <- c("team", "sp_rating_PY3", "sp_offense_rating_PY3", "sp_defense_rating_PY3", "sp_special_teams_rating_PY3")
  ## Eliminating NAs
  SP_Rankings_PY3[is.na(SP_Rankings_PY3)] = 0
  
  ## pulling FPI data
  FPI_df_PY1 <- espn_ratings_fpi(year = as.integer(year) - 1) %>%
    filter(name != "James Madison") %>%
    select(name, fpi, w, l)
  FPI_df_PY2 <- espn_ratings_fpi(year = as.integer(year) - 2) %>%
    filter(name != "James Madison") %>%
    select(name, fpi, w, l)
  FPI_df_PY3 <- espn_ratings_fpi(year = as.integer(year) - 3) %>%
    filter(name != "James Madison") %>%
    select(name, fpi, w, l)
  ## changing column names here since all of the columns used in the VoA are extracted in the first step
  FPI_PY1_colnames <- c("team", "FPI_PY1", "Wins_PY1", "Losses_PY1")
  FPI_PY2_colnames <- c("team", "FPI_PY2", "Wins_PY2", "Losses_PY2")
  FPI_PY3_colnames <- c("team", "FPI_PY3", "Wins_PY3", "Losses_PY3")
  colnames(FPI_df_PY1) <- FPI_PY1_colnames
  colnames(FPI_df_PY2) <- FPI_PY2_colnames
  colnames(FPI_df_PY3) <- FPI_PY3_colnames
  ## converting character columns to numeric
  FPI_df_PY1[,2:ncol(FPI_df_PY1)] <- FPI_df_PY1[,2:ncol(FPI_df_PY1)] %>% mutate_if(is.character,as.numeric)
  FPI_df_PY2[,2:ncol(FPI_df_PY2)] <- FPI_df_PY2[,2:ncol(FPI_df_PY2)] %>% mutate_if(is.character,as.numeric)
  FPI_df_PY3[,2:ncol(FPI_df_PY3)] <- FPI_df_PY3[,2:ncol(FPI_df_PY3)] %>% mutate_if(is.character,as.numeric)
  
  ## Changing FPI team names to match up with outputs of cfbdata functions
  ## Changing team names in FPI df to match what appears in cfbfastR stats function
  FPI_df_PY1 <- FPI_df_PY1 %>%
    mutate(school = case_when(team == 'Appalachian St' ~ 'Appalachian State',
                              team == 'C Michigan' ~ 'Central Michigan',
                              team == 'Coast Carolina' ~ 'Coastal Carolina',
                              team == 'UConn' ~ 'Connecticut',
                              team == 'E Michigan' ~ 'Eastern Michigan',
                              team == 'FAU' ~ 'Florida Atlantic',
                              team == 'Florida Intl' ~ 'Florida International',
                              team == 'FIU' ~ 'Florida International',
                              team == 'Georgia So' ~ 'Georgia Southern',
                              team == 'UL Monroe' ~ 'Louisiana Monroe',
                              team == 'LA Tech' ~ 'Louisiana Tech',
                              team == 'MTSU' ~ 'Middle Tennessee',
                              team == 'Mississippi St' ~ 'Mississippi State',
                              team == 'New Mexico St' ~ 'New Mexico State',
                              team == 'N Illinois' ~ 'Northern Illinois',
                              team == 'Oklahoma St' ~ 'Oklahoma State',
                              team == 'Oregon St' ~ 'Oregon State',
                              team == 'San Jose State' ~ 'San José State',
                              team == 'Southern Miss' ~ 'Southern Mississippi',
                              team == 'UTSA' ~ 'UT San Antonio',
                              team == 'Washington St' ~ 'Washington State',
                              team == 'Western KY' ~ 'Western Kentucky',
                              team == 'W Michigan' ~ 'Western Michigan',
                              TRUE ~ team)) %>%
    select(school, FPI_PY1, Wins_PY1, Losses_PY1)
  FPI_df_PY2 <- FPI_df_PY2 %>%
    mutate(school = case_when(team == 'Appalachian St' ~ 'Appalachian State',
                              team == 'C Michigan' ~ 'Central Michigan',
                              team == 'Coast Carolina' ~ 'Coastal Carolina',
                              team == 'UConn' ~ 'Connecticut',
                              team == 'E Michigan' ~ 'Eastern Michigan',
                              team == 'FAU' ~ 'Florida Atlantic',
                              team == 'Florida Intl' ~ 'Florida International',
                              team == 'FIU' ~ 'Florida International',
                              team == 'Georgia So' ~ 'Georgia Southern',
                              team == 'UL Monroe' ~ 'Louisiana Monroe',
                              team == 'LA Tech' ~ 'Louisiana Tech',
                              team == 'MTSU' ~ 'Middle Tennessee',
                              team == 'Mississippi St' ~ 'Mississippi State',
                              team == 'New Mexico St' ~ 'New Mexico State',
                              team == 'N Illinois' ~ 'Northern Illinois',
                              team == 'Oklahoma St' ~ 'Oklahoma State',
                              team == 'Oregon St' ~ 'Oregon State',
                              team == 'San Jose State' ~ 'San José State',
                              team == 'Southern Miss' ~ 'Southern Mississippi',
                              team == 'UTSA' ~ 'UT San Antonio',
                              team == 'Washington St' ~ 'Washington State',
                              team == 'Western KY' ~ 'Western Kentucky',
                              team == 'W Michigan' ~ 'Western Michigan',
                              TRUE ~ team)) %>%
    select(school, FPI_PY2, Wins_PY2, Losses_PY2)
  FPI_df_PY3 <- FPI_df_PY3 %>%
    mutate(school = case_when(team == 'Appalachian St' ~ 'Appalachian State',
                              team == 'C Michigan' ~ 'Central Michigan',
                              team == 'Coast Carolina' ~ 'Coastal Carolina',
                              team == 'UConn' ~ 'Connecticut',
                              team == 'E Michigan' ~ 'Eastern Michigan',
                              team == 'FAU' ~ 'Florida Atlantic',
                              team == 'Florida Intl' ~ 'Florida International',
                              team == 'FIU' ~ 'Florida International',
                              team == 'Georgia So' ~ 'Georgia Southern',
                              team == 'UL Monroe' ~ 'Louisiana Monroe',
                              team == 'LA Tech' ~ 'Louisiana Tech',
                              team == 'MTSU' ~ 'Middle Tennessee',
                              team == 'Mississippi St' ~ 'Mississippi State',
                              team == 'New Mexico St' ~ 'New Mexico State',
                              team == 'N Illinois' ~ 'Northern Illinois',
                              team == 'Oklahoma St' ~ 'Oklahoma State',
                              team == 'Oregon St' ~ 'Oregon State',
                              team == 'San Jose State' ~ 'San José State',
                              team == 'Southern Miss' ~ 'Southern Mississippi',
                              team == 'UTSA' ~ 'UT San Antonio',
                              team == 'Washington St' ~ 'Washington State',
                              team == 'Western KY' ~ 'Western Kentucky',
                              team == 'W Michigan' ~ 'Western Michigan',
                              TRUE ~ team)) %>%
    select(school, FPI_PY3, Wins_PY3, Losses_PY3)
  colnames(FPI_df_PY1) <- FPI_PY1_colnames
  colnames(FPI_df_PY2) <- FPI_PY2_colnames
  colnames(FPI_df_PY3) <- FPI_PY3_colnames
  
  ## filtering out COVID opt-outs from 2019 (PY3) data, will be merged into 2020 (PY2) data
  COVID_Optouts_fpi <- FPI_df_PY3 %>%
    filter(team == "Connecticut" | team == "New Mexico State" | team == "Old Dominion")
  colnames(COVID_Optouts_fpi) <- FPI_PY2_colnames
  FPI_df_PY2 <- rbind(FPI_df_PY2, COVID_Optouts_fpi)
  
  ## pulling in recruiting rankings
  recruit_PY1 <- cfbd_recruiting_team(year = as.numeric(year) - 1) %>%
    filter(team != "James Madison") %>%
    filter(team %in% Stats_PY1$team) %>%
    select(team, points)
  colnames(recruit_PY1) <- c("team", "recruit_pts_PY1")
  
  ## pulling in talent rankings
  talent_df_PY1 <- cfbd_team_talent(year = as.numeric(year) - 1) %>%
    filter(school != "James Madison") %>%
    filter(school %in% Stats_PY1$team) %>%
    select(school, talent)
  colnames(talent_df_PY1) <- c("team", "talent_PY1")
  
  ## pulling in recruiting rankings
  recruit_PY2 <- cfbd_recruiting_team(year = as.numeric(year) - 2) %>%
    filter(team != "James Madison") %>%
    filter(team %in% Stats_PY2$team) %>%
    select(team, points)
  colnames(recruit_PY2) <- c("team", "recruit_pts_PY2")
  
  ## pulling in talent rankings
  talent_df_PY2 <- cfbd_team_talent(year = as.numeric(year) - 2) %>%
    filter(school != "James Madison") %>%
    filter(school %in% Stats_PY2$team) %>%
    select(school, talent)
  colnames(talent_df_PY2) <- c("team", "talent_PY2")
  
  ## pulling in recruiting rankings
  recruit_PY3 <- cfbd_recruiting_team(year = as.numeric(year) - 3) %>%
    filter(team != "James Madison") %>%
    filter(team %in% Stats_PY3$team) %>%
    select(team, points)
  colnames(recruit_PY3) <- c("team", "recruit_pts_PY3")
  
  ## pulling in talent rankings
  talent_df_PY3 <- cfbd_team_talent(year = as.numeric(year) - 3) %>%
    filter(school != "James Madison") %>%
    filter(school %in% Stats_PY3$team) %>%
    select(school, talent)
  colnames(talent_df_PY3) <- c("team", "talent_PY3")
  
  ## incoming recruiting class rankings
  recruit <- cfbd_recruiting_team(year = as.numeric(year)) %>%
    filter(team != "James Madison") %>%
    filter(team %in% Stats_PY1$team) %>%
    select(team, points)
  colnames(recruit) <- c("team", "recruit_pts")
} else if (as.numeric(week) == 1) {
  ## reading in data for 3 previous years
  JMU_AllYears <- read_csv(here("Data", "VoA2022", "JamesMadisonPrevYears", "JMU_AllYears.csv"))
  Stats_PY1 <- cfbd_stats_season_team(year = as.integer(year) - 1, start_week = 1, end_week = 15) %>%
    filter(team != "James Madison") %>%
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
  Stats_PY2 <- cfbd_stats_season_team(year = as.integer(year) - 2, start_week = 1, end_week = 15) %>%
    filter(team != "James Madison") %>%
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
  Stats_PY3 <- cfbd_stats_season_team(year = as.integer(year) - 3, start_week = 1, end_week = 15) %>%
    filter(team != "James Madison") %>%
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
  
  ## advanced stats data
  Adv_Stats_PY1 <- cfbd_stats_season_advanced(year = as.integer(year) - 1, excl_garbage_time = FALSE, start_week = 1, end_week = 15) %>%
    filter(team != "James Madison") %>%
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
  Adv_Stats_PY2 <- cfbd_stats_season_advanced(year = as.integer(year) - 2, excl_garbage_time = FALSE, start_week = 1, end_week = 15) %>%
    filter(team != "James Madison") %>%
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
  Adv_Stats_PY3 <- cfbd_stats_season_advanced(year = as.integer(year) - 3, excl_garbage_time = FALSE, start_week = 1, end_week = 15) %>%
    filter(team != "James Madison") %>%
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
  
  ## pulling in SP+ data
  SP_Rankings_PY1 <-cfbd_ratings_sp(year = as.integer(year) - 1) %>%
    filter(team != "James Madison") %>%
    filter(team != "nationalAverages") %>%
    select(team, rating, offense_rating, defense_rating, special_teams_rating)
  colnames(SP_Rankings_PY1) <- c("team", "sp_rating_PY1", "sp_offense_rating_PY1", "sp_defense_rating_PY1", "sp_special_teams_rating_PY1")
  ## Eliminating NAs
  SP_Rankings_PY1[is.na(SP_Rankings_PY1)] = 0
  SP_Rankings_PY2 <-cfbd_ratings_sp(year = as.integer(year) - 2) %>%
    filter(team != "James Madison") %>%
    filter(team != "nationalAverages") %>%
    select(team, rating, offense_rating, defense_rating, special_teams_rating)
  colnames(SP_Rankings_PY2) <- c("team", "sp_rating_PY2", "sp_offense_rating_PY2", "sp_defense_rating_PY2", "sp_special_teams_rating_PY2")
  ## Eliminating NAs
  SP_Rankings_PY2[is.na(SP_Rankings_PY2)] = 0
  SP_Rankings_PY3 <-cfbd_ratings_sp(year = as.integer(year) - 3) %>%
    filter(team != "James Madison") %>%
    filter(team != "nationalAverages") %>%
    select(team, rating, offense_rating, defense_rating, special_teams_rating)
  colnames(SP_Rankings_PY3) <- c("team", "sp_rating_PY3", "sp_offense_rating_PY3", "sp_defense_rating_PY3", "sp_special_teams_rating_PY3")
  ## Eliminating NAs
  SP_Rankings_PY3[is.na(SP_Rankings_PY3)] = 0
  
  ## pulling FPI data
  FPI_df_PY1 <- espn_ratings_fpi(year = as.integer(year) - 1) %>%
    filter(name != "James Madison") %>%
    select(name, fpi, w, l)
  FPI_df_PY2 <- espn_ratings_fpi(year = as.integer(year) - 2) %>%
    filter(name != "James Madison") %>%
    select(name, fpi, w, l)
  FPI_df_PY3 <- espn_ratings_fpi(year = as.integer(year) - 3) %>%
    filter(name != "James Madison") %>%
    select(name, fpi, w, l)
  ## changing column names here since all of the columns used in the VoA are extracted in the first step
  FPI_PY1_colnames <- c("team", "FPI_PY1", "Wins_PY1", "Losses_PY1")
  FPI_PY2_colnames <- c("team", "FPI_PY2", "Wins_PY2", "Losses_PY2")
  FPI_PY3_colnames <- c("team", "FPI_PY3", "Wins_PY3", "Losses_PY3")
  colnames(FPI_df_PY1) <- FPI_PY1_colnames
  colnames(FPI_df_PY2) <- FPI_PY2_colnames
  colnames(FPI_df_PY3) <- FPI_PY3_colnames
  ## converting character columns to numeric
  FPI_df_PY1[,2:ncol(FPI_df_PY1)] <- FPI_df_PY1[,2:ncol(FPI_df_PY1)] %>% mutate_if(is.character,as.numeric)
  FPI_df_PY2[,2:ncol(FPI_df_PY2)] <- FPI_df_PY2[,2:ncol(FPI_df_PY2)] %>% mutate_if(is.character,as.numeric)
  FPI_df_PY3[,2:ncol(FPI_df_PY3)] <- FPI_df_PY3[,2:ncol(FPI_df_PY3)] %>% mutate_if(is.character,as.numeric)
  
  ## Changing FPI team names to match up with outputs of cfbdata functions
  ## Changing team names in FPI df to match what appears in cfbfastR stats function
  FPI_df_PY1 <- FPI_df_PY1 %>%
    mutate(school = case_when(team == 'Appalachian St' ~ 'Appalachian State',
                              team == 'C Michigan' ~ 'Central Michigan',
                              team == 'Coast Carolina' ~ 'Coastal Carolina',
                              team == 'UConn' ~ 'Connecticut',
                              team == 'E Michigan' ~ 'Eastern Michigan',
                              team == 'FAU' ~ 'Florida Atlantic',
                              team == 'Florida Intl' ~ 'Florida International',
                              team == 'FIU' ~ 'Florida International',
                              team == 'Georgia So' ~ 'Georgia Southern',
                              team == 'UL Monroe' ~ 'Louisiana Monroe',
                              team == 'LA Tech' ~ 'Louisiana Tech',
                              team == 'MTSU' ~ 'Middle Tennessee',
                              team == 'Mississippi St' ~ 'Mississippi State',
                              team == 'New Mexico St' ~ 'New Mexico State',
                              team == 'N Illinois' ~ 'Northern Illinois',
                              team == 'Oklahoma St' ~ 'Oklahoma State',
                              team == 'Oregon St' ~ 'Oregon State',
                              team == 'San Jose State' ~ 'San José State',
                              team == 'Southern Miss' ~ 'Southern Mississippi',
                              team == 'UTSA' ~ 'UT San Antonio',
                              team == 'Washington St' ~ 'Washington State',
                              team == 'Western KY' ~ 'Western Kentucky',
                              team == 'W Michigan' ~ 'Western Michigan',
                              TRUE ~ team)) %>%
    select(school, FPI_PY1, Wins_PY1, Losses_PY1)
  FPI_df_PY2 <- FPI_df_PY2 %>%
    mutate(school = case_when(team == 'Appalachian St' ~ 'Appalachian State',
                              team == 'C Michigan' ~ 'Central Michigan',
                              team == 'Coast Carolina' ~ 'Coastal Carolina',
                              team == 'UConn' ~ 'Connecticut',
                              team == 'E Michigan' ~ 'Eastern Michigan',
                              team == 'FAU' ~ 'Florida Atlantic',
                              team == 'Florida Intl' ~ 'Florida International',
                              team == 'FIU' ~ 'Florida International',
                              team == 'Georgia So' ~ 'Georgia Southern',
                              team == 'UL Monroe' ~ 'Louisiana Monroe',
                              team == 'LA Tech' ~ 'Louisiana Tech',
                              team == 'MTSU' ~ 'Middle Tennessee',
                              team == 'Mississippi St' ~ 'Mississippi State',
                              team == 'New Mexico St' ~ 'New Mexico State',
                              team == 'N Illinois' ~ 'Northern Illinois',
                              team == 'Oklahoma St' ~ 'Oklahoma State',
                              team == 'Oregon St' ~ 'Oregon State',
                              team == 'San Jose State' ~ 'San José State',
                              team == 'Southern Miss' ~ 'Southern Mississippi',
                              team == 'UTSA' ~ 'UT San Antonio',
                              team == 'Washington St' ~ 'Washington State',
                              team == 'Western KY' ~ 'Western Kentucky',
                              team == 'W Michigan' ~ 'Western Michigan',
                              TRUE ~ team)) %>%
    select(school, FPI_PY2, Wins_PY2, Losses_PY2)
  FPI_df_PY3 <- FPI_df_PY3 %>%
    mutate(school = case_when(team == 'Appalachian St' ~ 'Appalachian State',
                              team == 'C Michigan' ~ 'Central Michigan',
                              team == 'Coast Carolina' ~ 'Coastal Carolina',
                              team == 'UConn' ~ 'Connecticut',
                              team == 'E Michigan' ~ 'Eastern Michigan',
                              team == 'FAU' ~ 'Florida Atlantic',
                              team == 'Florida Intl' ~ 'Florida International',
                              team == 'FIU' ~ 'Florida International',
                              team == 'Georgia So' ~ 'Georgia Southern',
                              team == 'UL Monroe' ~ 'Louisiana Monroe',
                              team == 'LA Tech' ~ 'Louisiana Tech',
                              team == 'MTSU' ~ 'Middle Tennessee',
                              team == 'Mississippi St' ~ 'Mississippi State',
                              team == 'New Mexico St' ~ 'New Mexico State',
                              team == 'N Illinois' ~ 'Northern Illinois',
                              team == 'Oklahoma St' ~ 'Oklahoma State',
                              team == 'Oregon St' ~ 'Oregon State',
                              team == 'San Jose State' ~ 'San José State',
                              team == 'Southern Miss' ~ 'Southern Mississippi',
                              team == 'UTSA' ~ 'UT San Antonio',
                              team == 'Washington St' ~ 'Washington State',
                              team == 'Western KY' ~ 'Western Kentucky',
                              team == 'W Michigan' ~ 'Western Michigan',
                              TRUE ~ team)) %>%
    select(school, FPI_PY3, Wins_PY3, Losses_PY3)
  colnames(FPI_df_PY1) <- FPI_PY1_colnames
  colnames(FPI_df_PY2) <- FPI_PY2_colnames
  colnames(FPI_df_PY3) <- FPI_PY3_colnames
  
  ## pulling in recruiting rankings
  recruit_PY1 <- cfbd_recruiting_team(year = as.numeric(year) - 1) %>%
    filter(team != "James Madison") %>%
    filter(team %in% Stats_PY1$team) %>%
    select(team, points)
  colnames(recruit_PY1) <- c("team", "recruit_pts_PY1")
  
  ## pulling in talent rankings
  talent_df_PY1 <- cfbd_team_talent(year = as.numeric(year) - 1) %>%
    filter(school != "James Madison") %>%
    filter(school %in% Stats_PY1$team) %>%
    select(school, talent)
  colnames(talent_df_PY1) <- c("team", "talent_PY1")
  
  ## pulling in recruiting rankings
  recruit_PY2 <- cfbd_recruiting_team(year = as.numeric(year) - 2) %>%
    filter(team != "James Madison") %>%
    filter(team %in% Stats_PY2$team) %>%
    select(team, points)
  colnames(recruit_PY2) <- c("team", "recruit_pts_PY2")
  
  ## pulling in talent rankings
  talent_df_PY2 <- cfbd_team_talent(year = as.numeric(year) - 2) %>%
    filter(school != "James Madison") %>%
    filter(school %in% Stats_PY2$team) %>%
    select(school, talent)
  colnames(talent_df_PY2) <- c("team", "talent_PY2")
  
  ## pulling in recruiting rankings
  recruit_PY3 <- cfbd_recruiting_team(year = as.numeric(year) - 3) %>%
    filter(team != "James Madison") %>%
    filter(team %in% Stats_PY3$team) %>%
    select(team, points)
  colnames(recruit_PY3) <- c("team", "recruit_pts_PY3")
  
  ## pulling in talent rankings
  talent_df_PY3 <- cfbd_team_talent(year = as.numeric(year) - 3) %>%
    filter(school != "James Madison") %>%
    filter(school %in% Stats_PY3$team) %>%
    select(school, talent)
  colnames(talent_df_PY3) <- c("team", "talent_PY3")
  
  ## incoming recruiting class rankings
  recruit <- cfbd_recruiting_team(year = as.numeric(year)) %>%
    filter(team != "James Madison") %>%
    filter(team %in% Stats_PY1$team) %>%
    select(team, points)
  colnames(recruit) <- c("team", "recruit_pts")
  
  ### CURRENT SEASON
  Stats <- cfbd_stats_season_team(year = as.integer(year), start_week = 1, end_week = as.numeric(week)) %>%
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
  
  ## advanced stats data
  Adv_Stats <- cfbd_stats_season_advanced(year = as.integer(year), excl_garbage_time = FALSE, start_week = 1, end_week = as.numeric(week)) %>%
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
  
  ## IFF advanced stats data for current year is missing due to data issues
  missing_adv_teams <- anti_join(Stats, Adv_Stats)
  if (nrow(missing_adv_teams) > 0) {
    missing_adv_teams_adv_stats <- Adv_Stats_PY1 %>%
      filter(team %in% missing_adv_teams$team) 
  } else {
    print("no teams missing from advanced stats data frame")
  }
    
  ## current FPI data as of this week
  ## pulling FPI data
  FPI_df <- espn_ratings_fpi(year = as.integer(year)) %>%
    select(name, fpi, w, l)
  ## changing column names here since all of the columns used in the VoA are extracted in the first step
  FPI_colnames <- c("team", "FPI", "Wins", "Losses")
  colnames(FPI_df) <- FPI_colnames
  ## converting character columns to numeric
  FPI_df[,2:ncol(FPI_df)] <- FPI_df[,2:ncol(FPI_df)] %>% mutate_if(is.character,as.numeric)
  
  ## Changing FPI team names to match up with outputs of cfbdata functions
  ## Changing team names in FPI df to match what appears in cfbfastR stats function
  FPI_df <- FPI_df %>%
    mutate(school = case_when(team == 'Appalachian St' ~ 'Appalachian State',
                              team == 'C Michigan' ~ 'Central Michigan',
                              team == 'Coast Carolina' ~ 'Coastal Carolina',
                              team == 'UConn' ~ 'Connecticut',
                              team == 'E Michigan' ~ 'Eastern Michigan',
                              team == 'FAU' ~ 'Florida Atlantic',
                              team == 'Florida Intl' ~ 'Florida International',
                              team == 'FIU' ~ 'Florida International',
                              team == 'Georgia So' ~ 'Georgia Southern',
                              team == 'UL Monroe' ~ 'Louisiana Monroe',
                              team == 'LA Tech' ~ 'Louisiana Tech',
                              team == 'MTSU' ~ 'Middle Tennessee',
                              team == 'Mississippi St' ~ 'Mississippi State',
                              team == 'New Mexico St' ~ 'New Mexico State',
                              team == 'N Illinois' ~ 'Northern Illinois',
                              team == 'Oklahoma St' ~ 'Oklahoma State',
                              team == 'Oregon St' ~ 'Oregon State',
                              team == 'San Jose State' ~ 'San José State',
                              team == 'Southern Miss' ~ 'Southern Mississippi',
                              team == 'UTSA' ~ 'UT San Antonio',
                              team == 'Washington St' ~ 'Washington State',
                              team == 'Western KY' ~ 'Western Kentucky',
                              team == 'W Michigan' ~ 'Western Michigan',
                              TRUE ~ team)) %>%
    select(school, FPI, Wins, Losses)
  colnames(FPI_df) <- FPI_colnames
  ## Current SP+ data
  SP_Rankings <-cfbd_ratings_sp(year = as.integer(year)) %>%
    filter(team != "nationalAverages") %>%
    select(team, rating, offense_rating, defense_rating, special_teams_rating)
  colnames(SP_Rankings) <- c("team", "sp_rating", "sp_offense_rating", "sp_defense_rating", "sp_special_teams_rating")
  ## Eliminating NAs
  SP_Rankings[is.na(SP_Rankings)] = 0
} else if (as.numeric(week) <= 4) {
  ########## WEEKS 2-4
  ## reading in data for 2 previous years
  JMU_2Years <- read_csv(here("Data", "VoA2022", "JamesMadisonPrevYears", "JMU_2Years.csv"))
  Stats_PY1 <- cfbd_stats_season_team(year = as.integer(year) - 1, start_week = 1, end_week = 15) %>%
    filter(team != "James Madison") %>%
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
  Stats_PY2 <- cfbd_stats_season_team(year = as.integer(year) - 2, start_week = 1, end_week = 15) %>%
    filter(team != "James Madison") %>%
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
  
  ## advanced stats data
  Adv_Stats_PY1 <- cfbd_stats_season_advanced(year = as.integer(year) - 1, excl_garbage_time = FALSE, start_week = 1, end_week = 15) %>%
    filter(team != "James Madison") %>%
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
  Adv_Stats_PY2 <- cfbd_stats_season_advanced(year = as.integer(year) - 2, excl_garbage_time = FALSE, start_week = 1, end_week = 15) %>%
    filter(team != "James Madison") %>%
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
  
  ## pulling in SP+ data
  SP_Rankings_PY1 <-cfbd_ratings_sp(year = as.integer(year) - 1) %>%
    filter(team != "James Madison") %>%
    filter(team != "nationalAverages") %>%
    select(team, rating, offense_rating, defense_rating, special_teams_rating)
  colnames(SP_Rankings_PY1) <- c("team", "sp_rating_PY1", "sp_offense_rating_PY1", "sp_defense_rating_PY1", "sp_special_teams_rating_PY1")
  ## Eliminating NAs
  SP_Rankings_PY1[is.na(SP_Rankings_PY1)] = 0
  SP_Rankings_PY2 <-cfbd_ratings_sp(year = as.integer(year) - 2) %>%
    filter(team != "James Madison") %>%
    filter(team != "nationalAverages") %>%
    select(team, rating, offense_rating, defense_rating, special_teams_rating)
  colnames(SP_Rankings_PY2) <- c("team", "sp_rating_PY2", "sp_offense_rating_PY2", "sp_defense_rating_PY2", "sp_special_teams_rating_PY2")
  ## Eliminating NAs
  SP_Rankings_PY2[is.na(SP_Rankings_PY2)] = 0
  
  ## pulling FPI data
  FPI_df_PY1 <- espn_ratings_fpi(year = as.integer(year) - 1) %>%
    filter(name != "James Madison") %>%
    select(name, fpi, w, l)
  FPI_df_PY2 <- espn_ratings_fpi(year = as.integer(year) - 2) %>%
    filter(name != "James Madison") %>%
    select(name, fpi, w, l)
  ## changing column names here since all of the columns used in the VoA are extracted in the first step
  FPI_PY1_colnames <- c("team", "FPI_PY1", "Wins_PY1", "Losses_PY1")
  FPI_PY2_colnames <- c("team", "FPI_PY2", "Wins_PY2", "Losses_PY2")
  colnames(FPI_df_PY1) <- FPI_PY1_colnames
  colnames(FPI_df_PY2) <- FPI_PY2_colnames
  ## converting character columns to numeric
  FPI_df_PY1[,2:ncol(FPI_df_PY1)] <- FPI_df_PY1[,2:ncol(FPI_df_PY1)] %>% mutate_if(is.character,as.numeric)
  FPI_df_PY2[,2:ncol(FPI_df_PY2)] <- FPI_df_PY2[,2:ncol(FPI_df_PY2)] %>% mutate_if(is.character,as.numeric)
  
  ## Changing FPI team names to match up with outputs of cfbdata functions
  ## Changing team names in FPI df to match what appears in cfbfastR stats function
  FPI_df_PY1 <- FPI_df_PY1 %>%
    mutate(school = case_when(team == 'Appalachian St' ~ 'Appalachian State',
                              team == 'C Michigan' ~ 'Central Michigan',
                              team == 'Coast Carolina' ~ 'Coastal Carolina',
                              team == 'UConn' ~ 'Connecticut',
                              team == 'E Michigan' ~ 'Eastern Michigan',
                              team == 'FAU' ~ 'Florida Atlantic',
                              team == 'Florida Intl' ~ 'Florida International',
                              team == 'FIU' ~ 'Florida International',
                              team == 'Georgia So' ~ 'Georgia Southern',
                              team == 'UL Monroe' ~ 'Louisiana Monroe',
                              team == 'LA Tech' ~ 'Louisiana Tech',
                              team == 'MTSU' ~ 'Middle Tennessee',
                              team == 'Mississippi St' ~ 'Mississippi State',
                              team == 'New Mexico St' ~ 'New Mexico State',
                              team == 'N Illinois' ~ 'Northern Illinois',
                              team == 'Oklahoma St' ~ 'Oklahoma State',
                              team == 'Oregon St' ~ 'Oregon State',
                              team == 'San Jose State' ~ 'San José State',
                              team == 'Southern Miss' ~ 'Southern Mississippi',
                              team == 'UTSA' ~ 'UT San Antonio',
                              team == 'Washington St' ~ 'Washington State',
                              team == 'Western KY' ~ 'Western Kentucky',
                              team == 'W Michigan' ~ 'Western Michigan',
                              TRUE ~ team)) %>%
    select(school, FPI_PY1, Wins_PY1, Losses_PY1)
  FPI_df_PY2 <- FPI_df_PY2 %>%
    mutate(school = case_when(team == 'Appalachian St' ~ 'Appalachian State',
                              team == 'C Michigan' ~ 'Central Michigan',
                              team == 'Coast Carolina' ~ 'Coastal Carolina',
                              team == 'UConn' ~ 'Connecticut',
                              team == 'E Michigan' ~ 'Eastern Michigan',
                              team == 'FAU' ~ 'Florida Atlantic',
                              team == 'Florida Intl' ~ 'Florida International',
                              team == 'FIU' ~ 'Florida International',
                              team == 'Georgia So' ~ 'Georgia Southern',
                              team == 'UL Monroe' ~ 'Louisiana Monroe',
                              team == 'LA Tech' ~ 'Louisiana Tech',
                              team == 'MTSU' ~ 'Middle Tennessee',
                              team == 'Mississippi St' ~ 'Mississippi State',
                              team == 'New Mexico St' ~ 'New Mexico State',
                              team == 'N Illinois' ~ 'Northern Illinois',
                              team == 'Oklahoma St' ~ 'Oklahoma State',
                              team == 'Oregon St' ~ 'Oregon State',
                              team == 'San Jose State' ~ 'San José State',
                              team == 'Southern Miss' ~ 'Southern Mississippi',
                              team == 'UTSA' ~ 'UT San Antonio',
                              team == 'Washington St' ~ 'Washington State',
                              team == 'Western KY' ~ 'Western Kentucky',
                              team == 'W Michigan' ~ 'Western Michigan',
                              TRUE ~ team)) %>%
    select(school, FPI_PY2, Wins_PY2, Losses_PY2)
  colnames(FPI_df_PY1) <- FPI_PY1_colnames
  colnames(FPI_df_PY2) <- FPI_PY2_colnames
  
  ## pulling in recruiting rankings
  recruit_PY1 <- cfbd_recruiting_team(year = as.numeric(year) - 1) %>%
    filter(team != "James Madison") %>%
    filter(team %in% Stats_PY1$team) %>%
    select(team, points)
  colnames(recruit_PY1) <- c("team", "recruit_pts_PY1")
  
  ## pulling in talent rankings
  talent_df_PY1 <- cfbd_team_talent(year = as.numeric(year) - 1) %>%
    filter(school != "James Madison") %>%
    filter(school %in% Stats_PY1$team) %>%
    select(school, talent)
  colnames(talent_df_PY1) <- c("team", "talent_PY1")
  
  ## pulling in recruiting rankings
  recruit_PY2 <- cfbd_recruiting_team(year = as.numeric(year) - 2) %>%
    filter(team != "James Madison") %>%
    filter(team %in% Stats_PY2$team) %>%
    select(team, points)
  colnames(recruit_PY2) <- c("team", "recruit_pts_PY2")
  
  ## pulling in talent rankings
  talent_df_PY2 <- cfbd_team_talent(year = as.numeric(year) - 2) %>%
    filter(school != "James Madison") %>%
    filter(school %in% Stats_PY2$team) %>%
    select(school, talent)
  colnames(talent_df_PY2) <- c("team", "talent_PY2")
  
  ## incoming recruiting class rankings
  recruit <- cfbd_recruiting_team(year = as.numeric(year)) %>%
    filter(team != "James Madison") %>%
    filter(team %in% Stats_PY1$team) %>%
    select(team, points)
  colnames(recruit) <- c("team", "recruit_pts")
  
  ### CURRENT SEASON
  Stats <- cfbd_stats_season_team(year = as.integer(year), start_week = 1, end_week = as.numeric(week)) %>%
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
  
  ## advanced stats data
  Adv_Stats <- cfbd_stats_season_advanced(year = as.integer(year), excl_garbage_time = FALSE, start_week = 1, end_week = as.numeric(week)) %>%
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
  ## current FPI data as of this week
  ## pulling FPI data
  FPI_df <- espn_ratings_fpi(year = as.integer(year)) %>%
    select(name, fpi, w, l)
  ## changing column names here since all of the columns used in the VoA are extracted in the first step
  FPI_colnames <- c("team", "FPI", "Wins", "Losses")
  colnames(FPI_df) <- FPI_colnames
  ## converting character columns to numeric
  FPI_df[,2:ncol(FPI_df)] <- FPI_df[,2:ncol(FPI_df)] %>% mutate_if(is.character,as.numeric)
  
  ## Changing FPI team names to match up with outputs of cfbdata functions
  ## Changing team names in FPI df to match what appears in cfbfastR stats function
  FPI_df <- FPI_df %>%
    mutate(school = case_when(team == 'Appalachian St' ~ 'Appalachian State',
                              team == 'C Michigan' ~ 'Central Michigan',
                              team == 'Coast Carolina' ~ 'Coastal Carolina',
                              team == 'UConn' ~ 'Connecticut',
                              team == 'E Michigan' ~ 'Eastern Michigan',
                              team == 'FAU' ~ 'Florida Atlantic',
                              team == 'Florida Intl' ~ 'Florida International',
                              team == 'FIU' ~ 'Florida International',
                              team == 'Georgia So' ~ 'Georgia Southern',
                              team == 'UL Monroe' ~ 'Louisiana Monroe',
                              team == 'LA Tech' ~ 'Louisiana Tech',
                              team == 'MTSU' ~ 'Middle Tennessee',
                              team == 'Mississippi St' ~ 'Mississippi State',
                              team == 'New Mexico St' ~ 'New Mexico State',
                              team == 'N Illinois' ~ 'Northern Illinois',
                              team == 'Oklahoma St' ~ 'Oklahoma State',
                              team == 'Oregon St' ~ 'Oregon State',
                              team == 'San Jose State' ~ 'San José State',
                              team == 'Southern Miss' ~ 'Southern Mississippi',
                              team == 'UTSA' ~ 'UT San Antonio',
                              team == 'Washington St' ~ 'Washington State',
                              team == 'Western KY' ~ 'Western Kentucky',
                              team == 'W Michigan' ~ 'Western Michigan',
                              TRUE ~ team)) %>%
    select(school, FPI, Wins, Losses)
  colnames(FPI_df) <- FPI_colnames
  ## Current SP+ data
  SP_Rankings <-cfbd_ratings_sp(year = as.integer(year)) %>%
    filter(team != "nationalAverages") %>%
    select(team, rating, offense_rating, defense_rating, special_teams_rating)
  colnames(SP_Rankings) <- c("team", "sp_rating", "sp_offense_rating", "sp_defense_rating", "sp_special_teams_rating")
  ## Eliminating NAs
  SP_Rankings[is.na(SP_Rankings)] = 0
} else if (as.numeric(week) == 5) {
  ## reading in data for previous year
  JMU_PrevYear <- read_csv(here("Data", "VoA2022", "JamesMadisonPrevYears", "JMU_PreYear.csv"))
  Stats_PY1 <- cfbd_stats_season_team(year = as.integer(year) - 1, start_week = 1, end_week = 15) %>%
    filter(team != "James Madison") %>%
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
  
  ## advanced stats data
  Adv_Stats_PY1 <- cfbd_stats_season_advanced(year = as.integer(year) - 1, excl_garbage_time = FALSE, start_week = 1, end_week = 15) %>%
    filter(team != "James Madison") %>%
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
  
  ## pulling in SP+ data
  SP_Rankings_PY1 <-cfbd_ratings_sp(year = as.integer(year) - 1) %>%
    filter(team != "James Madison") %>%
    filter(team != "nationalAverages") %>%
    select(team, rating, offense_rating, defense_rating, special_teams_rating)
  colnames(SP_Rankings_PY1) <- c("team", "sp_rating_PY1", "sp_offense_rating_PY1", "sp_defense_rating_PY1", "sp_special_teams_rating_PY1")
  ## Eliminating NAs
  SP_Rankings_PY1[is.na(SP_Rankings_PY1)] = 0
  
  ## pulling FPI data
  FPI_df_PY1 <- espn_ratings_fpi(year = as.integer(year) - 1) %>%
    filter(name != "James Madison") %>%
    select(name, fpi, w, l)
  ## changing column names here since all of the columns used in the VoA are extracted in the first step
  FPI_PY1_colnames <- c("team", "FPI_PY1", "Wins_PY1", "Losses_PY1")
  colnames(FPI_df_PY1) <- FPI_PY1_colnames
  ## converting character columns to numeric
  FPI_df_PY1[,2:ncol(FPI_df_PY1)] <- FPI_df_PY1[,2:ncol(FPI_df_PY1)] %>% mutate_if(is.character,as.numeric)
  
  ## Changing FPI team names to match up with outputs of cfbdata functions
  ## Changing team names in FPI df to match what appears in cfbfastR stats function
  FPI_df_PY1 <- FPI_df_PY1 %>%
    mutate(school = case_when(team == 'Appalachian St' ~ 'Appalachian State',
                              team == 'C Michigan' ~ 'Central Michigan',
                              team == 'Coast Carolina' ~ 'Coastal Carolina',
                              team == 'UConn' ~ 'Connecticut',
                              team == 'E Michigan' ~ 'Eastern Michigan',
                              team == 'FAU' ~ 'Florida Atlantic',
                              team == 'Florida Intl' ~ 'Florida International',
                              team == 'FIU' ~ 'Florida International',
                              team == 'Georgia So' ~ 'Georgia Southern',
                              team == 'UL Monroe' ~ 'Louisiana Monroe',
                              team == 'LA Tech' ~ 'Louisiana Tech',
                              team == 'MTSU' ~ 'Middle Tennessee',
                              team == 'Mississippi St' ~ 'Mississippi State',
                              team == 'New Mexico St' ~ 'New Mexico State',
                              team == 'N Illinois' ~ 'Northern Illinois',
                              team == 'Oklahoma St' ~ 'Oklahoma State',
                              team == 'Oregon St' ~ 'Oregon State',
                              team == 'San Jose State' ~ 'San José State',
                              team == 'Southern Miss' ~ 'Southern Mississippi',
                              team == 'UTSA' ~ 'UT San Antonio',
                              team == 'Washington St' ~ 'Washington State',
                              team == 'Western KY' ~ 'Western Kentucky',
                              team == 'W Michigan' ~ 'Western Michigan',
                              TRUE ~ team)) %>%
    select(school, FPI_PY1, Wins_PY1, Losses_PY1)
  colnames(FPI_df_PY1) <- FPI_PY1_colnames
  
  ## pulling in recruiting rankings
  recruit_PY1 <- cfbd_recruiting_team(year = as.numeric(year) - 1) %>%
    filter(team != "James Madison") %>%
    filter(team %in% Stats_PY1$team) %>%
    select(team, points)
  colnames(recruit_PY1) <- c("team", "recruit_pts_PY1")
  
  ## pulling in talent rankings
  talent_df_PY1 <- cfbd_team_talent(year = as.numeric(year) - 1) %>%
    filter(school != "James Madison") %>%
    filter(school %in% Stats_PY1$team) %>%
    select(school, talent)
  colnames(talent_df_PY1) <- c("team", "talent_PY1")
  
  ## incoming recruiting class rankings
  recruit <- cfbd_recruiting_team(year = as.numeric(year)) %>%
    filter(team != "James Madison") %>%
    filter(team %in% Stats_PY1$team) %>%
    select(team, points)
  colnames(recruit) <- c("team", "recruit_pts")
  
  ### CURRENT SEASON STATS
  Stats <- cfbd_stats_season_team(year = as.integer(year), start_week = 1, end_week = as.numeric(week)) %>%
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
  
  ## advanced stats data
  Adv_Stats <- cfbd_stats_season_advanced(year = as.integer(year), excl_garbage_time = FALSE, start_week = 1, end_week = as.numeric(week)) %>%
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
  ## current FPI data as of this week
  ## pulling FPI data
  FPI_df <- espn_ratings_fpi(year = as.integer(year)) %>%
    select(name, fpi, w, l)
  ## changing column names here since all of the columns used in the VoA are extracted in the first step
  FPI_colnames <- c("team", "FPI", "Wins", "Losses")
  colnames(FPI_df) <- FPI_colnames
  ## converting character columns to numeric
  FPI_df[,2:ncol(FPI_df)] <- FPI_df[,2:ncol(FPI_df)] %>% mutate_if(is.character,as.numeric)
  
  ## Changing FPI team names to match up with outputs of cfbdata functions
  ## Changing team names in FPI df to match what appears in cfbfastR stats function
  FPI_df <- FPI_df %>%
    mutate(school = case_when(team == 'Appalachian St' ~ 'Appalachian State',
                              team == 'C Michigan' ~ 'Central Michigan',
                              team == 'Coast Carolina' ~ 'Coastal Carolina',
                              team == 'UConn' ~ 'Connecticut',
                              team == 'E Michigan' ~ 'Eastern Michigan',
                              team == 'FAU' ~ 'Florida Atlantic',
                              team == 'Florida Intl' ~ 'Florida International',
                              team == 'FIU' ~ 'Florida International',
                              team == 'Georgia So' ~ 'Georgia Southern',
                              team == 'UL Monroe' ~ 'Louisiana Monroe',
                              team == 'LA Tech' ~ 'Louisiana Tech',
                              team == 'MTSU' ~ 'Middle Tennessee',
                              team == 'Mississippi St' ~ 'Mississippi State',
                              team == 'New Mexico St' ~ 'New Mexico State',
                              team == 'N Illinois' ~ 'Northern Illinois',
                              team == 'Oklahoma St' ~ 'Oklahoma State',
                              team == 'Oregon St' ~ 'Oregon State',
                              team == 'San Jose State' ~ 'San José State',
                              team == 'Southern Miss' ~ 'Southern Mississippi',
                              team == 'UTSA' ~ 'UT San Antonio',
                              team == 'Washington St' ~ 'Washington State',
                              team == 'Western KY' ~ 'Western Kentucky',
                              team == 'W Michigan' ~ 'Western Michigan',
                              TRUE ~ team)) %>%
    select(school, FPI, Wins, Losses)
  colnames(FPI_df) <- FPI_colnames
  ## Current SP+ data
  SP_Rankings <-cfbd_ratings_sp(year = as.integer(year)) %>%
    filter(team != "nationalAverages") %>%
    select(team, rating, offense_rating, defense_rating, special_teams_rating)
  colnames(SP_Rankings) <- c("team", "sp_rating", "sp_offense_rating", "sp_defense_rating", "sp_special_teams_rating")
  ## Eliminating NAs
  SP_Rankings[is.na(SP_Rankings)] = 0
} else {
  ### CURRENT SEASON STATS
  Stats <- cfbd_stats_season_team(year = as.integer(year), start_week = 1, end_week = as.numeric(week)) %>%
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
  
  ## advanced stats data
  Adv_Stats <- cfbd_stats_season_advanced(year = as.integer(year), excl_garbage_time = FALSE, start_week = 1, end_week = as.numeric(week)) %>%
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
  ## current FPI data as of this week
  ## pulling FPI data
  FPI_df <- espn_ratings_fpi(year = as.integer(year)) %>%
    select(name, fpi, w, l)
  ## changing column names here since all of the columns used in the VoA are extracted in the first step
  FPI_colnames <- c("team", "FPI", "Wins", "Losses")
  colnames(FPI_df) <- FPI_colnames
  ## converting character columns to numeric
  FPI_df[,2:ncol(FPI_df)] <- FPI_df[,2:ncol(FPI_df)] %>% mutate_if(is.character,as.numeric)
  
  ## Changing FPI team names to match up with outputs of cfbdata functions
  ## Changing team names in FPI df to match what appears in cfbfastR stats function
  FPI_df <- FPI_df %>%
    mutate(school = case_when(team == 'Appalachian St' ~ 'Appalachian State',
                              team == 'C Michigan' ~ 'Central Michigan',
                              team == 'Coast Carolina' ~ 'Coastal Carolina',
                              team == 'UConn' ~ 'Connecticut',
                              team == 'E Michigan' ~ 'Eastern Michigan',
                              team == 'FAU' ~ 'Florida Atlantic',
                              team == 'Florida Intl' ~ 'Florida International',
                              team == 'FIU' ~ 'Florida International',
                              team == 'Georgia So' ~ 'Georgia Southern',
                              team == 'UL Monroe' ~ 'Louisiana Monroe',
                              team == 'LA Tech' ~ 'Louisiana Tech',
                              team == 'MTSU' ~ 'Middle Tennessee',
                              team == 'Mississippi St' ~ 'Mississippi State',
                              team == 'New Mexico St' ~ 'New Mexico State',
                              team == 'N Illinois' ~ 'Northern Illinois',
                              team == 'Oklahoma St' ~ 'Oklahoma State',
                              team == 'Oregon St' ~ 'Oregon State',
                              team == 'San Jose State' ~ 'San José State',
                              team == 'Southern Miss' ~ 'Southern Mississippi',
                              team == 'UTSA' ~ 'UT San Antonio',
                              team == 'Washington St' ~ 'Washington State',
                              team == 'Western KY' ~ 'Western Kentucky',
                              team == 'W Michigan' ~ 'Western Michigan',
                              TRUE ~ team)) %>%
    select(school, FPI, Wins, Losses)
  colnames(FPI_df) <- FPI_colnames
  ## Current SP+ data
  SP_Rankings <-cfbd_ratings_sp(year = as.integer(year)) %>%
    filter(team != "nationalAverages") %>%
    select(team, rating, offense_rating, defense_rating, special_teams_rating)
  colnames(SP_Rankings) <- c("team", "sp_rating", "sp_offense_rating", "sp_defense_rating", "sp_special_teams_rating")
  ## Eliminating NAs
  SP_Rankings[is.na(SP_Rankings)] = 0
  
  ## incoming class recruiting rankings
  recruit <- cfbd_recruiting_team(year = as.numeric(year)) %>%
    filter(team != "James Madison") %>%
    filter(team %in% Stats_PY1$team) %>%
    select(team, points)
  colnames(recruit) <- c("team", "recruit_pts")
}

## merging data frames together
if (as.numeric(week) == 0) {
  ## merging data frames together, arranging columns
  ## need to merge stats and advanced stats together first so I can change column names to avoid duplicate column names later on
  # then I will be merging data frames for the same year together
  # then I will merge years together by team
  PY3_stats_adv_stats_list <- list(Stats_PY3, Adv_Stats_PY3)
  PY3_stats_adv_stats_merge <- PY3_stats_adv_stats_list %>%
    reduce(full_join, by = "team") %>%
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
  PY3_df_list <- list(PY3_stats_adv_stats_merge, recruit_PY3, talent_df_PY3, SP_Rankings_PY3, FPI_df_PY3)
  PY3_df <- PY3_df_list %>%
    reduce(full_join, by = "team")
  
  PY2_stats_adv_stats_list <- list(Stats_PY2, Adv_Stats_PY2)
  PY2_stats_adv_stats_merge <- PY2_stats_adv_stats_list %>%
    reduce(full_join, by = "team") %>%
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
  PY2_df_list <- list(PY2_stats_adv_stats_merge, recruit_PY2, talent_df_PY2, SP_Rankings_PY2, FPI_df_PY2)
  PY2_df <- PY2_df_list %>%
    reduce(full_join, by = "team")
  ## exporting csv of 2020 COVID opt outs here in week 0 so that I don't have to re merge all of it in future weeks
  COVID_Optouts_total <- PY2_df %>%
    filter(team == "Old Dominion" | team == "New Mexico State" | team == "Connecticut")
  write_csv(COVID_Optouts_total, here("Data", "VoA2022", "COVIDOptouts_total.csv"))
  
  PY1_stats_adv_stats_list <- list(Stats_PY1, Adv_Stats_PY1)
  PY1_stats_adv_stats_merge <- PY1_stats_adv_stats_list %>%
    reduce(full_join, by = "team") %>%
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
  PY1_df_list <- list(PY1_stats_adv_stats_merge, recruit_PY1, talent_df_PY1, SP_Rankings_PY1, FPI_df_PY1)
  PY1_df <- PY1_df_list %>%
    reduce(full_join, by = "team")
  
  ## merging all data frames in order of PY3, PY2, PY1
  all_PY_df_list <- list(PY3_df, PY2_df, PY1_df, recruit)
  all_PY_df <- all_PY_df_list %>%
    reduce(full_join, by = "team")
  
  ## after binding JMU csv as new row to PY df
  VoA_Variables <- rbind(all_PY_df, JMU_AllYears) %>%
    mutate(FPI_SP_PY3_mean = (sp_rating_PY3 + FPI_PY3) / 2,
           FPI_SP_PY2_mean = (sp_rating_PY2 + FPI_PY2) / 2,
           FPI_SP_PY1_mean = (sp_rating_PY1 + FPI_PY1) / 2,
           AllPY_FPI_SP_mean = (FPI_SP_PY3_mean + FPI_SP_PY2_mean + FPI_SP_PY1_mean) / 3)
} else if (as.numeric(week) == 1) {
  ## merging data frames together, arranging columns
  ## need to merge stats and advanced stats together first so I can change column names to avoid duplicate column names later on
  # then I will be merging data frames for the same year together
  # then I will merge years together by team
  PY3_stats_adv_stats_list <- list(Stats_PY3, Adv_Stats_PY3)
  PY3_stats_adv_stats_merge <- PY3_stats_adv_stats_list %>%
    reduce(full_join, by = "team") %>%
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
  PY3_df_list <- list(PY3_stats_adv_stats_merge, recruit_PY3, talent_df_PY3, SP_Rankings_PY3, FPI_df_PY3)
  PY3_df <- PY3_df_list %>%
    reduce(full_join, by = "team")
  
  PY2_stats_adv_stats_list <- list(Stats_PY2, Adv_Stats_PY2)
  PY2_stats_adv_stats_merge <- PY2_stats_adv_stats_list %>%
    reduce(full_join, by = "team") %>%
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
  PY2_df_list <- list(PY2_stats_adv_stats_merge, recruit_PY2, talent_df_PY2, SP_Rankings_PY2, FPI_df_PY2)
  PY2_df <- PY2_df_list %>%
    reduce(full_join, by = "team")
  ## reading in full csv of opt outs, created in Preseason VoA to hopefully save time
  COVID_Optouts_total <- read_csv(here("Data", "VoA2022", "COVIDOptouts_total.csv"))
  PY2_df <- rbind(PY2_df, COVID_Optouts_total)
  
  PY1_stats_adv_stats_list <- list(Stats_PY1, Adv_Stats_PY1)
  PY1_stats_adv_stats_merge <- PY1_stats_adv_stats_list %>%
    reduce(full_join, by = "team") %>%
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
  PY1_df_list <- list(PY1_stats_adv_stats_merge, recruit_PY1, talent_df_PY1, SP_Rankings_PY1, FPI_df_PY1)
  PY1_df <- PY1_df_list %>%
    reduce(full_join, by = "team")
  
  ## Current Years dataframes
  stats_adv_stats_list <- list(Stats, Adv_Stats)
  stats_adv_stats_merge <- PY2_stats_adv_stats_list %>%
    reduce(full_join, by = "team") %>%
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
  Current_df_list <- list(stats_adv_stats_merge, recruit, SP_Rankings, FPI_df)
  Current_df <- Current_df_list %>%
    reduce(full_join, by = "team")
  
  ## merging all data frames in order of PY3, PY2, PY1
  all_PY_df_list <- list(PY3_df, PY2_df, PY1_df)
  all_PY_df <- all_PY_df_list %>%
    reduce(full_join, by = "team")
  all_PY_df <- rbind(all_PY_df, JMU_AllYears)
  
  ## after binding JMU csv as new row to PY df
  all_df_list <- list(Current_df, all_PY_df)
  VoA_Variables <- all_df_list %>%
    reduce(full_join, by = "team") %>%
    mutate(FPI_SP_PY3_mean = (sp_rating_PY3 + FPI_PY3) / 2,
           FPI_SP_PY2_mean = (sp_rating_PY2 + FPI_PY2) / 2,
           FPI_SP_PY1_mean = (sp_rating_PY1 + FPI_PY1) / 2,
           AllPY_FPI_SP_mean = (FPI_SP_PY3_mean + FPI_SP_PY2_mean + FPI_SP_PY1_mean) / 3,
           FPI_SP_mean = (sp_rating + FPI) / 2)
} else if (as.numeric(week) <= 4) {
  ## merging data frames together, arranging columns
  ## need to merge stats and advanced stats together first so I can change column names to avoid duplicate column names later on
  # then I will be merging data frames for the same year together
  # then I will merge years together by team
  PY2_stats_adv_stats_list <- list(Stats_PY2, Adv_Stats_PY2)
  PY2_stats_adv_stats_merge <- PY2_stats_adv_stats_list %>%
    reduce(full_join, by = "team") %>%
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
  PY2_df_list <- list(PY2_stats_adv_stats_merge, recruit_PY2, talent_df_PY2, SP_Rankings_PY2, FPI_df_PY2)
  PY2_df <- PY2_df_list %>%
    reduce(full_join, by = "team")
  ## reading in full csv of opt outs, created in Preseason VoA to hopefully save time
  COVID_Optouts_total <- read_csv(here("Data", "VoA2022", "COVIDOptouts_total.csv"))
  PY2_df <- rbind(PY2_df, COVID_Optouts_total)
  
  ## PY1 data frames being merged
  PY1_stats_adv_stats_list <- list(Stats_PY1, Adv_Stats_PY1)
  PY1_stats_adv_stats_merge <- PY1_stats_adv_stats_list %>%
    reduce(full_join, by = "team") %>%
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
  PY1_df_list <- list(PY1_stats_adv_stats_merge, recruit_PY1, talent_df_PY1, SP_Rankings_PY1, FPI_df_PY1)
  PY1_df <- PY1_df_list %>%
    reduce(full_join, by = "team")
  
  ## Current Years dataframes
  stats_adv_stats_list <- list(Stats, Adv_Stats)
  stats_adv_stats_merge <- stats_adv_stats_list %>%
    reduce(full_join, by = "team") %>%
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
  Current_df_list <- list(stats_adv_stats_merge, recruit, SP_Rankings, FPI_df)
  Current_df <- Current_df_list %>%
    reduce(full_join, by = "team")
  
  ## merging all PY data frames in order of PY2, PY1
  all_PY_df_list <- list(PY2_df, PY1_df)
  all_PY_df <- all_PY_df_list %>%
    reduce(full_join, by = "team")
  all_PY_df <- rbind(all_PY_df, JMU_2Years)
  
  ## after binding JMU csv as new row to PY df
  all_df_list <- list(Current_df, all_PY_df)
  VoA_Variables <- all_df_list %>%
    reduce(full_join, by = "team") %>%
    mutate(FPI_SP_PY2_mean = (sp_rating_PY2 + FPI_PY2) / 2,
           FPI_SP_PY1_mean = (sp_rating_PY1 + FPI_PY1) / 2,
           AllPY_FPI_SP_mean = (FPI_SP_PY2_mean + FPI_SP_PY1_mean) / 2,
           FPI_SP_mean = (sp_rating + FPI) / 2)
} else if (as.numeric(week) == 5) {
  ## merging data frames together, arranging columns
  ## need to merge stats and advanced stats together first so I can change column names to avoid duplicate column names later on
  # then I will be merging data frames for the same year together
  # then I will merge years together by team
  ## PY1 data frames being merged
  PY1_stats_adv_stats_list <- list(Stats_PY1, Adv_Stats_PY1)
  PY1_stats_adv_stats_merge <- PY1_stats_adv_stats_list %>%
    reduce(full_join, by = "team") %>%
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
  PY1_df_list <- list(PY1_stats_adv_stats_merge, recruit_PY1, talent_df_PY1, SP_Rankings_PY1, FPI_df_PY1)
  PY1_df <- PY1_df_list %>%
    reduce(full_join, by = "team")
  ## binding JMU PY1 csv to PY1_df to avoid row number issue with full df
  PY1_df <- rbind(PY1_df, JMU_PrevYear)
  
  ## Current Years dataframes
  stats_adv_stats_list <- list(Stats, Adv_Stats)
  stats_adv_stats_merge <- stats_adv_stats_list %>%
    reduce(full_join, by = "team") %>%
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
  Current_df_list <- list(stats_adv_stats_merge, recruit, SP_Rankings, FPI_df)
  Current_df <- Current_df_list %>%
    reduce(full_join, by = "team")
  
  ## merging all data frames
  all_df_list <- list(Current_df, PY1_df)
  VoA_Variables <- all_df_list %>%
    reduce(full_join, by = "team") %>%
    mutate(FPI_SP_PY1_mean = (sp_rating_PY1 + FPI_PY1) / 2,
           FPI_SP_mean = (sp_rating + FPI) / 2)
} else {
  ## Current Years data frames
  stats_adv_stats_list <- list(Stats, Adv_Stats)
  stats_adv_stats_merge <- stats_adv_stats_list %>%
    reduce(full_join, by = "team") %>%
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
  Current_df_list <- list(stats_adv_stats_merge, recruit, SP_Rankings, FPI_df)
  VoA_Variables <- Current_df_list %>%
    reduce(full_join, by = "team") %>%
    mutate(FPI_SP_mean = (sp_rating + FPI) / 2)
} 
## end of if statement

## eliminating NAs that may still exist
# leaving this outside an if statement because this could be an issue regardless of season or CFB_Week
VoA_Variables[is.na(VoA_Variables)] = 0
VoA_Variables <- VoA_Variables %>%
  mutate(conference_temp = case_when(team == "Old Dominion" ~ "Sun Belt",
                                     team == "Marshall" ~ "Sun Belt",
                                     team == "Southern Mississippi" ~ "Sun Belt",
                                     team == "Connecticut" ~ "FBS Independents",
                                     TRUE ~ conference), .before = 3) %>%
  select(-conference)
colnames(VoA_Variables)[colnames(VoA_Variables) == "conference_temp"] <- "conference"

## Adding Rank Columns
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
### if week = 5
# PY1 weighted 1x, current weighted 2x
### if week >= 6
# current will be only data source used, everything weighted "1x" (aside from special variables)

## different stats weighted differently as described below
## EPA/PPA stats, explosiveness stats, success rates, havoc rates, Yards/Play, pts/scoring opp weighted 2x in PYs, 3x for current season,
# all #x above refer to weighting being done on top of weighting being done based on which year the data is from
## recruiting 3x in PY3 and PY2, 2x in PY1, 1x for current year
## talent ranked 2x in PY3 and PY2, 3x in PY1
if (as.numeric(week) == 0) {
  # PY3 weighted 1x, PY2 weighted 2x, PY1 weighted 3x
  ## PY3 ranks added first, weighted once
  VoA_Variables <- VoA_Variables %>%
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
           Rank_Talent_PY3_col2 = dense_rank(desc(talent_PY3)),
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
           Rank_Talent_PY2_col2 = dense_rank(desc(talent_PY2)),
           Rank_SP_Rating_PY2_col2 = dense_rank(desc(sp_rating_PY2)),
           Rank_SP_Off_Rating_PY2_col2 = dense_rank(desc(sp_offense_rating_PY2)),
           Rank_SP_Def_Rating_PY2_col2 = dense_rank(sp_defense_rating_PY2),
           Rank_SP_SpecialTeams_Rating_PY2_col2 = dense_rank(desc(sp_special_teams_rating_PY2)),
           Rank_FPI_PY2_col2 = dense_rank(desc(FPI_PY2)),
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
           Rank_SP_Rating_PY1_col2 = dense_rank(desc(sp_rating_PY1)),
           Rank_SP_Off_Rating_PY1_col2 = dense_rank(desc(sp_offense_rating_PY1)),
           Rank_SP_Def_Rating_PY1_col2 = dense_rank(sp_defense_rating_PY1),
           Rank_SP_SpecialTeams_Rating_PY1_col2 = dense_rank(desc(sp_special_teams_rating_PY1)),
           Rank_FPI_PY1_col2 = dense_rank(desc(FPI_PY1)),
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
           Rank_SP_Rating_PY1_col3 = dense_rank(desc(sp_rating_PY1)),
           Rank_SP_Off_Rating_PY1_col3 = dense_rank(desc(sp_offense_rating_PY1)),
           Rank_SP_Def_Rating_PY1_col3 = dense_rank(sp_defense_rating_PY1),
           Rank_SP_SpecialTeams_Rating_PY1_col3 = dense_rank(desc(sp_special_teams_rating_PY1)),
           Rank_FPI_PY1_col3 = dense_rank(desc(FPI_PY1)),
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
           Rank_FPI_SP_PY3_mean = dense_rank(desc(FPI_SP_PY3_mean)),
           Rank_FPI_SP_PY2_mean = dense_rank(desc(FPI_SP_PY2_mean)),
           Rank_FPI_SP_PY1_mean = dense_rank(desc(FPI_SP_PY1_mean)),
           Rank_AllPY_FPI_SP_mean = dense_rank(desc(AllPY_FPI_SP_mean)))
} else if (as.numeric(week) == 1) {
  # PY3 weighted 1x, PY2 weighted 2x, PY1 weighted 3x, current weighted 1x
  ## PY3 ranks added first, weighted once
  VoA_Variables <- VoA_Variables %>%
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
           Rank_Talent_PY3_col2 = dense_rank(desc(talent_PY3)),
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
           Rank_Talent_PY2_col2 = dense_rank(desc(talent_PY2)),
           Rank_SP_Rating_PY2_col2 = dense_rank(desc(sp_rating_PY2)),
           Rank_SP_Off_Rating_PY2_col2 = dense_rank(desc(sp_offense_rating_PY2)),
           Rank_SP_Def_Rating_PY2_col2 = dense_rank(sp_defense_rating_PY2),
           Rank_SP_SpecialTeams_Rating_PY2_col2 = dense_rank(desc(sp_special_teams_rating_PY2)),
           Rank_FPI_PY2_col2 = dense_rank(desc(FPI_PY2)),
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
           Rank_SP_Rating_PY1_col2 = dense_rank(desc(sp_rating_PY1)),
           Rank_SP_Off_Rating_PY1_col2 = dense_rank(desc(sp_offense_rating_PY1)),
           Rank_SP_Def_Rating_PY1_col2 = dense_rank(sp_defense_rating_PY1),
           Rank_SP_SpecialTeams_Rating_PY1_col2 = dense_rank(desc(sp_special_teams_rating_PY1)),
           Rank_FPI_PY1_col2 = dense_rank(desc(FPI_PY1)),
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
           Rank_SP_Rating_PY1_col3 = dense_rank(desc(sp_rating_PY1)),
           Rank_SP_Off_Rating_PY1_col3 = dense_rank(desc(sp_offense_rating_PY1)),
           Rank_SP_Def_Rating_PY1_col3 = dense_rank(sp_defense_rating_PY1),
           Rank_SP_SpecialTeams_Rating_PY1_col3 = dense_rank(desc(sp_special_teams_rating_PY1)),
           Rank_FPI_PY1_col3 = dense_rank(desc(FPI_PY1)),
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
           Rank_FPI_SP_PY3_mean = dense_rank(desc(FPI_SP_PY3_mean)),
           Rank_FPI_SP_PY2_mean = dense_rank(desc(FPI_SP_PY2_mean)),
           Rank_FPI_SP_PY1_mean = dense_rank(desc(FPI_SP_PY1_mean)),
           Rank_AllPY_FPI_SP_mean = dense_rank(desc(AllPY_FPI_SP_mean)),
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
  # PY2 weighted 2x, PY1 weighted 3x, current weighted 1x
  VoA_Variables <- VoA_Variables %>%
    mutate(## PY2 ranks
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
      Rank_Talent_PY2_col2 = dense_rank(desc(talent_PY2)),
      Rank_SP_Rating_PY2_col2 = dense_rank(desc(sp_rating_PY2)),
      Rank_SP_Off_Rating_PY2_col2 = dense_rank(desc(sp_offense_rating_PY2)),
      Rank_SP_Def_Rating_PY2_col2 = dense_rank(sp_defense_rating_PY2),
      Rank_SP_SpecialTeams_Rating_PY2_col2 = dense_rank(desc(sp_special_teams_rating_PY2)),
      Rank_FPI_PY2_col2 = dense_rank(desc(FPI_PY2)),
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
      Rank_SP_Rating_PY1_col2 = dense_rank(desc(sp_rating_PY1)),
      Rank_SP_Off_Rating_PY1_col2 = dense_rank(desc(sp_offense_rating_PY1)),
      Rank_SP_Def_Rating_PY1_col2 = dense_rank(sp_defense_rating_PY1),
      Rank_SP_SpecialTeams_Rating_PY1_col2 = dense_rank(desc(sp_special_teams_rating_PY1)),
      Rank_FPI_PY1_col2 = dense_rank(desc(FPI_PY1)),
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
      Rank_SP_Rating_PY1_col3 = dense_rank(desc(sp_rating_PY1)),
      Rank_SP_Off_Rating_PY1_col3 = dense_rank(desc(sp_offense_rating_PY1)),
      Rank_SP_Def_Rating_PY1_col3 = dense_rank(sp_defense_rating_PY1),
      Rank_SP_SpecialTeams_Rating_PY1_col3 = dense_rank(desc(sp_special_teams_rating_PY1)),
      Rank_FPI_PY1_col3 = dense_rank(desc(FPI_PY1)),
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
      Rank_FPI_SP_PY2_mean = dense_rank(desc(FPI_SP_PY2_mean)),
      Rank_FPI_SP_PY1_mean = dense_rank(desc(FPI_SP_PY1_mean)),
      Rank_AllPY_FPI_SP_mean = dense_rank(desc(AllPY_FPI_SP_mean)),
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
  # PY2 weighted 2x, PY1 weighted 2x, current weighted 1x
  VoA_Variables <- VoA_Variables %>%
    mutate(## PY2 ranks
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
      Rank_Talent_PY2_col2 = dense_rank(desc(talent_PY2)),
      Rank_SP_Rating_PY2_col2 = dense_rank(desc(sp_rating_PY2)),
      Rank_SP_Off_Rating_PY2_col2 = dense_rank(desc(sp_offense_rating_PY2)),
      Rank_SP_Def_Rating_PY2_col2 = dense_rank(sp_defense_rating_PY2),
      Rank_SP_SpecialTeams_Rating_PY2_col2 = dense_rank(desc(sp_special_teams_rating_PY2)),
      Rank_FPI_PY2_col2 = dense_rank(desc(FPI_PY2)),
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
      Rank_SP_Rating_PY1_col2 = dense_rank(desc(sp_rating_PY1)),
      Rank_SP_Off_Rating_PY1_col2 = dense_rank(desc(sp_offense_rating_PY1)),
      Rank_SP_Def_Rating_PY1_col2 = dense_rank(sp_defense_rating_PY1),
      Rank_SP_SpecialTeams_Rating_PY1_col2 = dense_rank(desc(sp_special_teams_rating_PY1)),
      Rank_FPI_PY1_col2 = dense_rank(desc(FPI_PY1)),
      ## incoming recruiting class, weighted once
      Rank_Recruit_Pts = dense_rank(desc(recruit_pts)),
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
      Rank_FPI_SP_PY2_mean = dense_rank(desc(FPI_SP_PY2_mean)),
      Rank_FPI_SP_PY1_mean = dense_rank(desc(FPI_SP_PY1_mean)),
      Rank_AllPY_FPI_SP_mean = dense_rank(desc(AllPY_FPI_SP_mean)),
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
  # PY2 weighted 1x, PY1 weighted 2x, current weighted 2x
  VoA_Variables <- VoA_Variables %>%
    mutate(## PY2 ranks
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
      Rank_SP_Rating_PY1_col2 = dense_rank(desc(sp_rating_PY1)),
      Rank_SP_Off_Rating_PY1_col2 = dense_rank(desc(sp_offense_rating_PY1)),
      Rank_SP_Def_Rating_PY1_col2 = dense_rank(sp_defense_rating_PY1),
      Rank_SP_SpecialTeams_Rating_PY1_col2 = dense_rank(desc(sp_special_teams_rating_PY1)),
      Rank_FPI_PY1_col2 = dense_rank(desc(FPI_PY1)),
      ## incoming recruiting class, weighted once
      Rank_Recruit_Pts = dense_rank(desc(recruit_pts)),
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
      Rank_FPI_SP_PY2_mean = dense_rank(desc(FPI_SP_PY2_mean)),
      Rank_FPI_SP_PY1_mean = dense_rank(desc(FPI_SP_PY1_mean)),
      Rank_AllPY_FPI_SP_mean = dense_rank(desc(AllPY_FPI_SP_mean)),
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
      Rank_SP_Rating_col2 = dense_rank(desc(sp_rating)),
      Rank_SP_Off_Rating_col2 = dense_rank(desc(sp_offense_rating)),
      Rank_SP_Def_Rating_col2 = dense_rank(sp_defense_rating),
      Rank_SP_SpecialTeams_Rating_col2 = dense_rank(desc(sp_special_teams_rating)),
      Rank_FPI_col2 = dense_rank(desc(FPI)),
      Rank_FPI_SP_mean_col2 = dense_rank(desc(FPI_SP_mean)),
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
} else if (as.numeric(week) == 5) {
  # PY1 weighted 1x, current weighted 2x
  VoA_Variables <- VoA_Variables %>%
    mutate(## PY1 ranks
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
      ## incoming recruiting class, weighted once
      Rank_Recruit_Pts = dense_rank(desc(recruit_pts)),
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
      ## FPI_SP mean ranks added at the end, weighted once
      Rank_FPI_SP_PY1_mean = dense_rank(desc(FPI_SP_PY1_mean)),
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
      Rank_SP_Rating_col2 = dense_rank(desc(sp_rating)),
      Rank_SP_Off_Rating_col2 = dense_rank(desc(sp_offense_rating)),
      Rank_SP_Def_Rating_col2 = dense_rank(sp_defense_rating),
      Rank_SP_SpecialTeams_Rating_col2 = dense_rank(desc(sp_special_teams_rating)),
      Rank_FPI_col2 = dense_rank(desc(FPI)),
      Rank_FPI_SP_mean_col2 = dense_rank(desc(FPI_SP_mean)),
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
  # current will be only data source used, everything weighted "1x" (aside from special variables, and recruiting)
  VoA_Variables <- VoA_Variables %>%
    mutate(## Ranking current stats
      Rank_Wins = dense_rank(desc(Wins)),
      Rank_Losses = dense_rank(Losses),
      ## incoming recruiting class, weighted once
      Rank_Recruit_Pts = dense_rank(desc(recruit_pts)),
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

## calculating the mean stat ranking, VoA_Output
## Rank variables start at 244 for Week 0
## it will be a different number for the other weeks (except maybe week 2?)
## script wouldn't run properly without a real number in there so I'll have to come back
# and edit the number in during the season as I figure out how big VoA_Variables gets
if (as.numeric(week) == 0) {
  ## correcting "season" column to reflect the season for which these rankings are being produced
  VoA_Variables$season = rep(as.numeric(year), nrow(VoA_Variables))
  ## Append new column of Model output, which is the mean of all variables in VoARanks
  VoA_Variables <- VoA_Variables %>%
    mutate(VoA_Output = (rowMeans(VoA_Variables[,244:ncol(VoA_Variables)])))
  ## Append column of VoA Final Rankings
  # VoA_Variables <- VoA_Variables %>%
  #   mutate(VoA_Ranking = dense_rank(VoA_Output))
} else if (as.numeric(week) == 1) {
  ## Append new column of Model output, which is the mean of all variables in VoARanks
  VoA_Variables <- VoA_Variables %>%
    mutate(VoA_Output = (rowMeans(VoA_Variables[,24:ncol(VoA_Variables)])))
  ## Append column of VoA Final Rankings
  # VoA_Variables <- VoA_Variables %>%
  #   mutate(VoA_Ranking = dense_rank(VoA_Output))
} else if (as.numeric(week) <= 4) {
  ## Append new column of Model output, which is the mean of all variables in VoARanks
  VoA_Variables <- VoA_Variables %>%
    mutate(VoA_Output = (rowMeans(VoA_Variables[,24:ncol(VoA_Variables)])))
  ## Append column of VoA Final Rankings
  # VoA_Variables <- VoA_Variables %>%
  #   mutate(VoA_Ranking = dense_rank(VoA_Output))
} else if (as.numeric(week) == 5) {
  ## Append new column of Model output, which is the mean of all variables in VoARanks
  VoA_Variables <- VoA_Variables %>%
    mutate(VoA_Output = (rowMeans(VoA_Variables[,24:ncol(VoA_Variables)])))
  ## Append column of VoA Final Rankings
  # VoA_Variables <- VoA_Variables %>%
  #   mutate(VoA_Ranking = dense_rank(VoA_Output))
} else {
  ## Append new column of Model output, which is the mean of all variables in VoARanks
  VoA_Variables <- VoA_Variables %>%
    mutate(VoA_Output = (rowMeans(VoA_Variables[,24:ncol(VoA_Variables)])))
  ## Append column of VoA Final Rankings
  # VoA_Variables <- VoA_Variables %>%
  #   mutate(VoA_Ranking = dense_rank(VoA_Output))
}
## End of if statement

## Adding Column with CFB Week number
# same number for each team, numeric version of number input in readline function at beginning of script
VoA_Variables <- VoA_Variables %>%
  mutate(CFB_Week = rep(as.numeric(week), nrow(VoA_Variables)), .before = 2)

# VoA_NAs_why <- VoA_Variables %>%
# filter(is.na(VoA_Output))

## Using Intial VoA Rankings to add in conference strength metric
Conference_Outputs <- VoA_Variables %>%
  group_by(conference) %>%
  summarize(Rk_mean = mean(VoA_Output), Rk_median = median(VoA_Output)) %>%
  mutate(Conf_Rk = dense_rank(Rk_mean))

VoA_Variables <- VoA_Variables %>%
  select(-VoA_Output) %>%
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
                                         conference == "Sun Belt" ~ Conference_Outputs$Rk_mean[Conference_Outputs$conference == "Sun Belt"],)) %>%
  mutate(Conference_Strength_col2 = Conference_Strength,
         Conference_Strength_col3 = Conference_Strength,
         Conference_Strength_col4 = Conference_Strength,
         Conference_Strength_col5 = Conference_Strength,
         Conference_Strength_col6 = Conference_Strength,
         Conference_Strength_col7 = Conference_Strength,
         Conference_Strength_col8 = Conference_Strength,
         Conference_Strength_col9 = Conference_Strength,
         Conference_Strength_col10 = Conference_Strength)

## Re running rowMeans function to get VoA Output
## Rank variables start at 244 for Week 0
## it will be a different number for the other weeks (except maybe week 2?)
## script wouldn't run properly without a real number in there so I'll have to come back
# and edit the number in during the season as I figure out how big VoA_Variables gets
if (as.numeric(week) == 0) {
  ## Append new column of Model output, which is the mean of all variables in VoARanks
  VoA_Variables <- VoA_Variables %>%
    mutate(VoA_Output = (rowMeans(VoA_Variables[,244:ncol(VoA_Variables)])))
  ## Append column of VoA Final Rankings
  # VoA_Variables <- VoA_Variables %>%
  #   mutate(VoA_Ranking = dense_rank(VoA_Output))
} else if (as.numeric(week) == 1) {
  ## Append new column of Model output, which is the mean of all variables in VoARanks
  VoA_Variables <- VoA_Variables %>%
    mutate(VoA_Output = (rowMeans(VoA_Variables[,24:ncol(VoA_Variables)])))
  ## Append column of VoA Final Rankings
  # VoA_Variables <- VoA_Variables %>%
  #   mutate(VoA_Ranking = dense_rank(VoA_Output))
} else if (as.numeric(week) <= 4) {
  ## Append new column of Model output, which is the mean of all variables in VoARanks
  VoA_Variables <- VoA_Variables %>%
    mutate(VoA_Output = (rowMeans(VoA_Variables[,24:ncol(VoA_Variables)])))
  ## Append column of VoA Final Rankings
  # VoA_Variables <- VoA_Variables %>%
  #   mutate(VoA_Ranking = dense_rank(VoA_Output))
} else if (as.numeric(week) == 5) {
  ## Append new column of Model output, which is the mean of all variables in VoARanks
  VoA_Variables <- VoA_Variables %>%
    mutate(VoA_Output = (rowMeans(VoA_Variables[,24:ncol(VoA_Variables)])))
  ## Append column of VoA Final Rankings
  # VoA_Variables <- VoA_Variables %>%
  #   mutate(VoA_Ranking = dense_rank(VoA_Output))
} else {
  ## Append new column of Model output, which is the mean of all variables in VoARanks
  VoA_Variables <- VoA_Variables %>%
    mutate(VoA_Output = (rowMeans(VoA_Variables[,24:ncol(VoA_Variables)])))
  ## Append column of VoA Final Rankings
  # VoA_Variables <- VoA_Variables %>%
  #   mutate(VoA_Ranking = dense_rank(VoA_Output))
}
## End of if statement

## using R's linear model function to create FPI/SP+ like metric
# includes PPA, success rate, explosiveness, VoA_Output, VoA's Conference_Strength, and pts_per_opp (offense and defense where applicable)
set.seed(386)
if (as.numeric(week) == 0) {
  test_model <- lm(AllPY_FPI_SP_mean ~ off_ppa_PY1 + off_ppa_PY2 + def_ppa_PY1 + def_ppa_PY2 + off_ppa_PY3 + def_ppa_PY3 + VoA_Output + Conference_Strength + off_ypp_PY3 + off_ypp_PY2 + off_ypp_PY1 + off_success_rate_PY3 + off_success_rate_PY2 + off_success_rate_PY1 + def_success_rate_PY3 + def_success_rate_PY2 + def_success_rate_PY1 + off_explosiveness_PY3 + off_explosiveness_PY2 + off_explosiveness_PY1 + def_explosiveness_PY3 + def_explosiveness_PY2 + def_explosiveness_PY1 + off_pts_per_opp_PY3 + off_pts_per_opp_PY2 + off_pts_per_opp_PY1 + def_pts_per_opp_PY3 + def_pts_per_opp_PY2 + def_pts_per_opp_PY1, data = VoA_Variables)
  ## summary(test_model)
  VoA_Variables <- VoA_Variables %>%
    mutate(VoA_Rating = predict(test_model),
           VoA_Ranking = dense_rank(desc(VoA_Rating))) 
} else if (as.numeric(week) == 1) {
  test_model <- lm(AllPY_FPI_SP_mean ~ off_ppa + off_ppa_PY1 + off_ppa_PY2 + def_ppa + def_ppa_PY1 + def_ppa_PY2 + off_ppa_PY3 + def_ppa_PY3 + VoA_Output + Conference_Strength + off_ypp_PY3 + off_ypp_PY2 + off_ypp_PY1 + off_ypp + off_success_rate_PY3 + off_success_rate + off_success_rate_PY2 + off_success_rate_PY1 + def_success_rate_PY3 + def_success_rate_PY2 + def_success_rate_PY1 + def_success_rate + off_explosiveness_PY3 + off_explosiveness_PY2 + off_explosiveness_PY1 + off_explosiveness + def_explosiveness_PY3 + def_explosiveness_PY2 + def_explosiveness_PY1 + def_explosiveness + off_pts_per_opp_PY3 + off_pts_per_opp_PY2 + off_pts_per_opp_PY1 + off_pts_per_opp + def_pts_per_opp_PY3 + def_pts_per_opp_PY2 + def_pts_per_opp_PY1 + def_pts_per_opp, data = VoA_Variables)
  ## summary(test_model)
  VoA_Variables <- VoA_Variables %>%
    mutate(VoA_Rating = predict(test_model),
           VoA_Ranking = dense_rank(desc(VoA_Rating)))
} else if (as.numeric(week) <= 4) {
  test_model <- lm(AllPY_FPI_SP_mean ~ off_ppa + off_ppa_PY1 + off_ppa_PY2 + def_ppa + def_ppa_PY1 + def_ppa_PY2 + VoA_Output + Conference_Strength + off_ypp_PY2 + off_ypp_PY1 + off_ypp + off_success_rate + off_success_rate_PY2 + off_success_rate_PY1 + def_success_rate_PY2 + def_success_rate_PY1 + def_success_rate + off_explosiveness_PY2 + off_explosiveness_PY1 + off_explosiveness + def_explosiveness_PY2 + def_explosiveness_PY1 + def_explosiveness + off_pts_per_opp_PY2 + off_pts_per_opp_PY1 + off_pts_per_opp + def_pts_per_opp_PY2 + def_pts_per_opp_PY1 + def_pts_per_opp, data = VoA_Variables)
  ## summary(test_model)
  VoA_Variables <- VoA_Variables %>%
    mutate(VoA_Rating = predict(test_model),
           VoA_Ranking = dense_rank(desc(VoA_Rating)))
} else if (as.numeric(week) == 5) {
  test_model <- lm(AllPY_FPI_SP_mean ~ off_ppa + off_ppa_PY1 + def_ppa + def_ppa_PY1 + VoA_Output + Conference_Strength + off_ypp_PY1 + off_ypp + off_success_rate + off_success_rate_PY1 + def_success_rate_PY1 + def_success_rate + off_explosiveness_PY1 + off_explosiveness + def_explosiveness_PY1 + def_explosiveness + off_pts_per_opp_PY1 + off_pts_per_opp + def_pts_per_opp_PY1 + def_pts_per_opp, data = VoA_Variables)
  ## summary(test_model)
  VoA_Variables <- VoA_Variables %>%
    mutate(VoA_Rating = predict(test_model),
           VoA_Ranking = dense_rank(desc(VoA_Rating)))
} else {
  test_model <- lm(AllPY_FPI_SP_mean ~ off_ppa + def_ppa + VoA_Output + Conference_Strength + off_ypp + off_success_rate + def_success_rate + off_explosiveness + def_explosiveness + off_pts_per_opp + def_pts_per_opp, data = VoA_Variables)
  ## summary(test_model)
  VoA_Variables <- VoA_Variables %>%
    mutate(VoA_Rating = predict(test_model),
           VoA_Ranking = dense_rank(desc(VoA_Rating)))
}


## creating data frame with just team, VoA ranking, and VoA output
## Create Vector of teamname, VoA Ranking
FinalTable <- VoA_Variables %>%
  select(team, conference, CFB_Week, VoA_Output, VoA_Ranking, VoA_Rating) %>%
  arrange(VoA_Ranking)
FinalVoATop25 <- FinalTable %>% filter(VoA_Ranking < 26)
## tail(FinalVoATop25)
## cols_hide() appears to be working
# 2 dfs created below originally created to avoid excess columns in gt tables
# Final_gt_table <- FinalTable %>%
#   select(team, VoA_Ranking, VoA_Rating)
# Final_gt_Top25 <- FinalVoATop25 %>%
#   select(team, VoA_Ranking, VoA_Rating)

if (as.numeric(week) == 0) {
  ## Top 25 Table
  # adding title and subtitle
  VoATop25Table <- FinalVoATop25 %>%
    gt() %>% # use 'gt' to make an awesome table...
    gt_theme_espn() %>%
    tab_header(
      title = paste(year, preseason_text, VoA_Top25_text), # ...with this title
      subtitle = "Supremely Excellent Yet Salaciously Godlike And Infallibly Magnificent Vortex of Accuracy")  %>%  # and this subtitle
    ## tab_style(style = cell_fill("bisque"),
    ##           locations = cells_body()) %>%  # add fill color to table
    fmt_number( # A column (numeric data)
      columns = c(VoA_Rating), # What column variable? FinalVoATop25$VoA_Rating
      decimals = 5 # With four decimal places
    ) %>% 
    fmt_number( # Another column (also numeric data)
      columns = c(VoA_Ranking), # What column variable? FinalVoATop25$VoA_Ranking
      decimals = 0 # I want this column to have zero decimal places
    ) %>%
    data_color( # Update cell colors, testing different color palettes
      columns = c(VoA_Rating), # ...for dose column
      colors = scales::col_numeric( # <- bc it's numeric
        palette = brewer.pal(9, "Reds"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) %>%
    cols_label(VoA_Rating = "VoA Rating", VoA_Ranking = "VoA Ranking") %>% # Update labels
    cols_move_to_end(columns = "VoA_Rating") %>%
    cols_hide(c(conference, CFB_Week, VoA_Output)) %>%
    tab_footnote(
      footnote = "Data from CFB Data API, ESPN.com, and ESPN's Bill Connelly via cfbfastR, James Madison data mostly from stats.ncaa.org"
    )
  
  ## Full 130 teams table
  # adding title and subtitle
  VoA_Full_Table <- FinalTable %>%
    gt() %>% # use 'gt' to make an awesome table...
    gt_theme_538() %>%
    tab_header(
      title = paste(year, preseason_text, VoA_text), # ...with this title
      subtitle = "Supremely Excellent Yet Salaciously Godlike And Infallibly Magnificent Vortex of Accuracy")  %>%  # and this subtitle
    ##tab_style(style = cell_fill("bisque"),
    ##        locations = cells_body()) %>%  # add fill color to table
    fmt_number( # A column (numeric data)
      columns = c(VoA_Rating), # What column variable? FinalVoATop25$VoA_Rating
      decimals = 5 # With four decimal places
    ) %>% 
    fmt_number( # Another column (also numeric data)
      columns = c(VoA_Ranking), # What column variable? FinalVoATop25$VoA_Ranking
      decimals = 0 # I want this column to have zero decimal places
    ) %>% 
    data_color( # Update cell colors, testing different color palettes
      columns = c(VoA_Rating), # ...for dose column
      colors = scales::col_numeric( # <- bc it's numeric
        palette = brewer.pal(9, "RdBu"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = TRUE
      )
    ) %>%
    cols_label(VoA_Rating = "VoA Rating", VoA_Ranking = "VoA Ranking") %>% # Update labels
    cols_move_to_end(columns = "VoA_Rating") %>%
    cols_hide(c(conference, CFB_Week, VoA_Output)) %>%
    tab_footnote(
      footnote = "Data from CFB Data API, ESPN.com, and ESPN's Bill Connelly via cfbfastR, James Madison data mostly from stats.ncaa.org"
    )
} else {
  ## Top 25 Table
  # adding title and subtitle
  VoATop25Table <- FinalVoATop25 %>%
    gt() %>% # use 'gt' to make an awesome table...
    gt_theme_espn() %>%
    tab_header(
      title = paste(year, week_text, week, VoA_Top25_text), # ...with this title
      subtitle = "Supremely Excellent Yet Salaciously Godlike And Infallibly Magnificent Vortex of Accuracy")  %>%  # and this subtitle
    ## tab_style(style = cell_fill("bisque"),
    ##           locations = cells_body()) %>%  # add fill color to table
    fmt_number( # A column (numeric data)
      columns = c(VoA_Rating), # What column variable? FinalVoATop25$VoA_Rating
      decimals = 5 # With four decimal places
    ) %>% 
    fmt_number( # Another column (also numeric data)
      columns = c(VoA_Ranking), # What column variable? FinalVoATop25$VoA_Ranking
      decimals = 0 # I want this column to have zero decimal places
    ) %>%
    data_color( # Update cell colors, testing different color palettes
      columns = c(VoA_Rating), # ...for dose column
      colors = scales::col_numeric( # <- bc it's numeric
        palette = brewer.pal(9, "Reds"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = TRUE
      )
    ) %>%
    cols_label(VoA_Rating = "VoA Rating", VoA_Ranking = "VoA Ranking") %>% # Update labels
    cols_move_to_end(columns = "VoA_Rating") %>%
    cols_hide(c(conference, CFB_Week, VoA_Output)) %>%
    tab_footnote(
      footnote = "Data from CFB Data API, ESPN.com, and ESPN's Bill Connelly via cfbfastR, James Madison data mostly from stats.ncaa.org"
    )
  
  ## Full 130 teams table
  # adding title and subtitle
  VoA_Full_Table <- FinalTable %>%
    gt() %>% # use 'gt' to make an awesome table...
    gt_theme_538() %>%
    tab_header(
      title = paste(year, week_text, week, VoA_text), # ...with this title
      subtitle = "Supremely Excellent Yet Salaciously Godlike And Infallibly Magnificent Vortex of Accuracy")  %>%  # and this subtitle
    ##tab_style(style = cell_fill("bisque"),
    ##        locations = cells_body()) %>%  # add fill color to table
    fmt_number( # A column (numeric data)
      columns = c(VoA_Rating), # What column variable? FinalVoATop25$VoA_Rating
      decimals = 5 # With four decimal places
    ) %>% 
    fmt_number( # Another column (also numeric data)
      columns = c(VoA_Ranking), # What column variable? FinalVoATop25$VoA_Ranking
      decimals = 0 # I want this column to have zero decimal places
    ) %>% 
    data_color( # Update cell colors, testing different color palettes
      columns = c(VoA_Rating), # ...for dose column
      colors = scales::col_numeric( # <- bc it's numeric
        palette = brewer.pal(9, "RdBu"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = TRUE
      )
    ) %>%
    cols_label(VoA_Rating = "VoA Rating", VoA_Ranking = "VoA Ranking") %>% # Update labels
    cols_move_to_end(columns = "VoA_Rating") %>%
    cols_hide(c(conference, CFB_Week, VoA_Output)) %>%
    tab_footnote(
      footnote = "Data from CFB Data API, ESPN.com, and ESPN's Bill Connelly via cfbfastR, James Madison data mostly from stats.ncaa.org"
    )
}

## viewing and saving the gt tables outside the if statement so that I can see them in the RStudio viewer
VoATop25Table
VoATop25Table %>%
  gtsave(
    top25_file_pathway, expand = 5,
    path = here("RVoA", "Outputs")
  )
VoA_Full_Table
VoA_Full_Table %>%
  gtsave(
    fulltable_file_pathway, expand = 5,
    path = here("RVoA", "Outputs")
  )

## Exporting final dataframe as csv
write_csv(VoA_Variables, file_pathway)

#### possible future code for trying out different formats for top 25 tables
## testing adding column colors
# VoATableColors <- Final_gt_Top25 %>%
#   gt() %>% # Make a gt table with it
#   gt_theme_538() %>%
#   ## gt_color_rows(VoA_Output, palette = "ggsci::blue_material") %>%
#   tab_header(
#     title = paste(year, week_text, week, VoA_Top25_text), # Add a title
#     subtitle = "it's a brand new upgraded version of a table! Supremely Excellent Yet Salaciously Godlike And Infallibly Magnificent Vortex of Accuracy" # And a subtitle
#   ) %>%
#   fmt_passthrough( # Not sure about this but it works...
#     columns = c(Team) # First column: team (character)
#   ) %>%
#   fmt_number(
#     columns = c(VoA_Output), # Second column: VoA_Output (numeric)
#     decimals = 5 # With 5 decimal places
#   ) %>%
#   fmt_number(
#     columns = c(VoA_Ranking), # Third column: VoA_Ranking (numeric)
#     decimals = 0 # With 0 decimal places
#   ) %>%
#   #  data_color( # Update cell colors...
#   #    columns = c(VoA_Output), # ...for dose column
#   #    colors = scales::col_numeric( # <- bc it's numeric
#   #      palette = c(
#   #        "dodgerblue4","cadetblue1"), # A color scheme (gradient)
#   #      domain = c() # Column scale endpoints
#   #    )
#   # ) %>%
#   data_color( # Update cell colors, testing different color palettes
#     columns = c(VoA_Output), # ...for VoA_Output column
#     colors = scales::col_numeric( # <- bc it's numeric
#       palette = brewer.pal(9, "Reds"), # A color scheme (gradient)
#       domain = c() # Column scale endpoints
#     )
#   ) %>%
#   cols_label(team= "Team", VoA_Output = "Final VoA Output", VoA_Ranking = "VoA Ranking") %>% # Make the column headers
#   tab_footnote(
#     footnote = "rounded to 5 decimals", # Another line of footnote text
#     locations = cells_column_labels(
#       columns = c(VoA_Output) # Associated with column 'VoA_Output'
#     )
#   ) %>%
#   cols_move_to_end(columns = "VoA_Output")
# VoATableColors
# 
# ## Save GT table with colors in columns
# VoATableColors %>%
#   gtsave(
#    "VoAGTwithColors.png", expand = 5,
#    path = here("RVoA", "Outputs")
#  )

## Making the Unintelligible Chart
## Tracks VoA Ratings and Rankings by week
## now reading in and merging VoA rating and ranking data up to current week
if (as.numeric(week) == 2) {
  Week0_VoA <- read_csv(here("Data", "VoA2022", "2022Week0_VoA.csv")) %>%
    select(team, conference, CFB_Week, VoA_Output, VoA_Ranking, VoA_Rating)
  Week1_VoA <- read_csv(here("Data", "VoA2022", "2022Week1_VoA.csv")) %>%
    select(team, conference, CFB_Week, VoA_Output, VoA_Ranking, VoA_Rating)
  Full_Ratings_Rks <- rbind(Week0_VoA, Week1_VoA)
  Full_Ratings_Rks <- rbind(Full_Ratings_Rks, FinalTable)
  write_csv(Full_Ratings_Rks, paste(data_dir, "TrackingChartCSVs", "/", year, week_text, "0_2Ratings_Rks.csv", sep = ""))
} else if (as.numeric(week) == 3) {
  Full_Ratings_Rks <- read_csv(here("Data", "VoA2022", "TrackingChartCSVs", paste(year, week_text, "0_2Ratings_Rks.csv", sep = ""))) %>%
    select(team, conference, CFB_Week, VoA_Output, VoA_Ranking, VoA_Rating)
  Full_Ratings_Rks <- rbind(Full_Ratings_Rks, FinalTable)
  write_csv(Full_Ratings_Rks, paste(data_dir, "TrackingChartCSVs", "/", year, week_text, "0_3Ratings_Rks.csv", sep = ""))
} else if (as.numeric(week) == 4) {
  Full_Ratings_Rks <- read_csv(here("Data", "VoA2022", "TrackingChartCSVs", paste(year, week_text, "0_3Ratings_Rks.csv", sep = ""))) %>%
    select(team, conference, CFB_Week, VoA_Output, VoA_Ranking, VoA_Rating)
  Full_Ratings_Rks <- rbind(Full_Ratings_Rks, FinalTable)
  write_csv(Full_Ratings_Rks, paste(data_dir, "TrackingChartCSVs", "/", year, week_text, "0_4Ratings_Rks.csv", sep = ""))
} else if (as.numeric(week) == 5) {
  Full_Ratings_Rks <- read_csv(here("Data", "VoA2022", "TrackingChartCSVs", paste(year, week_text, "0_4Ratings_Rks.csv", sep = ""))) %>%
    select(team, conference, CFB_Week, VoA_Output, VoA_Ranking, VoA_Rating)
  Full_Ratings_Rks <- rbind(Full_Ratings_Rks, FinalTable)
  write_csv(Full_Ratings_Rks, paste(data_dir, "TrackingChartCSVs", "/", year, week_text, "0_5Ratings_Rks.csv", sep = ""))
} else if (as.numeric(week) == 6) {
  Full_Ratings_Rks <- read_csv(here("Data", "VoA2022", "TrackingChartCSVs", paste(year, week_text, "0_5Ratings_Rks.csv", sep = ""))) %>%
    select(team, conference, CFB_Week, VoA_Output, VoA_Ranking, VoA_Rating)
  Full_Ratings_Rks <- rbind(Full_Ratings_Rks, FinalTable)
  write_csv(Full_Ratings_Rks, paste(data_dir, "TrackingChartCSVs", "/", year, week_text, "0_6Ratings_Rks.csv", sep = ""))
} else if (as.numeric(week) == 7) {
  Full_Ratings_Rks <- read_csv(here("Data", "VoA2022", "TrackingChartCSVs", paste(year, week_text, "0_6Ratings_Rks.csv", sep = ""))) %>%
    select(team, conference, CFB_Week, VoA_Output, VoA_Ranking, VoA_Rating)
  Full_Ratings_Rks <- rbind(Full_Ratings_Rks, FinalTable)
  write_csv(Full_Ratings_Rks, paste(data_dir, "TrackingChartCSVs", "/", year, week_text, "0_7Ratings_Rks.csv", sep = ""))
} else if (as.numeric(week) == 8) {
  Full_Ratings_Rks <- read_csv(here("Data", "VoA2022", "TrackingChartCSVs", paste(year, week_text, "0_7Ratings_Rks.csv", sep = ""))) %>%
    select(team, conference, CFB_Week, VoA_Output, VoA_Ranking, VoA_Rating)
  Full_Ratings_Rks <- rbind(Full_Ratings_Rks, FinalTable)
  write_csv(Full_Ratings_Rks, paste(data_dir, "TrackingChartCSVs", "/", year, week_text, "0_8Ratings_Rks.csv", sep = ""))
} else if (as.numeric(week) == 9) {
  Full_Ratings_Rks <- read_csv(here("Data", "VoA2022", "TrackingChartCSVs", paste(year, week_text, "0_8Ratings_Rks.csv", sep = ""))) %>%
    select(team, conference, CFB_Week, VoA_Output, VoA_Ranking, VoA_Rating)
  Full_Ratings_Rks <- rbind(Full_Ratings_Rks, FinalTable)
  write_csv(Full_Ratings_Rks, paste(data_dir, "TrackingChartCSVs", "/", year, week_text, "0_9Ratings_Rks.csv", sep = ""))
} else if (as.numeric(week) == 10) {
  Full_Ratings_Rks <- read_csv(here("Data", "VoA2022", "TrackingChartCSVs", paste(year, week_text, "0_9Ratings_Rks.csv", sep = ""))) %>%
    select(team, conference, CFB_Week, VoA_Output, VoA_Ranking, VoA_Rating)
  Full_Ratings_Rks <- rbind(Full_Ratings_Rks, FinalTable)
  write_csv(Full_Ratings_Rks, paste(data_dir, "TrackingChartCSVs", "/", year, week_text, "0_10Ratings_Rks.csv", sep = ""))
} else if (as.numeric(week) == 11) {
  Full_Ratings_Rks <- read_csv(here("Data", "VoA2022", "TrackingChartCSVs", paste(year, week_text, "0_10Ratings_Rks.csv", sep = ""))) %>%
    select(team, conference, CFB_Week, VoA_Output, VoA_Ranking, VoA_Rating)
  Full_Ratings_Rks <- rbind(Full_Ratings_Rks, FinalTable)
  write_csv(Full_Ratings_Rks, paste(data_dir, "TrackingChartCSVs", "/", year, week_text, "0_11Ratings_Rks.csv", sep = ""))
} else if (as.numeric(week) == 12) {
  Full_Ratings_Rks <- read_csv(here("Data", "VoA2022", "TrackingChartCSVs", paste(year, week_text, "0_11Ratings_Rks.csv", sep = ""))) %>%
    select(team, conference, CFB_Week, VoA_Output, VoA_Ranking, VoA_Rating)
  Full_Ratings_Rks <- rbind(Full_Ratings_Rks, FinalTable)
  write_csv(Full_Ratings_Rks, paste(data_dir, "TrackingChartCSVs", "/", year, week_text, "0_12Ratings_Rks.csv", sep = ""))
} else if (as.numeric(week) == 13) {
  Full_Ratings_Rks <- read_csv(here("Data", "VoA2022", "TrackingChartCSVs", paste(year, week_text, "0_12Ratings_Rks.csv", sep = ""))) %>%
    select(team, conference, CFB_Week, VoA_Output, VoA_Ranking, VoA_Rating)
  Full_Ratings_Rks <- rbind(Full_Ratings_Rks, FinalTable)
  write_csv(Full_Ratings_Rks, paste(data_dir, "TrackingChartCSVs", "/", year, week_text, "0_13Ratings_Rks.csv", sep = ""))
} else if (as.numeric(week) == 14) {
  Full_Ratings_Rks <- read_csv(here("Data", "VoA2022", "TrackingChartCSVs", paste(year, week_text, "0_13Ratings_Rks.csv", sep = ""))) %>%
    select(team, conference, CFB_Week, VoA_Output, VoA_Ranking, VoA_Rating)
  Full_Ratings_Rks <- rbind(Full_Ratings_Rks, FinalTable)
  write_csv(Full_Ratings_Rks, paste(data_dir, "TrackingChartCSVs", "/", year, week_text, "0_14Ratings_Rks.csv", sep = ""))
} else if (as.numeric(week) == 15) {
  Full_Ratings_Rks <- read_csv(here("Data", "VoA2022", "TrackingChartCSVs", paste(year, week_text, "0_14Ratings_Rks.csv", sep = ""))) %>%
    select(team, conference, CFB_Week, VoA_Output, VoA_Ranking, VoA_Rating)
  Full_Ratings_Rks <- rbind(Full_Ratings_Rks, FinalTable)
  ## no need to write out a new csv since week 15 is the last week of the season
  ## write_csv(Full_Ratings_Rks, paste(data_dir, "TrackingChartCSVs", "/", year, week_text, "0_15Ratings_Rks.csv", sep = ""))
} else {
  print("No charts until Week 2!")
}
## end of if statement

## Filtering by conference for unintelligible charts
if (as.numeric(week) >= 2) {
  ## Subsetting by team, each conference (including independents) gets separate charts
  AAC_Ratings_Rks <- Full_Ratings_Rks %>% filter(conference == "American Athletic")
  ACC_Ratings_Rks <- Full_Ratings_Rks %>% filter(conference == "ACC")
  Big12_Ratings_Rks <- Full_Ratings_Rks %>% filter(conference == "Big 12")
  Big10_Ratings_Rks <- Full_Ratings_Rks %>% filter(conference == "Big Ten")
  CUSA_Ratings_Rks <- Full_Ratings_Rks %>% filter(conference == "Conference USA")
  Indy_Ratings_Rks <- Full_Ratings_Rks %>% filter(conference == "FBS Independents")
  MAC_Ratings_Rks <- Full_Ratings_Rks %>% filter(conference == "Mid-American")
  MWC_Ratings_Rks <- Full_Ratings_Rks %>% filter(conference == "Mountain West")
  Pac12_Ratings_Rks <- Full_Ratings_Rks %>% filter(conference == "Pac 12")
  SEC_Ratings_Rks <- Full_Ratings_Rks %>% filter(conference == "SEC")
  SunBelt_Ratings_Rks <- Full_Ratings_Rks %>% filter(conference == "Sun Belt")
  
  ## Creating Charts
  # charting VoA_Rating and VoA_Ranking for each week from week 2 on
  AAC_VoA_Rating_Chart <- ggplot(AAC_Ratings_Rks, aes(x = CFB_Week, y = VoA_Rating, colour = team, group = Team)) +
    geom_line(size = 1.5) +
    geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA_Rating") +
    ggtitle("American Conference Vortex of Accuracy Ratings by Week") +
    expand_limits(y = c(-40,40)) +
    scale_y_continuous(breaks = c(-40,-35,-30,-25,-20,-15,-10,-5,0,5,10,15,20,25,30,35,40)) +
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)) +
    theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
  AAC_VoA_Rating_Chart
  ggsave(AAC_Output_filename, path = output_dir, width = 50, height = 40, units = 'cm')
  
  AAC_VoA_Ranking_Chart <- ggplot(AAC_Ratings_Rks, aes(x = CFB_Week, y = VoA_Ranking, colour = team, group = Team)) +
    geom_line(size = 1.5) +
    geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA_Ranking") +
    ggtitle("American Conference Vortex of Accuracy Rankings by Week") +
    expand_limits(y = c(0,130)) +
    scale_y_continuous(breaks = c(0,20,40,60,80,100,130)) +
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)) +
    theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
  AAC_VoA_Ranking_Chart
  ggsave(AAC_Ranking_filename, path = output_dir, width = 50, height = 40, units = 'cm')
  
  ACC_VoA_Rating_Chart <- ggplot(ACC_Ratings_Rks, aes(x = CFB_Week, y = VoA_Rating, colour = team, group = Team)) +
    geom_line(size = 1.5) +
    geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA_Rating") +
    ggtitle("ACC Vortex of Accuracy Ratings by Week") +
    expand_limits(y = c(-40,40)) +
    scale_y_continuous(breaks = c(-40,-35,-30,-25,-20,-15,-10,-5,0,5,10,15,20,25,30,35,40)) +
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)) +
    theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
  ACC_VoA_Rating_Chart
  ggsave(ACC_Output_filename, path = output_dir, width = 50, height = 40, units = 'cm')
  
  ACC_VoA_Ranking_Chart <- ggplot(ACC_Ratings_Rks, aes(x = CFB_Week, y = VoA_Ranking, colour = team, group = Team)) +
    geom_line(size = 1.5) +
    geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA_Ranking") +
    ggtitle("ACC Vortex of Accuracy Rankings by Week") +
    expand_limits(y = c(0,130)) +
    scale_y_continuous(breaks = c(0,20,40,60,80,100,130)) +
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)) +
    theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
  ACC_VoA_Ranking_Chart
  ggsave(ACC_Ranking_filename, path = output_dir, width = 50, height = 40, units = 'cm')
  
  Big12_VoA_Rating_Chart <- ggplot(Big12_Ratings_Rks, aes(x = CFB_Week, y = VoA_Rating, colour = team, group = Team)) +
    geom_line(size = 1.5) +
    geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA_Rating") +
    ggtitle("Big 12 Vortex of Accuracy Ratings by Week") +
    expand_limits(y = c(-40,40)) +
    scale_y_continuous(breaks = c(-40,-35,-30,-25,-20,-15,-10,-5,0,5,10,15,20,25,30,35,40)) +
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)) +
    theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
  Big12_VoA_Rating_Chart
  ggsave(Big12_Output_filename, path = output_dir, width = 50, height = 40, units = 'cm')
  
  Big12_VoA_Ranking_Chart <- ggplot(Big12_Ratings_Rks, aes(x = CFB_Week, y = VoA_Ranking, colour = team, group = Team)) +
    geom_line(size = 1.5) +
    geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA_Ranking") +
    ggtitle("Big 12 Vortex of Accuracy Rankings by Week") +
    expand_limits(y = c(0,130)) +
    scale_y_continuous(breaks = c(0,20,40,60,80,100,130)) +
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)) +
    theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
  Big12_VoA_Ranking_Chart
  ggsave(Big12_Ranking_filename, path = output_dir, width = 50, height = 40, units = 'cm')
  
  Big10_VoA_Rating_Chart <- ggplot(Big10_Ratings_Rks, aes(x = CFB_Week, y = VoA_Rating, colour = team, group = Team)) +
    geom_line(size = 1.5) +
    geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA_Rating") +
    ggtitle("Big 10 Vortex of Accuracy Ratings by Week") +
    expand_limits(y = c(-40,40)) +
    scale_y_continuous(breaks = c(-40,-35,-30,-25,-20,-15,-10,-5,0,5,10,15,20,25,30,35,40)) +
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)) +
    theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
  Big10_VoA_Rating_Chart
  ggsave(Big10_Output_filename, path = output_dir, width = 50, height = 40, units = 'cm')
  
  Big10_VoA_Ranking_Chart <- ggplot(Big10_Ratings_Rks, aes(x = CFB_Week, y = VoA_Ranking, colour = team, group = Team)) +
    geom_line(size = 1.5) +
    geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA_Rating") +
    ggtitle("Big 10 Vortex of Accuracy Rankings by Week") +
    expand_limits(y = c(0,130)) +
    scale_y_continuous(breaks = c(0,20,40,60,80,100,130)) +
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)) +
    theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
  Big10_VoA_Ranking_Chart
  ggsave(Big10_Ranking_filename, path = output_dir, width = 50, height = 40, units = 'cm')
  
  CUSA_VoA_Rating_Chart <- ggplot(CUSA_Ratings_Rks, aes(x = CFB_Week, y = VoA_Rating, colour = team, group = Team)) +
    geom_line(size = 1.5) +
    geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA_Rating") +
    ggtitle("CUSA Vortex of Accuracy Ratings by Week") +
    expand_limits(y = c(-40,40)) +
    scale_y_continuous(breaks = c(-40,-35,-30,-25,-20,-15,-10,-5,0,5,10,15,20,25,30,35,40)) +
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)) +
    theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
  CUSA_VoA_Rating_Chart
  ggsave(CUSA_Output_filename, path = output_dir, width = 50, height = 40, units = 'cm')
  
  CUSA_VoA_Ranking_Chart <- ggplot(CUSA_Ratings_Rks, aes(x = CFB_Week, y = VoA_Ranking, colour = team, group = Team)) +
    geom_line(size = 1.5) +
    geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA_Ranking") +
    ggtitle("CUSA Vortex of Accuracy Rankings by Week") +
    expand_limits(y = c(0,130)) +
    scale_y_continuous(breaks = c(0,20,40,60,80,100,130)) +
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)) +
    theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
  CUSA_VoA_Ranking_Chart
  ggsave(CUSA_Ranking_filename, path = output_dir, width = 50, height = 40, units = 'cm')
  
  Indy_VoA_Rating_Chart <- ggplot(Indy_Ratings_Rks, aes(x = CFB_Week, y = VoA_Rating, colour = team, group = Team)) +
    geom_line(size = 1.5) +
    geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA_Rating") +
    ggtitle("Independents Vortex of Accuracy Ratings by Week") +
    expand_limits(y = c(-40,40)) +
    scale_y_continuous(breaks = c(-40,-35,-30,-25,-20,-15,-10,-5,0,5,10,15,20,25,30,35,40)) +
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)) +
    theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
  Indy_VoA_Rating_Chart
  ggsave(Indy_Output_filename, path = output_dir, width = 50, height = 40, units = 'cm')
  
  Indy_VoA_Ranking_Chart <- ggplot(Indy_Ratings_Rks, aes(x = CFB_Week, y = VoA_Ranking, colour = team, group = Team)) +
    geom_line(size = 1.5) +
    geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA_Ranking") +
    ggtitle("Independents Vortex of Accuracy Rankings by Week") +
    expand_limits(y = c(0,130)) +
    scale_y_continuous(breaks = c(0,20,40,60,80,100,130)) +
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)) +
    theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
  Indy_VoA_Ranking_Chart
  ggsave(Indy_Ranking_filename, path = output_dir, width = 50, height = 40, units = 'cm')
  
  MAC_VoA_Rating_Chart <- ggplot(MAC_Ratings_Rks, aes(x = CFB_Week, y = VoA_Rating, colour = team, group = Team)) +
    geom_line(size = 1.5) +
    geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA_Rating") +
    ggtitle("MAC Vortex of Accuracy Ratings by Week") +
    expand_limits(y = c(-40,40)) +
    scale_y_continuous(breaks = c(-40,-35,-30,-25,-20,-15,-10,-5,0,5,10,15,20,25,30,35,40)) +
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)) +
    theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
  MAC_VoA_Rating_Chart
  ggsave(MAC_Output_filename, path = output_dir, width = 50, height = 40, units = 'cm')
  
  MAC_VoA_Ranking_Chart <- ggplot(MAC_Ratings_Rks, aes(x = CFB_Week, y = VoA_Ranking, colour = team, group = Team)) +
    geom_line(size = 1.5) +
    geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA_Ranking") +
    ggtitle("MAC Vortex of Accuracy Rankings by Week") +
    expand_limits(y = c(0,130)) +
    scale_y_continuous(breaks = c(0,20,40,60,80,100,130)) +
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)) +
    theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
  MAC_VoA_Ranking_Chart
  ggsave(MAC_Ranking_filename, path = output_dir, width = 50, height = 40, units = 'cm')
  
  MWC_VoA_Rating_Chart <- ggplot(MWC_Ratings_Rks, aes(x = CFB_Week, y = VoA_Rating, colour = team, group = Team)) +
    geom_line(size = 1.5) +
    geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA_Rating") +
    ggtitle("Mountain West Vortex of Accuracy Ratings by Week") +
    expand_limits(y = c(-40,40)) +
    scale_y_continuous(breaks = c(-40,-35,-30,-25,-20,-15,-10,-5,0,5,10,15,20,25,30,35,40)) +
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)) +
    theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
  MWC_VoA_Rating_Chart
  ggsave(MWC_Output_filename, path = output_dir, width = 50, height = 40, units = 'cm')
  
  MWC_VoA_Ranking_Chart <- ggplot(MWC_Ratings_Rks, aes(x = CFB_Week, y = VoA_Ranking, colour = team, group = Team)) +
    geom_line(size = 1.5) +
    geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA_Ranking") +
    ggtitle("Mountain West Vortex of Accuracy Rankings by Week") +
    expand_limits(y = c(0,130)) +
    scale_y_continuous(breaks = c(0,20,40,60,80,100,130)) +
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)) +
    theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
  MWC_VoA_Ranking_Chart
  ggsave(MWC_Ranking_filename, path = output_dir, width = 50, height = 40, units = 'cm')
  
  Pac12_VoA_Rating_Chart <- ggplot(Pac12_Ratings_Rks, aes(x = CFB_Week, y = VoA_Rating, colour = team, group = Team)) +
    geom_line(size = 1.5) +
    geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA_Rating") +
    ggtitle("Pac 12 Vortex of Accuracy Ratings by Week") +
    expand_limits(y = c(-40,40)) +
    scale_y_continuous(breaks = c(-40,-35,-30,-25,-20,-15,-10,-5,0,5,10,15,20,25,30,35,40)) +
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)) +
    theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
  Pac12_VoA_Rating_Chart
  ggsave(Pac12_Output_filename, path = output_dir, width = 50, height = 40, units = 'cm')
  
  Pac12_VoA_Ranking_Chart <- ggplot(Pac12_Ratings_Rks, aes(x = CFB_Week, y = VoA_Ranking, colour = team, group = Team)) +
    geom_line(size = 1.5) +
    geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA_Ranking") +
    ggtitle("Pac 12 Vortex of Accuracy Rankings by Week") +
    expand_limits(y = c(0,130)) +
    scale_y_continuous(breaks = c(0,20,40,60,80,100,130)) +
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)) +
    theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
  Pac12_VoA_Ranking_Chart
  ggsave(Pac12_Ranking_filename, path = output_dir, width = 50, height = 40, units = 'cm')
  
  SEC_VoA_Rating_Chart <- ggplot(SEC_Ratings_Rks, aes(x = CFB_Week, y = VoA_Rating, colour = team, group = Team)) +
    geom_line(size = 1.5) +
    geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA_Rating") +
    ggtitle("SEC Vortex of Accuracy Ratings by Week") +
    expand_limits(y = c(-40,40)) +
    scale_y_continuous(breaks = c(-40,-35,-30,-25,-20,-15,-10,-5,0,5,10,15,20,25,30,35,40)) +
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)) +
    theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
  SEC_VoA_Rating_Chart
  ggsave(SEC_Output_filename, path = output_dir, width = 50, height = 40, units = 'cm')
  
  SEC_VoA_Ranking_Chart <- ggplot(SEC_Ratings_Rks, aes(x = CFB_Week, y = VoA_Ranking, colour = team, group = Team)) +
    geom_line(size = 1.5) +
    geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA_Ranking") +
    ggtitle("SEC Vortex of Accuracy Rankings by Week") +
    expand_limits(y = c(0,130)) +
    scale_y_continuous(breaks = c(0,20,40,60,80,100,130)) +
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)) +
    theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
  SEC_VoA_Ranking_Chart
  ggsave(SEC_Ranking_filename, path = output_dir, width = 50, height = 40, units = 'cm')
  
  SunBelt_VoA_Rating_Chart <- ggplot(SunBelt_Ratings_Rks, aes(x = CFB_Week, y = VoA_Rating, colour = team, group = Team)) +
    geom_line(size = 1.5) +
    geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA_Rating") +
    ggtitle("Sun Belt Vortex of Accuracy Ratings by Week") +
    expand_limits(y = c(-40,40)) +
    scale_y_continuous(breaks = c(-40,-35,-30,-25,-20,-15,-10,-5,0,5,10,15,20,25,30,35,40)) +
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)) +
    theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
  SunBelt_VoA_Rating_Chart
  ggsave(SunBelt_Output_filename, path = output_dir, width = 50, height = 40, units = 'cm')
  
  SunBelt_VoA_Ranking_Chart <- ggplot(SunBelt_Ratings_Rks, aes(x = CFB_Week, y = VoA_Ranking, colour = team, group = Team)) +
    geom_line(size = 1.5) +
    geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA_Rating") +
    ggtitle("Sun Belt Vortex of Accuracy Rankings by Week") +
    expand_limits(y = c(0,130)) +
    scale_y_continuous(breaks = c(0,20,40,60,80,100,130)) +
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)) +
    theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
  SunBelt_VoA_Ranking_Chart
  ggsave(SunBelt_Ranking_filename, path = output_dir, width = 50, height = 40, units = 'cm')
} else {
  print("No charts until Week 2!")
}

## Creating Histograms of VoA Output for all teams, and separate plots for power 5 and group of 5 teams subsetted out
# plots will be made for each week, not just after week 2 like Unintelligble Charts will
## subsetting teams
Power5_VoA <- VoA_Variables %>%
  filter(conference == "ACC" | conference == "Big 12" | conference == "Big Ten" | conference == "FBS Independents" | conference == "Pac-12" | conference == "SEC") %>%
  filter(team != "Connecticut" & team != "New Mexico State" & team != "BYU" & team != "Army" & team != "Liberty" & team != "UMass")

Group5_VoA <- VoA_Variables %>%
  filter(conference == "American Athletic" | conference == "Conference USA" | conference == "FBS Independents" | conference == "Mid-American" | conference == "Mountain West" | conference == "Sun Belt") %>%
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
  theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
VoA_Output_Rating_plot
ggsave(Output_Rating_Plot_filename, path = output_dir, width = 50, height = 40, units = 'cm')


## End of Script
end_time <- Sys.time()
end_time - start_time

