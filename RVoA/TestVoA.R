##### script includes assortment of testing options for data collection, analysis, visualization, etc
## test code for accessing cfb data API
library(pacman)
pacman::p_load(tidyverse, matrixStats, grid, gridExtra, gt, viridis, 
               webshot, writexl, rvest, cfbfastR, espnscrapeR, openxlsx, 
               here, ggsci, RColorBrewer, ggpubr, gtExtras, tidymodels)

## testing readline function with cfbfastR pkg
week <- readline(prompt = "What week is it? ")
year <- readline(prompt = "What year is it? ")

if (as.numeric(week) == 0) {
  ## reading in data for 3 previous years
  JMU_AllYears <- read_csv(here("Data", "VoA2022", "JamesMadisonPrevYears", "JMU_AllYears.csv"))
  Stats_PY1 <- cfbd_stats_season_team(year = as.integer(year) - 1, start_week = 1, end_week = 15) %>%
    mutate(total_yds_pg = total_yds/games,
           pass_yds_pg = net_pass_yds/games,
           rush_yds_pg = rush_yds/games,
           first_downs_pg = first_downs/games,
           def_interceptions_pg = passes_intercepted/games,
           pass_ypa = net_pass_yds / pass_atts,
           off_ypp = total_yds / (rush_atts + pass_atts),
           completion_pct = pass_comps / rush_atts,
           pass_ypr = net_pass_yds / pass_comps,
           int_pct = interceptions / pass_atts,
           rush_ypc = rush_yds / rush_atts,
           turnovers_pg = turnovers / games,
           third_conv_rate = third_down_convs / third_downs,
           fourth_conv_rate = fourth_down_convs / fourth_downs,
           penalty_yds_pg / penalty_yds / games,
           yards_per_penalty = penalty_yds / penalties,
           kick_return_avg = kick_return_yds / kick_returns,
           punt_return_avg = punt_return_yds / punt_returns)
  Stats_PY2 <- cfbd_stats_season_team(year = as.integer(year) - 2, start_week = 1, end_week = 15) %>%
    mutate(total_yds_pg = total_yds/games,
           pass_yds_pg = net_pass_yds/games,
           rush_yds_pg = rush_yds/games,
           first_downs_pg = first_downs/games,
           def_interceptions_pg = passes_intercepted/games,
           pass_ypa = net_pass_yds / pass_atts,
           off_ypp = total_yds / (rush_atts + pass_atts),
           completion_pct = pass_comps / rush_atts,
           pass_ypr = net_pass_yds / pass_comps,
           int_pct = interceptions / pass_atts,
           rush_ypc = rush_yds / rush_atts,
           turnovers_pg = turnovers / games,
           third_conv_rate = third_down_convs / third_downs,
           fourth_conv_rate = fourth_down_convs / fourth_downs,
           penalty_yds_pg / penalty_yds / games,
           yards_per_penalty = penalty_yds / penalties,
           kick_return_avg = kick_return_yds / kick_returns,
           punt_return_avg = punt_return_yds / punt_returns)
  Stats_PY3 <- cfbd_stats_season_team(year = as.integer(year) - 3, start_week = 1, end_week = 15) %>%
    mutate(total_yds_pg = total_yds/games,
           pass_yds_pg = net_pass_yds/games,
           rush_yds_pg = rush_yds/games,
           first_downs_pg = first_downs/games,
           def_interceptions_pg = passes_intercepted/games,
           pass_ypa = net_pass_yds / pass_atts,
           off_ypp = total_yds / (rush_atts + pass_atts),
           completion_pct = pass_comps / rush_atts,
           pass_ypr = net_pass_yds / pass_comps,
           int_pct = interceptions / pass_atts,
           rush_ypc = rush_yds / rush_atts,
           turnovers_pg = turnovers / games,
           third_conv_rate = third_down_convs / third_downs,
           fourth_conv_rate = fourth_down_convs / fourth_downs,
           penalty_yds_pg / penalty_yds / games,
           yards_per_penalty = penalty_yds / penalties,
           kick_return_avg = kick_return_yds / kick_returns,
           punt_return_avg = punt_return_yds / punt_returns)
  ## filtering out COVID opt-outs from 2019 (PY3) data, will be merged into 2020 (PY2) data
  COVID_Optouts <- Stats_PY3 %>%
    filter(team == "New Mexico State" | team == "Connecticut" | team == "Old Dominion")
  rbind(Stats_PY2, COVID_Optouts)
  
  ## advanced stats data
  Adv_Stats_PY1 <- cfbd_stats_season_advanced(year = as.integer(year) - 1, excl_garbage_time = FALSE, start_week = 1, end_week = 15) %>%
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
  rbind(Adv_Stats_PY2, COVID_Optouts_adv)
  
  ## pulling in SP+ data
  SP_Rankings_PY1 <-cfbd_ratings_sp(year = as.integer(year) - 1) %>%
    filter(team != "nationalAverages") %>%
    select(team, rating, offense_rating, defense_rating, special_teams_rating)
  colnames(SP_Rankings_PY1) <- c("team", "rating_PY1", "offense_rating_PY1", "defense_rating_PY1", "special_teams_PY1")
  ## Eliminating NAs
  SP_Rankings_PY1[is.na(SP_Rankings_PY1)] = 0
  SP_Rankings_PY2 <-cfbd_ratings_sp(year = as.integer(year) - 2) %>%
    filter(team != "nationalAverages") %>%
    select(team, rating, offense_rating, defense_rating, special_teams_rating)
  colnames(SP_Rankings_PY2) <- c("team", "rating_PY2", "offense_rating_PY2", "defense_rating_PY2", "special_teams_PY2")
  ## Eliminating NAs
  SP_Rankings_PY2[is.na(SP_Rankings_PY2)] = 0
  SP_Rankings_PY3 <-cfbd_ratings_sp(year = as.integer(year) - 3) %>%
    filter(team != "nationalAverages") %>%
    select(team, rating, offense_rating, defense_rating, special_teams_rating)
  colnames(SP_Rankings_PY3) <- c("team", "rating_PY3", "offense_rating_PY3", "defense_rating_PY3", "special_teams_PY3")
  ## Eliminating NAs
  SP_Rankings_PY3[is.na(SP_Rankings_PY3)] = 0
  
  ## pulling FPI data
  FPI_df_PY1 <- espn_ratings_fpi(year = as.integer(year) - 1) %>%
    select(name, fpi, w, l)
  FPI_df_PY2 <- espn_ratings_fpi(year = as.integer(year) - 2) %>%
    select(name, fpi, w, l)
  FPI_df_PY3 <- espn_ratings_fpi(year = as.integer(year) - 3) %>%
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
  rbind(FPI_df_PY2, COVID_Optouts_fpi)
  
  ## pulling in recruiting rankings
  recruit_PY1 <- cfbd_recruiting_team(year = 2021) %>%
    filter(team != "James Madison") %>%
    filter(team %in% Stats_PY1$team) %>%
    select(team, points)
  colnames(recruit_PY1) <- c("team", "recruit_pts_PY1")
  
  ## pulling in talent rankings
  talent_df_PY1 <- cfbd_team_talent(year = 2021) %>%
    filter(school != "James Madison") %>%
    filter(school %in% Stats_PY1$team) %>%
    select(school, talent)
  colnames(talent_df_PY1) <- c("team", "talent_PY1")
  
  ## pulling in recruiting rankings
  recruit_PY2 <- cfbd_recruiting_team(year = 2020) %>%
    filter(team != "James Madison") %>%
    filter(team %in% Stats_PY2$team) %>%
    select(team, points)
  colnames(recruit_PY2) <- c("team", "recruit_pts_PY2")
  
  ## pulling in talent rankings
  talent_df_PY2 <- cfbd_team_talent(year = 2020) %>%
    filter(team != "James Madison") %>%
    filter(school %in% Stats_PY2$team) %>%
    select(school, talent)
  colnames(talent_df_PY2) <- c("team", "talent_PY2")
  
  ## pulling in recruiting rankings
  recruit_PY3 <- cfbd_recruiting_team(year = 2019) %>%
    filter(team != "James Madison") %>%
    filter(team %in% Stats_PY3$team) %>%
    select(team, points)
  colnames(recruit_PY3) <- c("team", "recruit_pts_PY3")
  
  ## pulling in talent rankings
  talent_df_PY3 <- cfbd_team_talent(year = 2019) %>%
    filter(team != "James Madison") %>%
    filter(school %in% Stats_PY3$team) %>%
    select(school, talent)
  colnames(talent_df_PY3) <- c("team", "talent_PY3")
} else if (as.numeric(week) == 1) {
  ## reading in data for 3 previous years
  JMU_AllYears <- read_csv(here("Data", "VoA2022", "JamesMadisonPrevYears", "JMU_AllYears.csv"))
  Stats_PY1 <- cfbd_stats_season_team(year = as.integer(year) - 1, start_week = 1, end_week = 15) %>%
    mutate(total_yds_pg = total_yds/games,
           pass_yds_pg = net_pass_yds/games,
           rush_yds_pg = rush_yds/games,
           first_downs_pg = first_downs/games,
           def_interceptions_pg = passes_intercepted/games,
           pass_ypa = net_pass_yds / pass_atts,
           off_ypp = total_yds / (rush_atts + pass_atts),
           completion_pct = pass_comps / rush_atts,
           pass_ypr = net_pass_yds / pass_comps,
           int_pct = interceptions / pass_atts,
           rush_ypc = rush_yds / rush_atts,
           turnovers_pg = turnovers / games,
           third_conv_rate = third_down_convs / third_downs,
           fourth_conv_rate = fourth_down_convs / fourth_downs,
           penalty_yds_pg / penalty_yds / games,
           yards_per_penalty = penalty_yds / penalties,
           kick_return_avg = kick_return_yds / kick_returns,
           punt_return_avg = punt_return_yds / punt_returns)
  Stats_PY2 <- cfbd_stats_season_team(year = as.integer(year) - 2, start_week = 1, end_week = 15) %>%
    mutate(total_yds_pg = total_yds/games,
           pass_yds_pg = net_pass_yds/games,
           rush_yds_pg = rush_yds/games,
           first_downs_pg = first_downs/games,
           def_interceptions_pg = passes_intercepted/games,
           pass_ypa = net_pass_yds / pass_atts,
           off_ypp = total_yds / (rush_atts + pass_atts),
           completion_pct = pass_comps / rush_atts,
           pass_ypr = net_pass_yds / pass_comps,
           int_pct = interceptions / pass_atts,
           rush_ypc = rush_yds / rush_atts,
           turnovers_pg = turnovers / games,
           third_conv_rate = third_down_convs / third_downs,
           fourth_conv_rate = fourth_down_convs / fourth_downs,
           penalty_yds_pg / penalty_yds / games,
           yards_per_penalty = penalty_yds / penalties,
           kick_return_avg = kick_return_yds / kick_returns,
           punt_return_avg = punt_return_yds / punt_returns)
  Stats_PY3 <- cfbd_stats_season_team(year = as.integer(year) - 3, start_week = 7, end_week = 15) %>%
    mutate(total_yds_pg = total_yds/games,
           pass_yds_pg = net_pass_yds/games,
           rush_yds_pg = rush_yds/games,
           first_downs_pg = first_downs/games,
           def_interceptions_pg = passes_intercepted/games,
           pass_ypa = net_pass_yds / pass_atts,
           off_ypp = total_yds / (rush_atts + pass_atts),
           completion_pct = pass_comps / rush_atts,
           pass_ypr = net_pass_yds / pass_comps,
           int_pct = interceptions / pass_atts,
           rush_ypc = rush_yds / rush_atts,
           turnovers_pg = turnovers / games,
           third_conv_rate = third_down_convs / third_downs,
           fourth_conv_rate = fourth_down_convs / fourth_downs,
           penalty_yds_pg / penalty_yds / games,
           yards_per_penalty = penalty_yds / penalties,
           kick_return_avg = kick_return_yds / kick_returns,
           punt_return_avg = punt_return_yds / punt_returns)
  ## filtering out COVID opt-outs from 2019 (PY3) data, will be merged into 2020 (PY2) data
  COVID_Optouts <- Stats_PY3 %>%
    filter(team == "New Mexico State" | team == "Connecticut" | team == "Old Dominion")
  rbind(Stats_PY2, COVID_Optouts)
  
  ## advanced stats data
  Adv_Stats_PY1 <- cfbd_stats_season_advanced(year = as.integer(year) - 1, excl_garbage_time = FALSE, start_week = 1, end_week = 15) %>%
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
  Adv_Stats_PY3 <- cfbd_stats_season_advanced(year = as.integer(year) - 3, excl_garbage_time = FALSE, start_week = 7, end_week = 15) %>%
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
  rbind(Adv_Stats_PY2, COVID_Optouts_adv)
  
  ## pulling in SP+ data
  SP_Rankings_PY1 <-cfbd_ratings_sp(year = as.integer(year) - 1) %>%
    filter(team != "nationalAverages") %>%
    select(team, rating, offense_rating, defense_rating, special_teams_rating)
  colnames(SP_Rankings_PY1) <- c("team", "sp_rating_PY1", "sp_offense_rating_PY1", "sp_defense_rating_PY1", "sp_special_teams_rating_PY1")
  SP_Rankings_PY2 <-cfbd_ratings_sp(year = as.integer(year) - 2) %>%
    filter(team != "nationalAverages") %>%
    select(team, rating, offense_rating, defense_rating, special_teams_rating)
  colnames(SP_Rankings_PY2) <- c("team", "sp_rating_PY2", "sp_offense_rating_PY2", "sp_defense_rating_PY2", "sp_special_teams_rating_PY2")
  SP_Rankings_PY3 <-cfbd_ratings_sp(year = as.integer(year) - 3) %>%
    filter(team != "nationalAverages") %>%
    select(team, rating, offense_rating, defense_rating, special_teams_rating)
  colnames(SP_Rankings_PY3) <- c("team", "sp_rating_PY3", "sp_offense_rating_PY3", "sp_defense_rating_PY3", "sp_special_teams_rating_PY3")
  
  ## pulling FPI data
  FPI_df_PY1 <- espn_ratings_fpi(year = as.integer(year) - 1) %>%
    select(name, fpi, w, l)
  FPI_df_PY2 <- espn_ratings_fpi(year = as.integer(year) - 2) %>%
    select(name, fpi, w, l)
  FPI_df_PY3 <- espn_ratings_fpi(year = as.integer(year) - 3) %>%
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
  
  ## ## filtering out COVID opt-outs from 2019 (PY3) data, will be merged into 2020 (PY2) data
  COVID_Optouts_fpi <- FPI_df_PY3 %>%
    filter(team == "Connecticut" | team == "New Mexico State" | team == "Old Dominion")
  colnames(COVID_Optouts_fpi) <- FPI_PY2_colnames
  rbind(FPI_df_PY2, COVID_Optouts_fpi)
  
  ## pulling in recruiting rankings
  recruit_PY1 <- cfbd_recruiting_team(year = 2021) %>%
    filter(team != "James Madison") %>%
    filter(team %in% Stats_PY1$team) %>%
    select(team, points)
  colnames(recruit_PY1) <- c("team", "recruit_pts_PY1")
  
  ## pulling in talent rankings
  talent_df_PY1 <- cfbd_team_talent(year = 2021) %>%
    filter(school != "James Madison") %>%
    filter(school %in% Stats_PY1$team) %>%
    select(school, talent)
  colnames(talent_df_PY1) <- c("team", "talent_PY1")
  
  ## pulling in recruiting rankings
  recruit_PY2 <- cfbd_recruiting_team(year = 2020) %>%
    filter(team != "James Madison") %>%
    filter(team %in% Stats_PY2$team) %>%
    select(team, points)
  colnames(recruit_PY2) <- c("team", "recruit_pts_PY2")
  
  ## pulling in talent rankings
  talent_df_PY2 <- cfbd_team_talent(year = 2020) %>%
    filter(team != "James Madison") %>%
    filter(school %in% Stats_PY2$team) %>%
    select(school, talent)
  colnames(talent_df_PY2) <- c("team", "talent_PY2")
  
  ## pulling in recruiting rankings
  recruit_PY3 <- cfbd_recruiting_team(year = 2019) %>%
    filter(team != "James Madison") %>%
    filter(team %in% Stats_PY3$team) %>%
    select(team, points)
  colnames(recruit_PY3) <- c("team", "recruit_pts_PY3")
  
  ## pulling in talent rankings
  talent_df_PY3 <- cfbd_team_talent(year = 2019) %>%
    filter(team != "James Madison") %>%
    filter(school %in% Stats_PY3$team) %>%
    select(school, talent)
  colnames(talent_df_PY3) <- c("team", "talent_PY3")
} else if () {
  
}

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
                                           "def_passing_plays_success_rate_PY3", "def_passing_plays_explosiveness")
  PY3_df_list <- list(PY3_stats_adv_stats_merge, recruit_PY3, talent_df_PY3, SP_Rankings_PY3, FPI_df_PY3)
  PY3_df <- PY3_df_list %>%
    reduce(full_join, by = "team") %>%
    mutate(FPI_SP_PY3_mean = rowMeans(sp_rating_PY3, FPI_PY3))
  
  PY2_stats_adv_stats_list <- list(Stats_PY2, Adv_Stats_PY2)
  PY2_stats_adv_stats_merge <- PY2_stats_adv_stats_list %>%
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
                                           "def_passing_plays_success_rate_PY2", "def_passing_plays_explosiveness")
  PY2_df_list <- list(PY2_stats_adv_stats_merge, recruit_PY2, talent_df_PY2, SP_Rankings_PY2, FPI_df_PY2)
  PY2_df <- PY2_df_list %>%
    reduce(full_join, by = "team") %>%
    mutate(FPI_SP_PY2_mean = rowMeans(sp_rating_PY2, FPI_PY2))
  
  PY1_stats_adv_stats_list <- list(Stats_PY1, Adv_Stats_PY1)
  PY1_stats_adv_stats_merge <- PY1_stats_adv_stats_list %>%
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
                                           "def_passing_plays_success_rate_PY1", "def_passing_plays_explosiveness")
  PY1_df_list <- list(PY1_stats_adv_stats_merge, recruit_PY1, talent_df_PY1, SP_Rankings_PY1, FPI_df_PY1)
  PY1_df <- PY1_df_list %>%
    reduce(full_join, by = "team") %>%
    mutate(FPI_SP_PY3_mean = rowMeans(sp_rating_PY3, FPI_PY3))
  
  ## merging all data frames in order of PY3, PY2, PY1
  all_PY_df_list <- list(PY3_df, PY2_df, PY1_df)
  
  
  ## after binding JMU csv as new row to PY df
} else if (week == 1) {
  
}

## Adding Rank Columns
















## pull in data frames of season variables, sort and rank as normal VoA
## Scraping season data with cfbfastR
Stats <- cfbd_stats_season_team(year = as.integer(year), start_week = 1, end_week = as.integer(week))
# replacing NA values in data frame
Stats[is.na(Stats)] = 0
### why aren't defensive stats included in the regular stat cfbfastR function

## making all stats columns numeric
## Stats[,4:ncol(Stats)] <- sapply(Stats[,4:ncol(Stats)], as.numeric)
## Trying different method for converting to numeric
Stats <- Stats %>% mutate_if(is.integer,as.numeric)

## now bringing in advanced stats
Adv_Stats <- cfbd_stats_season_advanced(year = as.integer(year), excl_garbage_time = FALSE, start_week = 1, end_week = as.numeric(week)) %>%
  select(-one_of("season", "conference")) %>%
  filter(team != "James Madison")

## checking which teams are showing up for regular stats but not advanced
missing_teams <- dplyr::anti_join(Stats, Adv_Stats, by = "team")
missing_adv_teams <- dplyr::anti_join(Adv_Stats, Stats, by = "team")

## breaking script if stats and adv_stats don't match
# if (nrow(missing_teams) > 0) {
#   break
# } else if (nrow(missing_adv_teams) > 0) {
#   break
# }

## Pulling in adv_stats for teams not in original Adv_Stats df
# Adv_Stats_AirForce <- cfbd_stats_season_advanced(year = as.integer(year), team = "Air Force" , excl_garbage_time = FALSE, start_week = 1, end_week = as.numeric(week) + 1)

## now that I have Adv Stats data for teams missing from original week 1 search, adding team-specific dfs as new rows to Adv_Stats df
# Adv_Stats <- rbind(Adv_Stats, Adv_Stats_AirForce)

## making all stats columns numeric
## Adv_Stats[,2:ncol(Adv_Stats)] <- sapply(Adv_Stats[,2:ncol(Adv_Stats)], as.numeric)
## above line not working for some fucking reason, trying a different solution
Adv_Stats <- Adv_Stats %>% mutate_if(is.integer,as.numeric)

## merging stat tables, will rank afterwards
# VoA_Variables_Test <- merge(Stats, Adv_Stats, by = "team")
## above line not working for some reason, trying different solution
df_list <- list(Stats, Adv_Stats)
VoA_Variables_Test <- df_list %>%
  reduce(full_join, by = "team")

## Eliminating NAs
## VoA_Variables_Test[is.na(VoA_Variables_Test)] = 0

## Eliminating James Madison from data frame
VoA_Variables_Test <- VoA_Variables_Test %>%
  dplyr::filter(team != "James Madison")


## adding SP+ rankings
SP_Rankings <- cfbd_ratings_sp(year = as.numeric(year)) %>%
  filter(team != "nationalAverages") %>%
  select(-one_of("year", "conference"))
SP_Rankings <- SP_Rankings[,c("team", "rating", "offense_rating", 
                                        "defense_rating", "special_teams_rating")]
SP_colnames <- c("team", "sp_rating", "sp_offense_rating", "sp_defense_rating",
                 "sp_special_teams_rating")
colnames(SP_Rankings) <- SP_colnames

## Eliminating NAs
SP_Rankings[is.na(SP_Rankings)] = 0

## Making SP_Rankings non-team column numeric data
## SP_Rankings[,2:ncol(SP_Rankings)] <- sapply(SP_Rankings[,2:ncol(SP_Rankings)], as.numeric)
SP_Rankings <- SP_Rankings %>% mutate_if(is.integer,as.numeric)

## checking which teams are showing up for regular stats but not advanced
missing_SP_teams <- dplyr::anti_join(VoA_Variables_Test, SP_Rankings, by = "team")
missing_VoATest_teams <- dplyr::anti_join(SP_Rankings, VoA_Variables_Test, by = "team")

## pulling in FPI data
FPI <- espn_ratings_fpi(year= as.numeric(year)) %>%
  select(name, fpi, w, l)
## changing column names
FPI_colnames <- c("team", "FPI", "Wins", "Losses")
colnames(FPI) <- FPI_colnames
## converting character columns to numeric
FPI[,2:ncol(FPI)] <- FPI[,2:ncol(FPI)] %>% mutate_if(is.character,as.numeric)

## Changing team names in FPI df to match what appears in cfbfastR stats function
FPI <- FPI %>%
  mutate(school = case_when(team == 'Appalachian St' ~ 'Appalachian State',
                            team == 'C Michigan' ~ 'Central Michigan',
                            team == 'Coast Carolina' ~ 'Coastal Carolina',
                            team == 'UConn' ~ 'Connecticut',
                            team == 'E Michigan' ~ 'Eastern Michigan',
                            team == 'FAU' ~ 'Florida Atlantic',
                            team == 'Florida Intl' ~ 'Florida International',
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
colnames(FPI) <- FPI_colnames

## Merging stats with SP and FPI data
# VoA_Variables_Test <- merge(VoA_Variables_Test, SP_Rankings, by = "team")
## above line not working for some reason, trying different solution
df_list <- list(VoA_Variables_Test, SP_Rankings, FPI)
VoA_Variables_Test <- df_list %>%
  reduce(full_join, by = "team")


## list of variables to be ranked
# "completion_pct","pass_ypa","pass_ypr","int_pct","rush_ypc","turnovers_pg",
# "third_conv_rate","fourth_conv_rate","penalties_pg","penalty_yds_pg",
# "yards_per_penalty","kick_return_avg","punt_return_avg","total_yds_pg",
# "pass_yds_pg","rush_yds_pg","first_downs_pg","off_ypp", "def_interceptions_pg","off_ppa",
# "off_success_rate","off_explosiveness","off_power_success","off_stuff_rate",
# "off_line_yds","off_second_lvl_yds","off_open_field_yds","off_pts_per_opp",
# "off_field_pos_avg_predicted_points","off_havoc_total","off_havoc_front_seven",
# "off_havoc_db","off_standard_downs_ppa","off_standard_downs_success_rate",
# "off_standard_downs_explosiveness","off_passing_downs_ppa",
# "off_passing_downs_success_rate","off_passing_downs_explosiveness",
# "off_rushing_plays_ppa","off_rushing_plays_success_rate",
# "off_rushing_plays_explosiveness","off_passing_plays_ppa",
# "off_passing_plays_success_rate","off_passing_plays_explosiveness","def_ppa",
# "def_success_rate","def_explosiveness","def_power_success","def_stuff_rate",
# "def_line_yds","def_second_lvl_yds","def_open_field_yds","def_pts_per_opp",
# "def_field_pos_avg_predicted_points","def_havoc_total","def_havoc_front_seven",
# "def_havoc_db","def_standard_downs_ppa","def_standard_downs_success_rate",
# "def_standard_downs_explosiveness","def_passing_downs_ppa",
# "def_passing_downs_success_rate","def_passing_downs_explosiveness",
# "def_rushing_plays_ppa","def_rushing_plays_success_rate",
# "def_rushing_plays_explosiveness","def_passing_plays_ppa",
# "def_passing_plays_success_rate","def_passing_plays_explosiveness",
# "sp_rating","sp_offense_rating","sp_defense_rating","sp_special_teams_rating"
# "Wins", "Losses"

## updating data frame to only include season, team, conference, games, and rank columns
VoA_Variables_Test <- VoA_Variables_Test %>%
  select(season, team, conference, games, Wins, Losses, completion_pct, 
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
      def_passing_plays_success_rate, def_passing_plays_explosiveness, 
      recruit_pts, talent, sp_rating, sp_offense_rating, sp_defense_rating, 
      sp_special_teams_rating, FPI)

## adding rank columns
VoA_Variables_Test <- VoA_Variables_Test %>%
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
  Rank_Recruit_Pts = dense_rank(desc(recruit_pts)),
  Rank_Talent = dense_rank(desc(talent)),
  Rank_SP_Rating = dense_rank(desc(sp_rating)),
  Rank_SP_Off_Rating = dense_rank(desc(sp_offense_rating)),
  Rank_SP_Def_Rating = dense_rank(sp_defense_rating),
  Rank_SP_SpecialTeams_Rating = dense_rank(desc(sp_special_teams_rating)),
  Rank_FPI = dense_rank(desc(FPI)))




## Append new column of Model output, which is the mean of all variables in VoARanks
VoA_Variables_Test <- VoA_Variables_Test %>%
  mutate(VoA_Output = (rowMeans(VoA_Variables_Test[,77:ncol(VoA_Variables_Test)])))
## Append column of VoA Final Rankings
VoA_Variables_Test <- VoA_Variables_Test %>%
  mutate(VoA_Ranking = dense_rank(VoA_Output))

## Adding Column with CFB Week number
# same number for each team, numeric version of number input in readline function at beginning of script
VoA_Variables_Test <- VoA_Variables_Test %>%
  mutate(CFB_Week = rep(as.numeric(week), nrow(VoA_Variables_Test)), .before = 2)

## Creating df of
FinalTable <- VoA_Variables_Test %>%
  select(team,CFB_Week,VoA_Output,VoA_Ranking) %>%
  arrange(VoA_Ranking)
FinalVoATop25 <- subset(FinalTable, FinalTable$VoA_Ranking < 26)
head(FinalVoATop25)
FinalVoATop25

## setting strings for table titles, file pathways, unintelligible charts
output_dir <- here("RVoA", "Outputs", "Test")
VoAString <- "VoA_Test1.csv"
week_text <- "Week"
VoA_Top25_text <- "Vortex of Accuracy Top 25"
top25_png <- "VoATop25_Test.png"
fulltable_png <- "VoAFullTable_Test.png"
VoA_text <- "TEST 1 Vortex of Accuracy"
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
Output_text <- "_Outputs_Chart.jpeg"
Ranking_text <- "_Rankings_Chart.jpeg"
Histogram_text <- "_OutputHist.png"
Output_Rk_Plot_text <- "VoA Outputs vs VoA Rankings"
Output_Rk_Plot_png <- "Output_Rk.png"


FBS_hist_title <- paste(year, week_text, week, FBS_text, VoA_text, "Outputs")
Power5_hist_title <- paste(year, week_text, week, Power_Five_text, VoA_text, "Outputs")
Group5_hist_title <- paste(year, week_text, week, Group_Five_text, VoA_text, "Outputs")
Output_Rk_Plot_title <- paste(year, week_text, week, Output_Rk_Plot_text)
top25_file_pathway <- paste(week_text,week,"_",year,top25_png, sep = "")
fulltable_file_pathway <- paste(week_text,week,"_",year,fulltable_png, sep = "")
AAC_Output_filename <- paste(week_text, week, AAC_text, Output_text, sep = "")
AAC_Ranking_filename <- paste(week_text, week, AAC_text, Ranking_text, sep = "")
ACC_Output_filename <- paste(week_text, week, ACC_text, Output_text, sep = "")
ACC_Ranking_filename <- paste(week_text, week, ACC_text, Ranking_text, sep = "")
Big12_Output_filename <- paste(week_text, week, Big12_text, Output_text, sep = "")
Big12_Ranking_filename <- paste(week_text, week, Big12_text, Ranking_text, sep = "")
Big10_Output_filename <- paste(week_text, week, Big10_text, Output_text, sep = "")
Big10_Ranking_filename <- paste(week_text, week, Big10_text, Ranking_text, sep = "")
CUSA_Output_filename <- paste(week_text, week, CUSA_text, Output_text, sep = "")
CUSA_Ranking_filename <- paste(week_text, week, CUSA_text, Ranking_text, sep = "")
Indy_Output_filename <- paste(week_text, week, Indy_text, Output_text, sep = "")
Indy_Ranking_filename <- paste(week_text, week, Indy_text, Ranking_text, sep = "")
MAC_Output_filename <- paste(week_text, week, MAC_text, Output_text, sep = "")
MAC_Ranking_filename <- paste(week_text, week, MAC_text, Ranking_text, sep = "")
MWC_Output_filename <- paste(week_text, week, MWC_text, Output_text, sep = "")
MWC_Ranking_filename <- paste(week_text, week, MWC_text, Ranking_text, sep = "")
Pac12_Output_filename <- paste(week_text, week, Pac12_text, Output_text, sep = "")
Pac12_Ranking_filename <- paste(week_text, week, Pac12_text, Ranking_text, sep = "")
SEC_Output_filename <- paste(week_text, week, SEC_text, Output_text, sep = "")
SEC_Ranking_filename <- paste(week_text, week, SEC_text, Ranking_text, sep = "")
SunBelt_Output_filename <- paste(week_text, week, SunBelt_text, Output_text, sep = "")
SunBelt_Ranking_filename <- paste(week_text, week, SunBelt_text, Ranking_text, sep = "")
FBS_hist_filename <- paste(year, "_", week_text, week, "_", FBS_text, Histogram_text, sep = "")
Power5_hist_filename <- paste(year, "_", week_text, week, "_", Power_Five_text, Histogram_text, sep = "")
Group5_hist_filename <- paste(year, "_", week_text, week, "_", Group_Five_text, Histogram_text, sep = "")
Output_Rk_Plot_filename <- paste(year, "_", week_text, week, "_", Output_Rk_Plot_png, sep = "")
file_pathway <- paste(output_dir, "/",week_text, week,"_",year, VoAString, sep = "")

## Using Intial VoA Rankings to add in conference strength metric
Conference_Outputs <- VoA_Variables_Test %>%
  group_by(conference) %>%
  summarize(Rk_mean = mean(VoA_Output), Rk_median = median(VoA_Output)) %>%
  mutate(Conf_Rk = dense_rank(Rk_median))

#VoA_Variables_Test <- VoA_Variables_Test %>%
#  select(-VoA_Output, -VoA_Ranking)

## I want to make a function where it ranks the teams, takes the conference means and medians, ranks the conferences by median, then applies those rankings to each team based on what conference they are in, then re-ranks the teams




## Exporting final data frame as CSV
write_csv(VoA_Variables_Test, file_pathway)

## Top 25 Table
# adding title and subtitle
VoATop25Table <- FinalVoATop25 %>%
  gt() %>% # use 'gt' to make an awesome table...
  tab_header( 
    title = "SUPER BETA TESTED VERSION OF Vortex of Accuracy Top 25", # ...with this title
    subtitle = "Supremely Excellent Yet Salaciously Godlike And Infallibly Magnificent Vortex of Accuracy BEING TESTED ON cfbfastR DATA FROM 2021") %>% # and this subtitle
  ## tab_style(style = cell_fill("bisque"),
    ##         locations = cells_body()) %>%  # add fill color to table
  fmt_number( # A column (numeric data)
    columns = c(VoA_Output), # What column variable? VoA_Output
    decimals = 5 # With five decimal places
  ) %>% 
  fmt_number( # Another column (also numeric data)
    columns = c(VoA_Ranking), # What column variable? VoA_Ranking
    decimals = 0 # I want this column to have zero decimal place
  ) %>%
  data_color( # Update cell colors, testing different color palettes
    columns = c(VoA_Output), # ...for dose column
    colors = scales::col_numeric( # <- bc it's numeric
      palette = brewer.pal(9, "Reds"), # A color scheme (gradient)
      domain = c(), # Column scale endpoints
      reverse = TRUE
    )
  ) %>%
  cols_label(VoA_Output = "BETA Final VoA Output", VoA_Ranking = "BETA VoA Ranking") %>% # Update labels
  cols_move_to_end(columns = "VoA_Output") %>%
  tab_footnote(
    footnote = "Data from CFB Data API, ESPN.com, the NCAA, and ESPN's Bill Connelly via cfbfastR"
  )
VoATop25Table
VoATop25Table %>%
  gtsave(
    top25_file_pathway, expand = 5,
    path = here("RVoA", "Outputs", "Test")
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
    columns = c(VoA_Output), # What column variable? FinalVoATop25$VoA_Output
    decimals = 5 # With four decimal places
  ) %>% 
  fmt_number( # Another column (also numeric data)
    columns = c(VoA_Ranking), # What column variable? FinalVoATop25$VoA_Ranking
    decimals = 0 # I want this column to have zero decimal places
  ) %>% 
  data_color( # Update cell colors, testing different color palettes
    columns = c(VoA_Output), # ...for dose column
    colors = scales::col_numeric( # <- bc it's numeric
      palette = brewer.pal(9, "RdBu"), # A color scheme (gradient)
      domain = c(), # Column scale endpoints
      reverse = FALSE
    )
  ) %>%
  cols_label(VoA_Output = "Final VoA Output", VoA_Ranking = "VoA Ranking") %>% # Update labels
  cols_move_to_end(columns = "VoA_Output") %>%
  tab_footnote(
    footnote = "Data from CFB Data API, ESPN.com, the NCAA, and ESPN's Bill Connelly via cfbfastR"
  )
VoA_Full_Table
VoA_Full_Table %>%
  gtsave(
    fulltable_file_pathway, expand = 5,
    path = here("RVoA", "Outputs", "Test")
  )


