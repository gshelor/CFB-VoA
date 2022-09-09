##### This script is for cleaning up FCS data csvs to get data for James Madison which will be used in 2022 VoA
## Advanced analytics mostly unavailable for FCS teams
# only data I could get were final SP+ ratings from ESPN's Bill Connelly for 2021 season only
# other advanced stats will be G5 means used as extremely rough approximation
# basic stats will all be from James Madison
# This script should only need to be run once, to create csvs of previous years data to be used in VoA script before and during season
## loading packages
library(pacman)
pacman::p_load(tidyverse, matrixStats, grid, gridExtra, gt, here, ggpubr, gtExtras, cfbfastR)

## PY# represents "Previous Year" and the number is the number of years previous to the current season
# In this script, "current season" will always be the 2022 season, so PY1, PY2, and PY3 will only be 2021, 2020, 2019 for the purposes of this script

## reading in 2019 CSVs, formatting DFs
## defensive interceptions
Ints_PY3 <- read_csv(here("Data", "VoA2022", "JamesMadisonPrevYears", "FCSDefInts2019.csv")) %>%
  filter(Team == "James Madison (CAA)")
Ints_PY3_ColNames <- c("Rk_Delete", "team", "games_PY3", "Win_Loss", "Opp_Comps_PY3", "INTs", "INT_Yds", "INT_TDs")
colnames(Ints_PY3) <- Ints_PY3_ColNames
Ints_PY3 <- Ints_PY3 %>%
  mutate(conference = "Sun Belt", .before = 2) %>%
  separate(col = Win_Loss, into = c("Wins_PY3", "Losses_PY3"), sep = "-")
Ints_PY3[,4:ncol(Ints_PY3)] <- Ints_PY3[,4:ncol(Ints_PY3)] %>% mutate_if(is.character, as.numeric)
Ints_PY3 <- Ints_PY3 %>%
  mutate(def_interceptions_pg_PY3 = INTs / games_PY3) %>%
  select(team, conference, games_PY3, Wins_PY3, Losses_PY3, def_interceptions_pg_PY3)
Ints_PY3[1,1] = "James Madison"

## offensive 4th down stats
Fourth_Down_Off_PY3 <- read_csv(here("Data", "VoA2022", "JamesMadisonPrevYears", "FCSFourthDownOff2019.csv")) %>%
  filter(Team == "James Madison (CAA)")
Fourth_Down_Off_PY3_colnames <- c("rk_delete", "team", "games", "Win_Loss", "fourth_down_convs_PY3", "fourth_downs_PY3 ", "fourth_conv_rate_PY3")
colnames(Fourth_Down_Off_PY3) <- Fourth_Down_Off_PY3_colnames
Fourth_Down_Off_PY3 <- Fourth_Down_Off_PY3 %>%
  select(team, fourth_conv_rate_PY3)
Fourth_Down_Off_PY3[1,1] = "James Madison"

Kick_Returns_PY3 <- read_csv(here("Data", "VoA2022", "JamesMadisonPrevYears", "FCSKickReturns2019.csv")) %>%
  filter(Team == "James Madison (CAA)")
Kick_Returns_PY3_colnames <- c("rk_delete", "team", "games","win_loss", "Returns", "Return_Yds", "Return_TDs", "kick_return_avg_PY3")
colnames(Kick_Returns_PY3) <- Kick_Returns_PY3_colnames
Kick_Returns_PY3 <- Kick_Returns_PY3 %>%
  select(team, kick_return_avg_PY3)
Kick_Returns_PY3[1,1] = "James Madison"

PassOff_PY3 <- read_csv(here("Data", "VoA2022", "JamesMadisonPrevYears", "FCSPassOffense2019.csv")) %>%
  filter(Team == "James Madison (CAA)")
PassOff_PY3_colnames <- c("rk_del", "team", "games", "win_loss", "pass_atts_PY3", "pass_comps_PY3", "INTs", "Total_Pass_Yds", "pass_ypa_PY3", "pass_ypr_PY3", "Pass_TD", "pass_yds_pg_PY3")
colnames(PassOff_PY3) <- PassOff_PY3_colnames
PassOff_PY3[,5:ncol(PassOff_PY3)] <- PassOff_PY3[,5:ncol(PassOff_PY3)] %>% mutate_if(is.character,as.numeric)
PassOff_PY3 <- PassOff_PY3 %>%
  mutate(int_pct_PY3 = INTs / pass_atts_PY3) %>%
  mutate(completion_pct_PY3 = pass_comps_PY3 / pass_atts_PY3) %>%
  select(team, int_pct_PY3, pass_ypa_PY3, pass_ypr_PY3, pass_yds_pg_PY3, completion_pct_PY3)
PassOff_PY3[1,1] = "James Madison"

FirstDown_PY3 <- read_csv(here("Data", "VoA2022", "JamesMadisonPrevYears", "FCSFirstDowns2019.csv")) %>%
  filter(Team == "James Madison (CAA)")
FirstDown_PY3_colnames <- c("del", "team", "games", "win_loss", "run_fd", "pass_fd", "pen_fd", "total_fd")
colnames(FirstDown_PY3) <- FirstDown_PY3_colnames
FirstDown_PY3[,3:ncol(FirstDown_PY3)] <- FirstDown_PY3[,3:ncol(FirstDown_PY3)] %>% mutate_if(is.character, as.numeric)
FirstDown_PY3 <- FirstDown_PY3 %>%
  mutate(first_downs_pg_PY3 = total_fd / games) %>%
  select(team, first_downs_pg_PY3)
FirstDown_PY3[1,1] = "James Madison"

Penalties_PY3 <- read_csv(here("Data", "VoA2022", "JamesMadisonPrevYears", "FCSPenalties2019.csv")) %>%
  filter(Team == "James Madison (CAA)")
Penalties_PY3_colnames <- c("delete", "team", "games", "win_loss", "penalties", "penalty_yds", "penalty_yds_pg_PY3")
colnames(Penalties_PY3) <- Penalties_PY3_colnames
Penalties_PY3[,5:ncol(Penalties_PY3)] <- Penalties_PY3[,5:ncol(Penalties_PY3)] %>% mutate_if(is.character, as.numeric)
Penalties_PY3 <- Penalties_PY3 %>%
  mutate(yards_per_penalty_PY3 = penalty_yds / penalties) %>%
  select(team, penalty_yds_pg_PY3, yards_per_penalty_PY3)
Penalties_PY3[1,1] = "James Madison"

PuntReturns_PY3 <- read_csv(here("Data", "VoA2022", "JamesMadisonPrevYears", "FCSPuntReturns2019.csv")) %>%
  filter(Team == "James Madison (CAA)")
PuntReturns_PY3_colnames <- c("delete", "team", "games", "win_loss", "punt_returns", "return_yds", "return_tds", "punt_return_avg_PY3")
colnames(PuntReturns_PY3) <- PuntReturns_PY3_colnames
PuntReturns_PY3 <- PuntReturns_PY3 %>%
  select(team, punt_return_avg_PY3)
PuntReturns_PY3[1,1] = "James Madison"

RushOffense_PY3 <- read_csv(here("Data", "VoA2022", "JamesMadisonPrevYears", "FCSRushOffense2019.csv")) %>%
  filter(Team == "James Madison (CAA)")
RushOffense_PY3_colnames <- c("delete", "team", "games", "win_loss", "rush_atts", "rush_yds", "rush_ypc_PY3", "rush_td", "rush_yds_pg_PY3")
colnames(RushOffense_PY3) <- RushOffense_PY3_colnames
RushOffense_PY3 <- RushOffense_PY3 %>%
  select(team, rush_ypc_PY3, rush_yds_pg_PY3)
RushOffense_PY3[1,1] = "James Madison"

ThirdDown_PY3 <- read_csv(here("Data", "VoA2022", "JamesMadisonPrevYears", "FCSThirdDownOff2019.csv")) %>%
  filter(Team == "James Madison (CAA)")
ThirdDown_PY3_colnames <- c("del", "team", "gms", "wl", "third_atts", "third_convs", "third_conv_rate_PY3")
colnames(ThirdDown_PY3) <- ThirdDown_PY3_colnames
ThirdDown_PY3 <- ThirdDown_PY3 %>%
  select(team, third_conv_rate_PY3)
ThirdDown_PY3[1,1] = "James Madison"

## Time of Possession commented out because I don't think I'm going to use it in the VoA
# ToP_PY3 <- read_csv(here("Data", "VoA2022", "JamesMadisonPrevYears", "FCSToP2019.csv")) %>%
#   filter(Team == "James Madison (CAA)")
# ToP_PY3_colnames <- c("del", "team", "gms", "Wl", "total_top", "time_of_poss_pg_temp")
# colnames(ToP_PY3) <- ToP_PY3_colnames
# ToP_PY3 <- ToP_PY3 %>%
#   separate(time_of_poss_pg_temp, into = c("mins", "secs", "extra"), sep = ":")
# ToP_PY3[,6:ncol(ToP_PY3)] <- ToP_PY3[,6:ncol(ToP_PY3)] %>% mutate_if(is.character, as.numeric)
# ToP_PY3 <- ToP_PY3 %>%
#   mutate(time_of_poss_pg_PY3 = mins + (secs / 60)) %>%
#   select(team, time_of_poss_pg_PY3)
# ToP_PY3[1,1] = "James Madison"

TotalOffense_PY3 <- read_csv(here("Data", "VoA2022", "JamesMadisonPrevYears", "FCSTotalOffense2019.csv")) %>%
  filter(Team == "James Madison (CAA)")
TotalOffense_PY3_colnames <- c("del", "team", "gms", "wl", "plays", "totalyds", "off_ypp_PY3", "tds", "total_yds_pg_PY3")
colnames(TotalOffense_PY3) <- TotalOffense_PY3_colnames
TotalOffense_PY3 <- TotalOffense_PY3 %>%
  select(team, off_ypp_PY3, total_yds_pg_PY3)
TotalOffense_PY3[1,1] = "James Madison"

Turnovers_df_PY3 <- read_csv(here("Data", "VoA2022", "JamesMadisonPrevYears", "FCSTurnoversLost2019.csv")) %>%
  filter(Team == "James Madison (CAA)")
Turnovers_PY3_colnames <- c("del", "team", "gms", "wl", "fum", "int", "total_turnovers_PY3")
colnames(Turnovers_df_PY3) <- Turnovers_PY3_colnames
Turnovers_df_PY3[,3:ncol(Turnovers_df_PY3)] <- Turnovers_df_PY3[,3:ncol(Turnovers_df_PY3)] %>% mutate_if(is.character, as.numeric)
Turnovers_df_PY3 <- Turnovers_df_PY3 %>%
  mutate(turnovers_pg_PY3 = total_turnovers_PY3 / gms) %>%
  select(team, turnovers_pg_PY3)
Turnovers_df_PY3[1,1] = "James Madison"

## bringing in advanced stats for G5 and non-Notre Dame Indy teams
Adv_Stats_PY3 <- cfbd_stats_season_advanced(2019, excl_garbage_time = FALSE, start_week = 1, end_week = 15) %>%
  filter(conference == "Mountain West" | conference == "Mid-American" | conference == "Conference USA" | conference == "Sun Belt" | conference == "American Athletic" | conference == "FBS Independents") %>%
  filter(team != "Notre Dame") %>%
  filter(team != "James Madison") %>%
  select(-one_of("season", "conference")) %>%
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

## making all stats columns numeric
Adv_Stats_PY3 <- Adv_Stats_PY3 %>% mutate_if(is.integer,as.numeric)

## computing means of each column and assigning them to James Madison
## I, uh, don't know how to add a row and set all the variable values as the means of the rest of the column
JMU_Adv_Stats_PY3 <- Adv_Stats_PY3 %>%
  filter(team == "Akron")
JMU_Adv_Stats_PY3[1,1] = "James Madison"
## making all stats columns numeric
JMU_Adv_Stats_PY3 <- JMU_Adv_Stats_PY3 %>% mutate_if(is.integer,as.numeric)
for (rating in 2:ncol(JMU_Adv_Stats_PY3)) {
  JMU_Adv_Stats_PY3[1,rating] = colMeans(Adv_Stats_PY3[,rating])
}

## renaming Advanced stats columns to reflect them being from PY3
colnames(JMU_Adv_Stats_PY3) <- c("team", "off_ppa_PY3", "off_success_rate_PY3", "off_explosiveness_PY3", "off_power_success_PY3",
                                 "off_stuff_rate_PY3", "off_line_yds_PY3", "off_second_lvl_yds_PY3", "off_open_field_yds_PY3",
                                 "off_pts_per_opp_PY3", "off_field_pos_avg_predicted_points_PY3", "off_havoc_total_PY3", 
                                 "off_havoc_front_seven_PY3", "off_havoc_db_PY3", "off_standard_downs_ppa_PY3",
                                 "off_standard_downs_success_rate_PY3", "off_standard_downs_explosiveness_PY3",
                                 "off_passing_downs_ppa_PY3", "off_passing_downs_success_rate_PY3",
                                 "off_passing_downs_explosiveness_PY3", "off_rushing_plays_ppa_PY3",
                                 "off_rushing_plays_success_rate_PY3", "off_rushing_plays_explosiveness_PY3",
                                 "off_passing_plays_ppa_PY3", "off_passing_plays_success_rate_PY3",
                                 "off_passing_plays_explosiveness_PY3", "def_ppa_PY3", "def_success_rate_PY3",
                                 "def_explosiveness_PY3", "def_power_success_PY3", "def_stuff_rate_PY3", "def_line_yds_PY3",
                                 "def_second_lvl_yds_PY3", "def_open_field_yds_PY3", "def_pts_per_opp_PY3", 
                                 "def_field_pos_avg_predicted_points_PY3", "def_havoc_total_PY3", "def_havoc_front_seven_PY3",
                                 "def_havoc_db_PY3", "def_standard_downs_ppa_PY3", "def_standard_downs_success_rate_PY3",
                                 "def_standard_downs_explosiveness_PY3", "def_passing_downs_ppa_PY3",
                                 "def_passing_downs_success_rate_PY3", "def_passing_downs_explosiveness_PY3",
                                 "def_rushing_plays_ppa_PY3", "def_rushing_plays_success_rate_PY3",
                                 "def_rushing_plays_explosiveness_PY3", "def_passing_plays_ppa_PY3",
                                 "def_passing_plays_success_rate_PY3", "def_passing_plays_explosiveness_PY3")

## adding SP+ rankings
SP_Rankings_PY3 <- cfbd_ratings_sp(year = 2019) %>%
  filter(conference == "Mountain West" | conference == "Mid-American" | conference == "Conference USA" | conference == "Sun Belt" | conference == "American Athletic" | conference == "FBS Independents") %>%
  filter(team != "Notre Dame") %>%
  filter(team != "nationalAverages") %>%
  select(-one_of("year"))
SP_Rankings_PY3 <- SP_Rankings_PY3[,c("team", "rating", "offense_rating", 
                              "defense_rating", "special_teams_rating")]
SP_colnames_PY3 <- c("team", "sp_rating_PY3", "sp_offense_rating_PY3", "sp_defense_rating_PY3",
                 "sp_special_teams_rating_PY3")
colnames(SP_Rankings_PY3) <- SP_colnames_PY3

## computing means of each column and assigning them to James Madison
## I, uh, don't know how to add a row and set all the variable values as the means of the rest of the column
JMU_SP_PY3 <- SP_Rankings_PY3 %>%
  filter(team == "Akron")
JMU_SP_PY3[1,1] = "James Madison"
for (rating in 2:ncol(JMU_SP_PY3)) {
  JMU_SP_PY3[1,rating] = colMeans(SP_Rankings_PY3[,rating])
}

## pulling in FPI data
FPI_df_PY3 <- espn_ratings_fpi(year= 2019) %>%
  select(name, fpi)
## changing column names
FPI_colnames_PY3 <- c("team", "FPI_PY3")
colnames(FPI_df_PY3) <- FPI_colnames_PY3
## converting non-team character columns to numeric
FPI_df_PY3[,2] <- FPI_df_PY3[,2] %>% mutate_if(is.character,as.numeric)
## Changing team names in FPI df to match what appears in cfbfastR stats function
FPI_df_PY3 <- FPI_df_PY3 %>%
  mutate(school = case_when(team == 'Appalachian St' ~ 'Appalachian State',
                            team == 'C Michigan' ~ 'Central Michigan',
                            team == 'Coast Carolina' ~ 'Coastal Carolina',
                            team == 'Coastal Car' ~ 'Coastal Carolina',
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
                            team == 'Arizona St' ~ 'Arizona State',
                            team == 'Arkansas St' ~ 'Arkansas State',
                            team == 'Boise St' ~ 'Boise State',
                            team == 'Colorado St' ~ 'Colorado State',
                            team == 'Florida St' ~ 'Florida State',
                            team == 'Fresno St' ~ 'Fresno State',
                            team == 'Georgia St' ~ 'Georgia State',
                            team == 'Kansas St' ~ 'Kansas State',
                            team == 'Miami OH' ~ 'Miami (OH)',
                            team == 'Michigan St' ~ 'Michigan State',
                            team == 'Pitt' ~ 'Pittsburgh',
                            team == 'San Diego St' ~ 'San Diego State',
                            team == 'San José St' ~ 'San José State',
                            team == 'Texas St' ~ 'Texas State',
                            TRUE ~ team)) %>%
  select(school, FPI_PY3)
colnames(FPI_df_PY3) <- FPI_colnames_PY3

## joining conference to the FPI df to extract G5 and Non-ND Indy teams
Team_Conference <- cfbd_team_info() %>%
  select(school, conference) %>%
  filter(school != "James Madison")
colnames(Team_Conference) <- c("team", "conference")

## checking which teams are showing up in the FPI df but not teaminfo, then other way around
# missing_fpi_teams <- dplyr::anti_join(FPI_df_PY3, Team_Conference, by = "team")
# missing_teaminfo_teams <- dplyr::anti_join(Team_Conference, FPI_df_PY3, by = "team")

df_fpi_list <- list(FPI_df_PY3, Team_Conference)
FPI_df_PY3 <- df_fpi_list %>%
  reduce(full_join, by = "team") %>%
  filter(conference == "Mountain West" | conference == "Mid-American" | conference == "Conference USA" | conference == "Sun Belt" | conference == "American Athletic" | conference == "FBS Independents") %>%
  filter(team != "Notre Dame") %>%
  select(team, FPI_PY3)

JMU_FPI_PY3 <- FPI_df_PY3 %>%
  filter(team == "Akron")
JMU_FPI_PY3[1,1] = "James Madison"
JMU_FPI_PY3[1,2] = mean(FPI_df_PY3$FPI_PY3)

## pulling in recruiting rankings
recruit_PY3 <- cfbd_recruiting_team(year = 2019, team = "James Madison") %>%
  select(team, points)
recruit_PY3[,2] <- recruit_PY3[,2] %>% mutate_if(is.character, as.numeric)
colnames(recruit_PY3) <- c("team", "recruit_pts_PY3")

## pulling in talent rankings
talent_df_PY3 <- cfbd_team_talent(year = 2019) %>%
  filter(school == "James Madison") %>%
  select(school, talent)
colnames(talent_df_PY3) <- c("team", "talent_PY3")

## merging all the data frames
df_PY3_list <- list(Ints_PY3, Fourth_Down_Off_PY3, FirstDown_PY3, Kick_Returns_PY3, PassOff_PY3, Penalties_PY3, PuntReturns_PY3, RushOffense_PY3, ThirdDown_PY3, TotalOffense_PY3,Turnovers_df_PY3, JMU_Adv_Stats_PY3, JMU_FPI_PY3, JMU_SP_PY3, recruit_PY3, talent_df_PY3)
JMU_PY3 <- df_PY3_list %>%
  reduce(full_join, by = "team") %>%
  mutate(season = 2022, .before = 1)


## reading in 2020 CSVs
## defensive interceptions
Ints_PY2 <- read_csv(here("Data", "VoA2022", "JamesMadisonPrevYears", "FCSDefInts2020.csv")) %>%
  filter(Team == "James Madison (CAA)")
Ints_PY2_ColNames <- c("Rk_Delete", "team", "games_PY2", "Win_Loss", "Opp_Comps_PY2", "INTs", "INT_Yds", "INT_TDs")
colnames(Ints_PY2) <- Ints_PY2_ColNames
Ints_PY2 <- Ints_PY2 %>%
  mutate(conference = "Sun Belt", .before = 2) %>%
  separate(col = Win_Loss, into = c("Wins_PY2", "Losses_PY2"), sep = "-")
Ints_PY2[,4:ncol(Ints_PY2)] <- Ints_PY2[,4:ncol(Ints_PY2)] %>% mutate_if(is.character, as.numeric)
Ints_PY2 <- Ints_PY2 %>%
  mutate(def_interceptions_pg_PY2 = INTs / games_PY2) %>%
  select(team, conference, games_PY2, Wins_PY2, Losses_PY2, def_interceptions_pg_PY2)
Ints_PY2[1,1] = "James Madison"

## offensive 4th down stats
Fourth_Down_Off_PY2 <- read_csv(here("Data", "VoA2022", "JamesMadisonPrevYears", "FCSFourthDownOff2020.csv")) %>%
  filter(Team == "James Madison (CAA)")
Fourth_Down_Off_PY2_colnames <- c("rk_delete", "team", "games", "Win_Loss", "fourth_down_convs_PY2", "fourth_downs_PY2 ", "fourth_conv_rate_PY2")
colnames(Fourth_Down_Off_PY2) <- Fourth_Down_Off_PY2_colnames
Fourth_Down_Off_PY2 <- Fourth_Down_Off_PY2 %>%
  select(team, fourth_conv_rate_PY2)
Fourth_Down_Off_PY2[1,1] = "James Madison"

Kick_Returns_PY2 <- read_csv(here("Data", "VoA2022", "JamesMadisonPrevYears", "FCSKickReturns2020.csv")) %>%
  filter(Team == "James Madison (CAA)")
Kick_Returns_PY2_colnames <- c("rk_delete", "team", "games","win_loss", "Returns", "Return_Yds", "Return_TDs", "kick_return_avg_PY2")
colnames(Kick_Returns_PY2) <- Kick_Returns_PY2_colnames
Kick_Returns_PY2 <- Kick_Returns_PY2 %>%
  select(team, kick_return_avg_PY2)
Kick_Returns_PY2[1,1] = "James Madison"

PassOff_PY2 <- read_csv(here("Data", "VoA2022", "JamesMadisonPrevYears", "FCSPassOffense2020.csv")) %>%
  filter(Team == "James Madison (CAA)")
PassOff_PY2_colnames <- c("rk_del", "team", "games", "win_loss", "pass_atts_PY2", "pass_comps_PY2", "INTs", "Total_Pass_Yds", "pass_ypa_PY2", "pass_ypr_PY2", "Pass_TD", "pass_yds_pg_PY2")
colnames(PassOff_PY2) <- PassOff_PY2_colnames
PassOff_PY2[,5:ncol(PassOff_PY2)] <- PassOff_PY2[,5:ncol(PassOff_PY2)] %>% mutate_if(is.character,as.numeric)
PassOff_PY2 <- PassOff_PY2 %>%
  mutate(int_pct_PY2 = INTs / pass_atts_PY2) %>%
  mutate(completion_pct_PY2 = pass_comps_PY2 / pass_atts_PY2) %>%
  select(team, int_pct_PY2, pass_ypa_PY2, pass_ypr_PY2, pass_yds_pg_PY2, completion_pct_PY2)
PassOff_PY2[1,1] = "James Madison"

FirstDown_PY2 <- read_csv(here("Data", "VoA2022", "JamesMadisonPrevYears", "FCSFirstDowns2020.csv")) %>%
  filter(Team == "James Madison (CAA)")
FirstDown_PY2_colnames <- c("del", "team", "games", "win_loss", "run_fd", "pass_fd", "pen_fd", "total_fd")
colnames(FirstDown_PY2) <- FirstDown_PY2_colnames
FirstDown_PY2[,3:ncol(FirstDown_PY2)] <- FirstDown_PY2[,3:ncol(FirstDown_PY2)] %>% mutate_if(is.character, as.numeric)
FirstDown_PY2 <- FirstDown_PY2 %>%
  mutate(first_downs_pg_PY2 = total_fd / games) %>%
  select(team, first_downs_pg_PY2)
FirstDown_PY2[1,1] = "James Madison"

Penalties_PY2 <- read_csv(here("Data", "VoA2022", "JamesMadisonPrevYears", "FCSPenalties2020.csv")) %>%
  filter(Team == "James Madison (CAA)")
Penalties_PY2_colnames <- c("delete", "team", "games", "win_loss", "penalties", "penalty_yds", "penalty_yds_pg_PY2")
colnames(Penalties_PY2) <- Penalties_PY2_colnames
Penalties_PY2[,5:ncol(Penalties_PY2)] <- Penalties_PY2[,5:ncol(Penalties_PY2)] %>% mutate_if(is.character, as.numeric)
Penalties_PY2 <- Penalties_PY2 %>%
  mutate(yards_per_penalty_PY2 = penalty_yds / penalties) %>%
  select(team, penalty_yds_pg_PY2, yards_per_penalty_PY2)
Penalties_PY2[1,1] = "James Madison"

PuntReturns_PY2 <- read_csv(here("Data", "VoA2022", "JamesMadisonPrevYears", "FCSPuntReturns2020.csv")) %>%
  filter(Team == "James Madison (CAA)")
PuntReturns_PY2_colnames <- c("delete", "team", "games", "win_loss", "punt_returns", "return_yds", "return_tds", "punt_return_avg_PY2")
colnames(PuntReturns_PY2) <- PuntReturns_PY2_colnames
PuntReturns_PY2 <- PuntReturns_PY2 %>%
  select(team, punt_return_avg_PY2)
PuntReturns_PY2[1,1] = "James Madison"

RushOffense_PY2 <- read_csv(here("Data", "VoA2022", "JamesMadisonPrevYears", "FCSRushOffense2020.csv")) %>%
  filter(Team == "James Madison (CAA)")
RushOffense_PY2_colnames <- c("delete", "team", "games", "win_loss", "rush_atts", "rush_yds", "rush_ypc_PY2", "rush_td", "rush_yds_pg_PY2")
colnames(RushOffense_PY2) <- RushOffense_PY2_colnames
RushOffense_PY2 <- RushOffense_PY2 %>%
  select(team, rush_ypc_PY2, rush_yds_pg_PY2)
RushOffense_PY2[1,1] = "James Madison"

ThirdDown_PY2 <- read_csv(here("Data", "VoA2022", "JamesMadisonPrevYears", "FCSThirdDownOff2020.csv")) %>%
  filter(Team == "James Madison (CAA)")
ThirdDown_PY2_colnames <- c("del", "team", "gms", "wl", "third_atts", "third_convs", "third_conv_rate_PY2")
colnames(ThirdDown_PY2) <- ThirdDown_PY2_colnames
ThirdDown_PY2 <- ThirdDown_PY2 %>%
  select(team, third_conv_rate_PY2)
ThirdDown_PY2[1,1] = "James Madison"

## Time of Possession commented out because I don't think I'm going to use it in the VoA
# ToP_PY2 <- read_csv(here("Data", "VoA2022", "JamesMadisonPrevYears", "FCSToP2020.csv")) %>%
#   filter(Team == "James Madison (CAA)")
# ToP_PY2_colnames <- c("del", "team", "gms", "Wl", "total_top", "time_of_poss_pg_temp")
# colnames(ToP_PY2) <- ToP_PY2_colnames
# ToP_PY2 <- ToP_PY2 %>%
#   separate(time_of_poss_pg_temp, into = c("mins", "secs", "extra"), sep = ":")
# ToP_PY2[,6:ncol(ToP_PY2)] <- ToP_PY2[,6:ncol(ToP_PY2)] %>% mutate_if(is.character, as.numeric)
# ToP_PY2 <- ToP_PY2 %>%
#   mutate(time_of_poss_pg_PY2 = mins + (secs / 60)) %>%
#   select(team, time_of_poss_pg_PY2)
# ToP_PY2[1,1] = "James Madison"

TotalOffense_PY2 <- read_csv(here("Data", "VoA2022", "JamesMadisonPrevYears", "FCSTotalOffense2020.csv")) %>%
  filter(Team == "James Madison (CAA)")
TotalOffense_PY2_colnames <- c("del", "team", "gms", "wl", "plays", "totalyds", "off_ypp_PY2", "tds", "total_yds_pg_PY2")
colnames(TotalOffense_PY2) <- TotalOffense_PY2_colnames
TotalOffense_PY2 <- TotalOffense_PY2 %>%
  select(team, off_ypp_PY2, total_yds_pg_PY2)
TotalOffense_PY2[1,1] = "James Madison"

Turnovers_df_PY2 <- read_csv(here("Data", "VoA2022", "JamesMadisonPrevYears", "FCSTurnoversLost2020.csv")) %>%
  filter(Team == "James Madison (CAA)")
Turnovers_PY2_colnames <- c("del", "team", "gms", "wl", "fum", "int", "total_turnovers_PY2")
colnames(Turnovers_df_PY2) <- Turnovers_PY2_colnames
Turnovers_df_PY2[,3:ncol(Turnovers_df_PY2)] <- Turnovers_df_PY2[,3:ncol(Turnovers_df_PY2)] %>% mutate_if(is.character, as.numeric)
Turnovers_df_PY2 <- Turnovers_df_PY2 %>%
  mutate(turnovers_pg_PY2 = total_turnovers_PY2 / gms) %>%
  select(team, turnovers_pg_PY2)
Turnovers_df_PY2[1,1] = "James Madison"

## bringing in advanced stats for G5 and non-Notre Dame Indy teams
Adv_Stats_PY2 <- cfbd_stats_season_advanced(2020, excl_garbage_time = FALSE, start_week = 1, end_week = 15) %>%
  filter(conference == "Mountain West" | conference == "Mid-American" | conference == "Conference USA" | conference == "Sun Belt" | conference == "American Athletic" | conference == "FBS Independents") %>%
  filter(team != "Notre Dame") %>%
  filter(team != "James Madison") %>%
  select(-one_of("season", "conference")) %>%
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

## making all stats columns numeric
Adv_Stats_PY2 <- Adv_Stats_PY2 %>% mutate_if(is.integer,as.numeric)

## computing means of each column and assigning them to James Madison
## I, uh, don't know how to add a row and set all the variable values as the means of the rest of the column
JMU_Adv_Stats_PY2 <- Adv_Stats_PY2 %>%
  filter(team == "Akron")
JMU_Adv_Stats_PY2[1,1] = "James Madison"
## making all stats columns numeric
JMU_Adv_Stats_PY2 <- JMU_Adv_Stats_PY2 %>% mutate_if(is.integer,as.numeric)
for (rating in 2:ncol(JMU_Adv_Stats_PY2)) {
  JMU_Adv_Stats_PY2[1,rating] = colMeans(Adv_Stats_PY2[,rating])
}

## renaming Advanced stats columns to reflect them being from PY2
colnames(JMU_Adv_Stats_PY2) <- c("team", "off_ppa_PY2", "off_success_rate_PY2", "off_explosiveness_PY2", "off_power_success_PY2",
                                 "off_stuff_rate_PY2", "off_line_yds_PY2", "off_second_lvl_yds_PY2", "off_open_field_yds_PY2",
                                 "off_pts_per_opp_PY2", "off_field_pos_avg_predicted_points_PY2", "off_havoc_total_PY2", 
                                 "off_havoc_front_seven_PY2", "off_havoc_db_PY2", "off_standard_downs_ppa_PY2",
                                 "off_standard_downs_success_rate_PY2", "off_standard_downs_explosiveness_PY2",
                                 "off_passing_downs_ppa_PY2", "off_passing_downs_success_rate_PY2",
                                 "off_passing_downs_explosiveness_PY2", "off_rushing_plays_ppa_PY2",
                                 "off_rushing_plays_success_rate_PY2", "off_rushing_plays_explosiveness_PY2",
                                 "off_passing_plays_ppa_PY2", "off_passing_plays_success_rate_PY2",
                                 "off_passing_plays_explosiveness_PY2", "def_ppa_PY2", "def_success_rate_PY2",
                                 "def_explosiveness_PY2", "def_power_success_PY2", "def_stuff_rate_PY2", "def_line_yds_PY2",
                                 "def_second_lvl_yds_PY2", "def_open_field_yds_PY2", "def_pts_per_opp_PY2", 
                                 "def_field_pos_avg_predicted_points_PY2", "def_havoc_total_PY2", "def_havoc_front_seven_PY2",
                                 "def_havoc_db_PY2", "def_standard_downs_ppa_PY2", "def_standard_downs_success_rate_PY2",
                                 "def_standard_downs_explosiveness_PY2", "def_passing_downs_ppa_PY2",
                                 "def_passing_downs_success_rate_PY2", "def_passing_downs_explosiveness_PY2",
                                 "def_rushing_plays_ppa_PY2", "def_rushing_plays_success_rate_PY2",
                                 "def_rushing_plays_explosiveness_PY2", "def_passing_plays_ppa_PY2",
                                 "def_passing_plays_success_rate_PY2", "def_passing_plays_explosiveness_PY2")

## adding SP+ rankings
SP_Rankings_PY2 <- cfbd_ratings_sp(year = 2020) %>%
  filter(conference == "Mountain West" | conference == "Mid-American" | conference == "Conference USA" | conference == "Sun Belt" | conference == "American Athletic" | conference == "FBS Independents") %>%
  filter(team != "Notre Dame") %>%
  filter(team != "nationalAverages") %>%
  select(-one_of("year"))
SP_Rankings_PY2 <- SP_Rankings_PY2[,c("team", "rating", "offense_rating", 
                                      "defense_rating", "special_teams_rating")]
SP_colnames_PY2 <- c("team", "sp_rating_PY2", "sp_offense_rating_PY2", "sp_defense_rating_PY2",
                     "sp_special_teams_rating_PY2")
colnames(SP_Rankings_PY2) <- SP_colnames_PY2

## computing means of each column and assigning them to James Madison
## I, uh, don't know how to add a row and set all the variable values as the means of the rest of the column
JMU_SP_PY2 <- SP_Rankings_PY2 %>%
  filter(team == "Akron")
JMU_SP_PY2[1,1] = "James Madison"
for (rating in 2:ncol(JMU_SP_PY2)) {
  JMU_SP_PY2[1,rating] = colMeans(SP_Rankings_PY2[,rating])
}

## pulling in FPI data
FPI_df_PY2 <- espn_ratings_fpi(year= 2020) %>%
  select(name, fpi)
## changing column names
FPI_colnames_PY2 <- c("team", "FPI_PY2")
colnames(FPI_df_PY2) <- FPI_colnames_PY2
## converting non-team character columns to numeric
FPI_df_PY2[,2] <- FPI_df_PY2[,2] %>% mutate_if(is.character,as.numeric)
## Changing team names in FPI df to match what appears in cfbfastR stats function
FPI_df_PY2 <- FPI_df_PY2 %>%
  mutate(school = case_when(team == 'Appalachian St' ~ 'Appalachian State',
                            team == 'C Michigan' ~ 'Central Michigan',
                            team == 'Coast Carolina' ~ 'Coastal Carolina',
                            team == 'Coastal Car' ~ 'Coastal Carolina',
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
                            team == 'Arizona St' ~ 'Arizona State',
                            team == 'Arkansas St' ~ 'Arkansas State',
                            team == 'Boise St' ~ 'Boise State',
                            team == 'Colorado St' ~ 'Colorado State',
                            team == 'Florida St' ~ 'Florida State',
                            team == 'Fresno St' ~ 'Fresno State',
                            team == 'Georgia St' ~ 'Georgia State',
                            team == 'Kansas St' ~ 'Kansas State',
                            team == 'Miami OH' ~ 'Miami (OH)',
                            team == 'Michigan St' ~ 'Michigan State',
                            team == 'Pitt' ~ 'Pittsburgh',
                            team == 'San Diego St' ~ 'San Diego State',
                            team == 'San José St' ~ 'San José State',
                            team == 'Texas St' ~ 'Texas State',
                            TRUE ~ team)) %>%
  select(school, FPI_PY2)
colnames(FPI_df_PY2) <- FPI_colnames_PY2

## joining conference to the FPI df to extract G5 and Non-ND Indy teams
Team_Conference <- cfbd_team_info() %>%
  select(school, conference) %>%
  filter(school != "James Madison")
colnames(Team_Conference) <- c("team", "conference")

## checking which teams are showing up in the FPI df but not teaminfo, then other way around
# missing_fpi_teams <- dplyr::anti_join(FPI_df_PY2, Team_Conference, by = "team")
# missing_teaminfo_teams <- dplyr::anti_join(Team_Conference, FPI_df_PY2, by = "team")

df_fpi_list <- list(FPI_df_PY2, Team_Conference)
FPI_df_PY2 <- df_fpi_list %>%
  reduce(full_join, by = "team") %>%
  filter(conference == "Mountain West" | conference == "Mid-American" | conference == "Conference USA" | conference == "Sun Belt" | conference == "American Athletic" | conference == "FBS Independents") %>%
  filter(team != "Notre Dame") %>%
  filter(team != "Connecticut") %>%
  filter(team != "Florida International") %>%
  filter(team != "New Mexico State") %>%
  filter(team != "Old Dominion") %>%
  select(team, FPI_PY2)

JMU_FPI_PY2 <- FPI_df_PY2 %>%
  filter(team == "Akron")
JMU_FPI_PY2[1,1] = "James Madison"
JMU_FPI_PY2[1,2] = mean(FPI_df_PY2$FPI_PY2)

## pulling in recruiting rankings
recruit_PY2 <- cfbd_recruiting_team(year = 2020, team = "James Madison") %>%
  select(team, points)
recruit_PY2[,2] <- recruit_PY2[,2] %>% mutate_if(is.character, as.numeric)
colnames(recruit_PY2) <- c("team", "recruit_pts_PY2")

## pulling in talent rankings
talent_df_PY2 <- cfbd_team_talent(year = 2020) %>%
  filter(school == "James Madison") %>%
  select(school, talent)
colnames(talent_df_PY2) <- c("team", "talent_PY2")

## merging all the PY2 data frames
df_PY2_list <- list(Ints_PY2, Fourth_Down_Off_PY2, FirstDown_PY2, Kick_Returns_PY2, PassOff_PY2, Penalties_PY2, PuntReturns_PY2, RushOffense_PY2, ThirdDown_PY2, TotalOffense_PY2,Turnovers_df_PY2, JMU_Adv_Stats_PY2, JMU_FPI_PY2, JMU_SP_PY2, recruit_PY2, talent_df_PY2)
JMU_PY2 <- df_PY2_list %>%
  reduce(full_join, by = "team") %>%
  mutate(season = 2022, .before = 1)

## getting rid of NAs
JMU_PY2[is.na(JMU_PY2)] = 0

## reading in 2021 CSVs
## starting with Bill Connelly's official SP+ data first
# does not include special teams, this will be taken from mean of SP data pulled from cfbfastR
FCS_SP_PY1 <- read_csv(here("Data", "VoA2022", "JamesMadisonPrevYears", "FCS_SP2021.csv")) %>%
  filter(team == "James Madison")
colnames(FCS_SP_PY1) <- c("team", "conference", "record", "sp_rating_PY1", "rk", "percentile", "sp_offense_rating_PY1", "rank", "sp_defense_rating_PY1", "rankd")
FCS_SP_PY1 <- FCS_SP_PY1 %>%
  select(team, sp_rating_PY1, sp_offense_rating_PY1, sp_defense_rating_PY1)

## defensive interceptions
Ints_PY1 <- read_csv(here("Data", "VoA2022", "JamesMadisonPrevYears", "FCSDefInts2021.csv")) %>%
  filter(Team == "James Madison (CAA)")
Ints_PY1_ColNames <- c("Rk_Delete", "team", "games_PY1", "Win_Loss", "Opp_Comps_PY1", "INTs", "INT_Yds", "INT_TDs")
colnames(Ints_PY1) <- Ints_PY1_ColNames
Ints_PY1 <- Ints_PY1 %>%
  mutate(conference = "Sun Belt", .before = 2) %>%
  separate(col = Win_Loss, into = c("Wins_PY1", "Losses_PY1"), sep = "-")
Ints_PY1[,4:ncol(Ints_PY1)] <- Ints_PY1[,4:ncol(Ints_PY1)] %>% mutate_if(is.character, as.numeric)
Ints_PY1 <- Ints_PY1 %>%
  mutate(def_interceptions_pg_PY1 = INTs / games_PY1) %>%
  select(team, conference, games_PY1, Wins_PY1, Losses_PY1, def_interceptions_pg_PY1)
Ints_PY1[1,1] = "James Madison"

## offensive 4th down stats
Fourth_Down_Off_PY1 <- read_csv(here("Data", "VoA2022", "JamesMadisonPrevYears", "FCSFourthDownOff2021.csv")) %>%
  filter(Team == "James Madison (CAA)")
Fourth_Down_Off_PY1_colnames <- c("rk_delete", "team", "games", "Win_Loss", "fourth_down_convs_PY1", "fourth_downs_PY1 ", "fourth_conv_rate_PY1")
colnames(Fourth_Down_Off_PY1) <- Fourth_Down_Off_PY1_colnames
Fourth_Down_Off_PY1 <- Fourth_Down_Off_PY1 %>%
  select(team, fourth_conv_rate_PY1)
Fourth_Down_Off_PY1[1,1] = "James Madison"

Kick_Returns_PY1 <- read_csv(here("Data", "VoA2022", "JamesMadisonPrevYears", "FCSKickReturns2021.csv")) %>%
  filter(Team == "James Madison (CAA)")
Kick_Returns_PY1_colnames <- c("rk_delete", "team", "games","win_loss", "Returns", "Return_Yds", "Return_TDs", "kick_return_avg_PY1")
colnames(Kick_Returns_PY1) <- Kick_Returns_PY1_colnames
Kick_Returns_PY1 <- Kick_Returns_PY1 %>%
  select(team, kick_return_avg_PY1)
Kick_Returns_PY1[1,1] = "James Madison"

PassOff_PY1 <- read_csv(here("Data", "VoA2022", "JamesMadisonPrevYears", "FCSPassOffense2021.csv")) %>%
  filter(Team == "James Madison (CAA)")
PassOff_PY1_colnames <- c("rk_del", "team", "games", "win_loss", "pass_atts_PY1", "pass_comps_PY1", "INTs", "Total_Pass_Yds", "pass_ypa_PY1", "pass_ypr_PY1", "Pass_TD", "pass_yds_pg_PY1")
colnames(PassOff_PY1) <- PassOff_PY1_colnames
PassOff_PY1[,5:ncol(PassOff_PY1)] <- PassOff_PY1[,5:ncol(PassOff_PY1)] %>% mutate_if(is.character,as.numeric)
PassOff_PY1 <- PassOff_PY1 %>%
  mutate(int_pct_PY1 = INTs / pass_atts_PY1) %>%
  mutate(completion_pct_PY1 = pass_comps_PY1 / pass_atts_PY1) %>%
  select(team, int_pct_PY1, pass_ypa_PY1, pass_ypr_PY1, pass_yds_pg_PY1, completion_pct_PY1)
PassOff_PY1[1,1] = "James Madison"

FirstDown_PY1 <- read_csv(here("Data", "VoA2022", "JamesMadisonPrevYears", "FCSFirstDowns2021.csv")) %>%
  filter(Team == "James Madison (CAA)")
FirstDown_PY1_colnames <- c("del", "team", "games", "win_loss", "run_fd", "pass_fd", "pen_fd", "total_fd")
colnames(FirstDown_PY1) <- FirstDown_PY1_colnames
FirstDown_PY1[,3:ncol(FirstDown_PY1)] <- FirstDown_PY1[,3:ncol(FirstDown_PY1)] %>% mutate_if(is.character, as.numeric)
FirstDown_PY1 <- FirstDown_PY1 %>%
  mutate(first_downs_pg_PY1 = total_fd / games) %>%
  select(team, first_downs_pg_PY1)
FirstDown_PY1[1,1] = "James Madison"

Penalties_PY1 <- read_csv(here("Data", "VoA2022", "JamesMadisonPrevYears", "FCSPenalties2021.csv")) %>%
  filter(Team == "James Madison (CAA)")
Penalties_PY1_colnames <- c("delete", "team", "games", "win_loss", "penalties", "penalty_yds", "penalty_yds_pg_PY1")
colnames(Penalties_PY1) <- Penalties_PY1_colnames
Penalties_PY1[,5:ncol(Penalties_PY1)] <- Penalties_PY1[,5:ncol(Penalties_PY1)] %>% mutate_if(is.character, as.numeric)
Penalties_PY1 <- Penalties_PY1 %>%
  mutate(yards_per_penalty_PY1 = penalty_yds / penalties) %>%
  select(team, penalty_yds_pg_PY1, yards_per_penalty_PY1)
Penalties_PY1[1,1] = "James Madison"

PuntReturns_PY1 <- read_csv(here("Data", "VoA2022", "JamesMadisonPrevYears", "FCSPuntReturns2021.csv")) %>%
  filter(Team == "James Madison (CAA)")
PuntReturns_PY1_colnames <- c("delete", "team", "games", "win_loss", "punt_returns", "return_yds", "return_tds", "punt_return_avg_PY1")
colnames(PuntReturns_PY1) <- PuntReturns_PY1_colnames
PuntReturns_PY1 <- PuntReturns_PY1 %>%
  select(team, punt_return_avg_PY1)
PuntReturns_PY1[1,1] = "James Madison"

RushOffense_PY1 <- read_csv(here("Data", "VoA2022", "JamesMadisonPrevYears", "FCSRushOffense2021.csv")) %>%
  filter(Team == "James Madison (CAA)")
RushOffense_PY1_colnames <- c("delete", "team", "games", "win_loss", "rush_atts", "rush_yds", "rush_ypc_PY1", "rush_td", "rush_yds_pg_PY1")
colnames(RushOffense_PY1) <- RushOffense_PY1_colnames
RushOffense_PY1 <- RushOffense_PY1 %>%
  select(team, rush_ypc_PY1, rush_yds_pg_PY1)
RushOffense_PY1[1,1] = "James Madison"

ThirdDown_PY1 <- read_csv(here("Data", "VoA2022", "JamesMadisonPrevYears", "FCSThirdDownOff2021.csv")) %>%
  filter(Team == "James Madison (CAA)")
ThirdDown_PY1_colnames <- c("del", "team", "gms", "wl", "third_atts", "third_convs", "third_conv_rate_PY1")
colnames(ThirdDown_PY1) <- ThirdDown_PY1_colnames
ThirdDown_PY1 <- ThirdDown_PY1 %>%
  select(team, third_conv_rate_PY1)
ThirdDown_PY1[1,1] = "James Madison"

## Time of Possession commented out because I don't think I'm going to use it in the VoA
# ToP_PY1 <- read_csv(here("Data", "VoA2022", "JamesMadisonPrevYears", "FCSToP2021.csv")) %>%
#   filter(Team == "James Madison (CAA)")
# ToP_PY1_colnames <- c("del", "team", "gms", "Wl", "total_top", "time_of_poss_pg_temp")
# colnames(ToP_PY1) <- ToP_PY1_colnames
# ToP_PY1 <- ToP_PY1 %>%
#   separate(time_of_poss_pg_temp, into = c("mins", "secs", "extra"), sep = ":")
# ToP_PY1[,6:ncol(ToP_PY1)] <- ToP_PY1[,6:ncol(ToP_PY1)] %>% mutate_if(is.character, as.numeric)
# ToP_PY1 <- ToP_PY1 %>%
#   mutate(time_of_poss_pg_PY1 = mins + (secs / 60)) %>%
#   select(team, time_of_poss_pg_PY1)
# ToP_PY1[1,1] = "James Madison"

TotalOffense_PY1 <- read_csv(here("Data", "VoA2022", "JamesMadisonPrevYears", "FCSTotalOffense2021.csv")) %>%
  filter(Team == "James Madison (CAA)")
TotalOffense_PY1_colnames <- c("del", "team", "gms", "wl", "plays", "totalyds", "off_ypp_PY1", "tds", "total_yds_pg_PY1")
colnames(TotalOffense_PY1) <- TotalOffense_PY1_colnames
TotalOffense_PY1 <- TotalOffense_PY1 %>%
  select(team, off_ypp_PY1, total_yds_pg_PY1)
TotalOffense_PY1[1,1] = "James Madison"

Turnovers_df_PY1 <- read_csv(here("Data", "VoA2022", "JamesMadisonPrevYears", "FCSTurnoversLost2021.csv")) %>%
  filter(Team == "James Madison (CAA)")
Turnovers_PY1_colnames <- c("del", "team", "gms", "wl", "fum", "int", "total_turnovers_PY1")
colnames(Turnovers_df_PY1) <- Turnovers_PY1_colnames
Turnovers_df_PY1[,3:ncol(Turnovers_df_PY1)] <- Turnovers_df_PY1[,3:ncol(Turnovers_df_PY1)] %>% mutate_if(is.character, as.numeric)
Turnovers_df_PY1 <- Turnovers_df_PY1 %>%
  mutate(turnovers_pg_PY1 = total_turnovers_PY1 / gms) %>%
  select(team, turnovers_pg_PY1)
Turnovers_df_PY1[1,1] = "James Madison"

## bringing in advanced stats for G5 and non-Notre Dame Indy teams
Adv_Stats_PY1 <- cfbd_stats_season_advanced(2021, excl_garbage_time = FALSE, start_week = 1, end_week = 15) %>%
  filter(conference == "Mountain West" | conference == "Mid-American" | conference == "Conference USA" | conference == "Sun Belt" | conference == "American Athletic" | conference == "FBS Independents") %>%
  filter(team != "Notre Dame") %>%
  filter(team != "James Madison") %>%
  select(-one_of("season", "conference")) %>%
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

## making all stats columns numeric
Adv_Stats_PY1 <- Adv_Stats_PY1 %>% mutate_if(is.integer,as.numeric)

## computing means of each column and assigning them to James Madison
## I, uh, don't know how to add a row and set all the variable values as the means of the rest of the column
JMU_Adv_Stats_PY1 <- Adv_Stats_PY1 %>%
  filter(team == "Akron")
JMU_Adv_Stats_PY1[1,1] = "James Madison"
## making all stats columns numeric
JMU_Adv_Stats_PY1 <- JMU_Adv_Stats_PY1 %>% mutate_if(is.integer,as.numeric)
for (rating in 2:ncol(JMU_Adv_Stats_PY1)) {
  JMU_Adv_Stats_PY1[1,rating] = colMeans(Adv_Stats_PY1[,rating])
}

## renaming Advanced stats columns to reflect them being from PY1
colnames(JMU_Adv_Stats_PY1) <- c("team", "off_ppa_PY1", "off_success_rate_PY1", "off_explosiveness_PY1", "off_power_success_PY1",
                                 "off_stuff_rate_PY1", "off_line_yds_PY1", "off_second_lvl_yds_PY1", "off_open_field_yds_PY1",
                                 "off_pts_per_opp_PY1", "off_field_pos_avg_predicted_points_PY1", "off_havoc_total_PY1", 
                                 "off_havoc_front_seven_PY1", "off_havoc_db_PY1", "off_standard_downs_ppa_PY1",
                                 "off_standard_downs_success_rate_PY1", "off_standard_downs_explosiveness_PY1",
                                 "off_passing_downs_ppa_PY1", "off_passing_downs_success_rate_PY1",
                                 "off_passing_downs_explosiveness_PY1", "off_rushing_plays_ppa_PY1",
                                 "off_rushing_plays_success_rate_PY1", "off_rushing_plays_explosiveness_PY1",
                                 "off_passing_plays_ppa_PY1", "off_passing_plays_success_rate_PY1",
                                 "off_passing_plays_explosiveness_PY1", "def_ppa_PY1", "def_success_rate_PY1",
                                 "def_explosiveness_PY1", "def_power_success_PY1", "def_stuff_rate_PY1", "def_line_yds_PY1",
                                 "def_second_lvl_yds_PY1", "def_open_field_yds_PY1", "def_pts_per_opp_PY1", 
                                 "def_field_pos_avg_predicted_points_PY1", "def_havoc_total_PY1", "def_havoc_front_seven_PY1",
                                 "def_havoc_db_PY1", "def_standard_downs_ppa_PY1", "def_standard_downs_success_rate_PY1",
                                 "def_standard_downs_explosiveness_PY1", "def_passing_downs_ppa_PY1",
                                 "def_passing_downs_success_rate_PY1", "def_passing_downs_explosiveness_PY1",
                                 "def_rushing_plays_ppa_PY1", "def_rushing_plays_success_rate_PY1",
                                 "def_rushing_plays_explosiveness_PY1", "def_passing_plays_ppa_PY1",
                                 "def_passing_plays_success_rate_PY1", "def_passing_plays_explosiveness_PY1")

## adding SP+ rankings
SP_Rankings_PY1 <- cfbd_ratings_sp(year = 2021) %>%
  filter(conference == "Mountain West" | conference == "Mid-American" | conference == "Conference USA" | conference == "Sun Belt" | conference == "American Athletic" | conference == "FBS Independents") %>%
  filter(team != "Notre Dame") %>%
  filter(team != "nationalAverages") %>%
  select(-one_of("year"))
SP_Rankings_PY1 <- SP_Rankings_PY1[,c("team", "rating", "offense_rating", 
                                      "defense_rating", "special_teams_rating")]
SP_colnames_PY1 <- c("team", "sp_rating_PY1", "sp_offense_rating_PY1", "sp_defense_rating_PY1",
                     "sp_special_teams_rating_PY1")
colnames(SP_Rankings_PY1) <- SP_colnames_PY1
SP_Rankings_PY1 <- SP_Rankings_PY1 %>%
  select(team, sp_special_teams_rating_PY1)

## computing means of each column and assigning them to James Madison
## I, uh, don't know how to add a row and set all the variable values as the means of the rest of the column
JMU_SP_PY1 <- SP_Rankings_PY1 %>%
  filter(team == "Akron")
JMU_SP_PY1[1,1] = "James Madison"
## all of the SP + special teams values are NA, so I'm making the JMU value 0
JMU_SP_PY1[1,2] = 0

## pulling in FPI data
FPI_df_PY1 <- espn_ratings_fpi(year= 2021) %>%
  select(name, fpi)
## changing column names
FPI_colnames_PY1 <- c("team", "FPI_PY1")
colnames(FPI_df_PY1) <- FPI_colnames_PY1
## converting non-team character columns to numeric
FPI_df_PY1[,2] <- FPI_df_PY1[,2] %>% mutate_if(is.character,as.numeric)
## Changing team names in FPI df to match what appears in cfbfastR stats function
FPI_df_PY1 <- FPI_df_PY1 %>%
  mutate(school = case_when(team == 'Appalachian St' ~ 'Appalachian State',
                            team == 'C Michigan' ~ 'Central Michigan',
                            team == 'Coast Carolina' ~ 'Coastal Carolina',
                            team == 'Coastal Car' ~ 'Coastal Carolina',
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
                            team == 'Arizona St' ~ 'Arizona State',
                            team == 'Arkansas St' ~ 'Arkansas State',
                            team == 'Boise St' ~ 'Boise State',
                            team == 'Colorado St' ~ 'Colorado State',
                            team == 'Florida St' ~ 'Florida State',
                            team == 'Fresno St' ~ 'Fresno State',
                            team == 'Georgia St' ~ 'Georgia State',
                            team == 'Kansas St' ~ 'Kansas State',
                            team == 'Miami OH' ~ 'Miami (OH)',
                            team == 'Michigan St' ~ 'Michigan State',
                            team == 'Pitt' ~ 'Pittsburgh',
                            team == 'San Diego St' ~ 'San Diego State',
                            team == 'San José St' ~ 'San José State',
                            team == 'Texas St' ~ 'Texas State',
                            TRUE ~ team)) %>%
  select(school, FPI_PY1)
colnames(FPI_df_PY1) <- FPI_colnames_PY1

## joining conference to the FPI df to extract G5 and Non-ND Indy teams
Team_Conference <- cfbd_team_info() %>%
  select(school, conference) %>%
  filter(school != "James Madison")
colnames(Team_Conference) <- c("team", "conference")

## checking which teams are showing up in the FPI df but not teaminfo, then other way around
# missing_fpi_teams <- dplyr::anti_join(FPI_df_PY1, Team_Conference, by = "team")
# missing_teaminfo_teams <- dplyr::anti_join(Team_Conference, FPI_df_PY1, by = "team")

df_fpi_list <- list(FPI_df_PY1, Team_Conference)
FPI_df_PY1 <- df_fpi_list %>%
  reduce(full_join, by = "team") %>%
  filter(conference == "Mountain West" | conference == "Mid-American" | conference == "Conference USA" | conference == "Sun Belt" | conference == "American Athletic" | conference == "FBS Independents") %>%
  filter(team != "Notre Dame") %>%
  filter(team != "Florida International") %>%
  select(team, FPI_PY1)

JMU_FPI_PY1 <- FPI_df_PY1 %>%
  filter(team == "Akron")
JMU_FPI_PY1[1,1] = "James Madison"
JMU_FPI_PY1[1,2] = mean(FPI_df_PY1$FPI_PY1)

## pulling in recruiting rankings
recruit_PY1 <- cfbd_recruiting_team(year = 2021, team = "James Madison") %>%
  select(team, points)
recruit_PY1[,2] <- recruit_PY1[,2] %>% mutate_if(is.character, as.numeric)
colnames(recruit_PY1) <- c("team", "recruit_pts_PY1")

## pulling in talent rankings
talent_df_PY1 <- cfbd_team_talent(year = 2021) %>%
  filter(school == "James Madison") %>%
  select(school, talent)
colnames(talent_df_PY1) <- c("team", "talent_PY1")

## merging all the PY1 data frames
df_PY1_list <- list(Ints_PY1, Fourth_Down_Off_PY1, FirstDown_PY1, Kick_Returns_PY1, PassOff_PY1, Penalties_PY1, PuntReturns_PY1, RushOffense_PY1, ThirdDown_PY1, TotalOffense_PY1,Turnovers_df_PY1, JMU_Adv_Stats_PY1, JMU_FPI_PY1, JMU_SP_PY1, FCS_SP_PY1, recruit_PY1, talent_df_PY1)
JMU_PY1 <- df_PY1_list %>%
  reduce(full_join, by = "team") %>%
  mutate(season = 2022, .before = 1)

## current recruiting rankings
# JMU_recruit <- cfbd_recruiting_team(year = as.numeric(year)) %>%
#   filter(team == "James Madison") %>%
#   select(team, points)
# recruit[,2] <- recruit[,2] %>% mutate_if(is.character, as.numeric)
# colnames(JMU_recruit) <- c("team", "recruit_pts")

## getting columns in proper order, then merging them all together in the forms they'll be used in during the VoA
JMU_PY3 <- JMU_PY3 %>%
  select(season, team, conference, games_PY3, Wins_PY3, Losses_PY3, completion_pct_PY3, 
         pass_ypa_PY3, pass_ypr_PY3, int_pct_PY3, rush_ypc_PY3, turnovers_pg_PY3, 
         third_conv_rate_PY3, fourth_conv_rate_PY3, penalty_yds_pg_PY3, 
         yards_per_penalty_PY3, kick_return_avg_PY3, punt_return_avg_PY3, total_yds_pg_PY3, 
         pass_yds_pg_PY3, rush_yds_pg_PY3, first_downs_pg_PY3, off_ypp_PY3, def_interceptions_pg_PY3, off_ppa_PY3, 
         off_success_rate_PY3, off_explosiveness_PY3, off_power_success_PY3, off_stuff_rate_PY3, 
         off_line_yds_PY3, off_second_lvl_yds_PY3, off_open_field_yds_PY3, off_pts_per_opp_PY3, 
         off_field_pos_avg_predicted_points_PY3, off_havoc_total_PY3, off_havoc_front_seven_PY3, 
         off_havoc_db_PY3, off_standard_downs_ppa_PY3, off_standard_downs_success_rate_PY3, 
         off_standard_downs_explosiveness_PY3, off_passing_downs_ppa_PY3, 
         off_passing_downs_success_rate_PY3, off_passing_downs_explosiveness_PY3, 
         off_rushing_plays_ppa_PY3, off_rushing_plays_success_rate_PY3, 
         off_rushing_plays_explosiveness_PY3, off_passing_plays_ppa_PY3, 
         off_passing_plays_success_rate_PY3, off_passing_plays_explosiveness_PY3, def_ppa_PY3, 
         def_success_rate_PY3, def_explosiveness_PY3, def_power_success_PY3, def_stuff_rate_PY3, 
         def_line_yds_PY3, def_second_lvl_yds_PY3, def_open_field_yds_PY3, def_pts_per_opp_PY3, 
         def_field_pos_avg_predicted_points_PY3, def_havoc_total_PY3, def_havoc_front_seven_PY3, 
         def_havoc_db_PY3, def_standard_downs_ppa_PY3, def_standard_downs_success_rate_PY3, 
         def_standard_downs_explosiveness_PY3, def_passing_downs_ppa_PY3, 
         def_passing_downs_success_rate_PY3, def_passing_downs_explosiveness_PY3, 
         def_rushing_plays_ppa_PY3, def_rushing_plays_success_rate_PY3, 
         def_rushing_plays_explosiveness_PY3, def_passing_plays_ppa_PY3, 
         def_passing_plays_success_rate_PY3, def_passing_plays_explosiveness_PY3, 
         recruit_pts_PY3, talent_PY3, sp_rating_PY3, sp_offense_rating_PY3, 
         sp_defense_rating_PY3, sp_special_teams_rating_PY3, FPI_PY3)

## setting up PY2 df to be merged with PY3
## so no need for conference column
JMU_PY2_premerge <- JMU_PY2 %>%
  select(team, games_PY2, Wins_PY2, Losses_PY2, completion_pct_PY2, 
         pass_ypa_PY2, pass_ypr_PY2, int_pct_PY2, rush_ypc_PY2, turnovers_pg_PY2, 
         third_conv_rate_PY2, fourth_conv_rate_PY2, penalty_yds_pg_PY2, 
         yards_per_penalty_PY2, kick_return_avg_PY2, punt_return_avg_PY2, total_yds_pg_PY2, 
         pass_yds_pg_PY2, rush_yds_pg_PY2, first_downs_pg_PY2, off_ypp_PY2, def_interceptions_pg_PY2, off_ppa_PY2, 
         off_success_rate_PY2, off_explosiveness_PY2, off_power_success_PY2, off_stuff_rate_PY2, 
         off_line_yds_PY2, off_second_lvl_yds_PY2, off_open_field_yds_PY2, off_pts_per_opp_PY2, 
         off_field_pos_avg_predicted_points_PY2, off_havoc_total_PY2, off_havoc_front_seven_PY2, 
         off_havoc_db_PY2, off_standard_downs_ppa_PY2, off_standard_downs_success_rate_PY2, 
         off_standard_downs_explosiveness_PY2, off_passing_downs_ppa_PY2, 
         off_passing_downs_success_rate_PY2, off_passing_downs_explosiveness_PY2, 
         off_rushing_plays_ppa_PY2, off_rushing_plays_success_rate_PY2, 
         off_rushing_plays_explosiveness_PY2, off_passing_plays_ppa_PY2, 
         off_passing_plays_success_rate_PY2, off_passing_plays_explosiveness_PY2, def_ppa_PY2, 
         def_success_rate_PY2, def_explosiveness_PY2, def_power_success_PY2, def_stuff_rate_PY2, 
         def_line_yds_PY2, def_second_lvl_yds_PY2, def_open_field_yds_PY2, def_pts_per_opp_PY2, 
         def_field_pos_avg_predicted_points_PY2, def_havoc_total_PY2, def_havoc_front_seven_PY2, 
         def_havoc_db_PY2, def_standard_downs_ppa_PY2, def_standard_downs_success_rate_PY2, 
         def_standard_downs_explosiveness_PY2, def_passing_downs_ppa_PY2, 
         def_passing_downs_success_rate_PY2, def_passing_downs_explosiveness_PY2, 
         def_rushing_plays_ppa_PY2, def_rushing_plays_success_rate_PY2, 
         def_rushing_plays_explosiveness_PY2, def_passing_plays_ppa_PY2, 
         def_passing_plays_success_rate_PY2, def_passing_plays_explosiveness_PY2, 
         recruit_pts_PY2, talent_PY2, sp_rating_PY2, sp_offense_rating_PY2, 
         sp_defense_rating_PY2, sp_special_teams_rating_PY2, FPI_PY2)

## Setting up JMU_PY1 to be merged with either all years or just PY2
JMU_PY1_premerge <- JMU_PY1 %>%
  select(team, games_PY1, Wins_PY1, Losses_PY1, completion_pct_PY1, 
         pass_ypa_PY1, pass_ypr_PY1, int_pct_PY1, rush_ypc_PY1, turnovers_pg_PY1, 
         third_conv_rate_PY1, fourth_conv_rate_PY1, penalty_yds_pg_PY1, 
         yards_per_penalty_PY1, kick_return_avg_PY1, punt_return_avg_PY1, total_yds_pg_PY1, 
         pass_yds_pg_PY1, rush_yds_pg_PY1, first_downs_pg_PY1, off_ypp_PY1, def_interceptions_pg_PY1, off_ppa_PY1, 
         off_success_rate_PY1, off_explosiveness_PY1, off_power_success_PY1, off_stuff_rate_PY1, 
         off_line_yds_PY1, off_second_lvl_yds_PY1, off_open_field_yds_PY1, off_pts_per_opp_PY1, 
         off_field_pos_avg_predicted_points_PY1, off_havoc_total_PY1, off_havoc_front_seven_PY1, 
         off_havoc_db_PY1, off_standard_downs_ppa_PY1, off_standard_downs_success_rate_PY1, 
         off_standard_downs_explosiveness_PY1, off_passing_downs_ppa_PY1, 
         off_passing_downs_success_rate_PY1, off_passing_downs_explosiveness_PY1, 
         off_rushing_plays_ppa_PY1, off_rushing_plays_success_rate_PY1, 
         off_rushing_plays_explosiveness_PY1, off_passing_plays_ppa_PY1, 
         off_passing_plays_success_rate_PY1, off_passing_plays_explosiveness_PY1, def_ppa_PY1, 
         def_success_rate_PY1, def_explosiveness_PY1, def_power_success_PY1, def_stuff_rate_PY1, 
         def_line_yds_PY1, def_second_lvl_yds_PY1, def_open_field_yds_PY1, def_pts_per_opp_PY1, 
         def_field_pos_avg_predicted_points_PY1, def_havoc_total_PY1, def_havoc_front_seven_PY1, 
         def_havoc_db_PY1, def_standard_downs_ppa_PY1, def_standard_downs_success_rate_PY1, 
         def_standard_downs_explosiveness_PY1, def_passing_downs_ppa_PY1, 
         def_passing_downs_success_rate_PY1, def_passing_downs_explosiveness_PY1, 
         def_rushing_plays_ppa_PY1, def_rushing_plays_success_rate_PY1, 
         def_rushing_plays_explosiveness_PY1, def_passing_plays_ppa_PY1, 
         def_passing_plays_success_rate_PY1, def_passing_plays_explosiveness_PY1, 
         recruit_pts_PY1, talent_PY1, sp_rating_PY1, sp_offense_rating_PY1, 
         sp_defense_rating_PY1, sp_special_teams_rating_PY1, FPI_PY1)

## merging all years data frames
all_years_df_list <- list(JMU_PY3, JMU_PY2_premerge, JMU_PY1_premerge)
JMU_AllYears <- all_years_df_list %>%
  reduce(full_join, by = "team")
## saving as csv
write_csv(JMU_AllYears, here("Data", "VoA2022", "JamesMadisonPrevYears", "JMU_AllYears.csv"))

## setting up JMU_PY2 to be lead data frame
JMU_PY2 <- JMU_PY2 %>%
  select(team, games_PY2, Wins_PY2, Losses_PY2, completion_pct_PY2, 
         pass_ypa_PY2, pass_ypr_PY2, int_pct_PY2, rush_ypc_PY2, turnovers_pg_PY2, 
         third_conv_rate_PY2, fourth_conv_rate_PY2, penalty_yds_pg_PY2, 
         yards_per_penalty_PY2, kick_return_avg_PY2, punt_return_avg_PY2, total_yds_pg_PY2, 
         pass_yds_pg_PY2, rush_yds_pg_PY2, first_downs_pg_PY2, off_ypp_PY2, def_interceptions_pg_PY2, off_ppa_PY2, 
         off_success_rate_PY2, off_explosiveness_PY2, off_power_success_PY2, off_stuff_rate_PY2, 
         off_line_yds_PY2, off_second_lvl_yds_PY2, off_open_field_yds_PY2, off_pts_per_opp_PY2, 
         off_field_pos_avg_predicted_points_PY2, off_havoc_total_PY2, off_havoc_front_seven_PY2, 
         off_havoc_db_PY2, off_standard_downs_ppa_PY2, off_standard_downs_success_rate_PY2, 
         off_standard_downs_explosiveness_PY2, off_passing_downs_ppa_PY2, 
         off_passing_downs_success_rate_PY2, off_passing_downs_explosiveness_PY2, 
         off_rushing_plays_ppa_PY2, off_rushing_plays_success_rate_PY2, 
         off_rushing_plays_explosiveness_PY2, off_passing_plays_ppa_PY2, 
         off_passing_plays_success_rate_PY2, off_passing_plays_explosiveness_PY2, def_ppa_PY2, 
         def_success_rate_PY2, def_explosiveness_PY2, def_power_success_PY2, def_stuff_rate_PY2, 
         def_line_yds_PY2, def_second_lvl_yds_PY2, def_open_field_yds_PY2, def_pts_per_opp_PY2, 
         def_field_pos_avg_predicted_points_PY2, def_havoc_total_PY2, def_havoc_front_seven_PY2, 
         def_havoc_db_PY2, def_standard_downs_ppa_PY2, def_standard_downs_success_rate_PY2, 
         def_standard_downs_explosiveness_PY2, def_passing_downs_ppa_PY2, 
         def_passing_downs_success_rate_PY2, def_passing_downs_explosiveness_PY2, 
         def_rushing_plays_ppa_PY2, def_rushing_plays_success_rate_PY2, 
         def_rushing_plays_explosiveness_PY2, def_passing_plays_ppa_PY2, 
         def_passing_plays_success_rate_PY2, def_passing_plays_explosiveness_PY2, 
         recruit_pts_PY2, talent_PY2, sp_rating_PY2, sp_offense_rating_PY2, 
         sp_defense_rating_PY2, sp_special_teams_rating_PY2, FPI_PY2)

## merging previous 2 years data frames, to be used later in season when PY3 no longer included
two_years_df_list <- list(JMU_PY2, JMU_PY1_premerge)
JMU_2Years <- two_years_df_list %>%
  reduce(full_join, by = "team")
## saving as csv
write_csv(JMU_2Years, here("Data", "VoA2022", "JamesMadisonPrevYears", "JMU_2Years.csv"))

## setting up PY1 to match column order of VoA_Variables in main VoA
JMU_PY1 <- JMU_PY1 %>%
  select(team, games_PY1, Wins_PY1, Losses_PY1, completion_pct_PY1, 
         pass_ypa_PY1, pass_ypr_PY1, int_pct_PY1, rush_ypc_PY1, turnovers_pg_PY1, 
         third_conv_rate_PY1, fourth_conv_rate_PY1, penalty_yds_pg_PY1, 
         yards_per_penalty_PY1, kick_return_avg_PY1, punt_return_avg_PY1, total_yds_pg_PY1, 
         pass_yds_pg_PY1, rush_yds_pg_PY1, first_downs_pg_PY1, off_ypp_PY1, def_interceptions_pg_PY1, off_ppa_PY1, 
         off_success_rate_PY1, off_explosiveness_PY1, off_power_success_PY1, off_stuff_rate_PY1, 
         off_line_yds_PY1, off_second_lvl_yds_PY1, off_open_field_yds_PY1, off_pts_per_opp_PY1, 
         off_field_pos_avg_predicted_points_PY1, off_havoc_total_PY1, off_havoc_front_seven_PY1, 
         off_havoc_db_PY1, off_standard_downs_ppa_PY1, off_standard_downs_success_rate_PY1, 
         off_standard_downs_explosiveness_PY1, off_passing_downs_ppa_PY1, 
         off_passing_downs_success_rate_PY1, off_passing_downs_explosiveness_PY1, 
         off_rushing_plays_ppa_PY1, off_rushing_plays_success_rate_PY1, 
         off_rushing_plays_explosiveness_PY1, off_passing_plays_ppa_PY1, 
         off_passing_plays_success_rate_PY1, off_passing_plays_explosiveness_PY1, def_ppa_PY1, 
         def_success_rate_PY1, def_explosiveness_PY1, def_power_success_PY1, def_stuff_rate_PY1, 
         def_line_yds_PY1, def_second_lvl_yds_PY1, def_open_field_yds_PY1, def_pts_per_opp_PY1, 
         def_field_pos_avg_predicted_points_PY1, def_havoc_total_PY1, def_havoc_front_seven_PY1, 
         def_havoc_db_PY1, def_standard_downs_ppa_PY1, def_standard_downs_success_rate_PY1, 
         def_standard_downs_explosiveness_PY1, def_passing_downs_ppa_PY1, 
         def_passing_downs_success_rate_PY1, def_passing_downs_explosiveness_PY1, 
         def_rushing_plays_ppa_PY1, def_rushing_plays_success_rate_PY1, 
         def_rushing_plays_explosiveness_PY1, def_passing_plays_ppa_PY1, 
         def_passing_plays_success_rate_PY1, def_passing_plays_explosiveness_PY1, 
         recruit_pts_PY1, talent_PY1, sp_rating_PY1, sp_offense_rating_PY1, 
         sp_defense_rating_PY1, sp_special_teams_rating_PY1, FPI_PY1)

# PY1_current_recruit_list <- list(JMU_PY1, JMU_recruit)
# JMU_PY1 <- PY1_current_recruit_list %>%
#   reduce(full_join, by = "team")

## saving PY1 as csv, to be used when PY3 and PY2 no longer included in VoA
write_csv(JMU_PY1, here("Data", "VoA2022", "JamesMadisonPrevYears", "JMU_PrevYear.csv"))
