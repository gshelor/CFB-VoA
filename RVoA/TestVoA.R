##### script includes assortment of testing options for data collection, analysis, visualization, etc
## test code for accessing cfb data API
pacman::p_load(tidyverse, matrixStats, grid, gridExtra, gt, viridis, 
               webshot, writexl, rvest, cfbfastR, espnscrapeR, openxlsx, 
               here, ggsci, RColorBrewer, ggpubr, gtExtras)

## testing readline function with cfbfastR pkg
week <- readline(prompt = "What week is it? ")
year <- readline(prompt = "What year is it? ")

## test line using readline function to set end week for cfbfastR stats pull in function
# going to change this later, to read in whole 2018 season
## test_2018_allteam_stats <- cfbd_stats_season_team(2018, start_week = 1, end_week = as.integer(week))

# recruiting_2020 <- cfbd_recruiting_team(year = 2020)
# str(recruiting_2020)
# head(recruiting_2020)

## going to try to access CFBdataAPI using cfbfastR
## pull in data frames of season variables, sort and rank as normal VoA
## Scraping current season data with cfbfastR
Stats <- cfbd_stats_season_team(year = as.integer(year), start_week = 1, end_week = as.integer(week))
# replacing NA values in data frame
Stats[is.na(Stats)] = 0

## regular stats have error in rate stat columns (comp_pct, yards_per_attempt, etc)
# it's the exact same number for each team
# this is obviously not accurate
Stats$time_of_poss_pg = Stats$time_of_poss_total/(Stats$games * 15 * 60* 4)
Stats$completion_pct = Stats$pass_comps/Stats$pass_atts
Stats$pass_ypa = Stats$net_pass_yds/Stats$pass_atts
Stats$pass_ypr = Stats$net_pass_yds/Stats$pass_comps
Stats$int_pct = Stats$interceptions/Stats$pass_atts
Stats$rush_ypc = Stats$rush_yds/Stats$rush_atts
Stats$turnovers_pg = Stats$turnovers/Stats$games
Stats$third_conv_rate = Stats$third_down_convs/Stats$third_downs
Stats$fourth_conv_rate = Stats$fourth_down_convs/Stats$fourth_downs
Stats$penalties_pg = Stats$penalties/Stats$games
Stats$penalty_yds_pg = Stats$penalty_yds/Stats$games
Stats$yards_per_penalty = Stats$penalty_yds/Stats$penalties
Stats$kick_return_avg = Stats$kick_return_yds/Stats$kick_returns
Stats$punt_return_avg = Stats$punt_return_yds/Stats$punt_returns
## adding in per_game stats for offensive yards
Stats <- Stats %>%
  mutate(total_yds_pg = total_yds/games) %>%
  mutate(pass_yds_pg = net_pass_yds/games) %>%
  mutate(rush_yds_pg = rush_yds/games) %>%
  mutate(first_downs_pg = first_downs/games) %>%
  mutate(def_interceptions_pg = passes_intercepted/games)
### why aren't defensive stats included in the regular stat cfbfastR function

## making all stats columns numeric
## Stats[,4:ncol(Stats)] <- sapply(Stats[,4:ncol(Stats)], as.numeric)
## Trying different method for converting to numeric
Stats <- Stats %>% mutate_if(is.integer,as.numeric)

## now bringing in advanced stats
Adv_Stats <- cfbd_stats_season_advanced(year = as.integer(year), excl_garbage_time = FALSE, start_week = 1, end_week = as.numeric(week))

## checking which teams are showing up for regular stats but not advanced
missing_teams <- dplyr::anti_join(Stats, Adv_Stats, by = "team")

## Pulling in adv_stats for teams not in original Adv_Stats df
Adv_Stats_AirForce <- cfbd_stats_season_advanced(year = as.integer(year), team = "Air Force" , excl_garbage_time = FALSE, start_week = 1, end_week = as.numeric(week) + 1)
Adv_Stats_Buffalo <- cfbd_stats_season_advanced(year = as.integer(year), team = "Buffalo" , excl_garbage_time = FALSE, start_week = 1, end_week = as.numeric(week) + 1)
Adv_Stats_CCU <- cfbd_stats_season_advanced(year = as.integer(year), team = "Coastal Carolina" , excl_garbage_time = FALSE, start_week = 1, end_week = as.numeric(week) + 1)
Adv_Stats_CSU <- cfbd_stats_season_advanced(year = as.integer(year), team = "Colorado State" , excl_garbage_time = FALSE, start_week = 1, end_week = as.numeric(week) + 1)
Adv_Stats_EMU <- cfbd_stats_season_advanced(year = as.integer(year), team = "Eastern Michigan" , excl_garbage_time = FALSE, start_week = 1, end_week = as.numeric(week) + 1)
Adv_Stats_GeorgiaSouthern <- cfbd_stats_season_advanced(year = as.integer(year), team = "Georgia Southern" , excl_garbage_time = FALSE, start_week = 1, end_week = as.numeric(week) + 1)
Adv_Stats_GeorgiaTech <- cfbd_stats_season_advanced(year = as.integer(year), team = "Georgia Tech" , excl_garbage_time = FALSE, start_week = 1, end_week = as.numeric(week) + 1)
Adv_Stats_MTSU <- cfbd_stats_season_advanced(year = as.integer(year), team = "Middle Tennessee" , excl_garbage_time = FALSE, start_week = 1, end_week = 3)
Adv_Stats_NM <- cfbd_stats_season_advanced(year = as.integer(year), team = "New Mexico" , excl_garbage_time = FALSE, start_week = 1, end_week = as.numeric(week) + 1)
Adv_Stats_NIU <- cfbd_stats_season_advanced(year = as.integer(year), team = "Northern Illinois" , excl_garbage_time = FALSE, start_week = 1, end_week = as.numeric(week) + 1)
Adv_Stats_Ohio <- cfbd_stats_season_advanced(year = as.integer(year), team = "Ohio" , excl_garbage_time = FALSE, start_week = 1, end_week = as.numeric(week) + 1)
Adv_Stats_ODU <- cfbd_stats_season_advanced(year = as.integer(year), team = "Old Dominion" , excl_garbage_time = FALSE, start_week = 1, end_week = as.numeric(week) + 1)
Adv_Stats_SJSU <- cfbd_stats_season_advanced(year = as.integer(year), team = "San José State" , excl_garbage_time = FALSE, start_week = 1, end_week = 3)
Adv_Stats_Syracuse <- cfbd_stats_season_advanced(year = as.integer(year), team = "Syracuse" , excl_garbage_time = FALSE, start_week = 1, end_week = as.numeric(week) + 1)
Adv_Stats_TCU <- cfbd_stats_season_advanced(year = as.integer(year), team = "TCU" , excl_garbage_time = FALSE, start_week = 1, end_week = as.numeric(week) + 1)
Adv_Stats_Tulsa <- cfbd_stats_season_advanced(year = as.integer(year), team = "Tulsa" , excl_garbage_time = FALSE, start_week = 1, end_week = as.numeric(week) + 1)
Adv_Stats_UNLV <- cfbd_stats_season_advanced(year = as.integer(year), team = "UNLV" , excl_garbage_time = FALSE, start_week = 1, end_week = as.numeric(week) + 1)
Adv_Stats_USC <- cfbd_stats_season_advanced(year = as.integer(year), team = "USC" , excl_garbage_time = FALSE, start_week = 1, end_week = as.numeric(week) + 1)
Adv_Stats_Vandy <- cfbd_stats_season_advanced(year = as.integer(year), team = "Vanderbilt" , excl_garbage_time = FALSE, start_week = 1, end_week = as.numeric(week) + 1)
Adv_Stats_Wake <- cfbd_stats_season_advanced(year = as.integer(year), team = "Wake Forest" , excl_garbage_time = FALSE, start_week = 1, end_week = as.numeric(week) + 1)

## now that I have Adv Stats data for teams missing from original week 1 search, adding team-specific dfs as new rows to Adv_Stats df
Adv_Stats <- rbind(Adv_Stats, Adv_Stats_AirForce)
Adv_Stats <- rbind(Adv_Stats, Adv_Stats_Buffalo)
Adv_Stats <- rbind(Adv_Stats, Adv_Stats_CCU)
Adv_Stats <- rbind(Adv_Stats, Adv_Stats_CSU)
Adv_Stats <- rbind(Adv_Stats, Adv_Stats_EMU)
Adv_Stats <- rbind(Adv_Stats, Adv_Stats_GeorgiaSouthern)
Adv_Stats <- rbind(Adv_Stats, Adv_Stats_GeorgiaTech)
Adv_Stats <- rbind(Adv_Stats, Adv_Stats_MTSU)
Adv_Stats <- rbind(Adv_Stats, Adv_Stats_NIU)
Adv_Stats <- rbind(Adv_Stats, Adv_Stats_NM)
Adv_Stats <- rbind(Adv_Stats, Adv_Stats_ODU)
Adv_Stats <- rbind(Adv_Stats, Adv_Stats_Ohio)
Adv_Stats <- rbind(Adv_Stats, Adv_Stats_SJSU)
Adv_Stats <- rbind(Adv_Stats, Adv_Stats_Syracuse)
Adv_Stats <- rbind(Adv_Stats, Adv_Stats_TCU)
Adv_Stats <- rbind(Adv_Stats, Adv_Stats_Tulsa)
Adv_Stats <- rbind(Adv_Stats, Adv_Stats_UNLV)
Adv_Stats <- rbind(Adv_Stats, Adv_Stats_USC)
Adv_Stats <- rbind(Adv_Stats, Adv_Stats_Vandy)
Adv_Stats <- rbind(Adv_Stats, Adv_Stats_Wake)

## removing season and conference columns, already in regular stat dataframe
Adv_Stats <- Adv_Stats %>%
  select(-one_of("season", "conference"))

## making all stats columns numeric
## Adv_Stats[,2:ncol(Adv_Stats)] <- sapply(Adv_Stats[,2:ncol(Adv_Stats)], as.numeric)
## above line not working for some fucking reason, trying a different solution
Adv_Stats <- Adv_Stats %>% mutate_if(is.integer,as.numeric)

## merging stat tables, will rank afterwards
VoA_Variables_Test <- merge(Stats, Adv_Stats, by = "team")

## Eliminating NAs
VoA_Variables_Test[is.na(VoA_Variables_Test)] = 0

## adding SP+ rankings
SP_Rankings <- cfbd_ratings_sp(2021)
SP_Rankings <- SP_Rankings %>%
  select(-one_of("year", "conference"))
SP_Rankings <- SP_Rankings[,c("team", "rating", "offense_rating", 
                                        "defense_rating", "special_teams_rating")]
SP_colnames <- c("team", "sp_rating", "sp_offense_rating", "sp_defense_rating",
                 "sp_special_teams_rating")
colnames(SP_Rankings) <- SP_colnames

## dropping "nationalAverages" row from SP+ data
SP_Rankings <- SP_Rankings %>%
  dplyr::filter(team != "nationalAverages")

## Eliminating NAs
SP_Rankings[is.na(SP_Rankings)] = 0

## Making SP_Rankings non-team column numeric data
## SP_Rankings[,2:ncol(SP_Rankings)] <- sapply(SP_Rankings[,2:ncol(SP_Rankings)], as.numeric)
SP_Rankings <- SP_Rankings %>% mutate_if(is.integer,as.numeric)

## checking which teams are showing up for regular stats but not advanced
missing_SP_teams <- dplyr::anti_join(VoA_Variables_Test, SP_Rankings, by = "team")
missing_VoATest_teams <- dplyr::anti_join(SP_Rankings, VoA_Variables_Test, by = "team")

## Testing pulling in FPI data
FPI <- espn_ratings_fpi(year=2021)

## Merging stats with SP data
# VoA_Variables_Test <- merge(VoA_Variables_Test, SP_Rankings, by = "team")
## above line not working for some reason, trying different solution
df_list <- list(VoA_Variables_Test, SP_Rankings)
VoA_Variables_Test <- df_list %>%
  reduce(full_join, by = "team")


## list of variables to be ranked
# "completion_pct","pass_ypa","pass_ypr","int_pct","rush_ypc","turnovers_pg",
# "third_conv_rate","fourth_conv_rate","penalties_pg","penalty_yds_pg",
# "yards_per_penalty","kick_return_avg","punt_return_avg","total_yds_pg",
# "pass_yds_pg","rush_yds_pg","first_downs_pg","def_interceptions_pg","off_ppa",
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

## updating data frame to only include season, team, conference, games, and rank columns
VoA_Variables_Test <- VoA_Variables_Test[,c("team","season","conference","games","completion_pct",
                          "pass_ypa","pass_ypr","int_pct","rush_ypc","turnovers_pg",
                          "third_conv_rate","fourth_conv_rate","penalties_pg","penalty_yds_pg",
                          "yards_per_penalty","kick_return_avg","punt_return_avg","total_yds_pg",
                          "pass_yds_pg","rush_yds_pg","first_downs_pg","def_interceptions_pg","off_ppa",
                          "off_success_rate","off_explosiveness","off_power_success","off_stuff_rate",
                          "off_line_yds","off_second_lvl_yds","off_open_field_yds","off_pts_per_opp",
                          "off_field_pos_avg_predicted_points","off_havoc_total","off_havoc_front_seven",
                          "off_havoc_db","off_standard_downs_ppa","off_standard_downs_success_rate",
                          "off_standard_downs_explosiveness","off_passing_downs_ppa",
                          "off_passing_downs_success_rate","off_passing_downs_explosiveness",
                          "off_rushing_plays_ppa","off_rushing_plays_success_rate",
                          "off_rushing_plays_explosiveness","off_passing_plays_ppa",
                          "off_passing_plays_success_rate","off_passing_plays_explosiveness","def_ppa",
                          "def_success_rate","def_explosiveness","def_power_success","def_stuff_rate",
                          "def_line_yds","def_second_lvl_yds","def_open_field_yds","def_pts_per_opp",
                          "def_field_pos_avg_predicted_points","def_havoc_total","def_havoc_front_seven",
                          "def_havoc_db","def_standard_downs_ppa","def_standard_downs_success_rate",
                          "def_standard_downs_explosiveness","def_passing_downs_ppa",
                          "def_passing_downs_success_rate","def_passing_downs_explosiveness",
                          "def_rushing_plays_ppa","def_rushing_plays_success_rate",
                          "def_rushing_plays_explosiveness","def_passing_plays_ppa",
                          "def_passing_plays_success_rate","def_passing_plays_explosiveness",
                          "sp_rating","sp_offense_rating","sp_defense_rating","sp_special_teams_rating")]

## adding rank columns
VoA_Variables_Test <- VoA_Variables_Test %>%
  mutate(Rank_Comp_Pct = dense_rank(desc(completion_pct))) %>%
  mutate(Rank_Pass_YPA = dense_rank(desc(pass_ypa))) %>%
  mutate(Rank_Pass_YPR = dense_rank(desc(pass_ypr))) %>%
  mutate(Rank_Int_Pct = dense_rank(int_pct)) %>%
  mutate(Rank_Rush_YPC = dense_rank(desc(rush_ypc))) %>%
  mutate(Rank_Turnovers_pg = dense_rank(turnovers_pg)) %>%
  mutate(Rank_Third_Conv_Rate = dense_rank(desc(third_conv_rate))) %>%
  mutate(Rank_Fourth_Conv_Rate = dense_rank(desc(fourth_conv_rate))) %>%
  mutate(Rank_Penalties_pg = dense_rank(penalties_pg)) %>%
  mutate(Rank_Penalty_Yds_pg = dense_rank(penalty_yds_pg)) %>%
  mutate(Rank_Yds_Per_Penalty = dense_rank(yards_per_penalty)) %>%
  mutate(Rank_Kick_Return_Avg = dense_rank(desc(kick_return_avg))) %>%
  mutate(Rank_Punt_Return_Avg = dense_rank(desc(punt_return_avg))) %>%
  mutate(Rank_Total_Yds_pg = dense_rank(desc(total_yds_pg))) %>%
  mutate(Rank_Pass_Yds_pg = dense_rank(desc(pass_yds_pg))) %>%
  mutate(Rank_Rush_Yds_pg = dense_rank(desc(rush_yds_pg))) %>%
  mutate(Rank_First_Downs_pg = dense_rank(desc(first_downs_pg))) %>%
  mutate(Rank_Def_Ints_pg = dense_rank(desc(def_interceptions_pg))) %>%
  mutate(Rank_Off_PPA = dense_rank(desc(off_ppa))) %>%
  mutate(Rank_Off_Success_Rt = dense_rank(desc(off_success_rate))) %>%
  mutate(Rank_Off_Explosiveness = dense_rank(desc(off_explosiveness))) %>%
  mutate(Rank_Off_Pwr_Success = dense_rank(desc(off_power_success))) %>%
  mutate(Rank_Off_Stuff_Rt = dense_rank(off_stuff_rate)) %>%
  mutate(Rank_Off_Line_Yds = dense_rank(desc(off_line_yds))) %>%
  mutate(Rank_Off_Second_Lvl_Yds = dense_rank(desc(off_second_lvl_yds))) %>%
  mutate(Rank_Off_Open_Field_Yds = dense_rank(desc(off_open_field_yds))) %>%
  mutate(Rank_Off_Pts_Per_Opp = dense_rank(desc(off_pts_per_opp))) %>%
  mutate(Rank_Off_Field_Pos_Avg_Predicted_Pts = dense_rank(desc(off_field_pos_avg_predicted_points))) %>%
  mutate(Rank_Off_Havoc_Total = dense_rank(off_havoc_total)) %>%
  mutate(Rank_Off_Havoc_Front = dense_rank(off_havoc_front_seven)) %>%
  mutate(Rank_Off_Havoc_DB = dense_rank(off_havoc_db)) %>%
  mutate(Rank_Off_Standard_Down_PPA = dense_rank(desc(off_standard_downs_ppa))) %>%
  mutate(Rank_Off_Standard_Down_Success_Rt = dense_rank(desc(off_standard_downs_success_rate))) %>%
  mutate(Rank_Off_Standard_Down_Explosiveness = dense_rank(desc(off_standard_downs_explosiveness))) %>%
  mutate(Rank_Off_Pass_Down_PPA = dense_rank(desc(off_passing_downs_ppa))) %>%
  mutate(Rank_Off_Pass_Down_Success_Rt = dense_rank(desc(off_passing_downs_success_rate))) %>%
  mutate(Rank_Off_Pass_Down_Explosiveness = dense_rank(desc(off_passing_downs_explosiveness))) %>%
  mutate(Rank_Off_Rush_Play_PPA = dense_rank(desc(off_rushing_plays_ppa))) %>%
  mutate(Rank_Off_Rush_Play_Success_Rt = dense_rank(desc(off_rushing_plays_success_rate))) %>%
  mutate(Rank_Off_Rush_Play_Explosiveness = dense_rank(desc(off_rushing_plays_explosiveness))) %>%
  mutate(Rank_Off_Pass_Play_PPA = dense_rank(desc(off_passing_plays_ppa))) %>%
  mutate(Rank_Off_Pass_Play_Success_Rt = dense_rank(desc(off_passing_plays_success_rate))) %>%
  mutate(Rank_Off_Pass_Play_Explosiveness = dense_rank(desc(off_passing_plays_explosiveness))) %>%
  mutate(Rank_Def_PPA = dense_rank(def_ppa)) %>%
  mutate(Rank_Def_Success_Rt = dense_rank(def_success_rate)) %>%
  mutate(Rank_Def_Explosiveness = dense_rank(def_explosiveness)) %>%
  mutate(Rank_Def_Pwr_Success = dense_rank(def_power_success)) %>%
  mutate(Rank_Def_Stuff_Rt = dense_rank(desc(def_stuff_rate))) %>%
  mutate(Rank_Def_Line_Yds = dense_rank(def_line_yds)) %>%
  mutate(Rank_Def_Second_Lvl_Yds = dense_rank(def_second_lvl_yds)) %>%
  mutate(Rank_Def_Open_Field_Yds = dense_rank(def_open_field_yds)) %>%
  mutate(Rank_Def_Pts_Per_Opp = dense_rank(def_pts_per_opp)) %>%
  mutate(Rank_Def_Field_Pos_Avg_Predicted_Pts = dense_rank(def_field_pos_avg_predicted_points)) %>%
  mutate(Rank_Def_Havoc_Total = dense_rank(desc(def_havoc_total))) %>%
  mutate(Rank_Def_Havoc_Front_Seven = dense_rank(desc(def_havoc_front_seven))) %>%
  mutate(Rank_Def_Havoc_DB = dense_rank(desc(def_havoc_db))) %>%
  mutate(Rank_Def_Standard_Down_PPA = dense_rank(def_standard_downs_ppa)) %>%
  mutate(Rank_Def_Standard_Down_Success_Rt = dense_rank(def_standard_downs_success_rate)) %>%
  mutate(Rank_Def_Standard_Down_Explosiveness = dense_rank(def_standard_downs_explosiveness)) %>%
  mutate(Rank_Def_Pass_Down_PPA = dense_rank(def_passing_downs_ppa)) %>%
  mutate(Rank_Def_Pass_Down_Success_Rt = dense_rank(def_passing_downs_success_rate)) %>%
  mutate(Rank_Def_Pass_Down_Explosiveness = dense_rank(def_passing_downs_explosiveness)) %>%
  mutate(Rank_Def_Rush_Play_PPA = dense_rank(def_rushing_plays_ppa)) %>%
  mutate(Rank_Def_Rush_Play_Success_Rt = dense_rank(def_rushing_plays_success_rate)) %>%
  mutate(Rank_Def_Rush_Play_Explosiveness = dense_rank(def_rushing_plays_explosiveness)) %>%
  mutate(Rank_Def_Pass_Play_PPA = dense_rank(def_passing_plays_ppa)) %>%
  mutate(Rank_Def_Pass_Play_Success_Rt = dense_rank(def_passing_plays_success_rate)) %>%
  mutate(Rank_Def_Pass_Play_Explosiveness = dense_rank(def_passing_plays_explosiveness)) %>%
  mutate(Rank_SP_Rating = dense_rank(desc(sp_rating))) %>%
  mutate(Rank_SP_Off_Rating = dense_rank(desc(sp_offense_rating))) %>%
  mutate(Rank_SP_Def_Rating = dense_rank(sp_defense_rating))
  ## mutate(Rank_SP_SpecialTeams_Rating = dense_rank(desc(sp_special_teams_rating)))




## Append new column of Model output, which is the mean of all variables in VoARanks
VoA_Variables_Test <- VoA_Variables_Test %>%
  mutate(VoA_Output = (rowMeans(VoA_Variables_Test[,77:ncol(VoA_Variables_Test)])))
## Append column of VoA Final Rankings
VoA_Variables_Test <- VoA_Variables_Test %>%
  mutate(VoA_Ranking = dense_rank(VoA_Output))

## Adding Column with CFB Week number
# same number for each team, numeric version of number input in readline function at beginning of script
VoA_Variables_Test <- VoA_Variables_Test %>%
  mutate(CFB_Week = rep(as.numeric(week), nrow(VoA_Variables_Test)))

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
    footnote = "Data from CFB Data API and ESPN.com via cfbfastR"
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
    footnote = "Data from CFB Data API and ESPN.com via cfbfastR"
  )
VoA_Full_Table
VoA_Full_Table %>%
  gtsave(
    fulltable_file_pathway, expand = 5,
    path = here("RVoA", "Outputs", "Test")
  )


