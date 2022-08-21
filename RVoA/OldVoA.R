## The Vortex of Accuracy, Version 4.1.9
## Supremely Excellent Yet Salaciously Godlike And Infallibly Magnificent Vortex of Accuracy
## Created by Griffin Shelor
## installing packages
# install.packages(c("devtoools", "tidyverse", "matrixStats", "grid", "gridExtra", "gt", "viridis", "webshot", "writexl", "rvest", "cfbfastR", "espnscrapeR", "openxlsx", "here", "ggsci", "RColorBrewer", "ggpubr", "remotes", "pacman", "gtExtras"))
## Load Packages for Ranking Variables
library(pacman)
pacman::p_load(tidyverse, matrixStats, grid, gridExtra, gt, viridis, 
               webshot, writexl, rvest, cfbfastR, espnscrapeR, openxlsx, 
               here, ggsci, RColorBrewer, ggpubr, gtExtras)

## Creating Week and Year String for Top 25 Table Title, eventually could be used as part of reading in cfbfastR/cfbdata API data
year <- readline(prompt = "What year is it? ")
week <- readline(prompt = "What week is it? ")

## storing urls for scraping as text, will be used as variables throughout scraping portion of script
OffUrl <- ""
Off_Pass_Url <- ""
Off_Rush_Url <- ""
DownsUrl <- ""
DefUrl <- ""
PassDefUrl <- ""
RushDefUrl <- ""
ReturnUrl <- ""
KickingUrl <- ""
FPIResumeURL <- ""
FPIEfficiencyURL <- ""
Off_PY_Url <- ""
Off_PY_Pass_Url <- ""
Off_PY_Rush_Url <- ""
Down_PYUrl <- ""
Def_PY_Url <- ""
PassDef_PY_Url <- ""
RushDef_PY_Url <- ""
Return_PYUrl <- ""
Kicking_PYUrl <- ""
FPI_PY_ResumeURL <- ""
FPI_PY_EfficiencyURL <- ""
Off_2019Url <- ""
Off_2019_Pass_Url <- ""
Off_2019_Rush_Url <- ""
Down_2019Url <- ""
Def_2019_Url <- ""
PassDef_2019_Url <- ""
RushDef_2019_Url <- ""
Return_2019Url <- ""
Kicking_2019Url <- ""
FPI_2019_ResumeURL <- ""
FPI_2019_EfficiencyURL <- ""


## starting if statement for reading in data
if (as.numeric(week) <= 4) {
  ## scraping ESPN's website with Thomas Mock's snippet from rvest found on twitter
  
  ## Offensive Points and Yards Stats
  ## Team comes in as its own table for some reason, scraping team and stats separately
  OffStatsTeams <- OffUrl %>% read_html() %>% html_table() %>% .[[1]]
  ## head(OffStatsTeams)
  OffStatsTable <- OffUrl %>% read_html() %>% html_table() %>% .[[2]]
  ## head(OffStatsTable[,1:4])
  ## merging team and stats tables
  OffStatsTable <- cbind(OffStatsTeams, OffStatsTable)
  ## head(OffStatsTable[,1:4])
  ## column names are off, appear as first row instead, renaming column names
  NewOffColNames <- c("Team", "Games", "Total_Yds", "Total_Yds_pg",
                      "Pass_Yds", "Pass_Yds_pg", "Rush_Yds", "Rush_Yds_pg",
                      "Total_Pts", "Pts_pg")
  colnames(OffStatsTable) <- NewOffColNames
  colnames(OffStatsTable)
  ## head(OffStatsTable[,1:4])
  OffStatsTable <- OffStatsTable[-c(1),]
  ## head(OffStatsTable[,1:4])
  
  ## some columns have commas
  # must get rid of these using gsub() function
  OffStatsTable$Total_Yds = gsub(",","", OffStatsTable$Total_Yds)
  
  ## Selecting only variables which will be used for VoA
  ## eliminating sum stats, only keeping per-game stats
  ## keeping total yards and points in this table for a "yards per play" stat later on
  OffStatsTable <- OffStatsTable[,c("Team", "Games", "Total_Yds","Total_Yds_pg",
                                    "Pass_Yds_pg", "Rush_Yds_pg", "Total_Pts",
                                    "Total_Pts", "Pts_pg")]
  
  
  ## Offensive Passing Stats
  ## Team comes in as its own table for some reason, scraping team and stats separately
  Off_Pass_Stats_Teams <- Off_Pass_Url %>% read_html() %>% html_table() %>% .[[1]]
  ## head(Off_Pass_Stats_Teams)
  Off_Pass_Stats_Table <- Off_Pass_Url %>% read_html() %>% html_table() %>% .[[2]]
  ## head(Off_Pass_Stats_Table[,1:4])
  ## merging team and stats tables
  Off_Pass_Stats_Table <- cbind(Off_Pass_Stats_Teams, Off_Pass_Stats_Table)
  ## head(Off_Pass_Stats_Table[,1:4])
  ## column names are off, appear as first row instead, renaming column names
  New_Off_Pass_ColNames <- c("Team", "Games", "Completions", "Attempts", "Comp_Pct", 
                             "Total_Pass_Yds", "Pass_YPA", "Pass_YPG",
                             "Longest_Pass", "Pass_TD", "INTs_Thrown", 
                             "Off_Sacks_Allowed", "Off_Sack_Yds_Lost", "QB_RTG")
  colnames(Off_Pass_Stats_Table) <- New_Off_Pass_ColNames
  colnames(Off_Pass_Stats_Table)
  ## head(Off_Pass_Stats_Table[,1:4])
  ## for some reason, total off and def stats pages include row containing column labels when scraped from ESPN's website
  # passing and rush pages do not do this, so no need to remove top column
  ## Off_Pass_Stats_Table <- Off_Pass_Stats_Table[-c(1),]
  ## head(Off_Pass_Stats_Table[,1:4])
  
  ## some columns have commas
  # must get rid of these using gsub() function
  Off_Pass_Stats_Table$Total_Pass_Yds = gsub(",","", Off_Pass_Stats_Table$Total_Pass_Yds)
  ## changing Off_Pass_Stats_Table to have numeric values for non-team columns
  # need to do this early to add yds_per_completion
  Off_Pass_Stats_Table[,2:ncol(Off_Pass_Stats_Table)] <- sapply(Off_Pass_Stats_Table[,2:ncol(Off_Pass_Stats_Table)], as.numeric)
  ## Adding offensive YPC, normalizing INTs and Sack stats columns
  Off_Pass_Stats_Table <- Off_Pass_Stats_Table %>%
    mutate(Off_YPC = Total_Pass_Yds/Completions) %>%
    mutate(Off_INTs_pg = INTs_Thrown/Games) %>%
    mutate(Off_Sacks_Allowed_pg = Off_Sacks_Allowed/Games) %>%
    mutate(Off_Sack_Yds_Lost_pg = Off_Sack_Yds_Lost/Games)
  
  ## Selecting only variables which will be used for VoA
  ## eliminating sum stats, only keeping per-game stats, games already in Off_stats
  ## keeping attempts stat for a yards per play column later on
  Off_Pass_Stats_Table <- Off_Pass_Stats_Table[,c("Team", "Attempts","Comp_Pct", 
                                                  "Total_Pass_Yds", "Pass_YPA", 
                                                  "Off_YPC", "Off_INTs_pg", 
                                                  "Off_Sacks_Allowed_pg", 
                                                  "Off_Sack_Yds_Lost_pg",
                                                  "Pass_TD", "INTs_Thrown")]
  
  
  
  ## Offensive Rushing Stats
  
  ## Team comes in as its own table for some reason, scraping team and stats separately
  Off_Rush_Stats_Teams <- Off_Rush_Url %>% read_html() %>% html_table() %>% .[[1]]
  ## head(Off_Rush_Stats_Teams)
  Off_Rush_Stats_Table <- Off_Rush_Url %>% read_html() %>% html_table() %>% .[[2]]
  ## head(Off_Rush_Stats_Table[,1:4])
  ## merging team and stats tables
  Off_Rush_Stats_Table <- cbind(Off_Rush_Stats_Teams, Off_Rush_Stats_Table)
  ## head(Off_Rush_Stats_Table[,1:4])
  ## column names are off, appear as first row instead, renaming column names
  New_Off_Rush_ColNames <- c("Team", "Games", "Rush_Atts", "Total_Rush_Yds", 
                             "Off_Rush_YPA", "Off_Rush_Yds_pg", "Off_Longest_Rush", 
                             "Rush_TDs")
  colnames(Off_Rush_Stats_Table) <- New_Off_Rush_ColNames
  colnames(Off_Rush_Stats_Table)
  ## head(Off_Rush_Stats_Table[,1:4])
  ## for some reason, total off and def stats pages include row containing column labels when scraped from ESPN's website
  # passing and rush pages do not do this, so no need to remove top column
  ## Off_Rush_Stats_Table <- Off_Rush_Stats_Table[-c(1),]
  ## head(Off_Rush_Stats_Table[,1:4])
  
  ## Selecting only variables which will be used for VoA
  ## eliminating sum stats, only keeping per-game stats, games already in Off_stats
  Off_Rush_Stats_Table <- Off_Rush_Stats_Table[,c("Team", "Rush_Atts","Off_Rush_YPA")]
  
  
  
  ## Defensive Yards and Points Stats
  DefStatsTeams <- DefUrl %>% read_html() %>% html_table() %>% .[[1]]
  ## head(DefStatsTeams)
  DefStatsTable <- DefUrl %>% read_html() %>% html_table() %>% .[[2]]
  ## head(DefStatsTable[,1:4])
  DefStatsTable <- cbind(DefStatsTeams, DefStatsTable)
  ## head(DefStatsTable[,1:4])
  ## column names are off, appear as first row instead, renaming column names
  NewDefColNames <- c("Team", "Games", "Def_Total_Yds", "Def_Total_Yds_pg",
                      "Def_Pass_Yds", "Def_Pass_Yds_pg", "Def_Rush_Yds", 
                      "Def_Rush_Yds_pg", "Def_Points", "Def_Pts_pg")
  colnames(DefStatsTable) <- NewDefColNames
  colnames(DefStatsTable)
  ## head(DefStatsTable)
  DefStatsTable <- DefStatsTable[-c(1),]
  ## head(DefStatsTable[,1:4])
  
  ## some columns have commas
  # must get rid of these using gsub() function
  DefStatsTable$Def_Total_Yds = gsub(",","", DefStatsTable$Def_Total_Yds)
  
  ## Selecting only variables which will be used for VoA
  ## eliminating sum stats, only keeping per-game stats
  ## kept games played in offensive stats table, don't need to leave it for other tables
  ## keeping total yards, points stats for yards/points per play stats later on
  DefStatsTable <- DefStatsTable[,c("Team", "Def_Total_Yds","Def_Total_Yds_pg",
                                    "Def_Pass_Yds_pg", 
                                    "Def_Rush_Yds_pg","Def_Points", "Def_Pts_pg")]
  
  
  ## Special Teams kickoff and punt return stats
  ReturnStatsTeams <- ReturnUrl %>% read_html() %>% html_table() %>% .[[1]]
  ## head(ReturnStatsTeams)
  ReturnStatsTable <- ReturnUrl %>% read_html() %>% html_table() %>% .[[2]]
  ## head(ReturnStatsTable[,1:4])
  ReturnStatsTable <- cbind(ReturnStatsTeams, ReturnStatsTable)
  ## head(ReturnStatsTable[,1:4])
  ## column names are off, appear as first row instead, renaming column names
  NewReturnColNames <- c("Team", "Games", "Kick_Return_Attempts", 
                         "Total_Kick_Return_Yds","Kick_Return_Yd_Avg", 
                         "Longest_Kick_Return", "Kick_Return_TD", 
                         "Punt_Return_Attempts", "Punt_Return_Yds", 
                         "Punt_Return_Yd_Avg", "Longest_Punt_Return", 
                         "Punt_Return_TD")
  colnames(ReturnStatsTable) <- NewReturnColNames
  colnames(ReturnStatsTable)
  ## head(ReturnStatsTable[,1:4])
  ReturnStatsTable <- ReturnStatsTable[-c(1),]
  
  ## changing ReturnStatsTable to have numeric values for non-team columns
  # need to do this early to normalized kick and punt return columns
  ReturnStatsTable[,2:ncol(ReturnStatsTable)] <- sapply(ReturnStatsTable[,2:ncol(ReturnStatsTable)], as.numeric)
  ## changing fumbles forced column to a per-game stat to avoid biasing towards teams which played more game
  ReturnStatsTable <- ReturnStatsTable %>%
    dplyr::mutate(Kick_Return_TD_pg = Kick_Return_TD/Games) %>%
    dplyr::mutate(Punt_Return_TD_pg = Punt_Return_TD/Games)
  ## head(ReturnStatsTable[,1:4])
  
  ## Selecting only variables which will be used for VoA
  ## eliminating sum stats, only keeping per-game stats
  ## kept games played in offensive stats table, don't need to leave it for other tables
  ReturnStatsTable <- ReturnStatsTable[,c("Team", "Kick_Return_Yd_Avg", 
                                          "Kick_Return_TD_pg", "Punt_Return_Yd_Avg", 
                                          "Punt_Return_TD_pg")]
  
  ## Special Teams kicking stats
  KickingStatsTeams <- KickingUrl %>% read_html() %>% html_table() %>% .[[1]]
  ## head(KickingStatsTeams)
  KickingStatsTable <- KickingUrl %>% read_html() %>% html_table() %>% .[[2]]
  ## head(KickingStatsTable[,1:4])
  KickingStatsTable <- cbind(KickingStatsTeams, KickingStatsTable)
  ## head(KickingStatsTable[,1:4])
  ## column names are off, appear as first row instead, renaming column names
  NewKickingColNames <- c("Team", "Games", "FGM", "FGA","FG_Percent", 
                          "Longest_FG", "FG_1_19_Yds", "FG_20_29_Yds", 
                          "FG_30_39_Yds", "FG_40_49_Yds", "FG_50_Plus_Yds", 
                          "Extra_Pts_Made", "Extra_Pts_Attempted", 
                          "Extra_Pts_Percent")
  colnames(KickingStatsTable) <- NewKickingColNames
  colnames(KickingStatsTable)
  ## head(KickingStatsTable[,1:4])
  KickingStatsTable <- KickingStatsTable[-c(1),]
  ## head(KickingStatsTable[,1:4])
  
  ## Selecting only variables which will be used for VoA
  ## eliminating sum stats, only keeping per-game stats
  ## kept games played in offensive stats table, don't need to leave it for other tables
  KickingStatsTable <- KickingStatsTable[,c("Team", "FG_Percent",
                                            "Extra_Pts_Percent")]
  
  ## Downs stats
  DownStatsTeams <- DownsUrl %>% read_html() %>% html_table() %>% .[[1]]
  ## head(DownStatsTeams)
  DownStatsTable <- DownsUrl %>% read_html() %>% html_table() %>% .[[2]]
  ## head(DownStatsTable[,1:4])
  DownStatsTable <- cbind(DownStatsTeams, DownStatsTable)
  ## head(DownStatsTable[,1:4])
  ## column names are off, appear as first row instead, renaming column names
  NewDownColNames <- c("Team", "Games", "Total_First_Downs", "Rush_First_Downs",
                       "Pass_First_Downs", "Penalty_First_Downs", "Third_Down_Made", 
                       "Third_Down_Attempted", "Third_Down_Percent", 
                       "Fourth_Down_Made", "Fourth_Down_Attempt", "Fourth_Down_Percent", 
                       "Penalties", "Penalty_Yds")
  colnames(DownStatsTable) <- NewDownColNames
  colnames(DownStatsTable)
  ## head(DownStatsTable[,1:4])
  ## Top row is just original espn column names repeated
  DownStatsTable <- DownStatsTable[-c(1),]
  
  ## some columns have commas
  # removing commas
  DownStatsTable$Penalty_Yds = gsub(",","", DownStatsTable$Penalty_Yds)
  
  ## changing DownStatsTable to have numeric values for non-team columns
  # need to do this early to normalized first down and penalty columns
  DownStatsTable[,2:ncol(DownStatsTable)] <- sapply(DownStatsTable[,2:ncol(DownStatsTable)], as.numeric)
  ## adding first downs and penalty stats as per-game stats to avoid biasing towards teams which played more games
  DownStatsTable <- DownStatsTable %>%
    dplyr::mutate(First_Downs_pg = Total_First_Downs/Games) %>%
    dplyr::mutate(Penalties_pg = Penalties/Games) %>%
    dplyr::mutate(Penalty_Yds_pg = Penalty_Yds/Games)
  ## head(DownStatsTable[,1:4])
  
  ## Selecting only variables which will be used for VoA
  ## eliminating sum stats, only keeping per-game stats
  ## kept games played in offensive stats table, don't need to leave it for other tables
  DownStatsTable <- DownStatsTable[,c("Team", "First_Downs_pg",
                                      "Third_Down_Percent",
                                      "Fourth_Down_Percent", 
                                      "Penalties_pg", "Penalty_Yds_pg")]
  
  ## Pass Defense stats
  PassDefStatsTeams <- PassDefUrl %>% read_html() %>% html_table() %>% .[[1]]
  head(PassDefStatsTeams)
  PassDefStatsTable <- PassDefUrl %>% read_html() %>% html_table() %>% .[[2]]
  head(PassDefStatsTable[,1:4])
  PassDefStatsTable <- cbind(PassDefStatsTeams, PassDefStatsTable)
  head(PassDefStatsTable[,1:4])
  ## column names are off, appear as first row instead, renaming column names
  NewPassDefColNames <- c("Team", "Games", "Completions_Allowed", 
                          "Pass_Atts_Allowed","Def_Comp_Pct_Allowed", 
                          "Passing_Yds_Allowed", "Def_Pass_YPA", 
                          "Passing_Yds_Allowed_pg", "Longest_Pass_Allowed", 
                          "Passing_Tds_Allowed", "Interceptions", "Def_Sacks", 
                          "Def_Sack_Yds_Lost", "Passer_Rtg_Allowed")
  colnames(PassDefStatsTable) <- NewPassDefColNames
  colnames(PassDefStatsTable)
  ## head(PassDefStatsTable[,1:4])
  
  ## changing PassDefStatsTable to have numeric values for non-team columns
  # need to do this early to add interceptions_pg column
  PassDefStatsTable[,2:ncol(PassDefStatsTable)] <- sapply(PassDefStatsTable[,2:ncol(PassDefStatsTable)], as.numeric)
  
  ## changing fumbles forced column to a per-game stat to avoid biasing towards teams which played more game
  PassDefStatsTable <- PassDefStatsTable %>%
    dplyr::mutate(Interceptions_pg = Interceptions/Games)
  PassDefStatsTable <- PassDefStatsTable %>%
    dplyr::mutate(Def_Sacks_pg = Def_Sacks/Games)
  PassDefStatsTable <- PassDefStatsTable %>%
    dplyr::mutate(Def_Sack_Yds_Lost_pg = Def_Sack_Yds_Lost/Games)
  
  ## Some of the ESPN tables scraped with top column being a repeat of column names
  ## For some reason, pass defense and rush defense do not do this
  ## PassDefStatsTable <- PassDefStatsTable[-c(1),]
  head(PassDefStatsTable[,1:4])
  
  ## Selecting only variables which will be used for VoA
  ## eliminating sum stats, only keeping per-game stats
  ## kept games played in offensive stats table, don't need to leave it for other tables
  ## pass yds per game already included in previous defensive stats table
  ## keeping attempts for yards per play stat later on
  PassDefStatsTable <- PassDefStatsTable[,c("Team", "Pass_Atts_Allowed","Def_Comp_Pct_Allowed", 
                                            "Def_Pass_YPA", 
                                            "Interceptions_pg", "Def_Sacks_pg", 
                                            "Def_Sack_Yds_Lost_pg")]
  
  
  ## Rush Defense stats
  RushDefStatsTeams <- RushDefUrl %>% read_html() %>% html_table() %>% .[[1]]
  ## head(RushDefStatsTeams)
  RushDefStatsTable <- RushDefUrl %>% read_html() %>% html_table() %>% .[[2]]
  ## head(RushDefStatsTable[,1:4])
  RushDefStatsTable <- cbind(RushDefStatsTeams, RushDefStatsTable)
  ## head(RushDefStatsTable[,1:4])
  ## column names are off, appear as first row instead, renaming column names
  NewRushDefColNames <- c("Team", "Games", "Rush_Att_Allowed", 
                          "Rushing_Yards_Allowed","Yds_Per_Rush_Attempt_Allowed", 
                          "Rushing_Yards_pg_Allowed", 
                          "Longest_Rush_Allowed", "Rush_Tds_Allowed", 
                          "Fumbles_Forced")
  colnames(RushDefStatsTable) <- NewRushDefColNames
  colnames(RushDefStatsTable)
  ## head(RushDefStatsTable[,1:4])
  ## changing RushDefStatsTable to have numeric values for non-team columns
  # need to do this early to add fumbles_forced_pg column
  RushDefStatsTable[,2:ncol(RushDefStatsTable)] <- sapply(RushDefStatsTable[,2:ncol(RushDefStatsTable)], as.numeric)
  
  ## changing fumbles forced column to a per-game stat to avoid biasing towards teams which played more game
  RushDefStatsTable <- RushDefStatsTable %>%
    dplyr::mutate(Fumbles_Forced_pg = Fumbles_Forced/Games)
  
  ## Some of the ESPN tables scraped with top column being a repeat of column names
  ## For some reason, pass defense and rush defense do not do this
  ## RushDefStatsTable <- RushDefStatsTable[-c(1),]
  ## head(RushDefStatsTable[,1:4])
  
  ## Selecting only variables which will be used for VoA
  ## eliminating sum stats, only keeping per-game stats
  ## kept games played in offensive stats table, don't need to leave it for other tables
  ## Rush yds per game already included in previous defensive stats table
  ## keeping attempts stat for yards per play stat later on
  RushDefStatsTable <- RushDefStatsTable[,c("Team", "Rush_Att_Allowed","Yds_Per_Rush_Attempt_Allowed",
                                            "Fumbles_Forced_pg")]
  
  ## FPI resume stats
  FPITeams <- FPIResumeURL %>% read_html() %>% html_table() %>% .[[1]]
  head(FPITeams)
  FPIResumeStats <- FPIResumeURL %>% read_html() %>% html_table() %>% .[[2]]
  head(FPIResumeStats[,1:4])
  FPIResumeTable <- cbind(FPITeams, FPIResumeStats)
  head(FPIResumeTable[,1:4])
  ## column names are off, appear as first row instead, renaming column names
  FPIResumeColNames <- c("Team", "FPI_Rank", "AP/CFP", "SOR", "SOS","Rem_SOS", 
                         "Game_Control_Rank", "Avg_WinProb")
  colnames(FPIResumeTable) <- FPIResumeColNames
  colnames(FPIResumeTable)
  head(FPIResumeTable[,1:4])
  ## Some of the ESPN tables scraped with top column being a repeat of column names
  FPIResumeTable <- FPIResumeTable[-c(1),]
  head(FPIResumeTable[,1:4])
  
  ## Selecting only variables which will be used for VoA
  ## eliminating sum stats, only keeping per-game stats
  FPIResumeTable <- FPIResumeTable[,c("Team", "FPI_Rank","SOR", "SOS", 
                                      "Game_Control_Rank", "Avg_WinProb")]
  
  FPIResumeTable <- subset(FPIResumeTable, SOS > 0)
  
  ## FPI efficiency stats
  FPITeams2 <- FPIEfficiencyURL %>% read_html() %>% html_table() %>% .[[1]]
  ## head(FPITeams2)
  FPIEfficiency <- FPIEfficiencyURL %>% read_html() %>% html_table() %>% .[[2]]
  ## head(FPIEfficiency[,1:4])
  FPIEfficiency <- cbind(FPITeams2, FPIEfficiency)
  ## head(FPIEfficiency[,1:4])
  ## column names are off, appear as first row instead, renaming column names
  FPIEfficiencyColNames <- c("Team", "FPI_Ovrl_Eff", "FPI_Ovrl_Rk", "FPI_Off_Eff", 
                             "FPI_Off_Rk","FPI_Def_Eff", "FPI_Def_Rk", 
                             "FPI_ST_Eff", "FPI_ST_Rk")
  colnames(FPIEfficiency) <- FPIEfficiencyColNames
  colnames(FPIEfficiency)
  ## head(FPIEfficiency[,1:4])
  ## Some of the ESPN tables scraped with top column being a repeat of column names
  FPIEfficiency <- FPIEfficiency[-c(1),]
  head(FPIEfficiency[,1:4])
  
  ## Selecting only variables which will be used for VoA
  FPIEfficiency <- FPIEfficiency[,c("Team", "FPI_Ovrl_Rk", "FPI_Off_Rk",
                                    "FPI_Def_Rk", "FPI_ST_Rk")]
  FPIEfficiency <- subset(FPIEfficiency, FPI_Ovrl_Rk > 0)
  
  
  ## changing FPI tables to have numeric values for non-team columns
  # doing this before merging with VoA_Variables
  # FPI stats are all already ranks, so no need to include them before
  # additional variables which will need to be included in ranks
  # FPI columns will serve as first column in range of columns to be included
  # in rowmeans() function later on
  FPIResumeTable[,2:ncol(FPIResumeTable)] <- sapply(FPIResumeTable[,2:ncol(FPIResumeTable)], as.numeric)
  FPIEfficiency[,2:ncol(FPIEfficiency)] <- sapply(FPIEfficiency[,2:ncol(FPIEfficiency)], as.numeric)
  
  ## Adding means of FPI Efficiency stats (excluding overall)
  # first just offense and defense, 2nd includes special teams
  FPIEfficiency <- FPIEfficiency %>%
    mutate(FPI_Off_Def_Mean_Rk = rowMeans(FPIEfficiency[,2:3])) %>%
    mutate(FPI_Off_Def_ST_Mean_Rk = rowMeans(FPIEfficiency[,2:4]))
  
  ## Now scraping PY data, no tables merged yet
  ## scraping ESPN's website with Thomas Mock's snippet from rvest found on twitter
  
  ## Offensive Points and Yards Stats
  ## Team comes in as its own table for some reason, scraping team and stats separately
  Off_PY_StatsTeams <- Off_PY_Url %>% read_html() %>% html_table() %>% .[[1]]
  ## head(OffStatsTeams)
  Off_PY_StatsTable <- Off_PY_Url %>% read_html() %>% html_table() %>% .[[2]]
  ## head(Off_PY_StatsTable[,1:4])
  ## merging team and stats tables
  Off_PY_StatsTable <- cbind(Off_PY_StatsTeams, Off_PY_StatsTable)
  ## head(Off_PY_StatsTable[,1:4])
  ## column names are off, appear as first row instead, renaming column names
  NewOff_PY_ColNames <- c("Team", "Games_PY", "Total_Yds_PY", "Total_Yds_pg_PY",
                            "Pass_Yds_PY", "Pass_Yds_pg_PY", "Rush_Yds_PY", "Rush_Yds_pg_PY",
                            "Points_PY", "Pts_pg_PY")
  colnames(Off_PY_StatsTable) <- NewOff_PY_ColNames
  colnames(Off_PY_StatsTable)
  ## head(Off_PY_StatsTable[,1:4])
  Off_PY_StatsTable <- Off_PY_StatsTable[-c(1),]
  ## head(Off_PY_StatsTable[,1:4])
  
  ## some columns have commas
  # must get rid of these using gsub() function
  Off_PY_StatsTable$Total_Yds_PY = gsub(",","", Off_PY_StatsTable$Total_Yds_PY)
  
  ## Selecting only variables which will be used for VoA
  ## eliminating sum stats, only keeping per-game stats
  ## keeping total yds stat for a Yards per play stat later on
  Off_PY_StatsTable <- Off_PY_StatsTable[,c("Team", "Games_PY", "Total_Yds_PY",
                                                "Total_Yds_pg_PY","Pass_Yds_pg_PY", 
                                                "Rush_Yds_pg_PY", "Points_PY", 
                                                "Pts_pg_PY")]
  
  
  ## Offensive Passing Stats
  ## Team comes in as its own table for some reason, scraping team and stats separately
  Off_PY_Pass_Stats_Teams <- Off_PY_Pass_Url %>% read_html() %>% html_table() %>% .[[1]]
  ## head(Off_PY_Pass_Stats_Teams)
  Off_PY_Pass_Stats_Table <- Off_PY_Pass_Url %>% read_html() %>% html_table() %>% .[[2]]
  ## head(Off_PY_Pass_Stats_Table[,1:4])
  ## merging team and stats tables
  Off_PY_Pass_Stats_Table <- cbind(Off_PY_Pass_Stats_Teams, Off_PY_Pass_Stats_Table)
  ## head(Off_PY_Pass_Stats_Table[,1:4])
  ## column names are off, appear as first row instead, renaming column names
  New_Off_PY_Pass_ColNames <- c("Team", "Games_PY", "Completions_PY", "Attempts_PY", "Comp_Pct_PY", 
                                  "Total_Pass_Yds_PY", "Pass_YPA_PY", "Pass_YPG_PY",
                                  "Longest_Pass_PY", "Pass_TD_PY", "INTs_Thrown_PY", 
                                  "Off_Sacks_Allowed_PY", "Off_Sack_Yds_Lost_PY", "QB_RTG_PY")
  colnames(Off_PY_Pass_Stats_Table) <- New_Off_PY_Pass_ColNames
  colnames(Off_PY_Pass_Stats_Table)
  ## head(Off_PY_Pass_Stats_Table[,1:4])
  ## for some reason, total off and def stats pages include row containing column labels when scraped from ESPN's website
  # passing and rush pages do not do this, so no need to remove top column
  ## Off_PY_Pass_Stats_Table <- Off_PY_Pass_Stats_Table[-c(1),]
  ## head(Off_PY_Pass_Stats_Table[,1:4])
  ## some columns have commas
  # must get rid of these using gsub() function
  Off_PY_Pass_Stats_Table$Total_Pass_Yds_PY = gsub(",","", Off_PY_Pass_Stats_Table$Total_Pass_Yds_PY)
  ## changing Off_PY_Pass_Stats_Table to have numeric values for non-team columns
  # need to do this early to add yds_per_completion
  Off_PY_Pass_Stats_Table[,2:ncol(Off_PY_Pass_Stats_Table)] <- sapply(Off_PY_Pass_Stats_Table[,2:ncol(Off_PY_Pass_Stats_Table)], as.numeric)
  ## Adding offensive YPC, normalizing INTs and Sack stats columns
  Off_PY_Pass_Stats_Table <- Off_PY_Pass_Stats_Table %>%
    mutate(Off_YPC_PY = Total_Pass_Yds_PY/Completions_PY) %>%
    mutate(Off_INTs_pg_PY = INTs_Thrown_PY/Games_PY) %>%
    mutate(Off_Sacks_Allowed_pg_PY = Off_Sacks_Allowed_PY/Games_PY) %>%
    mutate(Off_Sack_Yds_Lost_pg_PY = Off_Sack_Yds_Lost_PY/Games_PY)
  
  ## Selecting only variables which will be used for VoA
  ## eliminating sum stats, only keeping per-game stats, games already in Off_stats
  ## keeping attemps for a yards per play stat later on
  Off_PY_Pass_Stats_Table <- Off_PY_Pass_Stats_Table[,c("Team", "Attempts_PY",
                                                            "Total_Pass_Yds_PY","Comp_Pct_PY", 
                                                            "Pass_YPA_PY", 
                                                            "Off_YPC_PY", "Off_INTs_pg_PY", 
                                                            "Off_Sacks_Allowed_pg_PY", 
                                                            "Off_Sack_Yds_Lost_pg_PY",
                                                            "INTs_Thrown_PY")]
  
  
  
  ## Offensive Rushing Stats
  ## Team comes in as its own table for some reason, scraping team and stats separately
  Off_PY_Rush_Stats_Teams <- Off_PY_Rush_Url %>% read_html() %>% html_table() %>% .[[1]]
  ## head(Off_PY_Rush_Stats_Teams)
  Off_PY_Rush_Stats_Table <- Off_PY_Rush_Url %>% read_html() %>% html_table() %>% .[[2]]
  ## head(Off_PY_Rush_Stats_Table[,1:4])
  ## merging team and stats tables
  Off_PY_Rush_Stats_Table <- cbind(Off_PY_Rush_Stats_Teams, Off_PY_Rush_Stats_Table)
  ## head(Off_PY_Rush_Stats_Table[,1:4])
  ## column names are off, appear as first row instead, renaming column names
  New_Off_PY_Rush_ColNames <- c("Team", "Games_PY", "Rush_Atts_PY", "Total_Rush_Yds_PY", 
                                  "Off_PY_Rush_YPA", "Off_PY_Rush_Yds_pg", "Off_Longest_Rush_PY", 
                                  "Rush_TDs_PY")
  colnames(Off_PY_Rush_Stats_Table) <- New_Off_PY_Rush_ColNames
  colnames(Off_PY_Rush_Stats_Table)
  ## head(Off_PY_Rush_Stats_Table[,1:4])
  ## for some reason, total off and def stats pages include row containing column labels when scraped from ESPN's website
  # passing and rush pages do not do this, so no need to remove top column
  ## Off_PY_Rush_Stats_Table <- Off_PY_Rush_Stats_Table[-c(1),]
  ## head(Off_PY_Rush_Stats_Table[,1:4])
  
  ## Selecting only variables which will be used for VoA
  ## eliminating sum stats, only keeping per-game stats, games already in Off_stats
  ## keeping attempts stat for yards per play stat later on
  Off_PY_Rush_Stats_Table <- Off_PY_Rush_Stats_Table[,c("Team", "Rush_Atts_PY","Off_PY_Rush_YPA")]
  
  
  
  ## Defensive Yards and Points Stats
  Def_PY_StatsTeams <- Def_PY_Url %>% read_html() %>% html_table() %>% .[[1]]
  ## head(Def_PY_StatsTeams)
  Def_PY_StatsTable <- Def_PY_Url %>% read_html() %>% html_table() %>% .[[2]]
  ## head(Def_PY_StatsTable[,1:4])
  Def_PY_StatsTable <- cbind(Def_PY_StatsTeams, Def_PY_StatsTable)
  ## head(Def_PY_StatsTable[,1:4])
  ## column names are off, appear as first row instead, renaming column names
  NewDef_PY_ColNames <- c("Team", "Games_PY", "Def_Total_Yds_PY", "Def_Total_Yds_pg_PY",
                            "Def_Pass_Yds_PY", "Def_Pass_Yds_pg_PY", "Def_Rush_Yds_PY", 
                            "Def_Rush_Yds_pg_PY", "Def_Pts_PY", "Def_Pts_pg_PY")
  colnames(Def_PY_StatsTable) <- NewDef_PY_ColNames
  colnames(Def_PY_StatsTable)
  ## head(Def_PY_StatsTable)
  Def_PY_StatsTable <- Def_PY_StatsTable[-c(1),]
  ## head(Def_PY_StatsTable[,1:4])
  
  ## some columns have commas
  # must get rid of these using gsub() function
  Def_PY_StatsTable$Def_Total_Yds_PY = gsub(",","", Def_PY_StatsTable$Def_Total_Yds_PY)
  
  ## Selecting only variables which will be used for VoA
  ## eliminating sum stats, only keeping per-game stats
  ## kept games played in offensive stats table, don't need to leave it for other tables
  ## keeping total yards stat for a yards per play stat later on
  Def_PY_StatsTable <- Def_PY_StatsTable[,c("Team", "Def_Total_Yds_PY",
                                                "Def_Total_Yds_pg_PY",
                                                "Def_Pass_Yds_pg_PY", 
                                                "Def_Rush_Yds_pg_PY", 
                                                "Def_Pts_PY", 
                                                "Def_Pts_pg_PY")]
  
  
  ## Special Teams kickoff and punt return stats
  Return_PYStatsTeams <- Return_PYUrl %>% read_html() %>% html_table() %>% .[[1]]
  ## head(Return_PYStatsTeams)
  Return_PYStatsTable <- Return_PYUrl %>% read_html() %>% html_table() %>% .[[2]]
  ## head(Return_PYStatsTable[,1:4])
  Return_PYStatsTable <- cbind(Return_PYStatsTeams, Return_PYStatsTable)
  ## head(Return_PYStatsTable[,1:4])
  ## column names are off, appear as first row instead, renaming column names
  NewReturn_PYColNames <- c("Team", "Games_PY", "Kick_Return_Atts_PY", 
                              "Total_Kick_Return_Yds_PY","Kick_Return_Yd_Avg_PY", 
                              "Longest_Kick_Return_PY", "Kick_Return_TD_PY", 
                              "Punt_Return_Atts_PY", "Punt_Return_Yds_PY", 
                              "Punt_Return_Yd_Avg_PY", "Longest_Punt_Return_PY", 
                              "Punt_Return_TD_PY")
  colnames(Return_PYStatsTable) <- NewReturn_PYColNames
  colnames(Return_PYStatsTable)
  ## head(Return_PYStatsTable[,1:4])
  Return_PYStatsTable <- Return_PYStatsTable[-c(1),]
  
  ## changing Return_PYStatsTable to have numeric values for non-team columns
  # need to do this early to normalized kick and punt Return_PY columns
  Return_PYStatsTable[,2:ncol(Return_PYStatsTable)] <- sapply(Return_PYStatsTable[,2:ncol(Return_PYStatsTable)], as.numeric)
  ## changing fumbles forced column to a per-game stat to avoid biasing towards teams which played more game
  Return_PYStatsTable <- Return_PYStatsTable %>%
    dplyr::mutate(Kick_Return_TD_pg_PY = Kick_Return_TD_PY/Games_PY) %>%
    dplyr::mutate(Punt_Return_TD_pg_PY = Punt_Return_TD_PY/Games_PY)
  ## head(Return_PYStatsTable[,1:4])
  
  ## Selecting only variables which will be used for VoA
  ## eliminating sum stats, only keeping per-game stats
  ## kept games played in offensive stats table, don't need to leave it for other tables
  Return_PYStatsTable <- Return_PYStatsTable[,c("Team", "Kick_Return_Yd_Avg_PY", 
                                                    "Kick_Return_TD_pg_PY", "Punt_Return_Yd_Avg_PY", 
                                                    "Punt_Return_TD_pg_PY")]
  
  ## Special Teams kicking stats
  Kicking_PYStatsTeams <- Kicking_PYUrl %>% read_html() %>% html_table() %>% .[[1]]
  ## head(Kicking_PYStatsTeams)
  Kicking_PYStatsTable <- Kicking_PYUrl %>% read_html() %>% html_table() %>% .[[2]]
  ## head(Kicking_PYStatsTable[,1:4])
  Kicking_PYStatsTable <- cbind(Kicking_PYStatsTeams, Kicking_PYStatsTable)
  ## head(Kicking_PYStatsTable[,1:4])
  ## column names are off, appear as first row instead, renaming column names
  NewKicking_PYColNames <- c("Team", "Games_PY", "FGM_PY", "FGA_PY","FG_Pct_PY", 
                               "Longest_FG_PY", "FG_1_19_Yds_PY", "FG_20_29_Yds_PY", 
                               "FG_30_39_Yds_PY", "FG_40_49_Yds_PY", "FG_50_Plus_Yds_PY", 
                               "Extra_Pts_Made_PY", "Extra_Pts_Attempted_PY", 
                               "Extra_Pts_Pct_PY")
  colnames(Kicking_PYStatsTable) <- NewKicking_PYColNames
  colnames(Kicking_PYStatsTable)
  ## head(Kicking_PYStatsTable[,1:4])
  Kicking_PYStatsTable <- Kicking_PYStatsTable[-c(1),]
  ## head(Kicking_PYStatsTable[,1:4])
  
  ## Selecting only variables which will be used for VoA
  ## eliminating sum stats, only keeping per-game stats
  ## kept games played in offensive stats table, don't need to leave it for other tables
  Kicking_PYStatsTable <- Kicking_PYStatsTable[,c("Team", "FG_Pct_PY",
                                                      "Extra_Pts_Pct_PY")]
  
  ## Downs stats
  Down_PYStatsTeams <- Down_PYUrl %>% read_html() %>% html_table() %>% .[[1]]
  ## head(Down_PYStatsTeams)
  Down_PYStatsTable <- Down_PYUrl %>% read_html() %>% html_table() %>% .[[2]]
  ## head(Down_PYStatsTable[,1:4])
  Down_PYStatsTable <- cbind(Down_PYStatsTeams, Down_PYStatsTable)
  ## head(Down_PYStatsTable[,1:4])
  ## column names are off, appear as first row instead, renaming column names
  NewDown_PYColNames <- c("Team", "Games_PY", "Total_First_Downs_PY", "Rush_First_Downs_PY",
                            "Pass_First_Downs_PY", "Penalty_First_Downs_PY", "Third_Down_Made_PY", 
                            "Third_Down_Attempted_PY", "Third_Down_Pct_PY", 
                            "Fourth_Down_Made_PY", "Fourth_Down_Attempt_PY", "Fourth_Down_Pct_PY", 
                            "Penalties_PY", "Penalty_Yds_PY")
  colnames(Down_PYStatsTable) <- NewDown_PYColNames
  colnames(Down_PYStatsTable)
  ## head(Down_PYStatsTable[,1:4])
  ## Top row is just original espn column names repeated
  Down_PYStatsTable <- Down_PYStatsTable[-c(1),]
  
  ## some columns have commas
  # removing commas
  Down_PYStatsTable$Penalty_Yds_PY = gsub(",","", Down_PYStatsTable$Penalty_Yds_PY)
  
  ## changing Down_PYStatsTable to have numeric values for non-team columns
  # need to do this early to normalized first Down_PY and penalty columns
  Down_PYStatsTable[,2:ncol(Down_PYStatsTable)] <- sapply(Down_PYStatsTable[,2:ncol(Down_PYStatsTable)], as.numeric)
  ## adding first downs and penalty stats as per-game stats to avoid biasing towards teams which played more games
  Down_PYStatsTable <- Down_PYStatsTable %>%
    dplyr::mutate(First_Downs_pg_PY = Total_First_Downs_PY/Games_PY) %>%
    dplyr::mutate(Penalties_pg_PY = Penalties_PY/Games_PY) %>%
    dplyr::mutate(Penalty_Yds_pg_PY = Penalty_Yds_PY/Games_PY)
  ## head(Down_PYStatsTable[,1:4])
  
  ## Selecting only variables which will be used for VoA
  ## eliminating sum stats, only keeping per-game stats
  ## kept games played in offensive stats table, don't need to leave it for other tables
  Down_PYStatsTable <- Down_PYStatsTable[,c("Team", "First_Downs_pg_PY",
                                                "Third_Down_Pct_PY",
                                                "Fourth_Down_Pct_PY", 
                                                "Penalties_pg_PY", "Penalty_Yds_pg_PY")]
  
  ## Pass Defense stats
  PassDef_PY_StatsTeams <- PassDef_PY_Url %>% read_html() %>% html_table() %>% .[[1]]
  head(PassDef_PY_StatsTeams)
  PassDef_PY_StatsTable <- PassDef_PY_Url %>% read_html() %>% html_table() %>% .[[2]]
  head(PassDef_PY_StatsTable[,1:4])
  PassDef_PY_StatsTable <- cbind(PassDef_PY_StatsTeams, PassDef_PY_StatsTable)
  head(PassDef_PY_StatsTable[,1:4])
  ## column names are off, appear as first row instead, renaming column names
  NewPassDef_PY_ColNames <- c("Team", "Games_PY", "Completions_Allowed_PY", 
                                "Pass_Atts_Allowed_PY","Def_Comp_Pct_Allowed_PY", 
                                "Passing_Yds_Allowed_PY", "Def_Pass_YPA_PY", 
                                "Passing_Yds_Allowed_pg_PY", "Longest_Pass_Allowed_PY", 
                                "Passing_Tds_Allowed_PY", "Interceptions_PY", "Def_Sacks_PY", 
                                "Def_Sack_Yds_Lost_PY", "Passer_Rtg_Allowed_PY")
  colnames(PassDef_PY_StatsTable) <- NewPassDef_PY_ColNames
  colnames(PassDef_PY_StatsTable)
  ## head(PassDef_PY_StatsTable[,1:4])
  
  ## changing PassDef_PY_StatsTable to have numeric values for non-team columns
  # need to do this early to add interceptions_pg column
  PassDef_PY_StatsTable[,2:ncol(PassDef_PY_StatsTable)] <- sapply(PassDef_PY_StatsTable[,2:ncol(PassDef_PY_StatsTable)], as.numeric)
  
  ## changing fumbles forced column to a per-game stat to avoid biasing towards teams which played more game
  PassDef_PY_StatsTable <- PassDef_PY_StatsTable %>%
    dplyr::mutate(Interceptions_pg_PY = Interceptions_PY/Games_PY) %>%
    dplyr::mutate(Def_Sacks_pg_PY = Def_Sacks_PY/Games_PY) %>%
    dplyr::mutate(Def_Sack_Yds_Lost_pg_PY = Def_Sack_Yds_Lost_PY/Games_PY)
  
  ## Some of the ESPN tables scraped with top column being a repeat of column names
  ## For some reason, pass defense and rush defense do not do this
  ## PassDef_PY_StatsTable <- PassDef_PY_StatsTable[-c(1),]
  head(PassDef_PY_StatsTable[,1:4])
  
  ## Selecting only variables which will be used for VoA
  ## eliminating sum stats, only keeping per-game stats
  ## kept games played in offensive stats table, don't need to leave it for other tables
  ## pass yds per game already included in previous defensive stats table
  ## keeping attempts stat in for yards per play stat later on
  PassDef_PY_StatsTable <- PassDef_PY_StatsTable[,c("Team", "Pass_Atts_Allowed_PY","Def_Comp_Pct_Allowed_PY", 
                                                        "Def_Pass_YPA_PY", 
                                                        "Interceptions_pg_PY", "Def_Sacks_pg_PY", 
                                                        "Def_Sack_Yds_Lost_pg_PY")]
  
  
  ## Rush Defense stats
  RushDef_PY_StatsTeams <- RushDef_PY_Url %>% read_html() %>% html_table() %>% .[[1]]
  ## head(RushDef_PY_StatsTeams)
  RushDef_PY_StatsTable <- RushDef_PY_Url %>% read_html() %>% html_table() %>% .[[2]]
  ## head(RushDef_PY_StatsTable[,1:4])
  RushDef_PY_StatsTable <- cbind(RushDef_PY_StatsTeams, RushDef_PY_StatsTable)
  ## head(RushDef_PY_StatsTable[,1:4])
  ## column names are off, appear as first row instead, renaming column names
  NewRushDef_PY_ColNames <- c("Team", "Games_PY", "Rush_Att_Allowed_PY", 
                                "Rushing_Yards_Allowed_PY","Yds_Per_Rush_Attempt_Allowed_PY", 
                                "Rushing_Yards_pg_Allowed_PY", 
                                "Longest_Rush_Allowed_PY", "Rush_Tds_Allowed_PY", 
                                "Fumbles_Forced_PY")
  colnames(RushDef_PY_StatsTable) <- NewRushDef_PY_ColNames
  colnames(RushDef_PY_StatsTable)
  ## head(RushDef_PY_StatsTable[,1:4])
  ## changing RushDef_PY_StatsTable to have numeric values for non-team columns
  # need to do this early to add fumbles_forced_pg column
  RushDef_PY_StatsTable[,2:ncol(RushDef_PY_StatsTable)] <- sapply(RushDef_PY_StatsTable[,2:ncol(RushDef_PY_StatsTable)], as.numeric)
  
  ## changing fumbles forced column to a per-game stat to avoid biasing towards teams which played more game
  RushDef_PY_StatsTable <- RushDef_PY_StatsTable %>%
    dplyr::mutate(Fumbles_Forced_pg_PY = Fumbles_Forced_PY/Games_PY)
  
  ## Some of the ESPN tables scraped with top column being a repeat of column names
  ## For some reason, pass defense and rush defense do not do this
  ## RushDef_PY_StatsTable <- RushDef_PY_StatsTable[-c(1),]
  ## head(RushDef_PY_StatsTable[,1:4])
  
  ## Selecting only variables which will be used for VoA
  ## eliminating sum stats, only keeping per-game stats
  ## kept games played in offensive stats table, don't need to leave it for other tables
  ## Rush yds per game already included in previous defensive stats table
  ## keeping attempts stat in for yards per play stat later on
  RushDef_PY_StatsTable <- RushDef_PY_StatsTable[,c("Team", "Rush_Att_Allowed_PY","Yds_Per_Rush_Attempt_Allowed_PY",
                                                        "Fumbles_Forced_pg_PY")]
  
  ## FPI resume stats
  FPI_PY_Teams <- FPI_PY_ResumeURL %>% read_html() %>% html_table() %>% .[[1]]
  ## head(FPI_PY_Teams)
  FPI_PY_ResumeStats <- FPI_PY_ResumeURL %>% read_html() %>% html_table() %>% .[[2]]
  ## head(FPI_PY_ResumeStats[,1:4])
  FPI_PY_ResumeTable <- cbind(FPI_PY_Teams, FPI_PY_ResumeStats)
  ## head(FPI_PY_ResumeTable[,1:4])
  ## column names are off, appear as first row instead, renaming column names
  FPI_PY_ResumeColNames <- c("Team", "FPI_Rank_PY", "AP_CFP_PY", "SOR_PY", "SOS_PY","Rem_SOS_PY", 
                               "Game_Control_Rank_PY", "Avg_WinProb_PY")
  colnames(FPI_PY_ResumeTable) <- FPI_PY_ResumeColNames
  colnames(FPI_PY_ResumeTable)
  head(FPI_PY_ResumeTable[,1:4])
  ## Some of the ESPN tables scraped with top column being a repeat of column names
  FPI_PY_ResumeTable <- FPI_PY_ResumeTable[-c(1),]
  head(FPI_PY_ResumeTable[,1:4])
  
  ## Selecting only variables which will be used for VoA
  ## eliminating sum stats, only keeping per-game stats
  FPI_PY_ResumeTable <- FPI_PY_ResumeTable[,c("Team", "FPI_Rank_PY","SOR_PY", "SOS_PY", 
                                                  "Game_Control_Rank_PY", "Avg_WinProb_PY")]
  
  FPI_PY_ResumeTable <- subset(FPI_PY_ResumeTable, SOS_PY > 0)
  
  ## FPI efficiency stats
  FPI_PY_Teams2 <- FPI_PY_EfficiencyURL %>% read_html() %>% html_table() %>% .[[1]]
  ## head(FPI_PY_Teams2)
  FPI_PY_Efficiency <- FPI_PY_EfficiencyURL %>% read_html() %>% html_table() %>% .[[2]]
  ## head(FPI_PY_Efficiency[,1:4])
  FPI_PY_Efficiency <- cbind(FPI_PY_Teams2, FPI_PY_Efficiency)
  ## head(FPI_PY_Efficiency[,1:4])
  ## column names are off, appear as first row instead, renaming column names
  FPI_PY_EfficiencyColNames <- c("Team", "FPI_Ovrl_Eff_PY", "FPI_Ovrl_Rk_PY", "FPI_Off_Eff_PY", 
                                   "FPI_Off_Rk_PY","FPI_Def_Eff_PY", "FPI_Def_Rk_PY", 
                                   "FPI_ST_Eff_PY", "FPI_ST_Rk_PY")
  colnames(FPI_PY_Efficiency) <- FPI_PY_EfficiencyColNames
  colnames(FPI_PY_Efficiency)
  ## head(FPI_PY_Efficiency[,1:4])
  ## Some of the ESPN tables scraped with top column being a repeat of column names
  FPI_PY_Efficiency <- FPI_PY_Efficiency[-c(1),]
  head(FPI_PY_Efficiency[,1:4])
  
  ## Selecting only variables which will be used for VoA
  FPI_PY_Efficiency <- FPI_PY_Efficiency[,c("Team", "FPI_Ovrl_Rk_PY", "FPI_Off_Rk_PY",
                                                "FPI_Def_Rk_PY", "FPI_ST_Rk_PY")]
  FPI_PY_Efficiency <- subset(FPI_PY_Efficiency, FPI_Ovrl_Rk_PY > 0)
  
  
  ## changing FPI tables to have numeric values for non-team columns
  # doing this before merging with VoA_Variables
  # FPI stats are all already ranks, so no need to include them before
  # additional variables which will need to be included in ranks
  # FPI columns will serve as first column in range of columns to be included
  # in rowmeans() function later on
  FPI_PY_ResumeTable[,2:ncol(FPI_PY_ResumeTable)] <- sapply(FPI_PY_ResumeTable[,2:ncol(FPI_PY_ResumeTable)], as.numeric)
  FPI_PY_Efficiency[,2:ncol(FPI_PY_Efficiency)] <- sapply(FPI_PY_Efficiency[,2:ncol(FPI_PY_Efficiency)], as.numeric)
  
  ## Adding means of FPI Efficiency stats (excluding overall)
  # first just offense and defense, 2nd includes special teams
  FPI_PY_Efficiency <- FPI_PY_Efficiency %>%
    mutate(FPI_Off_Def_Mean_Rk_PY = rowMeans(FPI_PY_Efficiency[,2:3])) %>%
    mutate(FPI_Off_Def_ST_Mean_Rk_PY = rowMeans(FPI_PY_Efficiency[,2:4]))
  
  
  ## Now scraping 2019 data, no tables merged yet
  ## need 2019 data for UConn, Old Dominion, and New Mexico St ONLY
  ## COLUMN LABELS INTENTIONALLY WRONGLY INCLUDE PY AS YEAR
  # this is done to make merging PY opt-out data from 2019 with PY table easier
  ## scraping ESPN's website with Thomas Mock's snippet from rvest found on twitter
  
  ## Offensive Points and Yards Stats
  ## Team comes in as its own table for some reason, scraping team and stats separately
  Off_2019_StatsTeams <- Off_2019Url %>% read_html() %>% html_table() %>% .[[1]]
  ## head(Off_2019_StatsTeams)
  Off_2019_StatsTable <- Off_2019Url %>% read_html() %>% html_table() %>% .[[2]]
  ## head(Off_2019_StatsTable[,1:4])
  ## merging team and stats tables
  Off_2019_StatsTable <- cbind(Off_2019_StatsTeams, Off_2019_StatsTable)
  ## head(Off_2019_StatsTable[,1:4])
  ## column names are off, appear as first row instead, renaming column names
  NewOff_2019_ColNames <- c("Team", "Games_PY", "Total_Yds_PY", "Total_Yds_pg_PY",
                            "Pass_Yds_PY", "Pass_Yds_pg_PY", "Rush_Yds_PY", "Rush_Yds_pg_PY",
                            "Points_PY", "Pts_pg_PY")
  colnames(Off_2019_StatsTable) <- NewOff_2019_ColNames
  colnames(Off_2019_StatsTable)
  ## head(Off_2019_StatsTable[,1:4])
  Off_2019_StatsTable <- Off_2019_StatsTable[-c(1),]
  ## head(Off_2019_StatsTable[,1:4])
  
  ## some columns have commas
  # must get rid of these using gsub() function
  Off_2019_StatsTable$Total_Yds_PY = gsub(",","", Off_2019_StatsTable$Total_Yds_PY)
  
  ## Selecting only variables which will be used for VoA
  ## eliminating sum stats, only keeping per-game stats
  ## keeping total yds stat for yards per play later on
  Off_2019_StatsTable <- Off_2019_StatsTable[,c("Team", "Games_PY", "Total_Yds_PY","Total_Yds_pg_PY",
                                                "Pass_Yds_pg_PY", "Rush_Yds_pg_PY",
                                                "Points_PY", "Pts_pg_PY")]
  
  
  ## Offensive Passing Stats
  ## Team comes in as its own table for some reason, scraping team and stats separately
  Off_2019_Pass_Stats_Teams <- Off_2019_Pass_Url %>% read_html() %>% html_table() %>% .[[1]]
  ## head(Off_2019_Pass_Stats_Teams)
  Off_2019_Pass_Stats_Table <- Off_2019_Pass_Url %>% read_html() %>% html_table() %>% .[[2]]
  ## head(Off_2019_Pass_Stats_Table[,1:4])
  ## merging team and stats tables
  Off_2019_Pass_Stats_Table <- cbind(Off_2019_Pass_Stats_Teams, Off_2019_Pass_Stats_Table)
  ## head(Off_2019_Pass_Stats_Table[,1:4])
  ## column names are off, appear as first row instead, renaming column names
  New_Off_2019_Pass_ColNames <- c("Team", "Games_PY", "Completions_PY", "Attempts_PY", "Comp_Pct_PY", 
                                  "Total_Pass_Yds_PY", "Pass_YPA_PY", "Pass_YPG_PY",
                                  "Longest_Pass_PY", "Pass_TD_PY", "INTs_Thrown_PY", 
                                  "Off_Sacks_Allowed_PY", "Off_Sack_Yds_Lost_PY", "QB_RTG_PY")
  colnames(Off_2019_Pass_Stats_Table) <- New_Off_2019_Pass_ColNames
  colnames(Off_2019_Pass_Stats_Table)
  ## head(Off_2019_Pass_Stats_Table[,1:4])
  ## for some reason, total off and def stats pages include row containing column labels when scraped from ESPN's website
  # passing and rush pages do not do this, so no need to remove top column
  ## Off_2019_Pass_Stats_Table <- Off_2019_Pass_Stats_Table[-c(1),]
  ## head(Off_2019_Pass_Stats_Table[,1:4])
  
  ## some columns have commas
  # must get rid of these using gsub() function
  Off_2019_Pass_Stats_Table$Total_Pass_Yds_PY = gsub(",","", Off_2019_Pass_Stats_Table$Total_Pass_Yds_PY)
  
  ## changing Off_2019_Pass_Stats_Table to have numeric values for non-team columns
  # need to do this early to add yds_per_completion
  Off_2019_Pass_Stats_Table[,2:ncol(Off_2019_Pass_Stats_Table)] <- sapply(Off_2019_Pass_Stats_Table[,2:ncol(Off_2019_Pass_Stats_Table)], as.numeric)
  ## Adding offensive YPC, normalizing INTs and Sack stats columns
  Off_2019_Pass_Stats_Table <- Off_2019_Pass_Stats_Table %>%
    mutate(Off_YPC_PY = Total_Pass_Yds_PY/Completions_PY) %>%
    mutate(Off_INTs_pg_PY = INTs_Thrown_PY/Games_PY) %>%
    mutate(Off_Sacks_Allowed_pg_PY = Off_Sacks_Allowed_PY/Games_PY) %>%
    mutate(Off_Sack_Yds_Lost_pg_PY = Off_Sack_Yds_Lost_PY/Games_PY)
  
  ## Selecting only variables which will be used for VoA
  ## eliminating sum stats, only keeping per-game stats, games already in Off_stats
  ## keeping attempts stat for yards per play stat later on
  Off_2019_Pass_Stats_Table <- Off_2019_Pass_Stats_Table[,c("Team", "Attempts_PY","Comp_Pct_PY", "Pass_YPA_PY", 
                                                            "Off_YPC_PY", "Off_INTs_pg_PY", 
                                                            "Off_Sacks_Allowed_pg_PY", 
                                                            "Off_Sack_Yds_Lost_pg_PY",
                                                            "INTs_Thrown_PY")]
  
  
  
  ## Offensive Rushing Stats
  ## Team comes in as its own table for some reason, scraping team and stats separately
  Off_2019_Rush_Stats_Teams <- Off_2019_Rush_Url %>% read_html() %>% html_table() %>% .[[1]]
  ## head(Off_2019_Rush_Stats_Teams)
  Off_2019_Rush_Stats_Table <- Off_2019_Rush_Url %>% read_html() %>% html_table() %>% .[[2]]
  ## head(Off_2019_Rush_Stats_Table[,1:4])
  ## merging team and stats tables
  Off_2019_Rush_Stats_Table <- cbind(Off_2019_Rush_Stats_Teams, Off_2019_Rush_Stats_Table)
  ## head(Off_2019_Rush_Stats_Table[,1:4])
  ## column names are off, appear as first row instead, renaming column names
  New_Off_2019_Rush_ColNames <- c("Team", "Games_PY", "Rush_Atts_PY", "Total_Rush_Yds_PY", 
                                  "Off_PY_Rush_YPA", "Off_PY_Rush_Yds_pg", "Off_Longest_Rush_PY", 
                                  "Rush_TDs_PY")
  colnames(Off_2019_Rush_Stats_Table) <- New_Off_2019_Rush_ColNames
  colnames(Off_2019_Rush_Stats_Table)
  ## head(Off_2019_Rush_Stats_Table[,1:4])
  ## for some reason, total off and def stats pages include row containing column labels when scraped from ESPN's website
  # passing and rush pages do not do this, so no need to remove top column
  ## Off_2019_Rush_Stats_Table <- Off_2019_Rush_Stats_Table[-c(1),]
  ## head(Off_2019_Rush_Stats_Table[,1:4])
  
  ## Selecting only variables which will be used for VoA
  ## eliminating sum stats, only keeping per-game stats, games already in Off_stats
  ## keeping attempts stat for yards per play stat later on
  Off_2019_Rush_Stats_Table <- Off_2019_Rush_Stats_Table[,c("Team", "Rush_Atts_PY", "Off_PY_Rush_YPA")]
  
  
  
  ## Defensive Yards and Points Stats
  Def_2019_StatsTeams <- Def_2019_Url %>% read_html() %>% html_table() %>% .[[1]]
  ## head(Def_2019_StatsTeams)
  Def_2019_StatsTable <- Def_2019_Url %>% read_html() %>% html_table() %>% .[[2]]
  ## head(Def_2019_StatsTable[,1:4])
  Def_2019_StatsTable <- cbind(Def_2019_StatsTeams, Def_2019_StatsTable)
  ## head(Def_2019_StatsTable[,1:4])
  ## column names are off, appear as first row instead, renaming column names
  NewDef_2019_ColNames <- c("Team", "Games_PY", "Def_Total_Yds_PY", "Def_Total_Yds_pg_PY",
                            "Def_Pass_Yds_PY", "Def_Pass_Yds_pg_PY", "Def_Rush_Yds_PY", 
                            "Def_Rush_Yds_pg_PY", "Def_Pts_PY", "Def_Pts_pg_PY")
  colnames(Def_2019_StatsTable) <- NewDef_2019_ColNames
  colnames(Def_2019_StatsTable)
  ## head(Def_2019_StatsTable)
  Def_2019_StatsTable <- Def_2019_StatsTable[-c(1),]
  ## head(Def_2019_StatsTable[,1:4])
  
  ## some columns have commas
  # must get rid of these using gsub() function
  Def_2019_StatsTable$Def_Total_Yds_PY = gsub(",","", Def_2019_StatsTable$Def_Total_Yds_PY)
  
  ## Selecting only variables which will be used for VoA
  ## eliminating sum stats, only keeping per-game stats
  ## kept games played in offensive stats table, don't need to leave it for other tables
  ## keeping total yds stat for yards per play stat later on
  Def_2019_StatsTable <- Def_2019_StatsTable[,c("Team", "Def_Total_Yds_PY", "Def_Total_Yds_pg_PY",
                                                "Def_Pass_Yds_pg_PY", 
                                                "Def_Rush_Yds_pg_PY", 
                                                "Def_Pts_PY", "Def_Pts_pg_PY")]
  
  
  ## Special Teams kickoff and punt return stats
  Return_2019StatsTeams <- Return_2019Url %>% read_html() %>% html_table() %>% .[[1]]
  ## head(Return_2019StatsTeams)
  Return_2019StatsTable <- Return_2019Url %>% read_html() %>% html_table() %>% .[[2]]
  ## head(Return_2019StatsTable[,1:4])
  Return_2019StatsTable <- cbind(Return_2019StatsTeams, Return_2019StatsTable)
  ## head(Return_2019StatsTable[,1:4])
  ## column names are off, appear as first row instead, renaming column names
  NewReturn_2019ColNames <- c("Team", "Games_PY", "Kick_Return_Atts_PY", 
                              "Total_Kick_Return_Yds_PY","Kick_Return_Yd_Avg_PY", 
                              "Longest_Kick_Return_PY", "Kick_Return_TD_PY", 
                              "Punt_Return_Atts_PY", "Punt_Return_Yds_PY", 
                              "Punt_Return_Yd_Avg_PY", "Longest_Punt_Return_PY", 
                              "Punt_Return_TD_PY")
  colnames(Return_2019StatsTable) <- NewReturn_2019ColNames
  colnames(Return_2019StatsTable)
  ## head(Return_2019StatsTable[,1:4])
  Return_2019StatsTable <- Return_2019StatsTable[-c(1),]
  
  ## changing Return_2019StatsTable to have numeric values for non-team columns
  # need to do this early to normalized kick and punt Return_2019 columns
  Return_2019StatsTable[,2:ncol(Return_2019StatsTable)] <- sapply(Return_2019StatsTable[,2:ncol(Return_2019StatsTable)], as.numeric)
  ## changing fumbles forced column to a per-game stat to avoid biasing towards teams which played more game
  Return_2019StatsTable <- Return_2019StatsTable %>%
    dplyr::mutate(Kick_Return_TD_pg_PY = Kick_Return_TD_PY/Games_PY) %>%
    dplyr::mutate(Punt_Return_TD_pg_PY = Punt_Return_TD_PY/Games_PY)
  ## head(Return_2019StatsTable[,1:4])
  
  ## Selecting only variables which will be used for VoA
  ## eliminating sum stats, only keeping per-game stats
  ## kept games played in offensive stats table, don't need to leave it for other tables
  Return_2019StatsTable <- Return_2019StatsTable[,c("Team", "Kick_Return_Yd_Avg_PY", 
                                                    "Kick_Return_TD_pg_PY", "Punt_Return_Yd_Avg_PY", 
                                                    "Punt_Return_TD_pg_PY")]
  
  ## Special Teams kicking stats
  Kicking_2019StatsTeams <- Kicking_2019Url %>% read_html() %>% html_table() %>% .[[1]]
  ## head(Kicking_2019StatsTeams)
  Kicking_2019StatsTable <- Kicking_2019Url %>% read_html() %>% html_table() %>% .[[2]]
  ## head(Kicking_2019StatsTable[,1:4])
  Kicking_2019StatsTable <- cbind(Kicking_2019StatsTeams, Kicking_2019StatsTable)
  ## head(Kicking_2019StatsTable[,1:4])
  ## column names are off, appear as first row instead, renaming column names
  NewKicking_2019ColNames <- c("Team", "Games_PY", "FGM_PY", "FGA_PY","FG_Pct_PY", 
                               "Longest_FG_PY", "FG_1_19_Yds_PY", "FG_20_29_Yds_PY", 
                               "FG_30_39_Yds_PY", "FG_40_49_Yds_PY", "FG_50_Plus_Yds_PY", 
                               "Extra_Pts_Made_PY", "Extra_Pts_Attempted_PY", 
                               "Extra_Pts_Pct_PY")
  colnames(Kicking_2019StatsTable) <- NewKicking_2019ColNames
  colnames(Kicking_2019StatsTable)
  ## head(Kicking_2019StatsTable[,1:4])
  Kicking_2019StatsTable <- Kicking_2019StatsTable[-c(1),]
  ## head(Kicking_2019StatsTable[,1:4])
  
  ## Selecting only variables which will be used for VoA
  ## eliminating sum stats, only keeping per-game stats
  ## kept games played in offensive stats table, don't need to leave it for other tables
  Kicking_2019StatsTable <- Kicking_2019StatsTable[,c("Team", "FG_Pct_PY",
                                                      "Extra_Pts_Pct_PY")]
  
  ## Downs stats
  Down_2019StatsTeams <- Down_2019Url %>% read_html() %>% html_table() %>% .[[1]]
  ## head(Down_2019StatsTeams)
  Down_2019StatsTable <- Down_2019Url %>% read_html() %>% html_table() %>% .[[2]]
  ## head(Down_2019StatsTable[,1:4])
  Down_2019StatsTable <- cbind(Down_2019StatsTeams, Down_2019StatsTable)
  ## head(Down_2019StatsTable[,1:4])
  ## column names are off, appear as first row instead, renaming column names
  NewDown_2019ColNames <- c("Team", "Games_PY", "Total_First_Downs_PY", "Rush_First_Downs_PY",
                            "Pass_First_Downs_PY", "Penalty_First_Downs_PY", "Third_Down_Made_PY", 
                            "Third_Down_Attempted_PY", "Third_Down_Pct_PY", 
                            "Fourth_Down_Made_PY", "Fourth_Down_Attempt_PY", "Fourth_Down_Pct_PY", 
                            "Penalties_PY", "Penalty_Yds_PY")
  colnames(Down_2019StatsTable) <- NewDown_2019ColNames
  colnames(Down_2019StatsTable)
  ## head(Down_2019StatsTable[,1:4])
  ## Top row is just original espn column names repeated
  Down_2019StatsTable <- Down_2019StatsTable[-c(1),]
  
  ## changing Down_2019StatsTable to have numeric values for non-team columns
  # need to do this early to normalized first Down_2019 and penalty columns
  Down_2019StatsTable[,2:ncol(Down_2019StatsTable)] <- sapply(Down_2019StatsTable[,2:ncol(Down_2019StatsTable)], as.numeric)
  ## adding first downs and penalty stats as per-game stats to avoid biasing towards teams which played more games
  Down_2019StatsTable <- Down_2019StatsTable %>%
    dplyr::mutate(First_Downs_pg_PY = Total_First_Downs_PY/Games_PY) %>%
    dplyr::mutate(Penalties_pg_PY = Penalties_PY/Games_PY) %>%
    dplyr::mutate(Penalty_Yds_pg_PY = Penalty_Yds_PY/Games_PY)
  ## head(Down_2019StatsTable[,1:4])
  
  ## Selecting only variables which will be used for VoA
  ## eliminating sum stats, only keeping per-game stats
  ## kept games played in offensive stats table, don't need to leave it for other tables
  Down_2019StatsTable <- Down_2019StatsTable[,c("Team", "First_Downs_pg_PY",
                                                "Third_Down_Pct_PY",
                                                "Fourth_Down_Pct_PY", 
                                                "Penalties_pg_PY", "Penalty_Yds_pg_PY")]
  
  ## Pass Defense stats
  PassDef_2019_StatsTeams <- PassDef_2019_Url %>% read_html() %>% html_table() %>% .[[1]]
  head(PassDef_2019_StatsTeams)
  PassDef_2019_StatsTable <- PassDef_2019_Url %>% read_html() %>% html_table() %>% .[[2]]
  head(PassDef_2019_StatsTable[,1:4])
  PassDef_2019_StatsTable <- cbind(PassDef_2019_StatsTeams, PassDef_2019_StatsTable)
  head(PassDef_2019_StatsTable[,1:4])
  ## column names are off, appear as first row instead, renaming column names
  NewPassDef_2019_ColNames <- c("Team", "Games_PY", "Completions_Allowed_PY", 
                                "Pass_Atts_Allowed_PY","Def_Comp_Pct_Allowed_PY", 
                                "Passing_Yds_Allowed_PY", "Def_Pass_YPA_PY", 
                                "Passing_Yds_Allowed_pg_PY", "Longest_Pass_Allowed_PY", 
                                "Passing_Tds_Allowed_PY", "Interceptions_PY", "Def_Sacks_PY", 
                                "Def_Sack_Yds_Lost_PY", "Passer_Rtg_Allowed_PY")
  colnames(PassDef_2019_StatsTable) <- NewPassDef_2019_ColNames
  colnames(PassDef_2019_StatsTable)
  ## head(PassDef_2019_StatsTable[,1:4])
  
  ## changing PassDef_2019_StatsTable to have numeric values for non-team columns
  # need to do this early to add interceptions_pg column
  PassDef_2019_StatsTable[,2:ncol(PassDef_2019_StatsTable)] <- sapply(PassDef_2019_StatsTable[,2:ncol(PassDef_2019_StatsTable)], as.numeric)
  
  ## changing fumbles forced column to a per-game stat to avoid biasing towards teams which played more game
  PassDef_2019_StatsTable <- PassDef_2019_StatsTable %>%
    dplyr::mutate(Interceptions_pg_PY = Interceptions_PY/Games_PY) %>%
    dplyr::mutate(Def_Sacks_pg_PY = Def_Sacks_PY/Games_PY) %>%
    dplyr::mutate(Def_Sack_Yds_Lost_pg_PY = Def_Sack_Yds_Lost_PY/Games_PY)
  
  ## Some of the ESPN tables scraped with top column being a repeat of column names
  ## For some reason, pass defense and rush defense do not do this
  ## PassDef_2019_StatsTable <- PassDef_2019_StatsTable[-c(1),]
  head(PassDef_2019_StatsTable[,1:4])
  
  ## Selecting only variables which will be used for VoA
  ## eliminating sum stats, only keeping per-game stats
  ## kept games played in offensive stats table, don't need to leave it for other tables
  ## pass yds per game already included in previous defensive stats table
  ## keeping attempts stat for yards per play stat later on
  PassDef_2019_StatsTable <- PassDef_2019_StatsTable[,c("Team", "Pass_Atts_Allowed_PY", "Def_Comp_Pct_Allowed_PY", 
                                                        "Def_Pass_YPA_PY", 
                                                        "Interceptions_pg_PY", "Def_Sacks_pg_PY", 
                                                        "Def_Sack_Yds_Lost_pg_PY")]
  
  
  ## Rush Defense stats
  RushDef_2019_StatsTeams <- RushDef_2019_Url %>% read_html() %>% html_table() %>% .[[1]]
  ## head(RushDef_2019_StatsTeams)
  RushDef_2019_StatsTable <- RushDef_2019_Url %>% read_html() %>% html_table() %>% .[[2]]
  ## head(RushDef_2019_StatsTable[,1:4])
  RushDef_2019_StatsTable <- cbind(RushDef_2019_StatsTeams, RushDef_2019_StatsTable)
  ## head(RushDef_2019_StatsTable[,1:4])
  ## column names are off, appear as first row instead, renaming column names
  NewRushDef_2019_ColNames <- c("Team", "Games_PY", "Rush_Att_Allowed_PY", 
                                "Rushing_Yards_Allowed_PY","Yds_Per_Rush_Attempt_Allowed_PY", 
                                "Rushing_Yards_pg_Allowed_PY", 
                                "Longest_Rush_Allowed_PY", "Rush_Tds_Allowed_PY", 
                                "Fumbles_Forced_PY")
  colnames(RushDef_2019_StatsTable) <- NewRushDef_2019_ColNames
  colnames(RushDef_2019_StatsTable)
  ## head(RushDef_2019_StatsTable[,1:4])
  ## changing RushDef_2019_StatsTable to have numeric values for non-team columns
  # need to do this early to add fumbles_forced_pg column
  RushDef_2019_StatsTable[,2:ncol(RushDef_2019_StatsTable)] <- sapply(RushDef_2019_StatsTable[,2:ncol(RushDef_2019_StatsTable)], as.numeric)
  
  ## changing fumbles forced column to a per-game stat to avoid biasing towards teams which played more game
  RushDef_2019_StatsTable <- RushDef_2019_StatsTable %>%
    dplyr::mutate(Fumbles_Forced_pg_PY = Fumbles_Forced_PY/Games_PY)
  
  ## Some of the ESPN tables scraped with top column being a repeat of column names
  ## For some reason, pass defense and rush defense do not do this
  ## RushDef_2019_StatsTable <- RushDef_2019_StatsTable[-c(1),]
  ## head(RushDef_2019_StatsTable[,1:4])
  
  ## Selecting only variables which will be used for VoA
  ## eliminating sum stats, only keeping per-game stats
  ## kept games played in offensive stats table, don't need to leave it for other tables
  ## Rush yds per game already included in previous defensive stats table
  ## keeping attempts stat for yards per play stat later on
  RushDef_2019_StatsTable <- RushDef_2019_StatsTable[,c("Team", "Rush_Att_Allowed_PY", "Yds_Per_Rush_Attempt_Allowed_PY",
                                                        "Fumbles_Forced_pg_PY")]
  
  ## FPI resume stats
  FPI_2019_Teams <- FPI_2019_ResumeURL %>% read_html() %>% html_table() %>% .[[1]]
  ## head(FPI_2019_Teams)
  FPI_2019_ResumeStats <- FPI_2019_ResumeURL %>% read_html() %>% html_table() %>% .[[2]]
  ## head(FPI_2019_ResumeStats[,1:4])
  FPI_2019_ResumeTable <- cbind(FPI_2019_Teams, FPI_2019_ResumeStats)
  ## head(FPI_2019_ResumeTable[,1:4])
  ## column names are off, appear as first row instead, renaming column names
  FPI_2019_ResumeColNames <- c("Team", "FPI_Rank_PY", "AP_CFP_PY", "SOR_PY", "SOS_PY","Rem_SOS_PY", 
                               "Game_Control_Rank_PY", "Avg_WinProb_PY")
  colnames(FPI_2019_ResumeTable) <- FPI_2019_ResumeColNames
  colnames(FPI_2019_ResumeTable)
  head(FPI_2019_ResumeTable[,1:4])
  ## Some of the ESPN tables scraped with top column being a repeat of column names
  FPI_2019_ResumeTable <- FPI_2019_ResumeTable[-c(1),]
  head(FPI_2019_ResumeTable[,1:4])
  
  ## Selecting only variables which will be used for VoA
  ## eliminating sum stats, only keeping per-game stats
  FPI_2019_ResumeTable <- FPI_2019_ResumeTable[,c("Team", "FPI_Rank_PY","SOR_PY", "SOS_PY", 
                                                  "Game_Control_Rank_PY", "Avg_WinProb_PY")]
  
  FPI_2019_ResumeTable <- subset(FPI_2019_ResumeTable, SOS_PY > 0)
  
  ## FPI efficiency stats
  FPI_2019_Teams2 <- FPI_2019_EfficiencyURL %>% read_html() %>% html_table() %>% .[[1]]
  ## head(FPI_2019_Teams2)
  FPI_2019_Efficiency <- FPI_2019_EfficiencyURL %>% read_html() %>% html_table() %>% .[[2]]
  ## head(FPI_2019_Efficiency[,1:4])
  FPI_2019_Efficiency <- cbind(FPI_2019_Teams2, FPI_2019_Efficiency)
  ## head(FPI_2019_Efficiency[,1:4])
  ## column names are off, appear as first row instead, renaming column names
  ## column names will be given year of PY to match PY table so PY opt-outs
  # can be properly merged
  FPI_2019_EfficiencyColNames <- c("Team", "FPI_Ovrl_Eff_PY", "FPI_Ovrl_Rk_PY", "FPI_Off_Eff_PY", 
                                   "FPI_Off_Rk_PY","FPI_Def_Eff_PY", "FPI_Def_Rk_PY", 
                                   "FPI_ST_Eff_PY", "FPI_ST_Rk_PY")
  colnames(FPI_2019_Efficiency) <- FPI_2019_EfficiencyColNames
  colnames(FPI_2019_Efficiency)
  ## head(FPI_2019_Efficiency[,1:4])
  ## Some of the ESPN tables scraped with top column being a repeat of column names
  FPI_2019_Efficiency <- FPI_2019_Efficiency[-c(1),]
  head(FPI_2019_Efficiency[,1:4])
  
  ## Selecting only variables which will be used for VoA
  FPI_2019_Efficiency <- FPI_2019_Efficiency[,c("Team", "FPI_Ovrl_Rk_PY", "FPI_Off_Rk_PY",
                                                "FPI_Def_Rk_PY", "FPI_ST_Rk_PY")]
  FPI_2019_Efficiency <- subset(FPI_2019_Efficiency, FPI_Ovrl_Rk_PY > 0)
  
  
  ## changing FPI tables to have numeric values for non-team columns
  # doing this before merging with VoA_Variables
  # FPI stats are all already ranks, so no need to include them before
  # additional variables which will need to be included in ranks
  # FPI columns will serve as first column in range of columns to be included
  # in rowmeans() function later on
  FPI_2019_ResumeTable[,2:ncol(FPI_2019_ResumeTable)] <- sapply(FPI_2019_ResumeTable[,2:ncol(FPI_2019_ResumeTable)], as.numeric)
  FPI_2019_Efficiency[,2:ncol(FPI_2019_Efficiency)] <- sapply(FPI_2019_Efficiency[,2:ncol(FPI_2019_Efficiency)], as.numeric)
  
  ## Adding means of FPI Efficiency stats (excluding overall)
  # first just offense and defense, 2nd includes special teams
  FPI_2019_Efficiency <- FPI_2019_Efficiency %>%
    mutate(FPI_Off_Def_Mean_Rk_PY = rowMeans(FPI_2019_Efficiency[,2:3])) %>%
    mutate(FPI_Off_Def_ST_Mean_Rk_PY = rowMeans(FPI_2019_Efficiency[,2:4]))
} else {
  ## scraping ESPN's website with Thomas Mock's snippet from rvest found on twitter
  
  ## Offensive Points and Yards Stats
  ## Team comes in as its own table for some reason, scraping team and stats separately
  OffStatsTeams <- OffUrl %>% read_html() %>% html_table() %>% .[[1]]
  ## head(OffStatsTeams)
  OffStatsTable <- OffUrl %>% read_html() %>% html_table() %>% .[[2]]
  ## head(OffStatsTable[,1:4])
  ## merging team and stats tables
  OffStatsTable <- cbind(OffStatsTeams, OffStatsTable)
  ## head(OffStatsTable[,1:4])
  ## column names are off, appear as first row instead, renaming column names
  NewOffColNames <- c("Team", "Games", "Total_Yds", "Total_Yds_pg",
                      "Pass_Yds", "Pass_Yds_pg", "Rush_Yds", "Rush_Yds_pg",
                      "Total_Pts", "Pts_pg")
  colnames(OffStatsTable) <- NewOffColNames
  colnames(OffStatsTable)
  ## head(OffStatsTable[,1:4])
  OffStatsTable <- OffStatsTable[-c(1),]
  ## head(OffStatsTable[,1:4])
  
  ## some columns have commas
  # must get rid of these using gsub() function
  OffStatsTable$Total_Yds = gsub(",","", OffStatsTable$Total_Yds)
  
  ## Selecting only variables which will be used for VoA
  ## eliminating sum stats, only keeping per-game stats
  ## keeping total yards in this table for a "yards per play" stat later on
  OffStatsTable <- OffStatsTable[,c("Team", "Games", "Total_Yds","Total_Yds_pg",
                                    "Pass_Yds_pg", "Rush_Yds_pg",
                                    "Total_Pts", "Pts_pg")]
  
  
  ## Offensive Passing Stats
  ## Team comes in as its own table for some reason, scraping team and stats separately
  Off_Pass_Stats_Teams <- Off_Pass_Url %>% read_html() %>% html_table() %>% .[[1]]
  ## head(Off_Pass_Stats_Teams)
  Off_Pass_Stats_Table <- Off_Pass_Url %>% read_html() %>% html_table() %>% .[[2]]
  ## head(Off_Pass_Stats_Table[,1:4])
  ## merging team and stats tables
  Off_Pass_Stats_Table <- cbind(Off_Pass_Stats_Teams, Off_Pass_Stats_Table)
  ## head(Off_Pass_Stats_Table[,1:4])
  ## column names are off, appear as first row instead, renaming column names
  New_Off_Pass_ColNames <- c("Team", "Games", "Completions", "Attempts", "Comp_Pct", 
                             "Total_Pass_Yds", "Pass_YPA", "Pass_YPG",
                             "Longest_Pass", "Pass_TD", "INTs_Thrown", 
                             "Off_Sacks_Allowed", "Off_Sack_Yds_Lost", "QB_RTG")
  colnames(Off_Pass_Stats_Table) <- New_Off_Pass_ColNames
  colnames(Off_Pass_Stats_Table)
  ## head(Off_Pass_Stats_Table[,1:4])
  ## for some reason, total off and def stats pages include row containing column labels when scraped from ESPN's website
  # passing and rush pages do not do this, so no need to remove top column
  ## Off_Pass_Stats_Table <- Off_Pass_Stats_Table[-c(1),]
  ## head(Off_Pass_Stats_Table[,1:4])
  
  ## some columns have commas
  # must get rid of these using gsub() function
  Off_Pass_Stats_Table$Total_Pass_Yds = gsub(",","", Off_Pass_Stats_Table$Total_Pass_Yds)
  ## changing Off_Pass_Stats_Table to have numeric values for non-team columns
  # need to do this early to add yds_per_completion
  Off_Pass_Stats_Table[,2:ncol(Off_Pass_Stats_Table)] <- sapply(Off_Pass_Stats_Table[,2:ncol(Off_Pass_Stats_Table)], as.numeric)
  ## Adding offensive YPC, normalizing INTs and Sack stats columns
  Off_Pass_Stats_Table <- Off_Pass_Stats_Table %>%
    mutate(Off_YPC = Total_Pass_Yds/Completions) %>%
    mutate(Off_INTs_pg = INTs_Thrown/Games) %>%
    mutate(Off_Sacks_Allowed_pg = Off_Sacks_Allowed/Games) %>%
    mutate(Off_Sack_Yds_Lost_pg = Off_Sack_Yds_Lost/Games)
  
  ## Selecting only variables which will be used for VoA
  ## eliminating sum stats, only keeping per-game stats, games already in Off_stats
  ## keeping attempts stat for a yards per play column later on
  Off_Pass_Stats_Table <- Off_Pass_Stats_Table[,c("Team", "Attempts","Comp_Pct", 
                                                  "Total_Pass_Yds", "Pass_YPA", 
                                                  "Off_YPC", "Off_INTs_pg", 
                                                  "Off_Sacks_Allowed_pg", 
                                                  "Off_Sack_Yds_Lost_pg",
                                                  "Pass_TD", "INTs_Thrown")]
  
  
  
  ## Offensive Rushing Stats
  ## Team comes in as its own table for some reason, scraping team and stats separately
  Off_Rush_Stats_Teams <- Off_Rush_Url %>% read_html() %>% html_table() %>% .[[1]]
  ## head(Off_Rush_Stats_Teams)
  Off_Rush_Stats_Table <- Off_Rush_Url %>% read_html() %>% html_table() %>% .[[2]]
  ## head(Off_Rush_Stats_Table[,1:4])
  ## merging team and stats tables
  Off_Rush_Stats_Table <- cbind(Off_Rush_Stats_Teams, Off_Rush_Stats_Table)
  ## head(Off_Rush_Stats_Table[,1:4])
  ## column names are off, appear as first row instead, renaming column names
  New_Off_Rush_ColNames <- c("Team", "Games", "Rush_Atts", "Total_Rush_Yds", 
                             "Off_Rush_YPA", "Off_Rush_Yds_pg", "Off_Longest_Rush", 
                             "Rush_TDs")
  colnames(Off_Rush_Stats_Table) <- New_Off_Rush_ColNames
  colnames(Off_Rush_Stats_Table)
  ## head(Off_Rush_Stats_Table[,1:4])
  ## for some reason, total off and def stats pages include row containing column labels when scraped from ESPN's website
  # passing and rush pages do not do this, so no need to remove top column
  ## Off_Rush_Stats_Table <- Off_Rush_Stats_Table[-c(1),]
  ## head(Off_Rush_Stats_Table[,1:4])
  
  ## Selecting only variables which will be used for VoA
  ## eliminating sum stats, only keeping per-game stats, games already in Off_stats
  Off_Rush_Stats_Table <- Off_Rush_Stats_Table[,c("Team", "Rush_Atts","Off_Rush_YPA")]
  
  
  
  ## Defensive Yards and Points Stats
  DefStatsTeams <- DefUrl %>% read_html() %>% html_table() %>% .[[1]]
  ## head(DefStatsTeams)
  DefStatsTable <- DefUrl %>% read_html() %>% html_table() %>% .[[2]]
  ## head(DefStatsTable[,1:4])
  DefStatsTable <- cbind(DefStatsTeams, DefStatsTable)
  ## head(DefStatsTable[,1:4])
  ## column names are off, appear as first row instead, renaming column names
  NewDefColNames <- c("Team", "Games", "Def_Total_Yds", "Def_Total_Yds_pg",
                      "Def_Pass_Yds", "Def_Pass_Yds_pg", "Def_Rush_Yds", 
                      "Def_Rush_Yds_pg", "Def_Points", "Def_Pts_pg")
  colnames(DefStatsTable) <- NewDefColNames
  colnames(DefStatsTable)
  ## head(DefStatsTable)
  DefStatsTable <- DefStatsTable[-c(1),]
  ## head(DefStatsTable[,1:4])
  
  ## some columns have commas
  # must get rid of these using gsub() function
  DefStatsTable$Def_Total_Yds = gsub(",","", DefStatsTable$Def_Total_Yds)
  
  ## Selecting only variables which will be used for VoA
  ## eliminating sum stats, only keeping per-game stats
  ## kept games played in offensive stats table, don't need to leave it for other tables
  ## keeping total yards stat for yards per play stat later on
  DefStatsTable <- DefStatsTable[,c("Team", "Def_Total_Yds","Def_Total_Yds_pg",
                                    "Def_Pass_Yds_pg", 
                                    "Def_Rush_Yds_pg", "Def_Points", 
                                    "Def_Pts_pg")]
  
  
  ## Special Teams kickoff and punt return stats
  ReturnStatsTeams <- ReturnUrl %>% read_html() %>% html_table() %>% .[[1]]
  ## head(ReturnStatsTeams)
  ReturnStatsTable <- ReturnUrl %>% read_html() %>% html_table() %>% .[[2]]
  ## head(ReturnStatsTable[,1:4])
  ReturnStatsTable <- cbind(ReturnStatsTeams, ReturnStatsTable)
  ## head(ReturnStatsTable[,1:4])
  ## column names are off, appear as first row instead, renaming column names
  NewReturnColNames <- c("Team", "Games", "Kick_Return_Attempts", 
                         "Total_Kick_Return_Yds","Kick_Return_Yd_Avg", 
                         "Longest_Kick_Return", "Kick_Return_TD", 
                         "Punt_Return_Attempts", "Punt_Return_Yds", 
                         "Punt_Return_Yd_Avg", "Longest_Punt_Return", 
                         "Punt_Return_TD")
  colnames(ReturnStatsTable) <- NewReturnColNames
  colnames(ReturnStatsTable)
  ## head(ReturnStatsTable[,1:4])
  ReturnStatsTable <- ReturnStatsTable[-c(1),]
  
  ## changing ReturnStatsTable to have numeric values for non-team columns
  # need to do this early to normalized kick and punt return columns
  ReturnStatsTable[,2:ncol(ReturnStatsTable)] <- sapply(ReturnStatsTable[,2:ncol(ReturnStatsTable)], as.numeric)
  ## changing fumbles forced column to a per-game stat to avoid biasing towards teams which played more game
  ReturnStatsTable <- ReturnStatsTable %>%
    dplyr::mutate(Kick_Return_TD_pg = Kick_Return_TD/Games) %>%
    dplyr::mutate(Punt_Return_TD_pg = Punt_Return_TD/Games)
  ## head(ReturnStatsTable[,1:4])
  
  ## Selecting only variables which will be used for VoA
  ## eliminating sum stats, only keeping per-game stats
  ## kept games played in offensive stats table, don't need to leave it for other tables
  ReturnStatsTable <- ReturnStatsTable[,c("Team", "Kick_Return_Yd_Avg", 
                                          "Kick_Return_TD_pg", "Punt_Return_Yd_Avg", 
                                          "Punt_Return_TD_pg")]
  
  ## Special Teams kicking stats
  KickingStatsTeams <- KickingUrl %>% read_html() %>% html_table() %>% .[[1]]
  ## head(KickingStatsTeams)
  KickingStatsTable <- KickingUrl %>% read_html() %>% html_table() %>% .[[2]]
  ## head(KickingStatsTable[,1:4])
  KickingStatsTable <- cbind(KickingStatsTeams, KickingStatsTable)
  ## head(KickingStatsTable[,1:4])
  ## column names are off, appear as first row instead, renaming column names
  NewKickingColNames <- c("Team", "Games", "FGM", "FGA","FG_Percent", 
                          "Longest_FG", "FG_1_19_Yds", "FG_20_29_Yds", 
                          "FG_30_39_Yds", "FG_40_49_Yds", "FG_50_Plus_Yds", 
                          "Extra_Pts_Made", "Extra_Pts_Attempted", 
                          "Extra_Pts_Percent")
  colnames(KickingStatsTable) <- NewKickingColNames
  colnames(KickingStatsTable)
  ## head(KickingStatsTable[,1:4])
  KickingStatsTable <- KickingStatsTable[-c(1),]
  ## head(KickingStatsTable[,1:4])
  
  ## Selecting only variables which will be used for VoA
  ## eliminating sum stats, only keeping per-game stats
  ## kept games played in offensive stats table, don't need to leave it for other tables
  KickingStatsTable <- KickingStatsTable[,c("Team", "FG_Percent",
                                            "Extra_Pts_Percent")]
  
  ## Downs stats
  DownStatsTeams <- DownsUrl %>% read_html() %>% html_table() %>% .[[1]]
  ## head(DownStatsTeams)
  DownStatsTable <- DownsUrl %>% read_html() %>% html_table() %>% .[[2]]
  ## head(DownStatsTable[,1:4])
  DownStatsTable <- cbind(DownStatsTeams, DownStatsTable)
  ## head(DownStatsTable[,1:4])
  ## column names are off, appear as first row instead, renaming column names
  NewDownColNames <- c("Team", "Games", "Total_First_Downs", "Rush_First_Downs",
                       "Pass_First_Downs", "Penalty_First_Downs", "Third_Down_Made", 
                       "Third_Down_Attempted", "Third_Down_Percent", 
                       "Fourth_Down_Made", "Fourth_Down_Attempt", "Fourth_Down_Percent", 
                       "Penalties", "Penalty_Yds")
  colnames(DownStatsTable) <- NewDownColNames
  colnames(DownStatsTable)
  ## head(DownStatsTable[,1:4])
  ## Top row is just original espn column names repeated
  DownStatsTable <- DownStatsTable[-c(1),]
  
  ## some columns have commas
  # removing commas
  DownStatsTable$Penalty_Yds = gsub(",","", DownStatsTable$Penalty_Yds)
  
  ## changing DownStatsTable to have numeric values for non-team columns
  # need to do this early to normalized first down and penalty columns
  DownStatsTable[,2:ncol(DownStatsTable)] <- sapply(DownStatsTable[,2:ncol(DownStatsTable)], as.numeric)
  ## adding first downs and penalty stats as per-game stats to avoid biasing towards teams which played more games
  DownStatsTable <- DownStatsTable %>%
    dplyr::mutate(First_Downs_pg = Total_First_Downs/Games) %>%
    dplyr::mutate(Penalties_pg = Penalties/Games) %>%
    dplyr::mutate(Penalty_Yds_pg = Penalty_Yds/Games)
  ## head(DownStatsTable[,1:4])
  
  ## Selecting only variables which will be used for VoA
  ## eliminating sum stats, only keeping per-game stats
  ## kept games played in offensive stats table, don't need to leave it for other tables
  DownStatsTable <- DownStatsTable[,c("Team", "First_Downs_pg",
                                      "Third_Down_Percent",
                                      "Fourth_Down_Percent", 
                                      "Penalties_pg", "Penalty_Yds_pg")]
  
  ## Pass Defense stats
  PassDefStatsTeams <- PassDefUrl %>% read_html() %>% html_table() %>% .[[1]]
  head(PassDefStatsTeams)
  PassDefStatsTable <- PassDefUrl %>% read_html() %>% html_table() %>% .[[2]]
  head(PassDefStatsTable[,1:4])
  PassDefStatsTable <- cbind(PassDefStatsTeams, PassDefStatsTable)
  head(PassDefStatsTable[,1:4])
  ## column names are off, appear as first row instead, renaming column names
  NewPassDefColNames <- c("Team", "Games", "Completions_Allowed", 
                          "Pass_Atts_Allowed","Def_Comp_Pct_Allowed", 
                          "Passing_Yds_Allowed", "Def_Pass_YPA", 
                          "Passing_Yds_Allowed_pg", "Longest_Pass_Allowed", 
                          "Passing_Tds_Allowed", "Interceptions", "Def_Sacks", 
                          "Def_Sack_Yds_Lost", "Passer_Rtg_Allowed")
  colnames(PassDefStatsTable) <- NewPassDefColNames
  colnames(PassDefStatsTable)
  ## head(PassDefStatsTable[,1:4])
  
  ## changing PassDefStatsTable to have numeric values for non-team columns
  # need to do this early to add interceptions_pg column
  PassDefStatsTable[,2:ncol(PassDefStatsTable)] <- sapply(PassDefStatsTable[,2:ncol(PassDefStatsTable)], as.numeric)
  
  ## changing fumbles forced column to a per-game stat to avoid biasing towards teams which played more game
  PassDefStatsTable <- PassDefStatsTable %>%
    dplyr::mutate(Interceptions_pg = Interceptions/Games)
  PassDefStatsTable <- PassDefStatsTable %>%
    dplyr::mutate(Def_Sacks_pg = Def_Sacks/Games)
  PassDefStatsTable <- PassDefStatsTable %>%
    dplyr::mutate(Def_Sack_Yds_Lost_pg = Def_Sack_Yds_Lost/Games)
  
  ## Some of the ESPN tables scraped with top column being a repeat of column names
  ## For some reason, pass defense and rush defense do not do this
  ## PassDefStatsTable <- PassDefStatsTable[-c(1),]
  head(PassDefStatsTable[,1:4])
  
  ## Selecting only variables which will be used for VoA
  ## eliminating sum stats, only keeping per-game stats
  ## kept games played in offensive stats table, don't need to leave it for other tables
  ## pass yds per game already included in previous defensive stats table
  ## keeping attempts for yards per play stat later on
  PassDefStatsTable <- PassDefStatsTable[,c("Team", "Pass_Atts_Allowed","Def_Comp_Pct_Allowed", 
                                            "Def_Pass_YPA", 
                                            "Interceptions_pg", "Def_Sacks_pg", 
                                            "Def_Sack_Yds_Lost_pg")]
  
  
  ## Rush Defense stats
  RushDefStatsTeams <- RushDefUrl %>% read_html() %>% html_table() %>% .[[1]]
  ## head(RushDefStatsTeams)
  RushDefStatsTable <- RushDefUrl %>% read_html() %>% html_table() %>% .[[2]]
  ## head(RushDefStatsTable[,1:4])
  RushDefStatsTable <- cbind(RushDefStatsTeams, RushDefStatsTable)
  ## head(RushDefStatsTable[,1:4])
  ## column names are off, appear as first row instead, renaming column names
  NewRushDefColNames <- c("Team", "Games", "Rush_Att_Allowed", 
                          "Rushing_Yards_Allowed","Yds_Per_Rush_Attempt_Allowed", 
                          "Rushing_Yards_pg_Allowed", 
                          "Longest_Rush_Allowed", "Rush_Tds_Allowed", 
                          "Fumbles_Forced")
  colnames(RushDefStatsTable) <- NewRushDefColNames
  colnames(RushDefStatsTable)
  ## head(RushDefStatsTable[,1:4])
  ## changing RushDefStatsTable to have numeric values for non-team columns
  # need to do this early to add fumbles_forced_pg column
  RushDefStatsTable[,2:ncol(RushDefStatsTable)] <- sapply(RushDefStatsTable[,2:ncol(RushDefStatsTable)], as.numeric)
  
  ## changing fumbles forced column to a per-game stat to avoid biasing towards teams which played more game
  RushDefStatsTable <- RushDefStatsTable %>%
    dplyr::mutate(Fumbles_Forced_pg = Fumbles_Forced/Games)
  
  ## Some of the ESPN tables scraped with top column being a repeat of column names
  ## For some reason, pass defense and rush defense do not do this
  ## RushDefStatsTable <- RushDefStatsTable[-c(1),]
  ## head(RushDefStatsTable[,1:4])
  
  ## Selecting only variables which will be used for VoA
  ## eliminating sum stats, only keeping per-game stats
  ## kept games played in offensive stats table, don't need to leave it for other tables
  ## Rush yds per game already included in previous defensive stats table
  ## keeping attempts stat for yards per play stat later on
  RushDefStatsTable <- RushDefStatsTable[,c("Team", "Rush_Att_Allowed","Yds_Per_Rush_Attempt_Allowed",
                                            "Fumbles_Forced_pg")]
  
  ## FPI resume stats
  FPITeams <- FPIResumeURL %>% read_html() %>% html_table() %>% .[[1]]
  head(FPITeams)
  FPIResumeStats <- FPIResumeURL %>% read_html() %>% html_table() %>% .[[2]]
  head(FPIResumeStats[,1:4])
  FPIResumeTable <- cbind(FPITeams, FPIResumeStats)
  head(FPIResumeTable[,1:4])
  ## column names are off, appear as first row instead, renaming column names
  FPIResumeColNames <- c("Team", "FPI_Rank", "AP/CFP", "SOR", "SOS","Rem_SOS", 
                         "Game_Control_Rank", "Avg_WinProb")
  colnames(FPIResumeTable) <- FPIResumeColNames
  colnames(FPIResumeTable)
  head(FPIResumeTable[,1:4])
  ## Some of the ESPN tables scraped with top column being a repeat of column names
  FPIResumeTable <- FPIResumeTable[-c(1),]
  head(FPIResumeTable[,1:4])
  
  ## Selecting only variables which will be used for VoA
  ## eliminating sum stats, only keeping per-game stats
  FPIResumeTable <- FPIResumeTable[,c("Team", "FPI_Rank","SOR", "SOS", 
                                      "Game_Control_Rank", "Avg_WinProb")]
  
  FPIResumeTable <- subset(FPIResumeTable, SOS > 0)
  
  ## FPI efficiency stats
  FPITeams2 <- FPIEfficiencyURL %>% read_html() %>% html_table() %>% .[[1]]
  ## head(FPITeams2)
  FPIEfficiency <- FPIEfficiencyURL %>% read_html() %>% html_table() %>% .[[2]]
  ## head(FPIEfficiency[,1:4])
  FPIEfficiency <- cbind(FPITeams2, FPIEfficiency)
  ## head(FPIEfficiency[,1:4])
  ## column names are off, appear as first row instead, renaming column names
  FPIEfficiencyColNames <- c("Team", "FPI_Ovrl_Eff", "FPI_Ovrl_Rk", "FPI_Off_Eff", 
                             "FPI_Off_Rk","FPI_Def_Eff", "FPI_Def_Rk", 
                             "FPI_ST_Eff", "FPI_ST_Rk")
  colnames(FPIEfficiency) <- FPIEfficiencyColNames
  colnames(FPIEfficiency)
  ## head(FPIEfficiency[,1:4])
  ## Some of the ESPN tables scraped with top column being a repeat of column names
  FPIEfficiency <- FPIEfficiency[-c(1),]
  head(FPIEfficiency[,1:4])
  
  ## Selecting only variables which will be used for VoA
  FPIEfficiency <- FPIEfficiency[,c("Team", "FPI_Ovrl_Rk", "FPI_Off_Rk",
                                    "FPI_Def_Rk", "FPI_ST_Rk")]
  FPIEfficiency <- subset(FPIEfficiency, FPI_Ovrl_Rk > 0)
  
  
  ## changing FPI tables to have numeric values for non-team columns
  # doing this before merging with VoA_Variables
  # FPI stats are all already ranks, so no need to include them before
  # additional variables which will need to be included in ranks
  # FPI columns will serve as first column in range of columns to be included
  # in rowmeans() function later on
  FPIResumeTable[,2:ncol(FPIResumeTable)] <- sapply(FPIResumeTable[,2:ncol(FPIResumeTable)], as.numeric)
  FPIEfficiency[,2:ncol(FPIEfficiency)] <- sapply(FPIEfficiency[,2:ncol(FPIEfficiency)], as.numeric)
  
  ## Adding means of FPI Efficiency stats (excluding overall)
  # first just offense and defense, 2nd includes special teams
  FPIEfficiency <- FPIEfficiency %>%
    mutate(FPI_Off_Def_Mean_Rk = rowMeans(FPIEfficiency[,2:3])) %>%
    mutate(FPI_Off_Def_ST_Mean_Rk = rowMeans(FPIEfficiency[,2:4]))
}

## merging tables by in order of year, so all current year stats first
# then previous year
## because of PY opt outs, PY will be merged by itself, as will 2019 data
# PY will be merged with 2019 data first, then it will all be merged together
## FPI data will be merged the same way, then merged all together last
# regular stats merged first, FPI merged last
if (as.numeric(week) <= 4) {
  VoA_Variables <- merge(OffStatsTable, Off_Pass_Stats_Table, by = "Team")
  VoA_Variables <- merge(VoA_Variables, Off_Rush_Stats_Table, by = "Team")
  VoA_Variables <- merge(VoA_Variables, DefStatsTable, by = "Team")
  VoA_Variables <- merge(VoA_Variables, PassDefStatsTable, by = "Team")
  VoA_Variables <- merge(VoA_Variables, RushDefStatsTable, by = "Team")
  VoA_Variables <- merge(VoA_Variables, DownStatsTable, by = "Team")
  VoA_Variables <- merge(VoA_Variables, ReturnStatsTable, by = "Team")
  VoA_Variables <- merge(VoA_Variables, KickingStatsTable, by = "Team")
  ## now merging PY data
  Temp_PY_df <- merge(Off_PY_StatsTable, Off_PY_Pass_Stats_Table, by = "Team")
  Temp_PY_df <- merge(Temp_PY_df, Off_PY_Rush_Stats_Table, by = "Team")
  Temp_PY_df <- merge(Temp_PY_df, Def_PY_StatsTable, by = "Team")
  Temp_PY_df <- merge(Temp_PY_df, PassDef_PY_StatsTable, by = "Team")
  Temp_PY_df <- merge(Temp_PY_df, RushDef_PY_StatsTable, by = "Team")
  Temp_PY_df <- merge(Temp_PY_df, Down_PYStatsTable, by = "Team")
  Temp_PY_df <- merge(Temp_PY_df, Return_PYStatsTable, by = "Team")
  Temp_PY_df <- merge(Temp_PY_df, Kicking_PYStatsTable, by = "Team")
  ## now merging 2019 data
  Temp_2019_df <- merge(Off_2019_StatsTable, Off_2019_Pass_Stats_Table, by = "Team")
  Temp_2019_df <- merge(Temp_2019_df, Off_2019_Rush_Stats_Table, by = "Team")
  Temp_2019_df <- merge(Temp_2019_df, Def_2019_StatsTable, by = "Team")
  Temp_2019_df <- merge(Temp_2019_df, PassDef_2019_StatsTable, by = "Team")
  Temp_2019_df <- merge(Temp_2019_df, RushDef_2019_StatsTable, by = "Team")
  Temp_2019_df <- merge(Temp_2019_df, Down_2019StatsTable, by = "Team")
  Temp_2019_df <- merge(Temp_2019_df, Return_2019StatsTable, by = "Team")
  Temp_2019_df <- merge(Temp_2019_df, Kicking_2019StatsTable, by = "Team")
  
  ## subsetting 2019 data to only include PY opt-outs
  Temp_2019_df <- subset(Temp_2019_df, Team == "UConn Huskies" | Team == "New Mexico State Aggies" | Team == "Old Dominion Monarchs")
  ## Combining PY and subsetted 2019 data
  Temp_Prev_Year_df <- rbind(Temp_PY_df, Temp_2019_df)
  ## Now merging PY FPI data, needs to be done early to be joined with merged 2019 FPI data
  Temp_FPI_PY_df <- merge(FPI_PY_ResumeTable, FPI_PY_Efficiency, by = "Team")
  ## now merging 2019 FPI data
  Temp_FPI_2019_df <- merge(FPI_2019_ResumeTable, FPI_2019_Efficiency, by = "Team")
  ## Subsetting 2019 data to only include opt-outs
  Temp_FPI_2019_df <- subset(Temp_FPI_2019_df, Team == "UConn Huskies" | Team == "New Mexico State Aggies" | Team == "Old Dominion Monarchs")
  ## Combining PY and subsetted 2019 FPI data
  Temp_Prev_Year_FPI_df <- rbind(Temp_FPI_PY_df, Temp_FPI_2019_df)
  
  ## Merging regular stat tables
  VoA_Variables <- merge(VoA_Variables, Temp_Prev_Year_df, by = "Team")
  ## changing VoA_Variables to have numeric values for non-team columns
  VoA_Variables[,2:ncol(VoA_Variables)] <- sapply(VoA_Variables[,2:ncol(VoA_Variables)], as.numeric)
  ## adding points_per_yard, MOV, yard differential, yards/play, pts/play stats
  VoA_Variables <- VoA_Variables %>%
    mutate(PPY = Pts_pg/Total_Yds_pg) %>%
    mutate(MOV = Pts_pg - Def_Pts_pg) %>%
    mutate(ANYA = ((Total_Pass_Yds / Attempts) + ((Pass_TD * 20) + (INTs_Thrown * -40)))) %>%
    mutate(Yd_Diff = Total_Yds_pg - Def_Total_Yds_pg) %>%
    mutate(PPY_PY = Pts_pg_PY/Total_Yds_pg_PY) %>%
    mutate(MOV_PY = Pts_pg_PY - Def_Pts_pg_PY) %>%
    mutate(ANYA_PY = (Total_Pass_Yds_PY / Attempts_PY) + ((Pass_TD_PY * 20) + (INTs_Thrown_PY * -40))) %>%
    mutate(Yd_Diff_PY = Total_Yds_pg_PY - Def_Total_Yds_pg_PY) %>%
    mutate(YPP = Total_Yds / (Rush_Atts + Attempts)) %>%
    mutate(Pts_per_play = Pts_pg / (Rush_Atts+Attempts)) %>%
    mutate(YPP_PY = Total_Yds_PY / (Rush_Atts_PY + Attempts_PY)) %>%
    mutate(Pts_per_play_PY = Pts_pg_PY / (Rush_Atts_PY + Attempts_PY)) %>%
    mutate(Def_YPP = Def_Total_Yds / (Rush_Att_Allowed + Pass_Atts_Allowed)) %>%
    mutate(Def_YPP_PY = Def_Total_Yds_PY / (Rush_Att_Allowed_PY + Pass_Atts_Allowed_PY)) %>%
    mutate(Def_Pts_per_play = Def_Points / (Rush_Att_Allowed + Pass_Atts_Allowed)) %>%
    mutate(Def_Pts_per_play_PY = Def_Pts_PY / (Rush_Att_Allowed_PY + Pass_Atts_Allowed_PY)) %>%
    mutate(Expected_Wins = ((Total_Pts^2.37)/(Total_Pts^2.37 + Def_Points^2.37))*Games)
  ## adding adjusted MOV
  # attempts to account for SOS in MOV
  SOS_Adj_MOV_Vector <- VoA_Variables$MOV / FPIResumeTable$SOS
  SOS_Adj_MOV_PY_Vector <- VoA_Variables$MOV_PY / FPI_PY_ResumeTable$SOS_PY
  SOR_Adj_MOV_Vector <- VoA_Variables$MOV / FPIResumeTable$SOR
  SOR_Adj_MOV_PY_Vector <- VoA_Variables$MOV / FPI_PY_ResumeTable$SOR_PY
  VoA_Variables <- VoA_Variables %>%
    mutate(SOS_Adj_MOV = SOS_Adj_MOV_Vector) %>%
    mutate(SOS_Adj_MOV_PY = SOS_Adj_MOV_PY_Vector) %>%
    mutate(SOR_Adj_MOV = SOR_Adj_MOV_Vector) %>%
    mutate(SOR_Adj_MOV_PY = SOR_Adj_MOV_PY_Vector)
  ## now merging FPI data
  VoA_Variables <- merge(VoA_Variables, FPIResumeTable, by = "Team")
  VoA_Variables <- merge(VoA_Variables, FPIEfficiency, by = "Team")
  VoA_Variables <- merge(VoA_Variables, Temp_Prev_Year_FPI_df, by = "Team")
} else {
  ## only merge current season data
  VoA_Variables <- merge(OffStatsTable, Off_Pass_Stats_Table, by = "Team")
  VoA_Variables <- merge(VoA_Variables, Off_Rush_Stats_Table, by = "Team")
  VoA_Variables <- merge(VoA_Variables, DefStatsTable, by = "Team")
  VoA_Variables <- merge(VoA_Variables, PassDefStatsTable, by = "Team")
  VoA_Variables <- merge(VoA_Variables, RushDefStatsTable, by = "Team")
  VoA_Variables <- merge(VoA_Variables, DownStatsTable, by = "Team")
  VoA_Variables <- merge(VoA_Variables, ReturnStatsTable, by = "Team")
  VoA_Variables <- merge(VoA_Variables, KickingStatsTable, by = "Team")
  
  ## changing VoA_Variables to have numeric values for non-team columns
  VoA_Variables[,2:ncol(VoA_Variables)] <- sapply(VoA_Variables[,2:ncol(VoA_Variables)], as.numeric)
  
  ## adding points_per_yard, MOV, yard differential, pythagorean wins stats
  # pythagorean win formula created by Football Outsiders
  ## also adding adjusted net yards/ pass attempt stat
  # using modifying formula used by ESPN's Bill Connelly
  VoA_Variables <- VoA_Variables %>%
    mutate(PPY = Pts_pg/Total_Yds_pg) %>%
    mutate(ANYA = (Total_Pass_Yds / Attempts) + ((Pass_TD * 20) + (INTs_Thrown * -40))) %>%
    mutate(MOV = Pts_pg - Def_Pts_pg) %>%
    mutate(Yd_Diff = Total_Yds_pg - Def_Total_Yds_pg) %>%
    mutate(YPP = Total_Yds / (Rush_Atts + Attempts)) %>%
    mutate(Pts_per_play = Total_Pts / (Rush_Atts + Attempts)) %>%
    mutate(Def_YPP = Def_Total_Yds / (Rush_Att_Allowed + Pass_Atts_Allowed)) %>%
    mutate(Def_Pts_per_play = Def_Points / (Rush_Att_Allowed + Pass_Atts_Allowed)) %>%
    mutate(Expected_Wins = ((Total_Pts^2.37)/(Total_Pts^2.37 + Def_Points^2.37))*Games)
  
  ## adding adjusted MOV
  # attempts to account for SOS in MOV
  SOS_Adj_MOV_Vector <- VoA_Variables$MOV / FPIResumeTable$SOS
  SOR_Adj_MOV_Vector <- VoA_Variables$MOV / FPIResumeTable$SOR
  VoA_Variables <- VoA_Variables %>%
    mutate(SOS_Adj_MOV = SOS_Adj_MOV_Vector) %>%
    mutate(SOR_Adj_MOV = SOR_Adj_MOV_Vector)
  VoA_Variables <- merge(VoA_Variables, FPIResumeTable, by = "Team")
  VoA_Variables <- merge(VoA_Variables, FPIEfficiency, by = "Team")
}

## updating data frame to only include team, games, and variables 
# which are actually included in VoA
if (as.numeric(week) <= 4) {
  VoA_Variables <- VoA_Variables[,c("Team", "Games", "Total_Yds_pg", "Pass_Yds_pg", 
                                    "Rush_Yds_pg", "Pts_pg", "Comp_Pct",
                                    "Pass_YPA", "Off_YPC", "Off_INTs_pg",           
                                    "Off_Sacks_Allowed_pg", "Off_Sack_Yds_Lost_pg",           
                                    "Off_Rush_YPA", "YPP", "Pts_per_play", 
                                    "Def_Total_Yds_pg","Def_Pass_Yds_pg", 
                                    "Def_Rush_Yds_pg", "Def_Pts_pg", 
                                    "Def_Comp_Pct_Allowed", "Def_Pass_YPA", 
                                    "Interceptions_pg", "Def_Sacks_pg", 
                                    "Def_Sack_Yds_Lost_pg",
                                    "Yds_Per_Rush_Attempt_Allowed", "Def_YPP",
                                    "Def_Pts_per_play","Fumbles_Forced_pg", 
                                    "First_Downs_pg", "Third_Down_Percent", 
                                    "Fourth_Down_Percent", "Penalties_pg", 
                                    "Penalty_Yds_pg", "Kick_Return_Yd_Avg", 
                                    "Kick_Return_TD_pg", "Punt_Return_Yd_Avg", 
                                    "Punt_Return_TD_pg", "FG_Percent", 
                                    "Extra_Pts_Percent", "Games_PY",
                                    "Total_Yds_pg_PY", "Pass_Yds_pg_PY", 
                                    "Rush_Yds_pg_PY", "Pts_pg_PY", 
                                    "Comp_Pct_PY", "Pass_YPA_PY",
                                    "Off_YPC_PY","Off_INTs_pg_PY",
                                    "Off_Sacks_Allowed_pg_PY", 
                                    "Off_Sack_Yds_Lost_pg_PY",
                                    "Off_PY_Rush_YPA", "Def_Total_Yds_pg_PY",            
                                    "Def_Pass_Yds_pg_PY","Def_Rush_Yds_pg_PY",             
                                    "Def_Pts_pg_PY", "Def_Comp_Pct_Allowed_PY",
                                    "Def_Pass_YPA_PY", "Interceptions_pg_PY",
                                    "Def_Sacks_pg_PY", "Def_Sack_Yds_Lost_pg_PY",            
                                    "Yds_Per_Rush_Attempt_Allowed_PY",
                                    "Fumbles_Forced_pg_PY",   
                                    "First_Downs_pg_PY", "Third_Down_Pct_PY",
                                    "Fourth_Down_Pct_PY", "Penalties_pg_PY",
                                    "Penalty_Yds_pg_PY", "Kick_Return_Yd_Avg_PY",        
                                    "Kick_Return_TD_pg_PY", 
                                    "Punt_Return_Yd_Avg_PY",    
                                    "Punt_Return_TD_pg_PY", "FG_Pct_PY",   
                                    "Extra_Pts_Pct_PY", "PPY","MOV", "ANYA",
                                    "Yd_Diff", "PPY_PY", "MOV_PY", 
                                    "ANYA_PY", "Yd_Diff_PY", "YPP_PY", 
                                    "Pts_per_play_PY", "Def_YPP_PY",
                                    "Def_Pts_per_play_PY", "SOS_Adj_MOV", 
                                    "SOS_Adj_MOV_PY", "SOR_Adj_MOV", 
                                    "SOR_Adj_MOV_PY", "Expected_Wins",
                                    "FPI_Rank", "SOR", "SOS", 
                                    "Game_Control_Rank", "Avg_WinProb", 
                                    "FPI_Ovrl_Rk", "FPI_Off_Rk", "FPI_Def_Rk", "FPI_ST_Rk",
                                    "FPI_Off_Def_Mean_Rk","FPI_Off_Def_ST_Mean_Rk",           
                                    "FPI_Rank_PY","SOR_PY","SOS_PY",
                                    "Game_Control_Rank_PY","Avg_WinProb_PY",
                                    "FPI_Ovrl_Rk_PY", "FPI_Off_Rk_PY",
                                    "FPI_Def_Rk_PY","FPI_ST_Rk_PY", 
                                    "FPI_Off_Def_Mean_Rk_PY",         
                                    "FPI_Off_Def_ST_Mean_Rk_PY")]
} else {
  VoA_Variables <- VoA_Variables[,c("Team", "Games", "Total_Yds_pg", "Pass_Yds_pg", 
                                    "Rush_Yds_pg", "Pts_pg", "Comp_Pct",
                                    "Pass_YPA", "Off_YPC", "Off_INTs_pg",           
                                    "Off_Sacks_Allowed_pg", "Off_Sack_Yds_Lost_pg",           
                                    "Off_Rush_YPA", "Def_Total_Yds_pg",
                                    "Def_Pass_Yds_pg", "Def_Rush_Yds_pg", 
                                    "Def_Pts_pg", "Def_Comp_Pct_Allowed", 
                                    "Def_Pass_YPA", "Interceptions_pg", 
                                    "Def_Sacks_pg", "Def_Sack_Yds_Lost_pg",
                                    "Yds_Per_Rush_Attempt_Allowed", 
                                    "Fumbles_Forced_pg", "First_Downs_pg", 
                                    "Third_Down_Percent", "Fourth_Down_Percent", 
                                    "Penalties_pg", "Penalty_Yds_pg", 
                                    "Kick_Return_Yd_Avg", "Kick_Return_TD_pg", 
                                    "Punt_Return_Yd_Avg", "Punt_Return_TD_pg", 
                                    "FG_Percent", "Extra_Pts_Percent",
                                    "PPY","MOV", "ANYA", "Yd_Diff","YPP", 
                                    "Pts_per_play", "Def_YPP", 
                                    "Def_Pts_per_play", "SOS_Adj_MOV", 
                                    "SOR_Adj_MOV", "Expected_Wins", "FPI_Rank",
                                    "SOR", "SOS", "Game_Control_Rank", 
                                    "Avg_WinProb", "FPI_Ovrl_Rk", "FPI_Off_Rk", 
                                    "FPI_Def_Rk", "FPI_ST_Rk", 
                                    "FPI_Off_Def_Mean_Rk","FPI_Off_Def_ST_Mean_Rk")]
}


## adding Rank columns
## each variable (aside from team and Games Played) ranked in either ascending
# or descending order using dense_rank()

## Vars to be ranked in ascending order: "Def_Total_Yds_pg"  
# "Def_Pass_Yds_pg", "Def_Rush_Yds_pg", "Def_Pts_pg"
# "Penalties_pg", "Penalty_Yds_pg", "Def_Comp_Pct_Allowed", "Yds_Per_Attempt_Allowed",
# "Yds_Per_Rush_Attempt_Allowed", "Off_INTs_pg", "Off_Sacks_Allowed_pg", 
# "Off_Sack_Yds_Lost_pg", "Def_YPP", "Def_Pts_per_play"

## Vars to be ranked in descending order: "Total_Yds_pg",
# "Pass_Yds_pg","Rush_Yds_pg", "Pts_pg" "Kick_Return_Yd_Avg",
# "Kick_Return_TD", "Punt_Return_Yd_Avg", "Punt_Return_TD", "FG_Percent",
# "Extra_Pts_Percent", "Total_1st_Downs","Third_Down_Percent","Fourth_Down_Percent"
# "Interceptions", "Def_Sacks_pg", "Def_Sack_Yards_Lost_pg"."Fumbles_Forced","PPY", "MOV", 
# "Yd_Diff", "SOS_Adj_MOV", "SOR_Adj_MOV, "FPI_Rank","SOR", "SOS", "Game_Control_Rank", "Avg_WinProb",
# "FPI_Ovrl_Rk", "FPI_Off_Rk", "FPI_Def_Rk", "FPI_ST_Rk", "Comp_Pct", 
# "Pass_YPA", "Off_YPC", "Off_Rush_YPA", "YPP", "Pts_per_play", "
# FPI_Off_Def_Mean_Rk", "FPI_Off_Def_ST_Rk", "Expected_Wins", "ANYA"
######## IF AFTER WEEK 4 
###### rank columns for rowmeans() start at column 32, ranked stats start at column 40
if (as.numeric(week) <= 3) {
  VoA_Variables <- VoA_Variables %>%
    mutate(FPI_Rank_Col2 = FPI_Rank) %>%
    mutate(SOR_Col2 = SOR) %>%
    mutate(SOS_Col2 = SOS) %>%
    mutate(Game_Control_Rank_Col2 = Game_Control_Rank) %>%
    mutate(Avg_WinProb_Col2 = Avg_WinProb) %>%
    mutate(FPI_Ovrl_Rk_Col2 = FPI_Ovrl_Rk) %>%
    mutate(FPI_Off_Rk_Col2 = FPI_Off_Rk) %>%
    mutate(FPI_Def_Rk_Col2 = FPI_Def_Rk) %>%
    mutate(FPI_ST_Rk_Col2 = FPI_ST_Rk) %>%
    mutate(FPI_Off_Def_ST_Mean_Rk_Col2 = FPI_Off_Def_ST_Mean_Rk) %>%
    mutate(Rank_Yds_pg = dense_rank(desc(Total_Yds_pg))) %>%
    mutate(Rank_Pass_Yds_pg = dense_rank(desc(Pass_Yds_pg))) %>%
    mutate(Rank_Rush_Yds_pg = dense_rank(desc(Rush_Yds_pg))) %>%
    mutate(Rank_Pts_pg = dense_rank(desc(Pts_pg))) %>%
    mutate(Rank_Comp_Pct = dense_rank(desc(Comp_Pct))) %>%
    mutate(Rank_Pass_YPA = dense_rank(desc(Pass_YPA))) %>%
    mutate(Rank_Off_YPC = dense_rank(desc(Off_YPC))) %>%
    mutate(Rank_Off_INTs_pg = dense_rank(Off_INTs_pg)) %>%
    mutate(Rank_Off_Sacks_Allowed_pg = dense_rank(Off_Sacks_Allowed_pg)) %>%
    mutate(Rank_Off_Sack_Yds_Lost_pg = dense_rank(Off_Sack_Yds_Lost_pg)) %>%
    mutate(Rank_Off_Rush_YPA = dense_rank(desc(Off_Rush_YPA))) %>%
    mutate(Rank_Def_Yds_pg = dense_rank(Def_Total_Yds_pg)) %>%
    mutate(Rank_Def_Pass_Yds_Game = dense_rank(Def_Pass_Yds_pg)) %>%
    mutate(Rank_Def_Rush_Yds_Game = dense_rank(Def_Rush_Yds_pg)) %>%
    mutate(Rank_Def_Pts_Game = dense_rank(Def_Pts_pg)) %>%
    mutate(Rank_Kick_Return_Yd_Avg = dense_rank(desc(Kick_Return_Yd_Avg))) %>%
    mutate(Rank_Kick_Return_TD = dense_rank(desc(Kick_Return_TD_pg))) %>%
    mutate(Rank_Punt_Return_Yd_Avg = dense_rank(desc(Punt_Return_Yd_Avg))) %>%
    mutate(Rank_Punt_Return_TD = dense_rank(desc(Punt_Return_TD_pg))) %>%
    mutate(Rank_FG_Pct = dense_rank(desc(FG_Percent))) %>%
    mutate(Rank_XP_Pct = dense_rank(desc(Extra_Pts_Percent))) %>%
    mutate(Rank_1st_Downs = dense_rank(desc(First_Downs_pg))) %>%
    mutate(Rank_3rd_Down_Pct = dense_rank(desc(Third_Down_Percent))) %>%
    mutate(Rank_4th_Down_Pct = dense_rank(desc(Fourth_Down_Percent))) %>%
    mutate(Rank_Penalties = dense_rank(Penalties_pg)) %>%
    mutate(Rank_Penalty_Yds = dense_rank(Penalty_Yds_pg)) %>%
    mutate(Rank_Def_Comp_Pct_Allowed = dense_rank(Def_Comp_Pct_Allowed)) %>%
    mutate(Rank_Def_Pass_YPA = dense_rank(Def_Pass_YPA)) %>%
    mutate(Rank_Ints = dense_rank(desc(Interceptions_pg))) %>%
    mutate(Rank_Def_Sacks_pg = dense_rank(desc(Def_Sacks_pg))) %>%
    mutate(Rank_Def_Sack_Yds_pg = dense_rank(desc(Def_Sack_Yds_Lost_pg))) %>%
    mutate(Rank_Yds_Rush_Att_Allowed = dense_rank(Yds_Per_Rush_Attempt_Allowed)) %>%
    mutate(Rank_Fumbles_Forced = dense_rank(desc(Fumbles_Forced_pg))) %>%
    mutate(Rank_PPY = dense_rank(desc(PPY))) %>%
    mutate(Rank_MOV = dense_rank(desc(MOV))) %>%
    mutate(Rank_ANYA = dense_rank_desc(ANYA)) %>%
    mutate(Rank_YPP = dense_rank(desc(YPP))) %>%
    mutate(Rank_Def_YPP = dense_rank(Def_YPP)) %>%
    mutate(Rank_Pts_per_play = dense_rank(desc(Pts_per_play))) %>%
    mutate(Rank_Def_Pts_per_play = dense_rank(Def_Pts_per_play)) %>%
    mutate(Rank_Yd_Diff = dense_rank(desc(Yd_Diff))) %>%
    mutate(Rank_Expected_Wins = dense_rank(desc(Expected_Wins))) %>%
    mutate(Rank_SOS_Adj_MOV = dense_rank(desc(SOS_Adj_MOV))) %>%
    mutate(Rank_SOR_Adj_MOV = dense_rank(desc(SOR_Adj_MOV))) %>%
    ## ranking previous season columns
    mutate(Rank_Yds_pg_PY = dense_rank(desc(Total_Yds_pg_PY))) %>%
    mutate(Rank_Yds_pg_PY_Col2 = dense_rank(desc(Total_Yds_pg_PY))) %>%
    mutate(Rank_Pass_Yds_pg_PY = dense_rank(desc(Pass_Yds_pg_PY))) %>%
    mutate(Rank_Pass_Yds_pg_PY_Col2 = dense_rank(desc(Pass_Yds_pg_PY))) %>%
    mutate(Rank_Rush_Yds_pg_PY = dense_rank(desc(Rush_Yds_pg_PY))) %>%
    mutate(Rank_Rush_Yds_pg_PY_Col2 = dense_rank(desc(Rush_Yds_pg_PY))) %>%
    mutate(Rank_Pts_pg_PY = dense_rank(desc(Pts_pg_PY))) %>%
    mutate(Rank_Pts_pg_PY_Col2 = dense_rank(desc(Pts_pg_PY))) %>%
    mutate(Rank_Comp_Pct_PY = dense_rank(desc(Comp_Pct_PY))) %>%
    mutate(Rank_Pass_YPA_PY = dense_rank(desc(Pass_YPA_PY))) %>%
    mutate(Rank_Pass_YPA_PY_Col2 = dense_rank(desc(Pass_YPA_PY))) %>%
    mutate(Rank_Off_YPC_PY = dense_rank(desc(Off_YPC_PY))) %>%
    mutate(Rank_Off_INTs_pg_PY = dense_rank(Off_INTs_pg_PY)) %>%
    mutate(Rank_Off_INTs_pg_PY_Col2 = dense_rank(Off_INTs_pg_PY)) %>%
    mutate(Rank_Off_Sacks_Allowed_pg_PY = dense_rank(Off_Sacks_Allowed_pg_PY)) %>%
    mutate(Rank_Off_Sack_Yds_Lost_pg_PY = dense_rank(Off_Sack_Yds_Lost_pg_PY)) %>%
    mutate(Rank_Off_Rush_YPA_PY = dense_rank(desc(Off_PY_Rush_YPA))) %>%
    mutate(Rank_Def_Yds_pg_PY = dense_rank(Def_Total_Yds_pg_PY)) %>%
    mutate(Rank_Def_Yds_pg_PY_Col2 = dense_rank(Def_Total_Yds_pg_PY)) %>%
    mutate(Rank_Def_Pass_Yds_Game_PY = dense_rank(Def_Pass_Yds_pg_PY)) %>%
    mutate(Rank_Def_Pass_Yds_Game_PY_Col2 = dense_rank(Def_Pass_Yds_pg_PY)) %>%
    mutate(Rank_Def_Rush_Yds_Game_PY = dense_rank(Def_Rush_Yds_pg_PY)) %>%
    mutate(Rank_Def_Rush_Yds_Game_PY_Col2 = dense_rank(Def_Rush_Yds_pg_PY)) %>%
    mutate(Rank_Def_Pts_Game_PY = dense_rank(Def_Pts_pg_PY)) %>%
    mutate(Rank_Def_Pts_Game_PY_Col2 = dense_rank(Def_Pts_pg_PY)) %>%
    mutate(Rank_Kick_Return_Yd_Avg_PY = dense_rank(desc(Kick_Return_Yd_Avg_PY))) %>%
    mutate(Rank_Kick_Return_TD_PY = dense_rank(desc(Kick_Return_TD_pg_PY))) %>%
    mutate(Rank_Punt_Return_Yd_Avg_PY = dense_rank(desc(Punt_Return_Yd_Avg_PY))) %>%
    mutate(Rank_Punt_Return_TD_PY = dense_rank(desc(Punt_Return_TD_pg_PY))) %>%
    mutate(Rank_FG_Pct_PY = dense_rank(desc(FG_Pct_PY))) %>%
    mutate(Rank_XP_Pct_PY = dense_rank(desc(Extra_Pts_Pct_PY))) %>%
    mutate(Rank_1st_Downs_PY = dense_rank(desc(First_Downs_pg_PY))) %>%
    mutate(Rank_3rd_Down_Pct_PY = dense_rank(desc(Third_Down_Pct_PY))) %>%
    mutate(Rank_3rd_Down_Pct_PY_Col2 = dense_rank(desc(Third_Down_Pct_PY))) %>%
    mutate(Rank_4th_Down_Pct_PY = dense_rank(desc(Fourth_Down_Pct_PY))) %>%
    mutate(Rank_Penalties_PY = dense_rank(Penalties_pg_PY)) %>%
    mutate(Rank_Penalty_Yds_PY = dense_rank(Penalty_Yds_pg_PY)) %>%
    mutate(Rank_Def_Comp_Pct_Allowed_PY = dense_rank(Def_Comp_Pct_Allowed_PY)) %>%
    mutate(Rank_Def_Comp_Pct_Allowed_PY_Col2 = dense_rank(Def_Comp_Pct_Allowed_PY)) %>%
    mutate(Rank_Def_Pass_YPA_PY = dense_rank(Def_Pass_YPA_PY)) %>%
    mutate(Rank_Def_Pass_YPA_PY_Col2 = dense_rank(Def_Pass_YPA_PY)) %>%
    mutate(Rank_Ints_PY = dense_rank(desc(Interceptions_pg_PY))) %>%
    mutate(Rank_Ints_PY_Col2 = dense_rank(desc(Interceptions_pg_PY))) %>%
    mutate(Rank_Def_Sacks_pg_PY = dense_rank(desc(Def_Sacks_pg_PY))) %>%
    mutate(Rank_Def_Sack_Yds_pg_PY = dense_rank(desc(Def_Sack_Yds_Lost_pg_PY))) %>%
    mutate(Rank_Yds_Rush_Att_Allowed_PY = dense_rank(Yds_Per_Rush_Attempt_Allowed_PY)) %>%
    mutate(Rank_Yds_Rush_Att_Allowed_PY_Col2 = dense_rank(Yds_Per_Rush_Attempt_Allowed_PY)) %>%
    mutate(Rank_Fumbles_Forced_PY = dense_rank(desc(Fumbles_Forced_pg_PY))) %>%
    mutate(Rank_PPY_PY = dense_rank(desc(PPY_PY))) %>%
    mutate(Rank_PPY_PY_Col2 = dense_rank(desc(PPY_PY))) %>%
    mutate(Rank_MOV_PY = dense_rank(desc(MOV_PY))) %>%
    mutate(Rank_MOV_PY_Col2 = dense_rank(desc(MOV_PY))) %>%
    mutate(Rank_ANYA_PY = dense_rank(desc(ANYA_PY))) %>%
    mutate(Rank_ANYA_PY_Col2 = Rank_ANYA_PY) %>%
    mutate(Rank_YPP_PY = dense_rank(desc(YPP_PY))) %>%
    mutate(Rank_YPP_PY_Col2 = Rank_YPP_PY) %>%
    mutate(Rank_Def_YPP_PY = dense_rank(Def_YPP_PY)) %>%
    mutate(Rank_Def_YPP_PY_Col2 = Rank_Def_YPP_PY) %>%
    mutate(Rank_Pts_per_play_PY = dense_rank(desc(Pts_per_play_PY))) %>%
    mutate(Rank_Pts_per_play_PY_Col2 = Rank_Pts_per_play_PY) %>%
    mutate(Rank_Def_Pts_per_play_PY = dense_rank(Def_Pts_per_play_PY)) %>%
    mutate(Rank_Def_Pts_per_play_PY_Col2 = Rank_Def_Pts_per_play_PY) %>%
    mutate(Rank_Yd_Diff_PY = dense_rank(desc(Yd_Diff_PY))) %>%
    mutate(Rank_Yd_Diff_PY_Col2 = dense_rank(desc(Yd_Diff_PY))) %>%
    mutate(Rank_SOS_Adj_MOV_PY = dense_rank(desc(SOS_Adj_MOV_PY))) %>%
    mutate(Rank_SOS_Adj_MOV_PY_Col2 = dense_rank(desc(SOS_Adj_MOV_PY))) %>%
    mutate(Rank_SOR_Adj_MOV_PY = dense_rank(desc(SOR_Adj_MOV_PY))) %>%
    mutate(Rank_SOR_Adj_MOV_PY_Col2 = Rank_SOR_Adj_MOV_PY)
} else if (as.numeric(week) <= 4) {
  VoA_Variables <- VoA_Variables %>%
    mutate(FPI_Rank_Col2 = FPI_Rank) %>%
    mutate(SOR_Col2 = SOR) %>%
    mutate(SOS_Col2 = SOS) %>%
    mutate(Game_Control_Rank_Col2 = Game_Control_Rank) %>%
    mutate(Avg_WinProb_Col2 = Avg_WinProb) %>%
    mutate(FPI_Ovrl_Rk_Col2 = FPI_Ovrl_Rk) %>%
    mutate(FPI_Off_Rk_Col2 = FPI_Off_Rk) %>%
    mutate(FPI_Def_Rk_Col2 = FPI_Def_Rk) %>%
    mutate(FPI_ST_Rk_Col2 = FPI_ST_Rk) %>%
    mutate(FPI_Off_Def_ST_Mean_Rk_Col2 = FPI_Off_Def_ST_Mean_Rk) %>%
    mutate(Rank_Yds_pg = dense_rank(desc(Total_Yds_pg))) %>%
    mutate(Rank_Pass_Yds_pg = dense_rank(desc(Pass_Yds_pg))) %>%
    mutate(Rank_Rush_Yds_pg = dense_rank(desc(Rush_Yds_pg))) %>%
    mutate(Rank_Pts_pg = dense_rank(desc(Pts_pg))) %>%
    mutate(Rank_Comp_Pct = dense_rank(desc(Comp_Pct))) %>%
    mutate(Rank_Pass_YPA = dense_rank(desc(Pass_YPA))) %>%
    mutate(Rank_Off_YPC = dense_rank(desc(Off_YPC))) %>%
    mutate(Rank_Off_INTs_pg = dense_rank(Off_INTs_pg)) %>%
    mutate(Rank_Off_Sacks_Allowed_pg = dense_rank(Off_Sacks_Allowed_pg)) %>%
    mutate(Rank_Off_Sack_Yds_Lost_pg = dense_rank(Off_Sack_Yds_Lost_pg)) %>%
    mutate(Rank_Off_Rush_YPA = dense_rank(desc(Off_Rush_YPA))) %>%
    mutate(Rank_Def_Yds_pg = dense_rank(Def_Total_Yds_pg)) %>%
    mutate(Rank_Def_Pass_Yds_Game = dense_rank(Def_Pass_Yds_pg)) %>%
    mutate(Rank_Def_Rush_Yds_Game = dense_rank(Def_Rush_Yds_pg)) %>%
    mutate(Rank_Def_Pts_Game = dense_rank(Def_Pts_pg)) %>%
    mutate(Rank_Kick_Return_Yd_Avg = dense_rank(desc(Kick_Return_Yd_Avg))) %>%
    mutate(Rank_Kick_Return_TD = dense_rank(desc(Kick_Return_TD_pg))) %>%
    mutate(Rank_Punt_Return_Yd_Avg = dense_rank(desc(Punt_Return_Yd_Avg))) %>%
    mutate(Rank_Punt_Return_TD = dense_rank(desc(Punt_Return_TD_pg))) %>%
    mutate(Rank_FG_Pct = dense_rank(desc(FG_Percent))) %>%
    mutate(Rank_XP_Pct = dense_rank(desc(Extra_Pts_Percent))) %>%
    mutate(Rank_1st_Downs = dense_rank(desc(First_Downs_pg))) %>%
    mutate(Rank_3rd_Down_Pct = dense_rank(desc(Third_Down_Percent))) %>%
    mutate(Rank_4th_Down_Pct = dense_rank(desc(Fourth_Down_Percent))) %>%
    mutate(Rank_Penalties = dense_rank(Penalties_pg)) %>%
    mutate(Rank_Penalty_Yds = dense_rank(Penalty_Yds_pg)) %>%
    mutate(Rank_Def_Comp_Pct_Allowed = dense_rank(Def_Comp_Pct_Allowed)) %>%
    mutate(Rank_Def_Pass_YPA = dense_rank(Def_Pass_YPA)) %>%
    mutate(Rank_Ints = dense_rank(desc(Interceptions_pg))) %>%
    mutate(Rank_Def_Sacks_pg = dense_rank(desc(Def_Sacks_pg))) %>%
    mutate(Rank_Def_Sack_Yds_pg = dense_rank(desc(Def_Sack_Yds_Lost_pg))) %>%
    mutate(Rank_Yds_Rush_Att_Allowed = dense_rank(Yds_Per_Rush_Attempt_Allowed)) %>%
    mutate(Rank_Fumbles_Forced = dense_rank(desc(Fumbles_Forced_pg))) %>%
    mutate(Rank_PPY = dense_rank(desc(PPY))) %>%
    mutate(Rank_MOV = dense_rank(desc(MOV))) %>%
    mutate(Rank_MOV_Col2 = Rank_MOV) %>%
    mutate(Rank_ANYA = dense_rank(desc(ANYA))) %>%
    mutate(Rank_ANYA_Col2 = Rank_ANYA) %>%
    mutate(Rank_YPP = dense_rank(desc(YPP))) %>%
    mutate(Rank_YPP_Col2 = Rank_YPP) %>%
    mutate(Rank_Def_YPP = dense_rank(Def_YPP)) %>%
    mutate(Rank_Def_YPP_Col2 = Rank_Def_YPP) %>%
    mutate(Rank_Pts_per_play = dense_rank(desc(Pts_per_play))) %>%
    mutate(Rank_Pts_per_play_Col2 = Rank_Pts_per_play) %>%
    mutate(Rank_Def_Pts_per_play = dense_rank(Def_Pts_per_play)) %>%
    mutate(Rank_Def_Pts_per_play_Col2 = Rank_Def_Pts_per_play) %>%
    mutate(Rank_Yd_Diff = dense_rank(desc(Yd_Diff))) %>%
    mutate(Rank_Yd_Diff_Col2 = Rank_Yd_Diff) %>%
    mutate(Rank_Expected_Wins = dense_rank(desc(Expected_Wins))) %>%
    mutate(Rank_Expected_Wins_Col2 = Rank_Expected_Wins) %>%
    mutate(Rank_SOS_Adj_MOV = dense_rank(desc(SOS_Adj_MOV))) %>%
    mutate(Rank_SOS_Adj_MOV_Col2 = Rank_SOS_Adj_MOV) %>%
    mutate(Rank_SOR_Adj_MOV = dense_rank(desc(SOR_Adj_MOV))) %>%
    mutate(Rank_SOR_Adj_MOV_Col2 = Rank_SOR_Adj_MOV) %>%
    ## ranking previous season columns
    mutate(Rank_Yds_pg_PY = dense_rank(desc(Total_Yds_pg_PY))) %>%
    mutate(Rank_Pass_Yds_pg_PY = dense_rank(desc(Pass_Yds_pg_PY))) %>%
    mutate(Rank_Rush_Yds_pg_PY = dense_rank(desc(Rush_Yds_pg_PY))) %>%
    mutate(Rank_Pts_pg_PY = dense_rank(desc(Pts_pg_PY))) %>%
    mutate(Rank_Comp_Pct_PY = dense_rank(desc(Comp_Pct_PY))) %>%
    mutate(Rank_Pass_YPA_PY = dense_rank(desc(Pass_YPA_PY))) %>%
    mutate(Rank_Off_YPC_PY = dense_rank(desc(Off_YPC_PY))) %>%
    mutate(Rank_Off_INTs_pg_PY = dense_rank(Off_INTs_pg_PY)) %>%
    mutate(Rank_Off_Sack_Yds_Lost_pg_PY = dense_rank(Off_Sack_Yds_Lost_pg_PY)) %>%
    mutate(Rank_Off_Rush_YPA_PY = dense_rank(desc(Off_PY_Rush_YPA))) %>%
    mutate(Rank_Def_Yds_pg_PY = dense_rank(Def_Total_Yds_pg_PY)) %>%
    mutate(Rank_Def_Pass_Yds_Game_PY = dense_rank(Def_Pass_Yds_pg_PY)) %>%
    mutate(Rank_Def_Rush_Yds_Game_PY = dense_rank(Def_Rush_Yds_pg_PY)) %>%
    mutate(Rank_Def_Pts_Game_PY = dense_rank(Def_Pts_pg_PY)) %>%
    mutate(Rank_FG_Pct_PY = dense_rank(desc(FG_Pct_PY))) %>%
    mutate(Rank_3rd_Down_Pct_PY = dense_rank(desc(Third_Down_Pct_PY))) %>%
    mutate(Rank_Def_Comp_Pct_Allowed_PY = dense_rank(Def_Comp_Pct_Allowed_PY)) %>%
    mutate(Rank_Def_Pass_YPA_PY = dense_rank(Def_Pass_YPA_PY)) %>%
    mutate(Rank_Ints_PY = dense_rank(desc(Interceptions_pg_PY))) %>%
    mutate(Rank_Def_Sack_Yds_pg_PY = dense_rank(desc(Def_Sack_Yds_Lost_pg_PY))) %>%
    mutate(Rank_Yds_Rush_Att_Allowed_PY = dense_rank(Yds_Per_Rush_Attempt_Allowed_PY)) %>%
    mutate(Rank_PPY_PY = dense_rank(desc(PPY_PY))) %>%
    mutate(Rank_MOV_PY = dense_rank(desc(MOV_PY))) %>%
    mutate(Rank_ANYA_PY = dense_rank(desc(ANYA_PY))) %>%
    mutate(Rank_YPP_PY = dense_rank(desc(YPP_PY))) %>%
    mutate(Rank_Def_YPP_PY = dense_rank(Def_YPP_PY)) %>%
    mutate(Rank_Pts_per_play_PY = dense_rank(desc(Pts_per_play_PY))) %>%
    mutate(Rank_Def_Pts_per_play_PY = dense_rank(Def_Pts_per_play_PY)) %>%
    mutate(Rank_Yd_Diff_PY = dense_rank(desc(Yd_Diff_PY))) %>%
    mutate(Rank_SOS_Adj_MOV_PY = dense_rank(desc(SOS_Adj_MOV_PY))) %>%
    mutate(Rank_SOR_Adj_MOV_PY = dense_rank(desc(SOR_Adj_MOV_PY)))
} else {
  VoA_Variables <- VoA_Variables %>%
    mutate(FPI_Rank_Col2 = FPI_Rank) %>%
    mutate(SOR_Col2 = SOR) %>%
    mutate(SOS_Col2 = SOS) %>%
    mutate(Game_Control_Rank_Col2 = Game_Control_Rank) %>%
    mutate(Avg_WinProb_Col2 = Avg_WinProb) %>%
    mutate(FPI_Ovrl_Rk_Col2 = FPI_Ovrl_Rk) %>%
    mutate(FPI_Off_Rk_Col2 = FPI_Off_Rk) %>%
    mutate(FPI_Def_Rk_Col2 = FPI_Def_Rk) %>%
    mutate(FPI_ST_Rk_Col2 = FPI_ST_Rk) %>%
    mutate(FPI_Off_Def_ST_Mean_Rk_Col2 = FPI_Off_Def_ST_Mean_Rk) %>%
    mutate(Rank_Yds_pg = dense_rank(desc(Total_Yds_pg))) %>%
    mutate(Rank_Yds_pg_Col2 = Rank_Yds_pg) %>%
    mutate(Rank_Pass_Yds_pg = dense_rank(desc(Pass_Yds_pg))) %>%
    mutate(Rank_Rush_Yds_pg = dense_rank(desc(Rush_Yds_pg))) %>%
    mutate(Rank_Pts_pg = dense_rank(desc(Pts_pg))) %>%
    mutate(Rank_Pts_pg_Col2 = Rank_Pts_pg) %>%
    mutate(Rank_Comp_Pct = dense_rank(desc(Comp_Pct))) %>%
    mutate(Rank_Pass_YPA = dense_rank(desc(Pass_YPA))) %>%
    mutate(Rank_Off_YPC = dense_rank(desc(Off_YPC))) %>%
    mutate(Rank_Off_INTs_pg = dense_rank(Off_INTs_pg)) %>%
    mutate(Rank_Off_Sacks_Allowed_pg = dense_rank(Off_Sacks_Allowed_pg)) %>%
    mutate(Rank_Off_Sack_Yds_Lost_pg = dense_rank(Off_Sack_Yds_Lost_pg)) %>%
    mutate(Rank_Off_Rush_YPA = dense_rank(desc(Off_Rush_YPA))) %>%
    mutate(Rank_Def_Yds_pg = dense_rank(Def_Total_Yds_pg)) %>%
    mutate(Rank_Def_Yds_pg_Col2 = Rank_Def_Yds_pg) %>%
    mutate(Rank_Def_Pass_Yds_pg = dense_rank(Def_Pass_Yds_pg)) %>%
    mutate(Rank_Def_Rush_Yds_pg = dense_rank(Def_Rush_Yds_pg)) %>%
    mutate(Rank_Def_Pts_pg = dense_rank(Def_Pts_pg)) %>%
    mutate(Rank_Def_Pts_pg_Col2 = Rank_Def_Pts_pg) %>%
    mutate(Rank_Kick_Return_Yd_Avg = dense_rank(desc(Kick_Return_Yd_Avg))) %>%
    mutate(Rank_Kick_Return_TD = dense_rank(desc(Kick_Return_TD_pg))) %>%
    mutate(Rank_Punt_Return_Yd_Avg = dense_rank(desc(Punt_Return_Yd_Avg))) %>%
    mutate(Rank_Punt_Return_TD = dense_rank(desc(Punt_Return_TD_pg))) %>%
    mutate(Rank_FG_Pct = dense_rank(desc(FG_Percent))) %>%
    mutate(Rank_XP_Pct = dense_rank(desc(Extra_Pts_Percent))) %>%
    mutate(Rank_1st_Downs = dense_rank(desc(First_Downs_pg))) %>%
    mutate(Rank_3rd_Down_Pct = dense_rank(desc(Third_Down_Percent))) %>%
    mutate(Rank_4th_Down_Pct = dense_rank(desc(Fourth_Down_Percent))) %>%
    mutate(Rank_Penalties_pg = dense_rank(Penalties_pg)) %>%
    mutate(Rank_Penalty_Yds_pg = dense_rank(Penalty_Yds_pg)) %>%
    mutate(Rank_Def_Comp_Pct_Allowed = dense_rank(Def_Comp_Pct_Allowed)) %>%
    mutate(Rank_Def_Pass_YPA = dense_rank(Def_Pass_YPA)) %>%
    mutate(Rank_Ints_pg = dense_rank(desc(Interceptions_pg))) %>%
    mutate(Rank_Def_Sacks_pg = dense_rank(desc(Def_Sacks_pg))) %>%
    mutate(Rank_Def_Sack_Yds_pg = dense_rank(desc(Def_Sack_Yds_Lost_pg))) %>%
    mutate(Rank_Yds_Rush_Att_Allowed = dense_rank(Yds_Per_Rush_Attempt_Allowed)) %>%
    mutate(Rank_Fumbles_Forced = dense_rank(desc(Fumbles_Forced_pg))) %>%
    mutate(Rank_PPY = dense_rank(desc(PPY))) %>%
    mutate(Rank_PPY_Col2 = Rank_PPY) %>%
    mutate(Rank_MOV = dense_rank(desc(MOV))) %>%
    mutate(Rank_MOV_Col2 = Rank_MOV) %>%
    mutate(Rank_ANYA = dense_rank(desc(ANYA))) %>%
    mutate(Rank_ANYA_Col2 = Rank_ANYA) %>%
    mutate(Rank_YPP = dense_rank(desc(YPP))) %>%
    mutate(Rank_YPP_Col2 = Rank_YPP) %>%
    mutate(Rank_Def_YPP = dense_rank(Def_YPP)) %>%
    mutate(Rank_Def_YPP_Col2 = Rank_Def_YPP) %>%
    mutate(Rank_Pts_per_play = dense_rank(desc(Pts_per_play))) %>%
    mutate(Rank_Pts_per_play_Col2 = Rank_Pts_per_play) %>%
    mutate(Rank_Def_Pts_per_play = dense_rank(Def_Pts_per_play)) %>%
    mutate(Rank_Def_Pts_per_play_Col2 = Rank_Def_Pts_per_play) %>%
    mutate(Rank_Yd_Diff = dense_rank(desc(Yd_Diff))) %>%
    mutate(Rank_Yd_Diff_Col2 = Rank_Yd_Diff) %>%
    mutate(Rank_Expected_Wins = dense_rank(desc(Expected_Wins))) %>%
    mutate(Rank_Expected_Wins_Col2 = Rank_Expected_Wins) %>%
    mutate(Rank_SOS_Adj_MOV = dense_rank(desc(SOS_Adj_MOV))) %>%
    mutate(Rank_SOS_Adj_MOV_Col2 = Rank_SOS_Adj_MOV) %>%
    mutate(Rank_SOR_Adj_MOV = dense_rank(desc(SOR_Adj_MOV))) %>%
    mutate(Rank_SOR_Adj_MOV_Col2 = Rank_SOR_Adj_MOV)
}

## getting an error in the below if statement about something not being numeric?
## let's hope this fixes that
## it fixed it, but I'll leave these comments here to document the frustration
## changing VoA_Variables to have numeric values for non-team columns
VoA_Variables[,2:ncol(VoA_Variables)] <- sapply(VoA_Variables[,2:ncol(VoA_Variables)], as.numeric)

### START ROW MEANS FUNCTION AT column 89 IF IT IS WEEK 4 OR EARLIER
## if week 4 or earlier, FPI ranks start at column 89, ranked stats start at column 99
## after week 4, FPI ranks start at column 47, ranked stats start at 68
## Taking the mean of all individual stat rankings
if (as.numeric(week) <= 4) {
  VoA_Variables <- VoA_Variables %>%
    mutate(VoA_Output = (rowMeans(VoA_Variables[,89:ncol(VoA_Variables)]))) %>%
    ## Append column of VoA Final Rankings
    mutate(VoA_Ranking = dense_rank(VoA_Output))
} else {
  VoA_Variables <- VoA_Variables %>%
    mutate(VoA_Output = (rowMeans(VoA_Variables[,47:ncol(VoA_Variables)]))) %>%
    ## Append column of VoA Final Rankings
    mutate(VoA_Ranking = dense_rank(VoA_Output))
}

## Adding Column with CFB Week number
# same number for each team, numeric version of number input in readline function at beginning of script
VoA_Variables <- VoA_Variables %>%
  mutate(CFB_Week = rep(as.numeric(week), 130))

## creating data frame with just team, VoA ranking, and VoA output
## Create Vector of Team name, VoA Ranking
FinalTable <- VoA_Variables %>%
  select(Team, CFB_Week, VoA_Output, VoA_Ranking) %>%
  arrange(VoA_Ranking)
FinalVoATop25 <- subset(FinalTable, FinalTable$VoA_Ranking < 26)
tail(FinalVoATop25)
## FinalVoATop25
Final_gt_table <- FinalTable %>%
  select(Team, VoA_Output, VoA_Ranking)
Final_gt_Top25 <- FinalVoATop25 %>%
  select(Team, VoA_Output, VoA_Ranking)


## setting strings for table titles, file pathways, unintelligible charts
output_dir <- here("RVoA", "Outputs")
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

## Top 25 Table
# adding title and subtitle
VoATop25Table <- Final_gt_Top25 %>%
  gt() %>% # use 'gt' to make an awesome table...
  gt_theme_espn() %>%
  tab_header(
    title = paste(year, week_text, week, VoA_Top25_text), # ...with this title
    subtitle = "Supremely Excellent Yet Salaciously Godlike And Infallibly Magnificent Vortex of Accuracy")  %>%  # and this subtitle
  ## tab_style(style = cell_fill("bisque"),
  ##           locations = cells_body()) %>%  # add fill color to table
  fmt_number( # A column (numeric data)
    columns = vars(VoA_Output), # What column variable? FinalVoATop25$VoA_Output
    decimals = 5 # With four decimal places
  ) %>% 
  fmt_number( # Another column (also numeric data)
    columns = vars(VoA_Ranking), # What column variable? FinalVoATop25$VoA_Ranking
    decimals = 0 # I want this column to have zero decimal places
  ) %>%
  data_color( # Update cell colors, testing different color palettes
    columns = vars(VoA_Output), # ...for dose column
    colors = scales::col_numeric( # <- bc it's numeric
      palette = brewer.pal(9, "Reds"), # A color scheme (gradient)
      domain = c(), # Column scale endpoints
      reverse = TRUE
    )
  ) %>%
  cols_label(VoA_Output = "Final VoA Output", VoA_Ranking = "VoA Ranking") %>% # Update labels
  cols_move_to_end(columns = "VoA_Output") %>%
  tab_footnote(
    footnote = "Data from ESPN.com"
  )
VoATop25Table
VoATop25Table %>%
  gtsave(
    top25_file_pathway, expand = 5,
    path = here("RVoA", "Outputs")
  )

## Full 130 teams table
# adding title and subtitle
VoA_Full_Table <- Final_gt_table %>%
  gt() %>% # use 'gt' to make an awesome table...
  gt_theme_538() %>%
  tab_header(
    title = paste(year, week_text, week, VoA_text), # ...with this title
    subtitle = "Supremely Excellent Yet Salaciously Godlike And Infallibly Magnificent Vortex of Accuracy")  %>%  # and this subtitle
  ##tab_style(style = cell_fill("bisque"),
    ##        locations = cells_body()) %>%  # add fill color to table
  fmt_number( # A column (numeric data)
    columns = vars(VoA_Output), # What column variable? FinalVoATop25$VoA_Output
    decimals = 5 # With four decimal places
  ) %>% 
  fmt_number( # Another column (also numeric data)
    columns = vars(VoA_Ranking), # What column variable? FinalVoATop25$VoA_Ranking
    decimals = 0 # I want this column to have zero decimal places
  ) %>% 
  data_color( # Update cell colors, testing different color palettes
    columns = vars(VoA_Output), # ...for dose column
    colors = scales::col_numeric( # <- bc it's numeric
      palette = brewer.pal(9, "RdBu"), # A color scheme (gradient)
      domain = c(), # Column scale endpoints
      reverse = FALSE
    )
  ) %>%
  cols_label(VoA_Output = "Final VoA Output", VoA_Ranking = "VoA Ranking") %>% # Update labels
  cols_move_to_end(columns = "VoA_Output") %>%
  tab_footnote(
    footnote = "Data from ESPN.com"
  )
VoA_Full_Table
VoA_Full_Table %>%
  gtsave(
    fulltable_file_pathway, expand = 5,
    path = here("RVoA", "Outputs")
  )

## creating string for excel spreadsheet pathway
file_pathway <- paste(output_dir, "/",week_text, week,"_",year, VoAString, sep = "")

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
#     columns = vars(Team) # First column: team (character)
#   ) %>%
#   fmt_number(
#     columns = vars(VoA_Output), # Second column: VoA_Output (numeric)
#     decimals = 5 # With 5 decimal places
#   ) %>%
#   fmt_number(
#     columns = vars(VoA_Ranking), # Third column: VoA_Ranking (numeric)
#     decimals = 0 # With 0 decimal places
#   ) %>%
#   #  data_color( # Update cell colors...
#   #    columns = vars(VoA_Output), # ...for dose column
#   #    colors = scales::col_numeric( # <- bc it's numeric
#   #      palette = c(
#   #        "dodgerblue4","cadetblue1"), # A color scheme (gradient)
#   #      domain = c() # Column scale endpoints
#   #    )
#   # ) %>%
#   data_color( # Update cell colors, testing different color palettes
#     columns = vars(VoA_Output), # ...for VoA_Output column
#     colors = scales::col_numeric( # <- bc it's numeric
#       palette = brewer.pal(9, "Reds"), # A color scheme (gradient)
#       domain = c() # Column scale endpoints
#     )
#   ) %>%
#   cols_label(Team = "Team", VoA_Output = "Final VoA Output", VoA_Ranking = "VoA Ranking") %>% # Make the column headers
#   tab_footnote(
#     footnote = "rounded to 5 decimals", # Another line of footnote text
#     locations = cells_column_labels(
#       columns = vars(VoA_Output) # Associated with column 'VoA_Output'
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
## Tracks VoA Outputs and Rankings by week
## reading in VoP spreadsheet for 2021/22 season, will phase this out next season
## will make preseason projections "VoA Week 0 Rankings"
VoP <- read.xlsx(xlsxFile = here("Data", "VoA2021", "VoP_2021.xlsx"))
colnames(VoP) <- VoP[1,]
VoP <- VoP[2:131,]
VoP <- VoP %>%
  select(Team, CFB_Week, VoA_Output, VoA_Ranking)
## colnames(VoP) <- c("Team", "VoA_Output", "VoA_Ranking")

## now reading in VoA data up to current week
if (as.numeric(week) == 2) {
  Week1_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week1_2021VoA.xlsx"))
  ## colnames(Week1_VoA) <- Week1_VoA[1,]
  ## Week1_VoA <- Week1_VoA[2:131,]
  Week1_VoA <- Week1_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
} else if (as.numeric(week) == 3) {
  Week1_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week1_2021VoA.xlsx"))
  Week1_VoA <- Week1_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week2_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week2_2021VoA.xlsx"))
  Week2_VoA <- Week2_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
} else if (as.numeric(week) == 4) {
  Week1_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week1_2021VoA.xlsx"))
  Week1_VoA <- Week1_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week2_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week2_2021VoA.xlsx"))
  Week2_VoA <- Week2_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week3_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week3_2021VoA.xlsx"))
  Week3_VoA <- Week3_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
} else if (as.numeric(week) == 5) {
  Week1_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week1_2021VoA.xlsx"))
  Week1_VoA <- Week1_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week2_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week2_2021VoA.xlsx"))
  Week2_VoA <- Week2_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week3_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week3_2021VoA.xlsx"))
  Week3_VoA <- Week3_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week4_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week4_2021VoA.xlsx"))
  Week4_VoA <- Week4_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
} else if (as.numeric(week) == 6) {
  Week1_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week1_2021VoA.xlsx"))
  Week1_VoA <- Week1_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week2_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week2_2021VoA.xlsx"))
  Week2_VoA <- Week2_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week3_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week3_2021VoA.xlsx"))
  Week3_VoA <- Week3_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week4_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week4_2021VoA.xlsx"))
  Week4_VoA <- Week4_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week5_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week5_2021VoA.xlsx"))
  Week5_VoA <- Week5_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
} else if (as.numeric(week) == 7) {
  Week1_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week1_2021VoA.xlsx"))
  Week1_VoA <- Week1_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week2_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week2_2021VoA.xlsx"))
  Week2_VoA <- Week2_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week3_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week3_2021VoA.xlsx"))
  Week3_VoA <- Week3_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week4_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week4_2021VoA.xlsx"))
  Week4_VoA <- Week4_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week5_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week5_2021VoA.xlsx"))
  Week5_VoA <- Week5_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week6_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week6_2021VoA.xlsx"))
  Week6_VoA <- Week6_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
} else if (as.numeric(week) == 8) {
  Week1_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week1_2021VoA.xlsx"))
  Week1_VoA <- Week1_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week2_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week2_2021VoA.xlsx"))
  Week2_VoA <- Week2_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week3_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week3_2021VoA.xlsx"))
  Week3_VoA <- Week3_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week4_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week4_2021VoA.xlsx"))
  Week4_VoA <- Week4_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week5_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week5_2021VoA.xlsx"))
  Week5_VoA <- Week5_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week6_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week6_2021VoA.xlsx"))
  Week6_VoA <- Week6_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week7_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week7_2021VoA.xlsx"))
  Week7_VoA <- Week7_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
} else if (as.numeric(week) == 9) {
  Week1_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week1_2021VoA.xlsx"))
  Week1_VoA <- Week1_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week2_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week2_2021VoA.xlsx"))
  Week2_VoA <- Week2_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week3_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week3_2021VoA.xlsx"))
  Week3_VoA <- Week3_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week4_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week4_2021VoA.xlsx"))
  Week4_VoA <- Week4_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week5_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week5_2021VoA.xlsx"))
  Week5_VoA <- Week5_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week6_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week6_2021VoA.xlsx"))
  Week6_VoA <- Week6_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week7_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week7_2021VoA.xlsx"))
  Week7_VoA <- Week7_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week8_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week8_2021VoA.xlsx"))
  Week8_VoA <- Week8_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
} else if (as.numeric(week) == 10) {
  Week1_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week1_2021VoA.xlsx"))
  Week1_VoA <- Week1_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week2_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week2_2021VoA.xlsx"))
  Week2_VoA <- Week2_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week3_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week3_2021VoA.xlsx"))
  Week3_VoA <- Week3_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week4_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week4_2021VoA.xlsx"))
  Week4_VoA <- Week4_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week5_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week5_2021VoA.xlsx"))
  Week5_VoA <- Week5_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week6_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week6_2021VoA.xlsx"))
  Week6_VoA <- Week6_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week7_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week7_2021VoA.xlsx"))
  Week7_VoA <- Week7_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week8_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week8_2021VoA.xlsx"))
  Week8_VoA <- Week8_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week9_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week9_2021VoA.xlsx"))
  Week9_VoA <- Week9_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
} else if (as.numeric(week) == 11) {
  Week1_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week1_2021VoA.xlsx"))
  Week1_VoA <- Week1_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week2_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week2_2021VoA.xlsx"))
  Week2_VoA <- Week2_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week3_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week3_2021VoA.xlsx"))
  Week3_VoA <- Week3_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week4_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week4_2021VoA.xlsx"))
  Week4_VoA <- Week4_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week5_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week5_2021VoA.xlsx"))
  Week5_VoA <- Week5_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week6_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week6_2021VoA.xlsx"))
  Week6_VoA <- Week6_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week7_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week7_2021VoA.xlsx"))
  Week7_VoA <- Week7_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week8_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week8_2021VoA.xlsx"))
  Week8_VoA <- Week8_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week9_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week9_2021VoA.xlsx"))
  Week9_VoA <- Week9_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week10_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week10_2021VoA.xlsx"))
  Week10_VoA <- Week10_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
} else if (as.numeric(week) == 12) {
  Week1_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week1_2021VoA.xlsx"))
  Week1_VoA <- Week1_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week2_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week2_2021VoA.xlsx"))
  Week2_VoA <- Week2_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week3_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week3_2021VoA.xlsx"))
  Week3_VoA <- Week3_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week4_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week4_2021VoA.xlsx"))
  Week4_VoA <- Week4_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week5_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week5_2021VoA.xlsx"))
  Week5_VoA <- Week5_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week6_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week6_2021VoA.xlsx"))
  Week6_VoA <- Week6_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week7_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week7_2021VoA.xlsx"))
  Week7_VoA <- Week7_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week8_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week8_2021VoA.xlsx"))
  Week8_VoA <- Week8_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week9_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week9_2021VoA.xlsx"))
  Week9_VoA <- Week9_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week10_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week10_2021VoA.xlsx"))
  Week10_VoA <- Week10_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week11_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week11_2021VoA.xlsx"))
  Week11_VoA <- Week11_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
} else if (as.numeric(week) == 13) {
  Week1_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week1_2021VoA.xlsx"))
  Week1_VoA <- Week1_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week2_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week2_2021VoA.xlsx"))
  Week2_VoA <- Week2_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week3_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week3_2021VoA.xlsx"))
  Week3_VoA <- Week3_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week4_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week4_2021VoA.xlsx"))
  Week4_VoA <- Week4_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week5_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week5_2021VoA.xlsx"))
  Week5_VoA <- Week5_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week6_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week6_2021VoA.xlsx"))
  Week6_VoA <- Week6_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week7_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week7_2021VoA.xlsx"))
  Week7_VoA <- Week7_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week8_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week8_2021VoA.xlsx"))
  Week8_VoA <- Week8_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week9_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week9_2021VoA.xlsx"))
  Week9_VoA <- Week9_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week10_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week10_2021VoA.xlsx"))
  Week10_VoA <- Week10_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week11_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week11_2021VoA.xlsx"))
  Week11_VoA <- Week11_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week12_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week12_2021VoA.xlsx"))
  Week12_VoA <- Week12_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
} else if (as.numeric(week) == 14) {
  Week1_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week1_2021VoA.xlsx"))
  Week1_VoA <- Week1_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week2_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week2_2021VoA.xlsx"))
  Week2_VoA <- Week2_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week3_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week3_2021VoA.xlsx"))
  Week3_VoA <- Week3_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week4_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week4_2021VoA.xlsx"))
  Week4_VoA <- Week4_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week5_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week5_2021VoA.xlsx"))
  Week5_VoA <- Week5_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week6_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week6_2021VoA.xlsx"))
  Week6_VoA <- Week6_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week7_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week7_2021VoA.xlsx"))
  Week7_VoA <- Week7_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week8_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week8_2021VoA.xlsx"))
  Week8_VoA <- Week8_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week9_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week9_2021VoA.xlsx"))
  Week9_VoA <- Week9_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week10_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week10_2021VoA.xlsx"))
  Week10_VoA <- Week10_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week11_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week11_2021VoA.xlsx"))
  Week11_VoA <- Week11_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week12_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week12_2021VoA.xlsx"))
  Week12_VoA <- Week12_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week13_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week13_2021VoA.xlsx"))
  Week13_VoA <- Week13_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
} else if (as.numeric(week) == 15) {
  Week1_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week1_2021VoA.xlsx"))
  Week1_VoA <- Week1_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week2_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week2_2021VoA.xlsx"))
  Week2_VoA <- Week2_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week3_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week3_2021VoA.xlsx"))
  Week3_VoA <- Week3_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week4_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week4_2021VoA.xlsx"))
  Week4_VoA <- Week4_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week5_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week5_2021VoA.xlsx"))
  Week5_VoA <- Week5_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week6_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week6_2021VoA.xlsx"))
  Week6_VoA <- Week6_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week7_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week7_2021VoA.xlsx"))
  Week7_VoA <- Week7_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week8_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week8_2021VoA.xlsx"))
  Week8_VoA <- Week8_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week9_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week9_2021VoA.xlsx"))
  Week9_VoA <- Week9_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week10_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week10_2021VoA.xlsx"))
  Week10_VoA <- Week10_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week11_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week11_2021VoA.xlsx"))
  Week11_VoA <- Week11_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week12_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week12_2021VoA.xlsx"))
  Week12_VoA <- Week12_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week13_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week13_2021VoA.xlsx"))
  Week13_VoA <- Week13_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
  Week14_VoA <- read.xlsx(xlsxFile = here("RVoA", "Outputs", "Week14_2021VoA.xlsx"))
  Week14_VoA <- Week14_VoA %>%
    select(Team, CFB_Week, VoA_Output, VoA_Ranking)
} else {
  print("No charts until Week 2!")
}

## Creating if statement to rbind tables up to current week (FinalTable)
if (as.numeric(week) == 2) {
  Full_Outputs_Rks <- rbind(VoP, Week1_VoA)
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, FinalTable)
  Full_Outputs_Rks[,2:ncol(Full_Outputs_Rks)] <- sapply(Full_Outputs_Rks[,2:ncol(Full_Outputs_Rks)], as.numeric)
} else if (as.numeric(week) == 3) {
  Full_Outputs_Rks <- rbind(VoP, Week1_VoA)
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week2_VoA)
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, FinalTable)
  Full_Outputs_Rks[,2:ncol(Full_Outputs_Rks)] <- sapply(Full_Outputs_Rks[,2:ncol(Full_Outputs_Rks)], as.numeric)
} else if (as.numeric(week) == 4) {
  Full_Outputs_Rks <- rbind(VoP, Week1_VoA) 
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week2_VoA)
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week3_VoA)
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, FinalTable)
  Full_Outputs_Rks[,2:ncol(Full_Outputs_Rks)] <- sapply(Full_Outputs_Rks[,2:ncol(Full_Outputs_Rks)], as.numeric)
} else if (as.numeric(week) == 5) {
  Full_Outputs_Rks <- rbind(VoP, Week1_VoA) 
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week2_VoA)
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week3_VoA)
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week4_VoA) 
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, FinalTable)
  Full_Outputs_Rks[,2:ncol(Full_Outputs_Rks)] <- sapply(Full_Outputs_Rks[,2:ncol(Full_Outputs_Rks)], as.numeric)
} else if (as.numeric(week) == 6) {
  Full_Outputs_Rks <- rbind(VoP, Week1_VoA) 
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week2_VoA)
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week3_VoA)
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week4_VoA) 
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week5_VoA)
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, FinalTable)
  Full_Outputs_Rks[,2:ncol(Full_Outputs_Rks)] <- sapply(Full_Outputs_Rks[,2:ncol(Full_Outputs_Rks)], as.numeric)
} else if (as.numeric(week) == 7) {
  Full_Outputs_Rks <- rbind(VoP, Week1_VoA) 
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week2_VoA)
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week3_VoA)
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week4_VoA) 
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week5_VoA)
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week6_VoA)
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, FinalTable)
  Full_Outputs_Rks[,2:ncol(Full_Outputs_Rks)] <- sapply(Full_Outputs_Rks[,2:ncol(Full_Outputs_Rks)], as.numeric)
} else if (as.numeric(week) == 8) {
  Full_Outputs_Rks <- rbind(VoP, Week1_VoA) 
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week2_VoA)
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week3_VoA)
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week4_VoA) 
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week5_VoA)
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week6_VoA)
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week7_VoA)
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, FinalTable)
  Full_Outputs_Rks[,2:ncol(Full_Outputs_Rks)] <- sapply(Full_Outputs_Rks[,2:ncol(Full_Outputs_Rks)], as.numeric)
} else if (as.numeric(week) == 9) {
  Full_Outputs_Rks <- rbind(VoP, Week1_VoA) 
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week2_VoA)
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week3_VoA)
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week4_VoA) 
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week5_VoA)
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week6_VoA)
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week7_VoA)
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week8_VoA)
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, FinalTable)
  Full_Outputs_Rks[,2:ncol(Full_Outputs_Rks)] <- sapply(Full_Outputs_Rks[,2:ncol(Full_Outputs_Rks)], as.numeric)
} else if (as.numeric(week) == 10) {
  Full_Outputs_Rks <- rbind(VoP, Week1_VoA) 
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week2_VoA)
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week3_VoA)
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week4_VoA) 
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week5_VoA)
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week6_VoA)
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week7_VoA)
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week8_VoA)
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week9_VoA)
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, FinalTable)
  Full_Outputs_Rks[,2:ncol(Full_Outputs_Rks)] <- sapply(Full_Outputs_Rks[,2:ncol(Full_Outputs_Rks)], as.numeric)
} else if (as.numeric(week) == 11) {
  Full_Outputs_Rks <- rbind(VoP, Week1_VoA) 
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week2_VoA)
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week3_VoA)
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week4_VoA) 
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week5_VoA)
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week6_VoA)
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week7_VoA)
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week8_VoA)
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week9_VoA)
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week10_VoA)
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, FinalTable)
  Full_Outputs_Rks[,2:ncol(Full_Outputs_Rks)] <- sapply(Full_Outputs_Rks[,2:ncol(Full_Outputs_Rks)], as.numeric)
} else if (as.numeric(week) == 12) {
  Full_Outputs_Rks <- rbind(VoP, Week1_VoA) 
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week2_VoA)
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week3_VoA)
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week4_VoA) 
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week5_VoA)
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week6_VoA)
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week7_VoA)
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week8_VoA)
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week9_VoA)
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week10_VoA)
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week11_VoA)
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, FinalTable)
  Full_Outputs_Rks[,2:ncol(Full_Outputs_Rks)] <- sapply(Full_Outputs_Rks[,2:ncol(Full_Outputs_Rks)], as.numeric)
} else if (as.numeric(week) == 13) {
  Full_Outputs_Rks <- rbind(VoP, Week1_VoA) 
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week2_VoA)
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week3_VoA)
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week4_VoA) 
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week5_VoA)
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week6_VoA)
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week7_VoA)
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week8_VoA)
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week9_VoA)
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week10_VoA)
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week11_VoA)
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week12_VoA)
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, FinalTable)
  Full_Outputs_Rks[,2:ncol(Full_Outputs_Rks)] <- sapply(Full_Outputs_Rks[,2:ncol(Full_Outputs_Rks)], as.numeric)
} else if (as.numeric(week) == 14) {
  Full_Outputs_Rks <- rbind(VoP, Week1_VoA) 
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week2_VoA)
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week3_VoA)
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week4_VoA) 
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week5_VoA)
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week6_VoA)
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week7_VoA)
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week8_VoA)
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week9_VoA)
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week10_VoA)
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week11_VoA)
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week12_VoA)
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week13_VoA)
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, FinalTable)
  Full_Outputs_Rks[,2:ncol(Full_Outputs_Rks)] <- sapply(Full_Outputs_Rks[,2:ncol(Full_Outputs_Rks)], as.numeric)
} else if (as.numeric(week) == 15) {
  Full_Outputs_Rks <- rbind(VoP, Week1_VoA) 
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week2_VoA)
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week3_VoA)
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week4_VoA) 
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week5_VoA)
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week6_VoA)
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week7_VoA)
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week8_VoA)
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week9_VoA)
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week10_VoA)
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week11_VoA)
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week12_VoA)
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week13_VoA)
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, Week14_VoA)
  Full_Outputs_Rks <- rbind(Full_Outputs_Rks, FinalTable)
  Full_Outputs_Rks[,2:ncol(Full_Outputs_Rks)] <- sapply(Full_Outputs_Rks[,2:ncol(Full_Outputs_Rks)], as.numeric)
} else {
  print("No charts until Week 2!")
}

if (as.numeric(week) >= 2) {
  # ## Subsetting by team, each conference (including independents) gets separate charts
  # AAC_Outputs_Rks <- subset(Full_Outputs_Rks, subset = Team == "Cincinnati" | Team == "Memphis" | Team == "SMU" | Team == "UCF" | Team == "Houston" | Team == "Temple" | Team == "Tulane" | Team == "East Carolina" | Team == "Navy" | Team == "South Florida" | Team == "Tulsa")
  # ACC_Outputs_Rks <- subset(Full_Outputs_Rks, subset = Team == "Boston College" | Team == "Wake Forest" | Team == "Louisville" | Team == "NC State" | Team == "Syracuse" | Team == "Florida State" | Team == "Virginia Tech" | Team == "Pittsburgh" | Team == "Virginia" | Team == "Duke" | Team == "Georgia Tech" | Team == "Miami" | Team == "North Carolina" | Team == "Clemson")
  # Big12_Outputs_Rks <- subset(Full_Outputs_Rks, subset = Team == "Baylor" | Team == "Kansas State" | Team == "Oklahoma" | Team == "Oklahoma State" | Team == "TCU" | Team == "Texas Tech" | Team == "Iowa State" | Team == "Kansas" | Team == "Texas" | Team == "West Virginia")
  # Big10_Outputs_Rks <- subset(Full_Outputs_Rks, subset = Team == "Maryland" | Team == "Michigan State" | Team == "Penn State" | Team == "Ohio State" | Team == "Michigan" | Team == "Rutgers" | Team == "Indiana" | Team == "Iowa" | Team == "Illinois" | Team == "Purdue" | Team == "Nebraska" | Team == "Minnesota" | Team == "Northwestern" | Team == "Wisconsin")
  # CUSA_Outputs_Rks <- subset(Full_Outputs_Rks, subset = Team == "Charlotte" | Team == "Marshall" | Team == "Florida Atlantic" | Team == "Florida International" | Team == "Middle Tennessee" | Team == "Old Dominion" | Team == "Western Kentucky" | Team == "UTSA" | Team == "UTEP" | Team == "Louisiana Tech" | Team == "North Texas" | Team == "Southern Miss" | Team == "UAB" | Team == "Rice")
  # Indy_Outputs_Rks <- subset(Full_Outputs_Rks, subset = Team == "Army" | Team == "BYU" | Team == "Liberty" | Team == "Notre Dame" | Team == "UMass" | Team == "New Mexico State" | Team == "UConn")
  # MAC_Outputs_Rks <- subset(Full_Outputs_Rks, subset = Team == "Buffalo" | Team == "Kent State" | Team == "Akron" | Team == "Bowling Green" | Team == "Miami (OH)" | Team == "Ohio" | Team == "Ball State" | Team == "Central Michigan" | Team == "Eastern Michigan" | Team == "Northern Illinois" | Team == "Toledo" | Team == "Western Michigan")
  # MWC_Outputs_Rks <- subset(Full_Outputs_Rks, subset = Team == "Air Force" | Team == "New Mexico" | Team == "Utah State" | Team == "Wyoming" | Team == "Boise State" | Team == "Colorado State" | Team == "Nevada" | Team == "San Diego State" | Team == "Fresno State" | Team == "San José State" | Team == "Hawai'i" | Team == "UNLV")
  # Pac12_Outputs_Rks <- subset(Full_Outputs_Rks, subset = Team == "Stanford" | Team == "Oregon" | Team == "Oregon State" | Team == "Washington State" | Team == "California" | Team == "Washington" | Team == "Arizona State" | Team == "UCLA" | Team == "Colorado" | Team == "Utah" | Team == "Arizona" | Team == "USC")
  # SEC_Outputs_Rks <- subset(Full_Outputs_Rks, subset = Team == "Kentucky" | Team == "Florida" | Team == "Georgia" | Team == "South Carolina" | Team == "Tennessee" | Team == "Vanderbilt" | Team == "Missouri" | Team == "Alabama" | Team == "Arkansas" | Team == "Auburn" | Team == "Mississippi State" | Team == "Ole Miss" | Team == "Texas A&M" | Team == "LSU")
  # SunBelt_Outputs_Rks <- subset(Full_Outputs_Rks, subset = Team == "Coastal Carolina" | Team == "Appalachian State" | Team == "Georgia Southern" | Team == "Troy" | Team == "Georgia State" | Team == "South Alabama" | Team == "Louisiana" | Team == "Arkansas State" | Team == "Texas State" | Team == "UL Monroe")
  
  ## ESPN for some fucking reason started the 2021 season using the full school names + nicknames, then switched to just school names, then for the last week switched back! why?!?! Who knows!
  # anyway, subsetting by team so each conference (including independents) gets separate charts
  ## Subsetting by team, each conference (including independents) gets separate charts
  AAC_Outputs_Rks <- subset(Full_Outputs_Rks, subset = Team == "Cincinnati Bearcats" | Team == "Memphis Tigers" | Team == "SMU Mustangs" | Team == "UCF Knights" | Team == "Houston Cougars" | Team == "Temple Owls" | Team == "Tulane Green Wave" | Team == "East Carolina Pirates" | Team == "Navy Midshipmen" | Team == "South Florida Bulls" | Team == "Tulsa Golden Hurricane")
  ACC_Outputs_Rks <- subset(Full_Outputs_Rks, subset = Team == "Boston College Eagles" | Team == "Wake Forest Demon Deacons" | Team == "Louisville Cardinals" | Team == "NC State Wolf Pack" | Team == "Syracuse Orange" | Team == "Florida State Seminoles" | Team == "Virginia Tech Hokies" | Team == "Pittsburgh Panthers" | Team == "Virginia Cavaliers" | Team == "Duke Blue Devils" | Team == "Georgia Tech Yellow Jackets" | Team == "Miami Hurricanes" | Team == "North Carolina Tar Heels" | Team == "Clemson Tigers")
  Big12_Outputs_Rks <- subset(Full_Outputs_Rks, subset = Team == "Baylor Bears" | Team == "Kansas State Wildcats" | Team == "Oklahoma Sooners" | Team == "Oklahoma State Cowboys" | Team == "TCU Horned Frogs" | Team == "Texas Tech Red Raiders" | Team == "Iowa State Cyclones" | Team == "Kansas Jayhawks" | Team == "Texas Longhorns" | Team == "West Virginia Mountaineers")
  Big10_Outputs_Rks <- subset(Full_Outputs_Rks, subset = Team == "Maryland Terrapins" | Team == "Michigan State Spartans" | Team == "Penn State Nittany Lions" | Team == "Ohio State Buckeyes" | Team == "Michigan Wolverines" | Team == "Rutgers Scarlet Knights" | Team == "Indiana Hoosiers" | Team == "Iowa Hawkeyes" | Team == "Illinois Fighting Illini" | Team == "Purdue Boilermakers" | Team == "Nebraska Cornhuskers" | Team == "Minnesota Golden Gophers" | Team == "Northwestern Wildcats" | Team == "Wisconsin Badgers")
  CUSA_Outputs_Rks <- subset(Full_Outputs_Rks, subset = Team == "Charlotte 49ers" | Team == "Marshall Thundering Herd" | Team == "Florida Atlantic Owls" | Team == "Florida International Panthers" | Team == "Middle Tennessee Blue Raiders" | Team == "Old Dominion Monarchs" | Team == "Western Kentucky Hilltoppers" | Team == "UTSA Roadrunners" | Team == "UTEP Miners" | Team == "Louisiana Tech Bulldogs" | Team == "North Texas Mean Green" | Team == "Southern Miss Golden Eagles" | Team == "UAB Blazers" | Team == "Rice Owls")
  Indy_Outputs_Rks <- subset(Full_Outputs_Rks, subset = Team == "Army Black Knights" | Team == "BYU Cougars" | Team == "Liberty Flames" | Team == "Notre Dame Fighting Irish" | Team == "UMass Minutemen" | Team == "New Mexico State Aggies" | Team == "UConn Huskies")
  MAC_Outputs_Rks <- subset(Full_Outputs_Rks, subset = Team == "Buffalo Bulls" | Team == "Kent State Golden Flashes" | Team == "Akron Zips" | Team == "Bowling Green Falcons" | Team == "Miami (OH) RedHawks" | Team == "Ohio Bobcats" | Team == "Ball State Cardinals" | Team == "Central Michigan Chippewas" | Team == "Eastern Michigan Eagles" | Team == "Northern Illinois Huskies" | Team == "Toledo Rockets" | Team == "Western Michigan Broncos")
  MWC_Outputs_Rks <- subset(Full_Outputs_Rks, subset = Team == "Air Force Falcons" | Team == "New Mexico Lobos" | Team == "Utah State Aggies" | Team == "Wyoming Cowboys" | Team == "Boise State Broncos" | Team == "Colorado State Rams" | Team == "Nevada Wolfpack" | Team == "San Diego State Aztecs" | Team == "Fresno State Bulldogs" | Team == "San José State Spartans" | Team == "Hawai'i Rainbow Warriors" | Team == "UNLV Rebels")
  Pac12_Outputs_Rks <- subset(Full_Outputs_Rks, subset = Team == "Stanford Cardinal" | Team == "Oregon Ducks" | Team == "Oregon State Beavers" | Team == "Washington State Cougars" | Team == "California Golden Bears" | Team == "Washington Huskies" | Team == "Arizona State Sun Devils" | Team == "UCLA Bruins" | Team == "Colorado Buffaloes" | Team == "Utah Utes" | Team == "Arizona Wildcats" | Team == "USC Trojans")
  SEC_Outputs_Rks <- subset(Full_Outputs_Rks, subset = Team == "Kentucky Wildcats" | Team == "Florida Gators" | Team == "Georgia Bulldogs" | Team == "South Carolina Gamecocks" | Team == "Tennessee Volunteers" | Team == "Vanderbilt Commodores" | Team == "Missouri Tigers" | Team == "Alabama Crimson Tide" | Team == "Arkansas Razorbacks" | Team == "Auburn Tigers" | Team == "Mississippi State Bulldogs" | Team == "Ole Miss Rebels" | Team == "Texas A&M Aggies" | Team == "LSU Tigers")
  SunBelt_Outputs_Rks <- subset(Full_Outputs_Rks, subset = Team == "Coastal Carolina Chanticleers" | Team == "Appalachian State Mountaineers" | Team == "Georgia Southern Eagles" | Team == "Troy Trojans" | Team == "Georgia State Panthers" | Team == "South Alabama Jaguars" | Team == "Louisiana Ragin' Cajuns" | Team == "Arkansas State Red Wolves" | Team == "Texas State Bobcats" | Team == "UL Monroe Warhawks")
  
  ## Creating Charts
  # charting VoA_Output and VoA_Ranking for each week from week 2 on
  AAC_VoA_Output_Chart <- ggplot(AAC_Outputs_Rks, aes(x = CFB_Week, y = VoA_Output, colour = Team, group = Team)) +
    geom_line(size = 1.5) +
    geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA_Output") +
    ggtitle("American Conference Vortex of Accuracy Outputs by Week") +
    expand_limits(y = c(0,100)) +
    scale_y_continuous(breaks = c(0,20,40,60,80,100,130)) +
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)) +
    theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
  AAC_VoA_Output_Chart
  ggsave(AAC_Output_filename, path = output_dir, width = 50, height = 40, units = 'cm')
  
  AAC_VoA_Ranking_Chart <- ggplot(AAC_Outputs_Rks, aes(x = CFB_Week, y = VoA_Ranking, colour = Team, group = Team)) +
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
  
  ACC_VoA_Output_Chart <- ggplot(ACC_Outputs_Rks, aes(x = CFB_Week, y = VoA_Output, colour = Team, group = Team)) +
    geom_line(size = 1.5) +
    geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA_Output") +
    ggtitle("ACC Vortex of Accuracy Outputs by Week") +
    expand_limits(y = c(0,100)) +
    scale_y_continuous(breaks = c(0,20,40,60,80,100,130)) +
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)) +
    theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
  ACC_VoA_Output_Chart
  ggsave(ACC_Output_filename, path = output_dir, width = 50, height = 40, units = 'cm')
  
  ACC_VoA_Ranking_Chart <- ggplot(ACC_Outputs_Rks, aes(x = CFB_Week, y = VoA_Ranking, colour = Team, group = Team)) +
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
  
  Big12_VoA_Output_Chart <- ggplot(Big12_Outputs_Rks, aes(x = CFB_Week, y = VoA_Output, colour = Team, group = Team)) +
    geom_line(size = 1.5) +
    geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA_Output") +
    ggtitle("Big 12 Vortex of Accuracy Outputs by Week") +
    expand_limits(y = c(0,100)) +
    scale_y_continuous(breaks = c(0,20,40,60,80,100,130)) +
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)) +
    theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
  Big12_VoA_Output_Chart
  ggsave(Big12_Output_filename, path = output_dir, width = 50, height = 40, units = 'cm')
  
  Big12_VoA_Ranking_Chart <- ggplot(Big12_Outputs_Rks, aes(x = CFB_Week, y = VoA_Ranking, colour = Team, group = Team)) +
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
  
  Big10_VoA_Output_Chart <- ggplot(Big10_Outputs_Rks, aes(x = CFB_Week, y = VoA_Output, colour = Team, group = Team)) +
    geom_line(size = 1.5) +
    geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA_Output") +
    ggtitle("Big 10 Vortex of Accuracy Outputs by Week") +
    expand_limits(y = c(0,100)) +
    scale_y_continuous(breaks = c(0,20,40,60,80,100,130)) +
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)) +
    theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
  Big10_VoA_Output_Chart
  ggsave(Big10_Output_filename, path = output_dir, width = 50, height = 40, units = 'cm')
  
  Big10_VoA_Ranking_Chart <- ggplot(Big10_Outputs_Rks, aes(x = CFB_Week, y = VoA_Ranking, colour = Team, group = Team)) +
    geom_line(size = 1.5) +
    geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA_Output") +
    ggtitle("Big 10 Vortex of Accuracy Rankings by Week") +
    expand_limits(y = c(0,130)) +
    scale_y_continuous(breaks = c(0,20,40,60,80,100,130)) +
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)) +
    theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
  Big10_VoA_Ranking_Chart
  ggsave(Big10_Ranking_filename, path = output_dir, width = 50, height = 40, units = 'cm')
  
  CUSA_VoA_Output_Chart <- ggplot(CUSA_Outputs_Rks, aes(x = CFB_Week, y = VoA_Output, colour = Team, group = Team)) +
    geom_line(size = 1.5) +
    geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA_Output") +
    ggtitle("CUSA Vortex of Accuracy Outputs by Week") +
    expand_limits(y = c(0,100)) +
    scale_y_continuous(breaks = c(0,20,40,60,80,100,130)) +
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)) +
    theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
  CUSA_VoA_Output_Chart
  ggsave(CUSA_Output_filename, path = output_dir, width = 50, height = 40, units = 'cm')
  
  CUSA_VoA_Ranking_Chart <- ggplot(CUSA_Outputs_Rks, aes(x = CFB_Week, y = VoA_Ranking, colour = Team, group = Team)) +
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
  
  Indy_VoA_Output_Chart <- ggplot(Indy_Outputs_Rks, aes(x = CFB_Week, y = VoA_Output, colour = Team, group = Team)) +
    geom_line(size = 1.5) +
    geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA_Output") +
    ggtitle("Independents Vortex of Accuracy Outputs by Week") +
    expand_limits(y = c(0,100)) +
    scale_y_continuous(breaks = c(0,20,40,60,80,100,130)) +
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)) +
    theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
  Indy_VoA_Output_Chart
  ggsave(Indy_Output_filename, path = output_dir, width = 50, height = 40, units = 'cm')
  
  Indy_VoA_Ranking_Chart <- ggplot(Indy_Outputs_Rks, aes(x = CFB_Week, y = VoA_Ranking, colour = Team, group = Team)) +
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
  
  MAC_VoA_Output_Chart <- ggplot(MAC_Outputs_Rks, aes(x = CFB_Week, y = VoA_Output, colour = Team, group = Team)) +
    geom_line(size = 1.5) +
    geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA_Output") +
    ggtitle("MAC Vortex of Accuracy Outputs by Week") +
    expand_limits(y = c(0,100)) +
    scale_y_continuous(breaks = c(0,20,40,60,80,100,130)) +
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)) +
    theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
  MAC_VoA_Output_Chart
  ggsave(MAC_Output_filename, path = output_dir, width = 50, height = 40, units = 'cm')
  
  MAC_VoA_Ranking_Chart <- ggplot(MAC_Outputs_Rks, aes(x = CFB_Week, y = VoA_Ranking, colour = Team, group = Team)) +
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
  
  MWC_VoA_Output_Chart <- ggplot(MWC_Outputs_Rks, aes(x = CFB_Week, y = VoA_Output, colour = Team, group = Team)) +
    geom_line(size = 1.5) +
    geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA_Output") +
    ggtitle("Mountain West Vortex of Accuracy Outputs by Week") +
    expand_limits(y = c(0,100)) +
    scale_y_continuous(breaks = c(0,20,40,60,80,100,130)) +
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)) +
    theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
  MWC_VoA_Output_Chart
  ggsave(MWC_Output_filename, path = output_dir, width = 50, height = 40, units = 'cm')
  
  MWC_VoA_Ranking_Chart <- ggplot(MWC_Outputs_Rks, aes(x = CFB_Week, y = VoA_Ranking, colour = Team, group = Team)) +
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
  
  Pac12_VoA_Output_Chart <- ggplot(Pac12_Outputs_Rks, aes(x = CFB_Week, y = VoA_Output, colour = Team, group = Team)) +
    geom_line(size = 1.5) +
    geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA_Output") +
    ggtitle("Pac 12 Vortex of Accuracy Outputs by Week") +
    expand_limits(y = c(0,100)) +
    scale_y_continuous(breaks = c(0,20,40,60,80,100,130)) +
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)) +
    theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
  Pac12_VoA_Output_Chart
  ggsave(Pac12_Output_filename, path = output_dir, width = 50, height = 40, units = 'cm')
  
  Pac12_VoA_Ranking_Chart <- ggplot(Pac12_Outputs_Rks, aes(x = CFB_Week, y = VoA_Ranking, colour = Team, group = Team)) +
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
  
  SEC_VoA_Output_Chart <- ggplot(SEC_Outputs_Rks, aes(x = CFB_Week, y = VoA_Output, colour = Team, group = Team)) +
    geom_line(size = 1.5) +
    geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA_Output") +
    ggtitle("SEC Vortex of Accuracy Outputs by Week") +
    expand_limits(y = c(0,100)) +
    scale_y_continuous(breaks = c(0,20,40,60,80,100,130)) +
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)) +
    theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
  SEC_VoA_Output_Chart
  ggsave(SEC_Output_filename, path = output_dir, width = 50, height = 40, units = 'cm')
  
  SEC_VoA_Ranking_Chart <- ggplot(SEC_Outputs_Rks, aes(x = CFB_Week, y = VoA_Ranking, colour = Team, group = Team)) +
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
  
  SunBelt_VoA_Output_Chart <- ggplot(SunBelt_Outputs_Rks, aes(x = CFB_Week, y = VoA_Output, colour = Team, group = Team)) +
    geom_line(size = 1.5) +
    geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA_Output") +
    ggtitle("Sun Belt Vortex of Accuracy Outputs by Week") +
    expand_limits(y = c(0,100)) +
    scale_y_continuous(breaks = c(0,20,40,60,80,100,130)) +
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)) +
    theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
  SunBelt_VoA_Output_Chart
  ggsave(SunBelt_Output_filename, path = output_dir, width = 50, height = 40, units = 'cm')
  
  SunBelt_VoA_Ranking_Chart <- ggplot(SunBelt_Outputs_Rks, aes(x = CFB_Week, y = VoA_Ranking, colour = Team, group = Team)) +
    geom_line(size = 1.5) +
    geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA_Output") +
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
# Power5_VoA <- subset(VoA_Variables, subset = Team == "Boston College" | Team == "Wake Forest" | 
#                        Team == "Louisville" | Team == "NC State" | Team == "Syracuse" | Team == "Florida State" | 
#                        Team == "Virginia Tech" | Team == "Pittsburgh" | Team == "Virginia" | 
#                        Team == "Duke" | Team == "Georgia Tech" | Team == "Miami" | 
#                        Team == "North Carolina" | Team == "Clemson" | Team == "Baylor" | 
#                        Team == "Kansas State" | Team == "Oklahoma" | Team == "Oklahoma State" | 
#                        Team == "TCU" | Team == "Texas Tech" | Team == "Iowa State" | 
#                        Team == "Kansas" | Team == "Texas" | Team == "West Virginia" | 
#                        Team == "Maryland" | Team == "Michigan State" | Team == "Penn State" | 
#                        Team == "Ohio State" | Team == "Michigan" | Team == "Rutgers" | 
#                        Team == "Indiana" | Team == "Iowa" | Team == "Illinois" | Team == "Purdue" | 
#                        Team == "Nebraska" | Team == "Minnesota" | Team == "Northwestern" | 
#                        Team == "Wisconsin"| Team == "Stanford" | Team == "Oregon" | 
#                        Team == "Oregon State" | Team == "Washington State" | Team == "California" | 
#                        Team == "Washington" | Team == "Arizona State" | Team == "UCLA" | 
#                        Team == "Colorado" | Team == "Utah" | Team == "Arizona" | Team == "USC" |  
#                        Team == "Kentucky" | Team == "Florida" | Team == "Georgia" | Team == "South Carolina" | 
#                        Team == "Tennessee" | Team == "Vanderbilt" | Team == "Missouri" | Team == "Alabama" | Team == "Arkansas" | 
#                        Team == "Auburn" | Team == "Mississippi State" | Team == "Ole Miss" | Team == "Texas A&M" | Team == "LSU" | 
#                        Team == "Notre Dame")
# Group5_VoA <- subset(VoA_Variables, subset = Team == "Cincinnati" | Team == "Memphis" | Team == "SMU" | 
#                        Team == "UCF" | Team == "Houston" | Team == "Temple" | Team == "Tulane" | 
#                        Team == "East Carolina" | Team == "Navy" | Team == "South Florida" | Team == "Tulsa" | 
#                        Team == "Charlotte" | Team == "Marshall" | Team == "Florida Atlantic" | Team == "Florida International" | 
#                        Team == "Middle Tennessee" | Team == "Old Dominion" | Team == "Western Kentucky" | 
#                        Team == "UTSA" | Team == "UTEP" | Team == "Louisiana Tech" | Team == "North Texas" | 
#                        Team == "Southern Miss" | Team == "UAB" | Team == "Rice" | Team == "Army" | Team == "BYU" | 
#                        Team == "Liberty" | Team == "UMass" | Team == "New Mexico State" | Team == "UConn" | 
#                        Team == "Buffalo" | Team == "Kent State" | Team == "Akron" | Team == "Bowling Green" | 
#                        Team == "Miami (OH)" | Team == "Ohio" | Team == "Ball State" | Team == "Central Michigan" | 
#                        Team == "Eastern Michigan" | Team == "Northern Illinois" | Team == "Toledo" | 
#                        Team == "Western Michigan" | Team == "Air Force" | Team == "New Mexico" | Team == "Utah State" | 
#                        Team == "Wyoming" | Team == "Boise State" | Team == "Colorado State" | Team == "Nevada" | 
#                        Team == "San Diego State" | Team == "Fresno State" | Team == "San José State" | Team == "Hawai'i" | 
#                        Team == "UNLV" | Team == "Coastal Carolina" | Team == "Appalachian State" | 
#                        Team == "Georgia Southern" | Team == "Troy" | Team == "Georgia State" | Team == "South Alabama" | 
#                        Team == "Louisiana" | Team == "Arkansas State" | Team == "Texas State" | Team == "UL Monroe")

Power5_VoA <- subset(VoA_Variables, subset = Team == "Boston College Eagles" | Team == "Wake Forest Demon Deacons" | Team == "Louisville Cardinals" | Team == "NC State Wolf Pack" | Team == "Syracuse Orange" | Team == "Florida State Seminoles" | Team == "Virginia Tech Hokies" | Team == "Pittsburgh Panthers" | Team == "Virginia Cavaliers" | Team == "Duke Blue Devils" | Team == "Georgia Tech Yellow Jackets" | Team == "Miami Hurricanes" | Team == "North Carolina Tar Heels" | Team == "Clemson Tigers" |
                       Team == "Baylor Bears" | Team == "Kansas State Wildcats" | Team == "Oklahoma Sooners" | Team == "Oklahoma State Cowboys" | Team == "TCU Horned Frogs" | Team == "Texas Tech Red Raiders" | Team == "Iowa State Cyclones" | Team == "Kansas Jayhawks" | Team == "Texas Longhorns" | Team == "West Virginia Mountaineers" |
                       Team == "Notre Dame Fighting Irish" |
                       Team == "Maryland Terrapins" | Team == "Michigan State Spartans" | Team == "Penn State Nittany Lions" | Team == "Ohio State Buckeyes" | Team == "Michigan Wolverines" | Team == "Rutgers Scarlet Knights" | Team == "Indiana Hoosiers" | Team == "Iowa Hawkeyes" | Team == "Illinois Fighting Illini" | Team == "Purdue Boilermakers" | Team == "Nebraska Cornhuskers" | Team == "Minnesota Golden Gophers" | Team == "Northwestern Wildcats" | Team == "Wisconsin Badgers" |
                       Team == "Stanford Cardinal" | Team == "Oregon Ducks" | Team == "Oregon State Beavers" | Team == "Washington State Cougars" | Team == "California Golden Bears" | Team == "Washington Huskies" | Team == "Arizona State Sun Devils" | Team == "UCLA Bruins" | Team == "Colorado Buffaloes" | Team == "Utah Utes" | Team == "Arizona Wildcats" | Team == "USC Trojans" |
                       Team == "Kentucky Wildcats" | Team == "Florida Gators" | Team == "Georgia Bulldogs" | Team == "South Carolina Gamecocks" | Team == "Tennessee Volunteers" | Team == "Vanderbilt Commodores" | Team == "Missouri Tigers" | Team == "Alabama Crimson Tide" | Team == "Arkansas Razorbacks" | Team == "Auburn Tigers" | Team == "Mississippi State Bulldogs" | Team == "Ole Miss Rebels" | Team == "Texas A&M Aggies" | Team == "LSU Tigers")

Group5_VoA <- subset(VoA_Variables, subset = Team == "Cincinnati Bearcats" | Team == "Memphis Tigers" | Team == "SMU Mustangs" | Team == "UCF Knights" | Team == "Houston Cougars" | Team == "Temple Owls" | Team == "Tulane Green Wave" | Team == "East Carolina Pirates" | Team == "Navy Midshipmen" | Team == "South Florida Bulls" | Team == "Tulsa Golden Hurricane" |
                       Team == "Charlotte 49ers" | Team == "Marshall Thundering Herd" | Team == "Florida Atlantic Owls" | Team == "Florida International Panthers" | Team == "Middle Tennessee Blue Raiders" | Team == "Old Dominion Monarchs" | Team == "Western Kentucky Hilltoppers" | Team == "UTSA Roadrunners" | Team == "UTEP Miners" | Team == "Louisiana Tech Bulldogs" | Team == "North Texas Mean Green" | Team == "Southern Miss Golden Eagles" | Team == "UAB Blazers" | Team == "Rice Owls" |
                       Team == "Army Black Knights" | Team == "BYU Cougars" | Team == "Liberty Flames" | Team == "UMass Minutemen" | Team == "New Mexico State Aggies" | Team == "UConn Huskies" |
                       Team == "Buffalo Bulls" | Team == "Kent State Golden Flashes" | Team == "Akron Zips" | Team == "Bowling Green Falcons" | Team == "Miami (OH) RedHawks" | Team == "Ohio Bobcats" | Team == "Ball State Cardinals" | Team == "Central Michigan Chippewas" | Team == "Eastern Michigan Eagles" | Team == "Northern Illinois Huskies" | Team == "Toledo Rockets" | Team == "Western Michigan Broncos" |
                       Team == "Air Force Falcons" | Team == "New Mexico Lobos" | Team == "Utah State Aggies" | Team == "Wyoming Cowboys" | Team == "Boise State Broncos" | Team == "Colorado State Rams" | Team == "Nevada Wolfpack" | Team == "San Diego State Aztecs" | Team == "Fresno State Bulldogs" | Team == "San José State Spartans" | Team == "Hawai'i Rainbow Warriors" | Team == "UNLV Rebels" |
                       Team == "Coastal Carolina Chanticleers" | Team == "Appalachian State Mountaineers" | Team == "Georgia Southern Eagles" | Team == "Troy Trojans" | Team == "Georgia State Panthers" | Team == "South Alabama Jaguars" | Team == "Louisiana Ragin' Cajuns" | Team == "Arkansas State Red Wolves" | Team == "Texas State Bobcats" | Team == "UL Monroe Warhawks")


FBS_Output_histogram <- ggplot(VoA_Variables, aes(VoA_Output)) +
  geom_histogram(binwidth = 5,
                 col = "black",
                 fill = "orange") +
  scale_x_continuous(breaks = seq(0,100,10)) +
  scale_y_continuous(breaks = seq(0,40,2)) +
  ggtitle(FBS_hist_title) +
  xlab("VoA Output") +
  ylab("Frequency") +
  theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
FBS_Output_histogram
ggsave(FBS_hist_filename, path = output_dir, width = 50, height = 40, units = 'cm')

Power5_Output_histogram <- ggplot(Power5_VoA, aes(VoA_Output)) +
  geom_histogram(binwidth = 5,
                 col = "black",
                 fill = "blue") +
  scale_x_continuous(breaks = seq(0,100,10)) +
  scale_y_continuous(breaks = seq(0,40,2)) +
  ggtitle(Power5_hist_title) +
  xlab("VoA Output") +
  ylab("Frequency") +
  theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
Power5_Output_histogram
ggsave(Power5_hist_filename, path = output_dir, width = 50, height = 40, units = 'cm')

Group5_Output_histogram <- ggplot(Group5_VoA, aes(VoA_Output)) +
  geom_histogram(binwidth = 5,
                 col = "black",
                 fill = "pink") +
  scale_x_continuous(breaks = seq(0,100,10)) +
  scale_y_continuous(breaks = seq(0,40,2)) +
  ggtitle(Group5_hist_title) +
  xlab("VoA Output") +
  ylab("Frequency") +
  theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
Group5_Output_histogram
ggsave(Group5_hist_filename, path = output_dir, width = 50, height = 40, units = 'cm')

## Creating Scatterplot of VoA_Output vs VoA_Ranking
VoA_Output_Rk_plot <- ggplot(VoA_Variables, aes(x = VoA_Ranking, y = VoA_Output)) +
  geom_point() +
  geom_smooth() +
  scale_x_continuous(breaks = seq(0,130,10)) +
  scale_y_continuous(breaks = seq(0,130,10)) +
  ggtitle(Output_Rk_Plot_title) +
  xlab("VoA Ranking") +
  ylab("VoA Output") +
  theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
VoA_Output_Rk_plot
## deciding not to export this one for now, it's interesting to look at, not that informative
## ggsave(Output_Rk_Plot_filename, path = output_dir, width = 50, height = 40, units = 'cm')


## End of Script


