##### script includes assortment of testing options for data collection, analysis, visualization, etc
### jumbled mess, needs to be cleaned up
## test code for accessing cfb data API
## installing cfbfastR using the devtools package
## devtools::install_github(repo = "saiemgilani/cfbfastR")
pacman::p_load(tidyverse, matrixStats, grid, gridExtra, gt, viridis, 
               webshot, writexl, rvest, cfbfastR, espnscrapeR, openxlsx, 
               here, ggsci, RColorBrewer)

## testing readline function with cfbfastR pkg
week <- readline(prompt = "What week is it? ")
year <- readline(prompt = "What year is it? ")
## test line using readline function to set end week for cfbfastR stats reading
## test_2019_clemson_stats <- cfbd_stats_season_team(2019, team = "Clemson", start_week = 1, end_week = as.integer(week))
## test_2018_allteam_stats <- cfbd_stats_season_team(2018, start_week = 1, end_week = as.integer(week))

## going to try to access CFBdataAPI using cfbfastR
## pull in data frames of season variables, sort and rank as normal VoA
# season data doesn't have team or conference columns, so testing normal VoA
# methodology with just advanced stats data
## Scraping current season data with cfbfastR
Stats <- cfbd_stats_season_team(year = 2021)
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
## why aren't defensive stats included in the regular stat cfbfastR function

## now bringing in advanced stats
Adv_Stats <- cfbd_stats_season_advanced(year = 2021, excl_garbage_time = TRUE)
## removing season and conference columns, already in regular stat dataframe
Adv_Stats <- Adv_Stats %>%
  select(-one_of("season", "conference"))

## merging stat tables, will rank afterwards
VoA_Stats <- merge(Stats, Adv_Stats, by = "team")

## adding SP+ rankings
SP_Rankings <- cfbd_ratings_sp(2021)
SP_Rankings <- SP_Rankings %>%
  select(-one_of("year", "conference"))
SP_Rankings <- SP_Rankings[,c("team", "rating", "offense_rating", 
                                        "defense_rating", "special_teams_rating")]
SP_colnames <- c("team", "sp_rating", "sp_offense_rating", "sp_defense_rating",
                 "sp_special_teams_rating")
colnames(SP_Rankings) <- SP_colnames

## Merging stats with SP data
VoA_Stats <- merge(VoA_Stats, SP_Rankings, by = "team")

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
VoA_Stats <- VoA_Stats[,c("team", "season", "conference", "games",
                                    "completion_pct","pass_ypa","pass_ypr","int_pct","rush_ypc","turnovers_pg",
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
VoA_Stats <- VoA_Stats %>%
  mutate(Rank_Comp_Pct = percent_rank(desc(completion_pct))) %>%
  mutate(Rank_Pass_YPA = percent_rank(desc(pass_ypa))) %>%
  mutate(Rank_Pass_YPR = percent_rank(desc(pass_ypr))) %>%
  mutate(Rank_Int_Pct = percent_rank(int_pct)) %>%
  mutate(Rank_Rush_YPC = percent_rank(desc(rush_ypc))) %>%
  mutate(Rank_Turnovers_pg = percent_rank(turnovers_pg)) %>%
  mutate(Rank_Third_Conv_Rate = percent_rank(desc(third_conv_rate))) %>%
  mutate(Rank_Fourth_Conv_Rate = percent_rank(desc(fourth_conv_rate))) %>%
  mutate(Rank_Penalties_pg = percent_rank(penalties_pg)) %>%
  mutate(Rank_Penalty_Yds_pg = percent_rank(penalty_yds_pg)) %>%
  mutate(Rank_Yds_Per_Penalty = percent_rank(yards_per_penalty)) %>%
  mutate(Rank_Kick_Return_Avg = percent_rank(desc(kick_return_avg))) %>%
  mutate(Rank_Punt_Return_Avg = percent_rank(desc(punt_return_avg))) %>%
  mutate(Rank_Total_Yds_pg = percent_rank(desc(total_yds_pg))) %>%
  mutate(Rank_Pass_Yds_pg = percent_rank(desc(pass_yds_pg))) %>%
  mutate(Rank_Rush_Yds_pg = percent_rank(desc(rush_yds_pg))) %>%
  mutate(Rank_First_Downs_pg = percent_rank(desc(first_downs_pg))) %>%
  mutate(Rank_Def_Ints_pg = percent_rank(desc(def_interceptions_pg))) %>%
  mutate(Rank_Off_PPA = percent_rank(desc(off_ppa))) %>%
  mutate(Rank_Off_Success_Rt = percent_rank(desc(off_success_rate))) %>%
  mutate(Rank_Off_Explosiveness = percent_rank(desc(off_explosiveness))) %>%
  mutate(Rank_Off_Pwr_Success = percent_rank(desc(off_power_success))) %>%
  mutate(Rank_Off_Stuff_Rt = percent_rank(off_stuff_rate)) %>%
  mutate(Rank_Off_Line_Yds = percent_rank(desc(off_line_yds))) %>%
  mutate(Rank_Off_Second_Lvl_Yds = percent_rank(desc(off_second_lvl_yds))) %>%
  mutate(Rank_Off_Open_Field_Yds = percent_rank(desc(off_open_field_yds))) %>%
  mutate(Rank_Off_Pts_Per_Opp = percent_rank(desc(off_pts_per_opp))) %>%
  mutate(Rank_Off_Field_Pos_Avg_Predicted_Pts = percent_rank(desc(off_field_pos_avg_predicted_points))) %>%
  mutate(Rank_Off_Havoc_Total = percent_rank(off_havoc_total)) %>%
  mutate(Rank_Off_Havoc_Front = percent_rank(off_havoc_front_seven)) %>%
  mutate(Rank_Off_Havoc_DB = percent_rank(off_havoc_db)) %>%
  mutate(Rank_Off_Standard_Down_PPA = percent_rank(desc(off_standard_downs_ppa))) %>%
  mutate(Rank_Off_Standard_Down_Success_Rt = percent_rank(desc(off_standard_downs_success_rate))) %>%
  mutate(Rank_Off_Standard_Down_Explosiveness = percent_rank(desc(off_standard_downs_explosiveness))) %>%
  mutate(Rank_Off_Pass_Down_PPA = percent_rank(desc(off_passing_downs_ppa))) %>%
  mutate(Rank_Off_Pass_Down_Success_Rt = percent_rank(desc(off_passing_downs_success_rate))) %>%
  mutate(Rank_Off_Pass_Down_Explosiveness = percent_rank(desc(off_passing_downs_explosiveness))) %>%
  mutate(Rank_Off_Rush_Play_PPA = percent_rank(desc(off_rushing_plays_ppa))) %>%
  mutate(Rank_Off_Rush_Play_Success_Rt = percent_rank(desc(off_rushing_plays_success_rate))) %>%
  mutate(Rank_Off_Rush_Play_Explosiveness = percent_rank(desc(off_rushing_plays_explosiveness))) %>%
  mutate(Rank_Off_Pass_Play_PPA = percent_rank(desc(off_passing_plays_ppa))) %>%
  mutate(Rank_Off_Pass_Play_Success_Rt = percent_rank(desc(off_passing_plays_success_rate))) %>%
  mutate(Rank_Off_Pass_Play_Explosiveness = percent_rank(desc(off_passing_plays_explosiveness))) %>%
  mutate(Rank_Def_PPA = percent_rank(def_ppa)) %>%
  mutate(Rank_Def_Success_Rt = percent_rank(def_success_rate)) %>%
  mutate(Rank_Def_Explosiveness = percent_rank(def_explosiveness)) %>%
  mutate(Rank_Def_Pwr_Success = percent_rank(def_power_success)) %>%
  mutate(Rank_Def_Stuff_Rt = percent_rank(desc(def_stuff_rate))) %>%
  mutate(Rank_Def_Line_Yds = percent_rank(def_line_yds)) %>%
  mutate(Rank_Def_Second_Lvl_Yds = percent_rank(def_second_lvl_yds)) %>%
  mutate(Rank_Def_Open_Field_Yds = percent_rank(def_open_field_yds)) %>%
  mutate(Rank_Def_Pts_Per_Opp = percent_rank(def_pts_per_opp)) %>%
  mutate(Rank_Def_Field_Pos_Avg_Predicted_Pts = percent_rank(def_field_pos_avg_predicted_points)) %>%
  mutate(Rank_Def_Havoc_Total = percent_rank(desc(def_havoc_total))) %>%
  mutate(Rank_Def_Havoc_Front_Seven = percent_rank(desc(def_havoc_front_seven))) %>%
  mutate(Rank_Def_Havoc_DB = percent_rank(desc(def_havoc_db))) %>%
  mutate(Rank_Def_Standard_Down_PPA = percent_rank(def_standard_downs_ppa)) %>%
  mutate(Rank_Def_Standard_Down_Success_Rt = percent_rank(def_standard_downs_success_rate)) %>%
  mutate(Rank_Def_Standard_Down_Explosiveness = percent_rank(def_standard_downs_explosiveness)) %>%
  mutate(Rank_Def_Pass_Down_PPA = percent_rank(def_passing_downs_ppa)) %>%
  mutate(Rank_Def_Pass_Down_Success_Rt = percent_rank(def_passing_downs_success_rate)) %>%
  mutate(Rank_Def_Pass_Down_Explosiveness = percent_rank(def_passing_downs_explosiveness)) %>%
  mutate(Rank_Def_Rush_Play_PPA = percent_rank(def_rushing_plays_ppa)) %>%
  mutate(Rank_Def_Rush_Play_Success_Rt = percent_rank(def_rushing_plays_success_rate)) %>%
  mutate(Rank_Def_Rush_Play_Explosiveness = percent_rank(def_rushing_plays_explosiveness)) %>%
  mutate(Rank_Def_Pass_Play_PPA = percent_rank(def_passing_plays_ppa)) %>%
  mutate(Rank_Def_Pass_Play_Success_Rt = percent_rank(def_passing_plays_success_rate)) %>%
  mutate(Rank_Def_Pass_Play_Explosiveness = percent_rank(def_passing_plays_explosiveness)) %>%
  mutate(Rank_SP_Rating = percent_rank(desc(sp_rating))) %>%
  mutate(Rank_SP_Off_Rating = percent_rank(desc(sp_offense_rating))) %>%
  mutate(Rank_SP_Def_Rating = percent_rank(sp_defense_rating))
  ## mutate(Rank_SP_SpecialTeams_Rating = percent_rank(desc(sp_special_teams_rating)))




## Append new column of Model output, which is the mean of all variables in VoARanks
VoA_Stats <- VoA_Stats %>%
  mutate(VoA_Output = (rowMeans(VoA_Stats[,77:ncol(VoA_Stats)])))
## Append column of VoA Final Rankings
VoA_Stats <- VoA_Stats %>%
  mutate(VoA_Ranking = dense_rank(VoA_Output))



## Create Vector of Team name, VoA Ranking
FinalTable <- VoA_Stats %>%
  select(team,VoA_Output,VoA_Ranking) %>%
  arrange(VoA_Ranking)
FinalVoATop25 <- subset(FinalTable, FinalTable$VoA_Ranking < 26)
head(FinalVoATop25)
FinalVoATop25

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
    columns = vars(VoA_Output), # What column variable? VoA_Output
    decimals = 5 # With five decimal places
  ) %>% 
  fmt_number( # Another column (also numeric data)
    columns = vars(VoA_Ranking), # What column variable? VoA_Ranking
    decimals = 0 # I want this column to have zero decimal place
  ) %>%
  data_color( # Update cell colors, testing different color palettes
    columns = vars(VoA_Output), # ...for dose column
    colors = scales::col_numeric( # <- bc it's numeric
      palette = brewer.pal(9, "Reds"), # A color scheme (gradient)
      domain = c(), # Column scale endpoints
      reverse = TRUE
    )
  ) %>%
  cols_label(VoA_Output = "BETA Final VoA Output", VoA_Ranking = "BETA VoA Ranking") %>% # Update labels
  cols_move_to_end(columns = "VoA_Output")
VoATop25Table
VoATop25Table %>%
  gtsave(
    "SuperBetaVoATop25_v2.png", expand = 5,
    path = here("RVoA", "Outputs")
  )











## In Meantime, can test RandomForest model on 2019 VoA Spreadsheet
# VoA2019 <- read.csv("~/Documents/Personal/CFPAnalysis/RVoA/VoA2019/VoA2019.csv")
# MyRanking <- VoA2019$MY_Ranking
# 
# pacman::p_load(raster, shapefiles, sp, rgdal, randomForest, caret, e1071, MASS, ROCR, corrplot, rfUtilities, VSURF, ggplot2)
# 
# set.seed(123)
# ## "data" should be replaced with final VoA data frame (VoAVars) created from CFBDataAPI data
# str(VoA2019) # 130 obs of 116 variables
# 
# ## create dataframe of predictor variables
# ## Turning Columns into Vectors
# Team <- VoA2019$Team
# ConStrength <- VoA2019$Conference_Strength
# CFPRank <- VoA2019$CFP_Playoff_Ranking
# APRank <- VoA2019$AP_Ranking
# CoachRank <- VoA2019$Coaches_Ranking
# FPIRank <- VoA2019$FPI_Ranking
# SPRank <- VoA2019$SP_Ranking
# FPISPMean <- VoA2019$FPI_SP_Mean
# PollMean <- VoA2019$Mean_Of_Polls_Algorithms
# PollMedian <- VoA2019$Median_Polls_Algorithms
# OppFPI <- VoA2019$Mean_Opponent_FPI
# DefeatFPI <- VoA2019$Mean_FPI_Of_Defeats
# FCSTeamsCount <- VoA2019$FCS_Teams_Played
# FCSTeamsRank <- FCSTeamsCount*5
# FCSOppFPI <- VoA2019$Opp_FPI_With_FCS_Teams
# TotalMOV <- VoA2019$Mean_Total_MOV
# AdjTotalMov <- VoA2019$Adj_Mean_MOV
# MOL <- VoA2019$Mean_MOL
# AdjMol <- VoA2019$Adj_Mean_MOL
# Wins <- VoA2019$Total_Wins
# Loss <- VoA2019$Total_Losses
# WinPercent <- VoA2019$Win_Percent
# TopWins <- VoA2019$Top25_Wins
# TopLoss <- VoA2019$Top25_Losses
# TopPercent <- VoA2019$Top25_Win_Percent
# NonTopWins <- VoA2019$NonTop25_Wins
# NonTopLoss <- VoA2019$NonTop25_Losses
# NonTopPercent <- VoA2019$NonTop25_Win_Percent
# PtsScored <- VoA2019$Avg_Pts_Scored
# TotalOffYds <- VoA2019$Avg_Offensive_Yds
# OffPassYds <- VoA2019$Avg_Passing_Yds
# OffRushYds <- VoA2019$Avg_Rushing_Yds
# ppy <- VoA2019$Points_Per_Yard
# DefPts <- VoA2019$Avg_Pts_Allowed
# DefYds <- VoA2019$Avg_Yds_Allowed
# DefPassYds <- VoA2019$Avg_Passing_Yds_Allowed
# DefRushYds <- VoA2019$Avg_Rushing_Yds_Allowed
# Turnovers <- VoA2019$Turnovers_Generated
# YdDiff <- VoA2019$Yd_Differential
# ThirdDown <- VoA2019$Third_Down_Percent
# FPIOvrl <- VoA2019$FPI_Overall_Index
# FPIOff <- VoA2019$FPI_Offensive_Index
# FPIDef <- VoA2019$FPI_Defensive_Index
# FPIST <- VoA2019$FPI_SpecialTeams_Index
# SPOff <- VoA2019$SP_Offensive_Index
# SPDef <- VoA2019$SP_Defensive_Index
# SPST <- VoA2019$SP_SpecialTeams_Index
# ConChampRank <- VoA2019$RANKING_Conference_Champ
# # VoAVars <- data.frame(Team,ConStrength,CFPRank,APRank,CoachRank,FPIRank,
# #                       SPRank,FPISPMean,PollMean,PollMedian,OppFPI,DefeatFPI,
# #                       FCSTeamsCount,FCSTeamsRank,FCSOppFPI,TotalMOV,
# #                       AdjTotalMov,MOL,AdjMol,Wins,Loss,WinPercent,TopWins,
# #                       TopLoss,TopPercent,NonTopWins,NonTopLoss,NonTopPercent,
# #                       PtsScored,TotalOffYds,OffPassYds,OffRushYds,ppy,
# #                       DefPts,DefYds,DefPassYds,DefRushYds,Turnovers,YdDiff,
# #                       ThirdDown,FPIOvrl,FPIOff,FPIDef,FPIST,SPOff,SPDef,
# #                       SPST,ConChampRank)
# # str(VoAVars)
# VoAVars2 <- data.frame(Team,ConStrength,OppFPI,DefeatFPI,FCSTeamsCount,
#                       FCSTeamsRank,FCSOppFPI,TotalMOV,AdjTotalMov,MOL,AdjMol,
#                       Wins,Loss,WinPercent,TopWins,TopLoss,TopPercent,
#                       NonTopWins,NonTopLoss,NonTopPercent,PtsScored,
#                       TotalOffYds,OffPassYds,OffRushYds,ppy,DefPts,DefYds,
#                       DefPassYds,DefRushYds,Turnovers,YdDiff,ThirdDown,FPIOvrl,
#                       FPIOff,FPIDef,FPIST,SPOff,SPDef,SPST,ConChampRank)
# 
# 
# str(VoAVars2)
# #values and make this a new object
# CFPPredictors <- VoAVars2[,2:40]
# 
# ## Let's look at variable importance and parsimony (reduce number of predictors
# ## as much as possible without losing too much explanatory power) in VoA model
# 
# varimportance_cov <- rf.modelSel(CFPPredictors, MyRanking, imp.scale="se")
# plot(varimportance_cov)
# 
# varimportance_cov <- cbind(rownames(varimportance_cov$sel.importance), varimportance_cov$sel.importance)
# rownames(varimportance_cov) <- NULL
# colnames(varimportance_cov) <- c("name", "imp")
# 
# varimportance_cov_ord <- varimportance_cov[order(-varimportance_cov$imp),]
# varimportance_cov_ord
# 
# # drop columns that are not as important based on rfmodelsel
# 
# raster_cov_names <- varimportance_cov_ord$name[1:10]
# raster_cov_names
# 
# rastercolumns_cov <- CFPPredictors[,as.character(raster_cov_names)]
# labels(rastercolumns_cov)
# 
# # calculate correlation coefficient matrix
# correlation <-cor(rastercolumns_cov, method="pearson")
# 
# # plot the correlation. the darker the number, the more correlated the two
# # variables
# corrplot(correlation,method="number")
# 
# ## You can now manually remove any variables that are above your correlation 
# ## threshold. In this case we will just leave them all in, but we could remove
# ## them by using the following code
# 
# # make data set with just percent cover and raster columns of choice
# data_cov_model <- cbind(rastercolumns_cov, MyRanking = MyRanking)
# 
# 
# # Now let's restrict our final predictors to those that are most important (Change manually) 
# # and run the continuous model
# 
# rf_model1 = randomForest(MyRanking ~ ., data=data_cov_model, importance = TRUE, ntree = 5000, mtry = 2)
# rf_model1
# 
# 
# 
# #rf_model1<- randomForest(PcentBgrnd ~ Slope + Elevation + NBR + SAVI + BSI, importance = TRUE, ntree = 5000, mtry = 3)
# #rf_model1
# 
# # LONG STORY SHORT: higher values of %IncMSE mean that a predictor is more
# # important relative to other predictors
# 
# importance(rf_model1)
# 
# varImpPlot(rf_model1)
# 
# # Plot predicted vs. Observed: now we want to see how well our algorithm
# # predicts bareground presence/absence and compare that to the actual bareground presence.
# 
# # sets graphic parameter so it is a 1 x 1 output
# par(mfrow=c(1,1))
# 
# # creates a vector of predicted values of bareground presence based on model (one for each
# # actual observation)
# predicted <- rf_model1$predicted
# 
# # creates a vector of actual values of bareground based on collected data
# observed<-SPRank
# # accuracy(observed,predicted)
# plot(observed,predicted)
# observed
# ## Test to see Random Forest Output as ranked table
# hmm <- data.frame(Team,predicted)
# gt(hmm)
# 







### THINGS BELOW THIS LINE EITHER NOT NEEDED OR NOT YET FOUND USEFUL


# Class <- factor(c(0, 0, 1, 1))
# PClass <- factor(c(0, 1, 0, 1))
# Y      <- c(2816, 248, 34, 235)
# df <- data.frame(TClass, PClass, Y)
# ggplot(data =  df, mapping = aes(x = observed, y = predicted)) +
#   geom_tile(aes(fill = Y), colour = "white") +
#   geom_text(aes(label = sprintf("%1.0f", Y)), vjust = 1) +
#   scale_fill_gradient(low = "beige", high = "red4") +
#   theme_bw() + theme(legend.position = "none")
# 
# # plot observed values on the x-axis and predicted values on the y-axis. we are
# # looking for these to be correlated (so close to a 1:1 line)
# plot(observed,predicted, ylab= "Predicted", xlab = "Observed", main = "Bareground Percent Cover", pch = 20)
# 
# # we can fit a line to denote the central tendency and plot it on top of the 
# # points
# abline(fit <- lm(predicted ~ observed), col='Black')
# 
# # we can add a legend that calculates an R2 value (ranges from 0 to 1, the
# # closer to 1 the better)
# legend("top", bty="n", legend=paste("R2 is", format(summary(fit)$adj.r.squared, digits=4)))
# 
# # Mean absolute error, Median absolute error, Mean squared error, root mean squared error
# library("Metrics")
# mae(observed, predicted)
# mdae(observed,predicted)
# mse(observed,predicted)
# rmse(observed,predicted)
# #auc(observed,predicted)
# # AUC metric only works on binary vectors
# 
# # Created AUC Code
# #library(ROCR)
# # Calculate the probability of new observations belonging to each class
# # prediction_for_roc_curve will be a matrix with dimensions data_set_size x number_of_classes
# prediction_for_roc_curve <- predict(rf_model1,observed,type="prob")
# # Use pretty colours:
# pretty_colours <- c("#F8766D","#00BA38","#619CFF")
# # Specify the different classes 
# #classes <- levels(validation1$Species)
# # For each class
# for (i in 1:4)
# {
#   # Define which observations belong to class[i]
#   true_values <- ifelse(observed==Class[i],1,0)
#   # Assess the performance of classifier for class[i]
#   pred <- prediction(prediction_for_roc_curve[,i],true_values)
#   perf <- performance(pred, "tpr", "fpr")
#   if (i==1)
#   {
#     plot(perf,main="ROC Curve",col=pretty_colours[i]) 
#   }
#   else
#   {
#     plot(perf,main="ROC Curve",col=pretty_colours[i],add=TRUE) 
#   }
#   # Calculate the AUC and print it to screen
#   auc.perf <- performance(pred, measure = "auc")
#   print(auc.perf@y.values)
# }  
# 
# ## End of Grand Canyon Water Resources code 
# (project which I used this RF model on previously)
# 
#