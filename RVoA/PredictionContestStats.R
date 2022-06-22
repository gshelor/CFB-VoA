## Analyzing how far off I was compared to the spread provided in collegefootballdata.com's prediction contest for the 2021-22 season and investigating possible patterns and/or trends related to it
## Load Packages
pacman::p_load(tidyverse, matrixStats, grid, gridExtra, gt, viridis, 
               webshot, writexl, rvest, cfbfastR, espnscrapeR, openxlsx, 
               here, ggsci, RColorBrewer, ggpubr, Metrics)
## reading in csv of games and spreqds
# filtering to exclude weeks 1 and 2 because it was only in week 3 that I started making my winner predictions based exclusively on VoA
# due to lack of skill so far I predicted spreads manually and not with good statistical modeling, but tried to do it based on VoA output gap between 2 teams
Predictions <- read_csv(here("Data", "VoA2021", "CFBData_2122_Predictions.csv")) %>%
  filter(Week >= 3)

## Filtering to specifically identify games where VoA and Spread disagreed on winner
Disagreements_1 <- Predictions %>%
  filter(Spread > 0 & Prediction < 0)
Disagreements_2 <- Predictions %>%
  filter(Spread < 0 & Prediction > 0)
## merging 2 disagreement types back together
Disagreements <- rbind(Disagreements_1, Disagreements_2)

## Adding column to indicate when VoA scored a straight up win or loss
Disagreements <- Disagreements %>% dplyr::mutate(Win = case_when(
  Prediction < 0 & Actual < 0 | Prediction > 0 & Actual > 0 ~ "Win",
  Prediction < 0 & Actual > 0 | Prediction > 0 & Actual < 0 ~ "Loss"
))

## Making a table to show wins and losses, print 
WinLossTable <- data.frame(table(Disagreements$Win))
colnames(WinLossTable) <- c("Result", "Frequency")
print(paste("VoA's record in straight up predictions against the spread was ", as.character(WinLossTable[2,2]), "-", as.character(WinLossTable[1,2]), "."))

## adding column of difference, absolute difference between prediction and actual
Disagreements <- Disagreements %>%
  dplyr::mutate(Difference = Prediction - Actual) %>%
  dplyr::mutate(Abs_Diff = abs(Prediction) - abs(Actual))

## Calculate RMSE for disagreements
rmse_VoA <- rmse(Disagreements$Actual, Disagreements$Prediction)
rmse_VoA
mae_VoA <- mae(Disagreements$Actual, Disagreements$Prediction)
mae_VoA


histogram <- ggplot(data, aes(column)) +
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