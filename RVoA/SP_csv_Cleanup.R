## This script is designed to read in Bill Connelly's SP+ csvs, select the game, winner, 
# margin and win probability columns, string together each week's csv into 1 data frame,
# export data frame as csv, and read in that csv in the VoP script to derive a
# VoA-based Win Probability metric based on Vortex of Projection's projected margin for
# each game in a given week
# Essentially, I'm trying to use Bill Connelly's win probabilities as training data
# currently I'm using lm() with a quadratic formula of Bill's Proj_margins
# the coefficients are then grabbed, stored in a data frame, applied in VoP script
# VoP script should be run immediately after this script with all the objects retained in the global environment
## Created by Griffin Shelor
## installing packages
# install.packages(c("devtools", "tidyverse", "matrixStats", "gt", "viridis", "webshot", "rvest", "cfbfastR", "here", "ggsci", "RColorBrewer", "ggpubr", "remotes", "pacman", "gtExtras", cfbplotR))
## Load Packages for Ranking Variables
library(pacman)
pacman::p_load(tidyverse, gt, here, RColorBrewer, gtExtras, ggpubr, Metrics)

## Reading in csvs
# some commented out until that week's projections are released
week1_sp <- read_csv(here("Data", "2022SPWeek1FBS.csv")) |>
  select(Game, Proj_winner, Proj_margin, Win_prob)
week2_sp <- read_csv(here("Data", "2022SPWeek2FBS.csv")) |>
  select(Game, Proj_winner, Proj_margin, Win_prob)
week3_sp <- read_csv(here("Data", "2022SPWeek3FBS.csv")) |>
  select(Game, Proj_winner, Proj_margin, Win_prob)
week4_sp <- read_csv(here("Data", "2022SPWeek4FBS.csv")) |>
  select(Game, Proj_winner, Proj_margin, Win_prob)
week5_sp <- read_csv(here("Data", "2022SPWeek5FBS.csv")) |>
  select(Game, Proj_winner, Proj_margin, Win_prob)
week6_sp <- read_csv(here("Data", "2022SPWeek6FBS.csv")) |>
  select(Game, Proj_winner, Proj_margin, Win_prob)
week7_sp <- read_csv(here("Data", "2022SPWeek7FBS.csv")) |>
  select(Game, Proj_winner, Proj_margin, Win_prob)
week8_sp <- read_csv(here("Data", "2022SPWeek8FBS.csv")) |>
  select(Game, Proj_winner, Proj_margin, Win_prob)
# week9_sp <- read_csv(here("Data", "2022SPWeek9FBS.csv")) |>
# select(Game, Proj_winner, Proj_margin, Win_prob)
# week10_sp <- read_csv(here("Data", "2022SPWeek10FBS.csv")) |>
# select(Game, Proj_winner, Proj_margin, Win_prob)
# week11_sp <- read_csv(here("Data", "2022SPWeek11FBS.csv")) |>
# select(Game, Proj_winner, Proj_margin, Win_prob)
# week12_sp <- read_csv(here("Data", "2022SPWeek12FBS.csv")) |>
# select(Game, Proj_winner, Proj_margin, Win_prob)
# week13_sp <- read_csv(here("Data", "2022SPWeek13FBS.csv")) |>
# select(Game, Proj_winner, Proj_margin, Win_prob)
# week14_sp <- read_csv(here("Data", "2022SPWeek14FBS.csv")) |>
# select(Game, Proj_winner, Proj_margin, Win_prob)
# week15_sp <- read_csv(here("Data", "2022SPWeek15FBS.csv")) |>
# select(Game, Proj_winner, Proj_margin, Win_prob)
# week16_sp <- read_csv(here("Data", "2022SPWeek16FBS.csv")) |>
# select(Game, Proj_winner, Proj_margin, Win_prob)


## combining weekly SP+ data frames to get one df of games and SP+ proj margins and win probabilities
all_sp <- rbind(week1_sp, week2_sp)
all_sp <- rbind(all_sp, week3_sp)
all_sp <- rbind(all_sp, week4_sp)
all_sp <- rbind(all_sp, week5_sp)
all_sp <- rbind(all_sp, week6_sp)
all_sp <- rbind(all_sp, week7_sp)
all_sp <- rbind(all_sp, week8_sp)
# all_sp <- rbind(all_sp, week9_sp)
# all_sp <- rbind(all_sp, week10_sp)
# all_sp <- rbind(all_sp, week11_sp)
# all_sp <- rbind(all_sp, week12_sp)
# all_sp <- rbind(all_sp, week13_sp)
# all_sp <- rbind(all_sp, week14_sp)
# all_sp <- rbind(all_sp, week15_sp)
# all_sp <- rbind(all_sp, week16_sp)

## removing nas
all_sp <- all_sp |>
  drop_na()

## converting win probs into numeric format, dropping % sign
all_sp <- all_sp |>
  separate(col = Win_prob, into = "Win_prob",sep = "%")
all_sp$Win_prob <- as.numeric(all_sp$Win_prob)


## win probability plot
WP_plot <- ggplot(all_sp, aes(x = Proj_margin, y = Win_prob)) +
  geom_point(size = 2) +
  geom_smooth() +
  stat_regline_equation(label.y = 90, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 80, aes(label = ..rr.label..))
WP_plot

## trying to match Bill Connelly's WP numbers
## win probability linear model
WP_model <- lm(Win_prob ~ Proj_margin, data = all_sp)
# summary(WP_model)
## adding WP predictions to all_sp
all_sp <- all_sp |> mutate(WP_predict = predict(WP_model))

## quadratic WP model
## adding square of Bill Connelly's projected margin to all_sp
all_sp <- all_sp |> mutate(Proj_margin_sqr = Proj_margin^2, .before = 4)

WP_model_squares <- lm(Win_prob ~ Proj_margin + Proj_margin_sqr, data = all_sp)
## adding WP predictions to all_sp
all_sp <- all_sp |> mutate(WP_predict_sqrs = predict(WP_model_squares))

## win probability plot
WP_plot_sqrs <- ggplot(all_sp, aes(x = Proj_margin_sqr, y = Win_prob)) +
  geom_point(size = 2) +
  geom_smooth() +
  stat_regline_equation(label.x = 2500, label.y = 70, aes(label = ..eq.label..)) +
  stat_regline_equation(label.x = 2500, label.y = 60, aes(label = ..rr.label..))
WP_plot_sqrs

## creating error metrics
errors <- c(mae(all_sp$Win_prob, all_sp$WP_predict), mae(all_sp$Win_prob, all_sp$WP_predict_sqrs),
            rmse(all_sp$Win_prob, all_sp$WP_predict), rmse(all_sp$Win_prob, all_sp$WP_predict_sqrs))
errors

## adding individual error column to all_sp
all_sp <- all_sp |> mutate(WP_predict_ae = ae(Win_prob, WP_predict),
                           WP_predict_se = se(Win_prob, WP_predict),
                           WP_predict_sqrs_ae = ae(Win_prob, WP_predict_sqrs),
                           WP_predict_sqrs_se = se(Win_prob, WP_predict_sqrs))

## plotting WP_predict_sqrs against Bill Connelly's Win_prob
WP_comparison <- ggplot(all_sp, aes(x = WP_predict_sqrs, y = Win_prob)) +
  geom_point(size = 2) +
  geom_smooth() +
  stat_regline_equation(label.y = 90, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 80, aes(label = ..rr.label..))
WP_comparison

## exporting all_sp as csv
# write_csv(all_sp, here("Data", "All_SP.csv"))

## storing coefficients in data frame so they can be referenced in VoP script
coefficients <- data.frame(WP_model_squares$coefficients) # |>
  # pivot_longer()
