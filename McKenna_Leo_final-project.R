# Final Project ----
# Stat 301-1

## load packages and data----
library(tidyverse)
#read in data
elo_data <- read_csv("data/nfl_elo.csv")

#playing around with data
elo_data |> 
  filter(!is.na(playoff)) |> 
  summarize(
    mean_game_elo_pre = mean((elo1_pre + elo2_pre)/2)
  ) |> 
  arrange(desc(mean_game_elo_pre))

elo_data |> 
  filter(is.na(playoff)) |> 
  summarize(
    mean_game_elo_pre = mean((elo1_pre + elo2_pre)/2)
  ) |> 
  arrange(desc(mean_game_elo_pre))

elo_data |> 
  filter(elo1_pre > 1800 | elo2_pre > 1800,
         is.na(playoff))
