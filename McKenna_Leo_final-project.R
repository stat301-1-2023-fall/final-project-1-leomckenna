# Final Project ----
# Stat 301-1

## load packages and data----
library(tidyverse)
#read in data
nfl_elo_data <- read_csv("data/nfl_elo.csv")

## Data quality & complexity check
#There are 33 total variables and 17,379 observations. 
nfl_elo_data |> 
  select(where(is.numeric))

nfl_elo_data |> 
  filter(!is.na(total_rating))
  


#playing around with data
nfl_elo_data |> 
  filter(!is.na(playoff)) |> 
  summarize(
    mean_game_elo_pre = mean((elo1_pre + elo2_pre)/2)
  ) |> 
  arrange(desc(mean_game_elo_pre))

nfl_elo_data |> 
  filter(is.na(playoff)) |> 
  summarize(
    mean_game_elo_pre = mean((elo1_pre + elo2_pre)/2)
  ) |> 
  arrange(desc(mean_game_elo_pre))

nfl_elo_data |> 
  filter(elo1_pre > 1800 | elo2_pre > 1800,
         is.na(playoff))
