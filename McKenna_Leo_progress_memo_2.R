# Final Project Progress Memo 2 ----
# Stat 301-1

## load packages and data----
library(tidyverse)
library(skimr)
#read in data
nfl_elo_data <- read_csv("data/nfl_elo.csv")
draft_picks <- 
  read_csv("https://raw.githubusercontent.com/leesharpe/nfldata/master/data/draft_picks.csv")

##joining data
#searching for intersecting names
names(nfl_elo_data) |> 
  intersect(names(draft_picks))

draft_picks <- draft_picks |> 
  mutate(team_drafted = team)

nfl_elo_data |> 
  filter(season >= 1980, team1 == "NE" | team2 == "NE") |> 


draft_picks |> 
  filter(team == "NE",
         round == 1)

