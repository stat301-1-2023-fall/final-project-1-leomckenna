# Final Project Progress Memo 2 ----
# Stat 301-1

## load packages and data----
library(tidyverse)
#read in data
nfl_elo_data <- read_csv("data/nfl_elo.csv")
foxboro_weather <- read_csv("data/foxboro_weather_data.csv")

#cleaning data
foxboro_weather_cleaned <- foxboro_weather |> 
  select(-c(1, 16)) |> 
  slice(-c(1, 23, 24, 25, 26, 32)) |> 
  slice(-27)

foxboro_weather_cleaned <- foxboro_weather_cleaned |> 
  setNames(foxboro_weather_cleaned[1, ]) |> 
  slice(-1) |> 
  pivot_longer(cols = -c(Year, Annual), names_to = "Month", values_to = "Value") 

foxboro_weather_cleaned$Value[foxboro_weather_cleaned$Value == "M"] <- NA
foxboro_weather_cleaned$Value <- as.numeric(foxboro_weather_cleaned$Value)
  
foxboro_weather_cleaned <- foxboro_weather_cleaned |>   
  group_by(Year) |> 
  summarize(
    mean_temp = mean(Value, na.rm = TRUE),
    max_temp = max(Value, na.rm = TRUE),
    min_temp = min(Value, na.rm = TRUE)
  ) 

  
#searching for intersecting names
names(nfl_elo_data) |> 
  intersect(names(foxboro_weather_cleaned))

#filtering data and getting rid of unnecessary teams
pats_elo_data <- nfl_elo_data |> 
  filter(season >= 2000, team1 == "NE" | team2 == "NE") |> 
  group_by(season) 

pats_elo_data <- pats_elo_data |> 
  filter(team1 == "NE" | team2 == "NE") |> 
  mutate(
    team = if_else(team1 == "NE", team1, team2),
    elo_pre = if_else(team1 == "NE", elo1_pre, elo2_pre),
    elo_prob = if_else(team1 == "NE", elo_prob1, elo_prob2),
    elo_post = if_else(team1 == "NE", elo1_post, elo2_post),
    qbelo_pre = if_else(team1 == "NE", qbelo1_pre, qbelo2_pre),
    qb = if_else(team1 == "NE", qb1, qb2),
    qb_value_pre = if_else(team1 == "NE", qb1_value_pre, qb2_value_pre),
    qb_adj = if_else(team1 == "NE", qb1_adj, qb2_adj),
    qbelo_prob = if_else(team1 == "NE", qbelo_prob1, qbelo_prob2),
    qb_game_value = if_else(team1 == "NE", qb1_game_value, qb2_game_value),
    qb_value_post = if_else(team1 == "NE", qb1_value_post, qb2_value_post),
    qbelo_post = if_else(team1 == "NE", qbelo1_post, qbelo2_post)
  ) |> 
  select(date, season, neutral, playoff, team, elo_pre, elo_prob, elo_post,
         qbelo_pre, qb, qb_value_pre, qb_adj, qbelo_prob, qb_game_value,
         qb_value_post, qbelo_post, quality, importance, total_rating)

#joining data

pats_elo_data <- pats_elo_data |> 
  mutate(season = as.character(season)) |> 
  select(-5)

pats_elo_data |> 
  left_join(foxboro_weather_cleaned, by = join_by(season == Year)) |> 
  select(-c(contains('qb')))


