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
    Mean = mean(Value, na.rm = TRUE),
    Max = max(Value, na.rm = TRUE),
    Min = min(Value, na.rm = TRUE)
  ) 

foxboro_weather_cleaned
  

##joining data
#searching for intersecting names
names(nfl_elo_data) |> 
  intersect(names(foxboro_weather_cleaned))

pats_elo_data <- nfl_elo_data |> 
  filter(season >= 2000, team1 == "NE" | team2 == "NE") |> 
  group_by(season) 


pats_elo_results <- data.frame()

for (i in 1:nrow(pats_elo_data)) {
  team1_is_ne <- pats_elo_data[i, "team1"] == "NE"
  team2_is_ne <- pats_elo_data[i, "team2"] == "NE"
  if (team1_is_ne) {
    selected_columns <- c("date", "season", "neutral", "playoff", "team1", 
                          "elo1_pre", "elo2_pre", "elo_prob1", "elo_prob2", "elo1_post",
                          "elo2_post" , "qbelo1_pre" , "qbelo2_pre" , "qb1", "qb2",
                          "qb1_value_pre" , "qb2_value_pre" , "qb1_adj" ,"qb2_adj" , "qbelo_prob1" ,
                           "qbelo_prob2" , "qb1_game_value" ,"qb2_game_value" , "qb1_value_post" ,
                          "qb2_value_post" , "qbelo1_post" , "qbelo2_post" )
  } else if (team2_is_ne) {
    selected_columns <- c("date", "season", "neutral", "playoff", "team1", 
                          "elo1_pre", "elo2_pre", "elo_prob1", "elo_prob2", "elo1_post",
                          "elo2_post" , "qbelo1_pre" , "qbelo2_pre" , "qb1", "qb2",
                          "qb1_value_pre" , "qb2_value_pre" , "qb1_adj" ,"qb2_adj" , "qbelo_prob1" ,
                          "qbelo_prob2" , "qb1_game_value" ,"qb2_game_value" , "qb1_value_post" ,
                          "qb2_value_post" , "qbelo1_post" , "qbelo2_post" )
  } else {
    # If NE is not in team1 or team2, skip the iteration
    next
  }
  row_data <- pats_elo_data[i, selected_columns, drop = FALSE]
}





