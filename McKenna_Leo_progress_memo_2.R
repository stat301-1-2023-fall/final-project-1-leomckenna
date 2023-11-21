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
  
foxboro_weather_cleaned |>   
  group_by(Year) |> 
  summarize(
    Mean = mean(Value, na.rm = TRUE),
    Max = max(Value, na.rm = TRUE),
    Min = min(Value, na.rm = TRUE)
  ) |> 
  bind_cols(select(foxboro_weather_cleaned, Year, Annual)) %>%
  arrange(Year)

  

##joining data
#searching for intersecting names
names(nfl_elo_data) |> 
  intersect(names(draft_picks))

nfl_elo_data |> 
  filter(season >= 2000, team1 == "NE" | team2 == "NE")



