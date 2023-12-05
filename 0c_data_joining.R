#loading rds files
rain_cleaned <- read_rds("data/rain_data.rds")
temp_cleaned <- read_rds("data/temp_data.rds")
snow_cleaned <- read_rds("data/snow_data.rds")
monthly_pats_elo <- read_rds("data/elo_data.rds")
pats_games <- read_rds("data/game_data.rds")


#searching for intersecting names
names(monthly_pats_elo) |> 
  intersect(names(temp_cleaned))

#joining the three data sets
monthly_pats_elo <- temp_cleaned |> 
  left_join(monthly_pats_elo, by = join_by(year, month)) |> 
  group_by(year, month)

names(monthly_pats_elo) |> 
  intersect(names(pats_games))

pats_temp_data <- monthly_pats_elo |> 
  left_join(pats_games, by = join_by(year, month)) 

pats_weather_data <- pats_temp_data |> 
  left_join(snow_cleaned, by = join_by(year, month)) |> 
  left_join(rain_cleaned, by = join_by(year, month))
