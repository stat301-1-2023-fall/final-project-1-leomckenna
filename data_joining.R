
#searching for intersecting names
names(monthly_pats_elo) |> 
  intersect(names(foxboro_weather_cleaned))

monthly_foxboro_data <- foxboro_weather_cleaned |> 
  left_join(monthly_pats_elo, by = join_by(year, month)) |> 
  group_by(year, month) 

names(monthly_foxboro_data) |> 
  intersect(names(pats_games))

pats_weather_data <- monthly_foxboro_data |> 
  left_join(pats_games, by = join_by(month, week, season))