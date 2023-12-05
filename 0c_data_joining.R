
#searching for intersecting names
names(monthly_pats_elo) |> 
  intersect(names(foxboro_temp_cleaned))

#joining the three data sets
monthly_foxboro_data <- foxboro_temp_cleaned |> 
  left_join(monthly_pats_elo, by = join_by(year, month)) |> 
  group_by(year, month) 

names(monthly_foxboro_data) |> 
  intersect(names(pats_games))

pats_temp_data <- monthly_foxboro_data |> 
  left_join(pats_games, by = join_by(month, week, season))


