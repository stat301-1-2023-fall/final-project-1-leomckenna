#cleaning temp data
foxboro_temp_cleaned <- foxboro_temp |> 
  select(-c(1, 16)) |> 
  slice(-c(1, 23, 24, 25, 26, 32)) |> 
  slice(-27)

foxboro_temp_cleaned <- foxboro_temp_cleaned |> 
  setNames(foxboro_temp_cleaned[1, ]) |> 
  slice(-1) |> 
  pivot_longer(cols = -c(Year, Annual), names_to = "Month", values_to = "Value") |> 
  rename(annual_mean_temp = Annual, monthly_mean_temp = Value)

foxboro_temp_cleaned$monthly_mean_temp[foxboro_temp_cleaned$monthly_mean_temp == "M"] <- NA
foxboro_temp_cleaned$monthly_mean_temp <- as.numeric(foxboro_temp_cleaned$monthly_mean_temp)

foxboro_temp_cleaned <- foxboro_temp_cleaned |>   
  group_by(Year) |> 
  rename_all(tolower) 

foxboro_temp_cleaned <- foxboro_temp_cleaned |>
  ungroup() |> 
  slice(-c(241:300)) |> 
  filter(!is.na(monthly_mean_temp)) |> 
  group_by(month) |> 
  filter(month == "Sep" | month == "Oct" | month == "Nov" | month == "Dec" | month == "Jan" | month == "Feb") |> 
  relocate(year, month)

#cleaning snow data

foxboro_snow <- foxboro_snow |> 
  select(-c(1, 16)) |> 
  slice(-c(1, 27:33)) 

snow_cleaned <- foxboro_snow |> 
  setNames(foxboro_snow[1, ]) |> 
  slice(-1) |> 
  pivot_longer(cols = -c(Year, Season), names_to = "month", values_to = "snowfall") |> 
  rename_all(tolower) |> 
  mutate(year = str_sub(year, 1, 4)) |> 
  group_by(year, month) |> 
  relocate(year, month) |> 
  filter(year > 1999 & (month == "Sep" | month == "Oct" | month == "Nov" | 
                          month == "Dec" | month == "Jan" | month == "Feb")) |> 
  mutate(snowfall = if_else(snowfall == "T", NA_character_, snowfall)) |> 
  rename(annual = season)

#filtering elo data and getting rid of unnecessary teams
pats_elo_data <- nfl_elo_data |> 
  filter(season >= 2000, team1 == "NE" | team2 == "NE") |> 
  mutate(winner = case_when(score1 > score2 ~ team1,
                            score2 > score1 ~ team2,
                            TRUE ~ "Tie")) |> 
  mutate(pats_score = if_else(team1 == "NE", score1, if_else(team2 == "NE", score2, NA_real_))) |> 
  select(date, season, winner, everything())

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
         qb_value_post, qbelo_post, quality, importance, total_rating, score1, score2, winner, playoff, pats_score) 



#Summarizing data by month for the elo data set to so I can analyze using monthly temp trends

monthly_pats_elo <- pats_elo_data |> 
  mutate(season = as.character(season),
         year = year(date),
         month = month(date),
         week = week(date)) |> 
  select(-c(1, 5)) |> 
  relocate(year, month, week, winner) |> 
  group_by(year, month, season, winner, playoff) |> 
  mutate(wins_per_month = sum(winner == "NE")) |> 
  group_by(year, month, season) |> 
  mutate(elo_pre_per_month = mean(elo_pre),
         elo_post_per_month = mean(elo_post)) |> 
  ungroup() |> 
  relocate(wins_per_month, elo_pre_per_month, elo_post_per_month)|> 
  mutate(year = as.character(year)) |> 
  mutate(month = month.abb[month]) |> 
  mutate(wins_per_month = as_factor(wins_per_month))


#Filtering nfl_games data set

pats_games <- nfl_games |> 
  select(season, game_id, gameday, away_team, temp, wind, stadium_id, referee, overtime, week) |> 
  filter(str_detect(stadium_id, "BOS") & season > 1999) |> 
  mutate(month = str_extract(gameday, "(?<=-)\\d{2}")) |> 
  mutate(month = as.numeric(month)) |> 
  mutate(month = month.abb[month]) |> 
  mutate(season = as.character(season)) |> 
  relocate(season, month)


