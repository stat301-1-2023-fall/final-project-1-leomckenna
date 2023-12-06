#loading rds files
rain_cleaned <- read_rds("data/rain_data.rds")
temp_cleaned <- read_rds("data/temp_data.rds")
snow_cleaned <- read_rds("data/snow_data.rds")
monthly_pats_elo <- read_rds("data/elo_data.rds")
pats_games <- read_rds("data/game_data.rds")
pats_weather_data <- read_rds("data/pats_weather_data.rds")
write_csv(rain_cleaned, "data/rain_data.csv")
write_csv(temp_cleaned, "data/temp_data.csv")
write_csv(snow_cleaned, "data/snow_data.csv")
write_csv(monthly_pats_elo, "data/elo_data.csv")
write_csv(pats_games, "data/game_data.csv")
write_csv(pats_weather_data, "data/pats_weather_data.csv")

#Analyzing rain data
annual_rain_line_plot <- rain_cleaned |> 
  ggplot(aes(as.Date(paste(year, month, "01"), format = "%Y %b %d"),annual_mean_rain)) +
  geom_line(color = "darkblue", size = 1.5) +
  labs(x = "Time", y = "Annual Mean Rain", title = "Annual Mean Rain in Foxboro From 2000-2020") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  
        plot.title = element_text(hjust = 0.5),             
        axis.title = element_text(face = "bold"))
ggsave("2_figures/annual_rain_line_plot.png", annual_rain_line_plot)

monthly_rain_line_plot <- rain_cleaned |> 
  ggplot(aes(as.Date(paste(year, month, "01"), format = "%Y %b %d"), monthly_mean_rain)) +
  geom_line(color = "darkblue", size = 0.5) +
  labs(x = "Time", y = "Mean Monthly Precipitation", title = "Mean Monthly Precipitation in Foxboro From 2000-2020") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  
        plot.title = element_text(hjust = 0.5),             
        axis.title = element_text(face = "bold"))
ggsave("2_figures/monthly_rain_line_plot.png", monthly_rain_line_plot) 

#Analyzing temperature data
# Plot 1: Bar plot for annual mean temperature
annual_temp_plot <- temp_cleaned |> 
  ggplot(aes(as.factor(year), annual_mean_temp)) +
  geom_bar(stat = "summary", fun = "mean", fill = "darkred", color = "black", alpha = 0.7) +
  labs(x = "Year", y = "Annual Mean Temperature", title = "Annual Mean Temperature in Foxboro From 2000-2020") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("2_figures/annual_temp_plot.png", annual_temp_plot) 

# Plot 2: Bar plot for monthly mean temperature
month_order <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
monthly_temp_plot <- temp_cleaned |> 
  ggplot(aes(x = factor(month, levels = month_order), y = monthly_mean_temp)) +
  geom_bar(stat = "summary", fun = "mean", fill = "darkred", color = "black", alpha = 0.7) +
  labs(x = "Month", y = "Monthly Mean Temperature", title = "Monthly Mean Temperature in Foxboro from 2000-2020") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("2_figures/monthly_temp_plot.png", monthly_temp_plot) 

#Analyzing snow data - FIX
annual_snow_plot <- snow_cleaned |> 
  mutate(annual_snowfall = as.numeric(annual_snowfall)) |> 
  ggplot(aes(year, annual_snowfall, group = 1)) +
  geom_line(color = "steelblue") +
  geom_point(color = "steelblue") +
  geom_text(aes(label = annual_snowfall), vjust = -0.5, color = "darkred", size = 3) +
  labs(
    title = "Annual Snowfall from 2000-2022",
    x = "Year",
    y = "Annual Snowfall (inches)"
  ) +
  theme_minimal() +
  scale_x_discrete(labels = function(x) paste0("'", substr(x, 3, 4)))
ggsave("2_figures/annual_snow_plot.png", annual_snow_plot) 

monthly_snow_facet_plot <- snow_cleaned |> 
  filter(!is.na(snowfall)) |> 
  mutate(snowfall = as.numeric(snowfall)) |> 
  ggplot(aes(month, snowfall, group = year, color = year)) +
  geom_line() +
  geom_point() +
  facet_wrap(~year, scales = "free_y", ncol = 4) +
  labs(title = "Monthly Snowfall From 2000-2022", x = "Month", y = "Snowfall (inches)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("2_figures/monthly_snow_facet_plot.png", monthly_snow_facet_plot) 

#Analyzing elo and games data
monthly_wins_facet_plot <- monthly_pats_elo |> 
  filter(!is.na(wins_per_month)) |> 
  ggplot(aes(month, wins_per_month, group = year, color = year)) +
  geom_line() +
  geom_point() +
  facet_wrap(~year, scales = "free_y", ncol = 4) +
  labs(title = "Wins Per Month From 2000-2022", x = "Month", y = "Wins") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("2_figures/monthly_wins_facet_plot.png", monthly_wins_facet_plot) 

avg_score_facet_plot <- monthly_pats_elo |> 
  filter(!is.na(avg_score)) |> 
  ggplot(aes(month, avg_score, group = year, color = year)) +
  geom_line() +
  geom_point() +
  facet_wrap(~year, scales = "free_y", ncol = 4) +
  labs(title = "Average Score Per Month From 2000-2022", x = "Month", y = "Wins") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("2_figures/avg_score_facet_plot.png", avg_score_facet_plot)

avg_wind_facet_plot <- pats_games |> 
  filter(!is.na(wind_per_month)) |> 
  ggplot(aes(month, wind_per_month, group = year, color = year)) +
  geom_line() +
  geom_point() +
  facet_wrap(~year, scales = "free_y", ncol = 4) +
  labs(title = "Average Wind Per Month From 2000-2022", x = "Month", y = "Wind") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("2_figures/avg_wind_facet_plot.png", avg_wind_facet_plot)

