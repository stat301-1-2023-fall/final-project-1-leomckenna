#loading rds files
rain_cleaned <- read_rds("data/rain_data.rds")
temp_cleaned <- read_rds("data/temp_data.rds")
snow_cleaned <- read_rds("data/snow_data.rds")
monthly_pats_elo <- read_rds("data/elo_data.rds")
pats_games <- read_rds("data/game_data.rds")

#Analyzing rain data
annual_rain_line_plot <- rain_cleaned |> 
  ggplot(aes(as.Date(paste(year, month, "01"), format = "%Y %b %d"),annual_mean_rain)) +
  geom_line(color = "darkblue", size = 1.5) +
  labs(x = "Time", y = "Annual Mean Rain", title = "Annual Mean Rain in Foxboro From 2000-2020") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  
        plot.title = element_text(hjust = 0.5),             
        axis.title = element_text(face = "bold"))
ggsave("figures and tables/annual_rain_line_plot.png", annual_rain_line_plot)

monthly_rain_line_plot <- rain_cleaned |> 
  ggplot(aes(as.Date(paste(year, month, "01"), format = "%Y %b %d"), monthly_mean_rain)) +
  geom_line(color = "darkblue", size = 0.5) +
  labs(x = "Time", y = "Mean Monthly Precipitation", title = "Mean Monthly Precipitation in Foxboro From 2000-2020") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  
        plot.title = element_text(hjust = 0.5),             
        axis.title = element_text(face = "bold"))
ggsave("figures and tables/monthly_rain_line_plot.png", monthly_rain_line_plot) 

#Analyzing temperature data
# Plot 1: Bar plot for annual mean temperature
temp_cleaned |> 
  ggplot(aes(as.factor(year), annual_mean_temp)) +
  geom_bar(stat = "summary", fun = "mean", fill = "darkred", color = "black", alpha = 0.7) +
  labs(x = "Year", y = "Annual Mean Temperature", title = "Annual Mean Temperature in FoxboroFrom 2000-2023") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot 2: Bar plot for monthly mean temperature
month_order <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
temp_cleaned |> 
  ggplot(aes(x = factor(month, levels = month_order), y = monthly_mean_temp)) +
  geom_bar(stat = "summary", fun = "mean", fill = "darkred", color = "black", alpha = 0.7) +
  labs(x = "Month", y = "Monthly Mean Temperature", title = "Monthly Mean Temperature in Foxboro from 2000-2023") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Analyzing snow data






