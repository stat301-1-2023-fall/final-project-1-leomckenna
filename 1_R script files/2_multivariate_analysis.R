pat_stats <- pats_weather_data |>
  ungroup() |> 
  mutate(wins_per_month = as.numeric(wins_per_month),
         annual_mean_temp = as.numeric(annual_mean_temp)) |> 
  skim_without_charts(wins_per_month, pats_score, monthly_mean_temp, annual_mean_temp)

sum_stats_table <- kable(pat_stats)

cold_stats <- pats_weather_data |> 
  filter(snowfall > 15 & monthly_mean_temp < 32) 

cold_stats_table <- kable(cold_stats)

#Average Score Plots
score_temp_scatterplot <- pats_weather_data |> 
  ggplot(aes(x = avg_score, y = monthly_mean_temp)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = lm, color = "darkred", fill = "black", alpha = 0.2) +
  labs(x = "Patriots Scores per Month", y = "Monthly Mean Temperature", title = "Relationship between Average
       Patriots Scores
       and Monthly Mean Temperature") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    text = element_text(color = "black"),
    plot.title = element_text(hjust = 0.5),
    axis.title = element_text(face = "bold"),
    legend.position = "top"
  )
ggsave("figures and tables/score_temp_scatterplot.png", score_temp_scatterplot)
      
         
score_snow_scatterplot <- pats_weather_data |> 
  ggplot(aes(x = avg_score, y = snowfall)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = lm, color = "darkred", fill = "black", alpha = 0.2) +
  labs(x = "Patriots Scores per Month", y = "Monthly Snowfall", title = "Relationship between Average
       Patriots Scores
       and Monthly Snowfall") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    text = element_text(color = "black"),
    plot.title = element_text(hjust = 0.5),
    axis.title = element_text(face = "bold"),
    legend.position = "top"
  )
ggsave("figures and tables/score_snow_scatterplot.png", score_snow_scatterplot)

wins_snow_scatterplot <- pats_weather_data |> 
  ggplot(aes(x = wins_per_month, y = snowfall)) +
  geom_bar(stat = "summary", fun = "mean", fill = "darkred", color = "black", alpha = 0.7) +
  labs(x = "Wins per Month", y = "Monthly Snowfall") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    text = element_text(color = "black")
  )
ggsave("figures and tables/wins_snow_scatterplot.png", wins_snow_scatterplot)

wins__temp_scatterplot <- pats_weather_data |> 
  ggplot(aes(x = wins_per_month, y = monthly_mean_temp)) +
  geom_bar(stat = "summary", fun = "mean", fill = "darkred", color = "black", alpha = 0.7) +
  labs(x = "Wins per Month", y = "Monthly Mean Temperature") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    text = element_text(color = "black")
  )
ggsave("figures and tables/wins_temp_scatterplot.png", wins_temp_scatterplot)

pats_weather_data |> 
  ggplot(aes(x = wins_per_month, y = monthly_mean_rain)) +
  geom_bar(stat = "summary", fun = "mean", fill = "darkred", color = "black", alpha = 0.7) +
  labs(x = "Wins per Month", y = "Monthly Mean Precipitation") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    text = element_text(color = "black")
  )
  
pats_weather_data |> 
  ggplot(aes(x = avg_score, y = monthly_mean_rain)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = lm, color = "darkred", fill = "black", alpha = 0.2) +
  labs(x = "Patriots Scores per Month", y = "Monthly Precipitation", title = "Relationship between Average
       Patriots Scores
       and Monthly Precipitation") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    text = element_text(color = "black"),
    plot.title = element_text(hjust = 0.5),
    axis.title = element_text(face = "bold"),
    legend.position = "top"
  )

multivar_rain_boxplot <- pats_weather_data |> 
  filter(!is.na(wins_per_month) & (wins_per_month == 1) | (wins_per_month == 2) | (wins_per_month == 3) | 
           (wins_per_month == 4)) |> 
  ggplot(aes(wins_per_month, avg_score, fill = cut_number(monthly_mean_rain, 3))) +
  geom_boxplot()
ggsave("figures and tables/multivar_rain_boxplot.png", multivar_rain_boxplot)
       
multivar_temp_boxplot <- pats_weather_data |> 
  filter(!is.na(wins_per_month) & (wins_per_month == 1) | (wins_per_month == 2) | (wins_per_month == 3) | 
           (wins_per_month == 4)) |> 
  ggplot(aes(wins_per_month, avg_score, fill = cut_number(monthly_mean_temp, 3))) +
  geom_boxplot()
ggsave("figures and tables/multivar_temp_boxplot.png", multivar_temp_boxplot)

pats_corr <- pats_weather_data |> 
  mutate(wins_per_month = as.numeric(wins_per_month),
         snowfall = as.numeric(snowfall)
  ) |> 
  ungroup() |> 
  select(where(is.numeric)) |> 
  select(monthly_mean_temp, wind_per_month, snowfall, avg_score, qbelo_post, 
         wins_per_month, monthly_mean_rain) |> 
  na.omit()

cor_matrix <- cor(pats_corr)

corr_plot <- ggcorrplot(cor_matrix, 
                        hc.order = TRUE, 
                        type = "upper", 
                        outline.col = "white", 
                        lab_size = 3
)
ggsave("figures and tables/correlation_plot.png", corr_plot)



