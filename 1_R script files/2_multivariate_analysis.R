pat_stats <- pats_weather_data |>
  ungroup() |> 
  mutate(wins_per_month = as.numeric(wins_per_month),
         annual_mean_temp = as.numeric(annual_mean_temp)) |> 
  skim_without_charts(monthly_mean_temp, annual_mean_temp)

sum_stats_table <- kable(pat_stats)


cold_stats <- pats_weather_data |> 
  filter(snowfall > 15 & monthly_mean_temp < 32 | monthly_mean_rain > 7) 

cold_stats_table <- kable(cold_stats)

titans_pats <- pats_weather_data |> 
  filter(year == 2009, away_team == "TEN") |> 
  select(year, month, wind_per_month, monthly_mean_rain, away_team, score1, score2)

titans_pats_table <- kable(titans_pats)

#Checking 2006 production versus 2014
comparison_wins_plot <- pats_weather_data |> 
  filter(year == 2006 | year == 2014) |> 
  relocate(wins_per_month, avg_score) |> 
  group_by(year, month) |> 
  summarize(wins_per_month = sum(winner == "NE"), 
            avg_score = mean(avg_score)) |> 
  filter(year %in% c("2006", "2014")) |> 
  ggplot(aes(x = month, y = wins_per_month, fill = factor(year))) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "Wins per Month in 2006 vs. 2014",
       x = "Month",
       y = "Wins",
       fill = "Year") +
  theme_minimal()
ggsave("2_figures/comparison_wins_plot.png", comparison_wins_plot)

comparison_score_plot <- pats_weather_data |> 
  filter(year == 2006 | year == 2014) |> 
  relocate(wins_per_month, avg_score) |> 
  group_by(year, month) |> 
  summarize(wins_per_month = sum(winner == "NE"), 
            avg_score = mean(avg_score)) |> 
  filter(year %in% c("2006", "2014")) |> 
  ggplot(aes(x = month, y = avg_score, fill = factor(year))) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "Average Score per Month in 2006 vs. 2014",
       x = "Month",
       y = "Average Score",
       fill = "Year") +
  theme_minimal()
ggsave("2_figures/comparison_score_plot.png", comparison_score_plot)

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
ggsave("2_figures/score_temp_scatterplot.png", score_temp_scatterplot)
      
         
score_snow_scatterplot <- pats_weather_data |> 
  mutate(snowfall = as.numeric(snowfall)) |> 
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
ggsave("2_figures/score_snow_scatterplot.png", score_snow_scatterplot)

wins_snow_barplot <- pats_weather_data |> 
  mutate(snowfall = as.numeric(snowfall)) |> 
  ggplot(aes(x = wins_per_month, y = snowfall)) +
  geom_bar(stat = "summary", fun = "mean", fill = "darkred", color = "black", alpha = 0.7) +
  labs(x = "Wins per Month", y = "Monthly Snowfall") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    text = element_text(color = "black")
  )
ggsave("2_figures/wins_snow_barplot.png", wins_snow_barplot)

wins_temp_scatterplot <- pats_weather_data |> 
  ggplot(aes(x = wins_per_month, y = monthly_mean_temp)) +
  geom_bar(stat = "summary", fun = "mean", fill = "darkred", color = "black", alpha = 0.7) +
  labs(x = "Wins per Month", y = "Monthly Mean Temperature") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    text = element_text(color = "black")
  )
ggsave("2_figures/wins_temp_scatterplot.png", wins_temp_scatterplot)

wins_rain_plot <- pats_weather_data |> 
  ggplot(aes(x = wins_per_month, y = monthly_mean_rain)) +
  geom_bar(stat = "summary", fun = "mean", fill = "darkred", color = "black", alpha = 0.7) +
  labs(x = "Wins per Month", y = "Monthly Mean Precipitation") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    text = element_text(color = "black")
  )
ggsave("2_figures/wins_rain_plot.png", wins_rain_plot)
  
score_rain_plot <- pats_weather_data |> 
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
ggsave("2_figures/score_rain_plot.png", score_rain_plot)

multivar_rain_boxplot <- pats_weather_data |> 
  filter(!is.na(wins_per_month) & (wins_per_month == 1) | (wins_per_month == 2) | (wins_per_month == 3) | 
           (wins_per_month == 4)) |> 
  ggplot(aes(wins_per_month, avg_score, fill = cut_number(monthly_mean_rain, 3))) +
  geom_boxplot()
ggsave("2_figures/multivar_rain_boxplot.png", multivar_rain_boxplot)
       
multivar_temp_boxplot <- pats_weather_data |> 
  filter(!is.na(wins_per_month) & (wins_per_month == 1) | (wins_per_month == 2) | (wins_per_month == 3) | 
           (wins_per_month == 4)) |> 
  ggplot(aes(wins_per_month, avg_score, fill = cut_number(monthly_mean_temp, 3))) +
  geom_boxplot()
ggsave("2_figures/multivar_temp_boxplot.png", multivar_temp_boxplot)

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
ggsave("2_figures/correlation_plot.png", corr_plot)



