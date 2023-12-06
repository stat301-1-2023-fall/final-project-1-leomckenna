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

score_scatterplot <- pats_temp_data |> 
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

ggsave("figures and tables/avg_score_scatterplot.png", score_scatterplot)

wins_scatterplot <- pats_temp_data |> 
  ggplot(aes(x = wins_per_month, y = monthly_mean_temp)) +
  geom_bar(stat = "summary", fun = "mean", fill = "darkred", color = "black", alpha = 0.7) +
  labs(x = "Wins per Month", y = "Monthly Mean Temperature") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    text = element_text(color = "black")
  )
ggsave("figures and tables/wins_scatterplot.png", wins_scatterplot)
      
         



