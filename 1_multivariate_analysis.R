
pats_weather_data |> 
  ggplot(aes(x = elo_post_per_month, y = monthly_mean_temp)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = lm, color = "darkred", fill = "black", alpha = 0.2) +
  labs(x = "ELO Post per Month", y = "Monthly Mean Temperature", title = "Relationship between Average Monthly
       ELO Post and Monthly Mean Temperature") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    text = element_text(color = "black"),
    plot.title = element_text(hjust = 0.5),
    axis.title = element_text(face = "bold"),
    legend.position = "top"
  )

pats_weather_data |> 
  ggplot(aes(x = wins_per_month, y = monthly_mean_temp)) +
  geom_bar(stat = "summary", fun = "mean", fill = "darkred", color = "black", alpha = 0.7) +
  labs(x = "Wins per Month", y = "Monthly Mean Temperature") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    text = element_text(color = "black")
  )