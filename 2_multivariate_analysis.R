
pats_temp_data |> 
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

pats_temp_data |> 
  ggplot(aes(x = wins_per_month, y = monthly_mean_temp)) +
  geom_bar(stat = "summary", fun = "mean", fill = "darkred", color = "black", alpha = 0.7) +
  labs(x = "Wins per Month", y = "Monthly Mean Temperature") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    text = element_text(color = "black")
  )


pats_temp_data <- pats_temp_data |> 
  relocate(pats_score) |> 
  ungroup() |> 
  group_by(month, year) |> 
  mutate(mean_pats = mean(pats_score)) |> 
  relocate(mean_pats)

pats_temp_data |> 
  ggplot(aes(x = mean_pats, y = monthly_mean_temp)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = lm, color = "darkred", fill = "black", alpha = 0.2) +
  labs(x = "Average Score Per Month", y = "Monthly Mean Temperature", title = "Relationship between Average Monthly
       Score and Monthly Mean Temperature") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    text = element_text(color = "black"),
    plot.title = element_text(hjust = 0.5),
    axis.title = element_text(face = "bold"),
    legend.position = "top"
  )

pats_temp_data |> 
  ungroup() |> 
  mutate(mean_pats_category = fct_collapse(factor(mean_pats),
                                            "Low Score" = c("0-19"),
                                            "Medium Score" = c("20-30"),
                                            "High Score" = c("31-")))

pats_temp_data
       
         



