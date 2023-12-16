# Assuming you have a vector named southern_states containing the names of northern states

# Plotting with ggplot2
ggplot(long_data, aes(x = Year, y = Value, group = StateName)) +
  # Plot all states not in southern_states in grey
  geom_line(
    data = filter(long_data, !StateName %in% southern_states),
    aes(color = "Other States"), size = 0.5
  ) +
  # Highlight southern_states with individual colors
  geom_line(
    data = filter(long_data, StateName %in% southern_states),
    aes(color = StateName), linewidth = 1.5
  ) +
  scale_color_manual(values = c(
    "Other States" = "grey",
    setNames(rainbow(length(southern_states)), southern_states)
  )) +
  scale_x_continuous(breaks = seq(min(long_data$Year), max(long_data$Year), by = 5)) +
  theme_minimal() +
  labs(
    title = "Time Series Analysis with Highlight on Northern States",
    x = "Year",
    y = "Value"
  ) +
  theme(legend.position = "bottom") # Include legend
