#                         state_union_membership_density
# Reshape the data from wide to long format
long_data <- state_union_membership_density %>%
  pivot_longer(
    cols = -c(StateName, StateID), # Exclude non-year columns
    names_to = "Year",
    values_to = "Value"
  )
# Convert Year to a numeric value for plotting
long_data$Year <- as.numeric(long_data$Year)
# Plotting with ggplot2
ggplot(long_data, aes(x = Year, y = Value, color = StateName)) +
  geom_line() +
  theme_minimal() +
  labs(
    title = "Time Series Analysis by State",
    x = "Year",
    y = "Value",
    color = "State"
  ) +
  theme(legend.position = "bottom")



southern_states <- c(
  "Alabama", "Arkansas", "Florida", "Georgia", "Kentucky",
  "Louisiana", "Mississippi", "North Carolina", "South Carolina",
  "Tennessee", "Texas", "Virginia", "West Virginia"
)
# To view the vector
print(southern_states)
northern_states <- c(
  "Connecticut", "Illinois", "Indiana", "Iowa", "Maine",
  "Massachusetts", "Michigan", "Minnesota", "New Hampshire",
  "New Jersey", "New York", "Ohio", "Pennsylvania", "Rhode Island",
  "Vermont", "Wisconsin"
)
# To view the vector
print(northern_states)



#           northern_states time series
# Plotting with ggplot2
ggplot(long_data, aes(x = Year, y = Value, group = StateName)) +
  # Plot all states not in northern_states in grey
  geom_line(
    data = filter(long_data, !StateName %in% northern_states),
    aes(color = "Other States"), size = 0.5
  ) +
  # Highlight northern_states with individual colors
  geom_line(
    data = filter(long_data, StateName %in% northern_states),
    aes(color = StateName), linewidth = 1.5
  ) +
  scale_color_manual(values = c(
    "Other States" = "grey",
    setNames(rainbow(length(northern_states)), northern_states)
  )) +
  scale_x_continuous(breaks = seq(min(long_data$Year), max(long_data$Year), by = 5)) +
  theme_minimal() +
  labs(
    title = "Time Series Analysis with Highlight on Northern States",
    x = "Year",
    y = "Union Membership Density",
  ) +
  theme(legend.position = "bottom") # Include legend




#          southern_states time series
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
    title = "Time Series Analysis with Highlight on Southern States",
    x = "Year",
    y = "Union Membership Density",
  ) +
  theme(legend.position = "bottom") # Include legend
