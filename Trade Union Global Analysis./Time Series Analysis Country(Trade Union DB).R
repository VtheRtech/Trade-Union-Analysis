# Assuming your data frame is named joined_full_data_set
# and it has columns like ref_area, time, and Union Density

# Unique countries in the dataset
country_focus <- unique(joined_full_data_set$ref_area)

# Loop over each country and create a plot
plots <- list()
for (country in country_focus) {
  # Filter data for the current country
  TUD_country <- joined_full_data_set %>%
    filter(ref_area == country)

  # Plotting the trend of Union Density over time
  p <- ggplot(TUD_country, aes(x = time, y = `Union Density`)) +
    geom_line(color = "#00BFC4", size = 1.2) + # Use a distinct color and increase line thickness
    labs(
      title = paste("Time Series of Union Density in", country, "(ILOdata)"),
      x = "Year",
      y = "Union Density (in %)"
    ) +
    theme_minimal(base_size = 14) + # Adjust base font size for readability
    theme(
      plot.title = element_text(size = 16, face = "bold"), # Bold and larger title
      axis.title.x = element_text(size = 14, face = "bold"), # Bold X axis title
      axis.title.y = element_text(size = 14, face = "bold"), # Bold Y axis title
      panel.grid.major = element_line(color = "gray80"), # Lighter color for major grid lines
      panel.grid.minor = element_blank(), # Remove minor grid lines
      plot.margin = margin(1, 1, 1, 1, "cm") # Adjust plot margins
    )
  plots[[country]] <- p
}

# If you want to view a specific plot, you can do so by
# print(plots[["United States"]]) # as an example

print(plots[["United States"]])

# Assuming TUDR is your dataset and it contains columns: ref_area, time, and obs_value

# Filter out any NA values if necessary
joined_full_data_set <- na.omit(joined_full_data_set)

# Plotting the trend of Trade Union Density over time for all countries
h <- ggplot(joined_full_data_set, aes(x = time, y = `Union Density`, group = ref_area, color = ref_area)) +
  geom_line(size = 1) + # Slightly thicker lines for visibility
  labs(
    title = "Time Series of Trade Union Density by Country",
    x = "Year",
    y = "Union Density (in %)"
  ) +
  theme_minimal(base_size = 14) + # Adjust base font size
  theme(
    plot.title = element_text(size = 16, face = "bold"), # Larger, bold title
    axis.title.x = element_text(size = 14, face = "bold"), # Bold X axis title
    axis.title.y = element_text(size = 14, face = "bold"), # Bold Y axis title
    legend.position = "bottom", # Move legend to the bottom
    legend.title = element_blank(), # Optionally remove the legend title
    legend.text = element_text(size = 12), # Adjust legend text size
    panel.grid.major = element_line(color = "gray80"), # Adjust major grid lines
    panel.grid.minor = element_blank(), # Remove minor grid lines
    plot.margin = margin(1, 1, 1, 1, "cm") # Adjust plot margins
  ) +
  scale_color_brewer(palette = "Set1") # Use a color palette that's distinct and clear# Print the plot
print(h)



# Unique countries in the dataset
country_focus <- unique(TUDR$ref_area)

# Loop over each country and create a plot
plots <- list()
for (country in country_focus) {
  # Filter data for the current country
  TUDR_Country <- TUDR %>%
    filter(ref_area == country)

  # Plotting the trend of Trade Union Density over time
  p <- ggplot(TUDR_Country, aes(x = time, y = obs_value)) +
    geom_line(color = "#1b9e77", size = 1.2) + # A distinct color for the line, increased thickness
    geom_point(color = "#7570b3", size = 2, alpha = 0.7) + # Adding data points in a different color
    labs(
      title = paste("Time Series of Trade Union Density in", country, "(ILOdata)"),
      x = "Year",
      y = "Trade Union Density(in %)"
    ) +
    theme_minimal(base_size = 14) + # Adjusting base font size for better readability
    theme(
      plot.title = element_text(size = 16, face = "bold"), # Making title bold and larger
      axis.title.x = element_text(size = 14, face = "bold"), # Bold X axis title
      axis.title.y = element_text(size = 14, face = "bold"), # Bold Y axis title
      panel.grid.major = element_line(color = "gray80"), # Lighter color for major grid lines
      panel.grid.minor = element_blank(), # Removing minor grid lines
      plot.margin = margin(1, 1, 1, 1, "cm") # Adjusting plot margins for aesthetics
    )
  plots[[country]] <- p
}

# If you want to view a specific plot, you can do so by
# print(plots[["United States"]]) # as an example

print(plots[["United States"]])







# Unique countries in the dataset
country_focus <- unique(CBCR$ref_area)

# Loop over each country and create a plot
plots <- list()
for (country in country_focus) {
  # Filter data for the current country
  CBCR_Country <- CBCR %>%
    filter(ref_area == country)

  # Plotting the trend of Collective Bargaining Coverage over time
  p <- ggplot(CBCR_Country, aes(x = time, y = obs_value)) +
    geom_line(color = "#2ca02c", size = 1.2) + # Use a green color and thicker line
    geom_point(color = "#d62728", size = 2, alpha = 0.7) + # Add red data points
    labs(
      title = paste("Collective Bargaining Coverage Over Time in", country, "(ILOdata)"),
      x = "Year",
      y = "Collective Bargaining Coverage (in %)"
    ) +
    theme_minimal(base_size = 14) + # Adjust base font size
    theme(
      plot.title = element_text(size = 16, face = "bold"), # Bold and larger title
      axis.title.x = element_text(size = 14, face = "bold"), # Bold X axis title
      axis.title.y = element_text(size = 14, face = "bold"), # Bold Y axis title
      panel.grid.major = element_line(color = "gray80"), # Lighter major grid lines
      panel.grid.minor = element_blank(), # Hide minor grid lines
      plot.margin = margin(1, 1, 1, 1, "cm") # Adjust plot margins
    )
  plots[[country]] <- p
}

# If you want to view a specific plot, you can do so by
# print(plots[["United States"]]) # as an example

print(plots[["United States"]])




# Unique countries in the dataset
country_focus <- unique(WorkplaceRights$ref_area)

# Loop over each country and create a plot
plots <- list()
for (country in country_focus) {
  # Filter data for the current country
  WorkplaceRights_Country <- WorkplaceRights %>%
    filter(ref_area == country)

  # Plotting the trend of Compliance with International Labor Law over time
  p <- ggplot(WorkplaceRights_Country, aes(x = time, y = obs_value)) +
    geom_line(color = "#1f78b4", size = 1.2) + # Change color and line size
    geom_point(color = "#33a02c", size = 2, alpha = 0.7) + # Add data points
    labs(
      title = paste("Compliance with International Labor Law Over Time in", country, "(ILOdata)"),
      x = "Year",
      y = "Compliance with International Labor Law (Rating)"
    ) +
    theme_minimal(base_size = 14) + # Adjust base font size
    theme(
      plot.title = element_text(size = 16, face = "bold"), # Customize title
      axis.title.x = element_text(size = 14, face = "bold"), # Customize X axis title
      axis.title.y = element_text(size = 14, face = "bold"), # Customize Y axis title
      panel.grid.major = element_line(color = "gray80"), # Adjust major grid lines
      panel.grid.minor = element_blank(), # Remove minor grid lines
      plot.margin = margin(1, 1, 1, 1, "cm") # Adjust plot margins
    )

  plots[[country]] <- p
}

# If you want to view a specific plot, you can do so by
# print(plots[["United States"]]) as an example


print(plots[["United States"]])




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





#                          HiGHLIGHTED DATA IN THE GRAPHS
#
# Reshape the data from wide to long format
long_data <- state_union_membership_density %>%
  pivot_longer(
    cols = -c(StateName, StateID), # Exclude non-year columns
    names_to = "Year",
    values_to = "Value"
  )

# Convert Year to a numeric value for plotting
long_data$Year <- as.numeric(long_data$Year)

# Create a new column for the color mapping
long_data$Color <- ifelse(long_data$StateName == "Maryland", "Maryland", "Other States")

# Plotting with ggplot2
ggplot(long_data, aes(x = Year, y = Value, group = StateName, color = Color)) +
  geom_line() +
  scale_color_manual(values = c("Maryland" = "blue", "Other States" = "grey")) +
  theme_minimal() +
  labs(
    title = "Time Series Analysis with Highlight on Maryland",
    x = "Year",
    y = "Value"
  ) +
  theme(legend.position = "none") # Remove legend
