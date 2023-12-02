# Assuming your data frame is named joined_full_data_set
# and it has columns like ref_area, time, and Union Density

# Unique countries in the dataset
country_focus <- unique(joined_full_data_set$ref_area)

# Loop over each country and create a plot
plots <- list()
for(country in country_focus){
  # Filter data for the current country
  TUD_country <- joined_full_data_set %>%
    filter(ref_area == country)
  
  # Plotting the trend of Union Density over time
  p <- ggplot(TUD_country, aes(x = time, y = `Union Density`)) +
    geom_line() +
    labs(title = paste("Time Series of Union Density in", country),
         x = "Time",
         y = "Union Density") +
    theme_minimal()
  
  plots[[country]] <- p
}

# If you want to view a specific plot, you can do so by
# print(plots[["United States"]]) # as an example

print(plots[["United States"]])
print(plots[["Germany"]])
print(plots[["Austria"]])
print(plots[["Sweden"]])
print(plots[["China"]])
print(plots[["Brazil"]])
print(plots[["Spain"]])
print(plots[["Japan"]])
print(plots[["Australia"]])

library(ggplot2)
library(dplyr)

# Assuming TUDR is your dataset and it contains columns: ref_area, time, and obs_value

# Filter out any NA values if necessary
joined_full_data_set <- na.omit(joined_full_data_set)

# Plotting the trend of Trade Union Density over time for all countries
h <- ggplot(joined_full_data_set, aes(x = time, y = `Union Density`, group = ref_area, color = ref_area)) +
  geom_line() +
  labs(title = "Time Series of Trade Union Density by Country (ILOdata)",
       x = "Time",
       y = "Union Density") +
  theme_minimal() +
  theme(legend.position="bottom")

# Print the plot
print(h)



# Unique countries in the dataset
country_focus <- unique(TUDR$ref_area)

# Loop over each country and create a plot
plots <- list()
for(country in country_focus){
  # Filter data for the current country
  TUDR_Country <- TUDR %>%
    filter(ref_area == country)
  
  # Plotting the trend of Trade Union Density over time
  p <- ggplot(TUDR_Country, aes(x = time, y = obs_value)) +
    geom_line() +
    labs(title = paste("Time Series of Trade Union Density in", country,"(ILOdata)"),
         x = "Time",
         y = "Trade Union Density") +
    theme_minimal()
  
  plots[[country]] <- p
}

# If you want to view a specific plot, you can do so by
# print(plots[["United States"]]) # as an example

print(plots[["United States"]])
print(plots[["Germany"]])
print(plots[["Austria"]])
print(plots[["Sweden"]])
print(plots[["China"]])
print(plots[["Brazil"]])
print(plots[["Spain"]])
print(plots[["Japan"]])
print(plots[["Australia"]])








# Unique countries in the dataset
country_focus <- unique(CBCR$ref_area)

# Loop over each country and create a plot
plots <- list()
for(country in country_focus){
  # Filter data for the current country
  CBCR_Country <- CBCR %>%
    filter(ref_area == country)
  
  # Plotting the trend of Collective Bargaining Coverage over time
  p <- ggplot(CBCR_Country, aes(x = time, y = obs_value)) +
    geom_line() +
    labs(title = paste("(ILOdata) Collective Bargaining Coverage Over Time in", country),
         x = "Time",
         y = "(ILOdata) Collective Bargaining Coverage") +
    theme_minimal()
  
  plots[[country]] <- p
}

# If you want to view a specific plot, you can do so by
# print(plots[["United States"]]) # as an example

print(plots[["United States"]])
print(plots[["Germany"]])
print(plots[["Austria"]])
print(plots[["Sweden"]])
print(plots[["China"]])
print(plots[["Brazil"]])
print(plots[["Spain"]])
print(plots[["Japan"]])
print(plots[["Australia"]])




# Unique countries in the dataset
country_focus <- unique(WorkplaceRights$ref_area)

# Loop over each country and create a plot
plots <- list()
for(country in country_focus){
  # Filter data for the current country
  WorkplaceRights_Country <- WorkplaceRights %>%
    filter(ref_area == country)
  
  # Plotting the trend of Compliance with International Labor Law over time
  p <- ggplot(WorkplaceRights_Country, aes(x = time, y = obs_value)) +
    geom_line() +
    labs(title = paste("(ILOdata) Compliance with International Labor Law Over Time in", country),
         x = "Time",
         y = "(ILOdata) Compliance with International Labor Law") +
    theme_minimal()
  
  plots[[country]] <- p
}

# If you want to view a specific plot, you can do so by
# print(plots[["United States"]]) # as an example


print(plots[["United States"]])
print(plots[["Germany"]])
print(plots[["Austria"]])
print(plots[["Sweden"]])
print(plots[["China"]])
print(plots[["Brazil"]])
print(plots[["Spain"]])
print(plots[["Japan"]])
print(plots[["Australia"]])



