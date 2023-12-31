library(ggplot2)
library(dplyr)
library(forcats)
library(tibble)

# Assuming your data frames are named 'joined_full_data_set', 'tudr', 'cbcr', 'workplace_rights'
# and they have columns like 'ref_area', 'time', and relevant data columns

# Initialize an empty list to store plots
plots_list <- list()

# Loop for joined_full_data_set (Union Density)
country_focus <- unique(joined_full_data_set$ref_area)
for (country in country_focus) {
  TUD_country <- joined_full_data_set %>%
    filter(ref_area == country)
  plot <- ggplot(TUD_country, aes(x = time, y = `Union Density`)) +
    geom_line(color = "#00BFC4", size = 1.2) +
    labs(
      title = paste("Time Series of Union Density in", country, "(ILOdata)"),
      x = "Year", y = "Union Density (in %)"
    ) +
    theme_minimal(base_size = 14) +
    # Add your custom theme here, if tstheme is defined
    tstheme
  plots_list[[paste("Union_Density", country)]] <- plot
}

# Repeat similar loops for other data sets 'tudr', 'cbcr', 'workplace_rights'
# Ensure you change the data frame, y-axis, and plot title accordingly

# Loop for 'tudr' data frame
country_focus <- unique(tudr$ref_area)
for (country in country_focus) {
  tudr_country <- tudr %>%
    filter(ref_area == country)
  plot <- ggplot(tudr_country, aes(x = time, y = obs_value)) +
    geom_line(color = "#1b9e77", size = 1.2) +
    geom_point(color = "#7570b3", size = 2, alpha = 0.7) +
    labs(
      title = paste("Time Series of Trade Union Density in", country, "(ILOdata)"),
      x = "Year", y = "Trade Union Density (in %)"
    ) +
    theme_minimal(base_size = 14) +
    tstheme
  plots_list[[paste("Trade_Union_Density", country)]] <- plot
}

# Loop for 'cbcr' data frame
country_focus <- unique(cbcr$ref_area)
for (country in country_focus) {
  cbcr_country <- cbcr %>%
    filter(ref_area == country)
  plot <- ggplot(cbcr_country, aes(x = time, y = obs_value)) +
    geom_line(color = "#2ca02c", size = 1.2) +
    geom_point(color = "#d62728", size = 2, alpha = 0.7) +
    labs(
      title = paste("Collective Bargaining Coverage Over Time in", country, "(ILOdata)"),
      x = "Year", y = "Collective Bargaining Coverage (in %)"
    ) +
    theme_minimal(base_size = 14) +
    tstheme
  plots_list[[paste("CB_Coverage", country)]] <- plot
}

# Loop for 'workplace_rights' data frame
country_focus <- unique(workplace_rights$ref_area)
for (country in country_focus) {
  workplace_rights_country <- workplace_rights %>%
    filter(ref_area == country)
  plot <- ggplot(workplace_rights_country, aes(x = time, y = obs_value)) +
    geom_line(color = "#1f78b4", size = 1.2) +
    geom_point(color = "#33a02c", size = 2, alpha = 0.7) +
    labs(
      title = paste("Compliance with International Labor Law Over Time in", country, "(ILOdata)"),
      x = "Year", y = "Compliance with International Labor Law (Rating)"
    ) +
    theme_minimal(base_size = 14) +
    tstheme +
    annotate("text",
      x = Inf, y = Inf, label = "Source: Your Source Here",
      hjust = 1.1, vjust = 2, size = 3.5, color = "grey50"
    )
  plots_list[[paste("Labor_Compliance", country)]] <- plot
}

# Convert the list of plots to a tibble
tsplots_tibble <- tibble::enframe(plots_list, name = "Plot_Type", value = "Plot")
tsplots_tibble


# Assuming your tibble is namedtsplots_tibble
# Display "Trade_Union_Density United States" plot
desired_plot_row <- tsplots_tibble %>%
  filter(Plot_Type == "Trade_Union_Density United States")
desired_plot <- desired_plot_row$Plot
print(desired_plot)
file_path <- ("~/Lab2/graphs/tudus.pdf")
ggsave(file_path, plot = desired_plot[[1]])

# Display "Labor_Compliance United States" plot
desired_plot_row <- tsplots_tibble %>%
  filter(Plot_Type == "Labor_Compliance United States")
desired_plot <- desired_plot_row$Plot
print(desired_plot)
file_path <- ("~/Lab2/graphs/lcus.pdf")
ggsave(file_path, plot = desired_plot[[1]])

# Display "CB_Coverage United States" plot
desired_plot_row <- tsplots_tibble %>%
  filter(Plot_Type == "CB_Coverage United States")
desired_plot <- desired_plot_row$Plot
print(desired_plot)
file_path <- ("~/Lab2/graphs/cbus.pdf")
ggsave(file_path, plot = desired_plot[[1]])
# Assuming your tibble is namedtsplots_tibble

# Display "Trade_Union_Density China" plot
desired_plot_row <- tsplots_tibble %>%
  filter(Plot_Type == "Trade_Union_Density China")
desired_plot <- desired_plot_row$Plot
print(desired_plot)
file_path <- ("~/Lab2/graphs/tudchina.pdf")
ggsave(file_path, plot = desired_plot[[1]])

# Display "Labor_Compliance China" plot
desired_plot_row <- tsplots_tibble %>%
  filter(Plot_Type == "Labor_Compliance China")
desired_plot <- desired_plot_row$Plot
print(desired_plot)
file_path <- ("~/Lab2/graphs/lcchina.pdf")
ggsave(file_path, plot = desired_plot[[1]])

# Display "CB_Coverage China" plot
desired_plot_row <- tsplots_tibble %>%
  filter(Plot_Type == "CB_Coverage China")
desired_plot <- desired_plot_row$Plot
print(desired_plot)
file_path <- ("~/Lab2/graphs/cbchina.pdf")
ggsave(file_path, plot = desired_plot[[1]])
