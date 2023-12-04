library(tidyverse)
library(gghighlight)

# Common theme adjustments
common_theme <- function() {
  theme(
    plot.background = element_rect(fill = "black", color = NA),
    panel.background = element_rect(fill = "black", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    text = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    axis.title = element_text(color = "white"),
    strip.text = element_text(color = "white"),
    legend.text = element_text(color = "white"),
    legend.title = element_text(color = "white"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
}

# Filtering the dataset for the specified countries
selected_countries <- c("United States", "Germany", "Austria", "Sweden", "China", "Brazil", "Spain", "Japan", "Australia")
filtered_data <- joined_full_data_set %>%
  filter(ref_area %in% selected_countries)

# Create a custom color mapping with brighter colors and hex codes
custom_colors <- setNames(c("steelblue", "darkorange", "red", "limegreen", "deepskyblue", "gold", "purple", "#FF00FF", "pink"),
                          unique(filtered_data$ref_area))
custom_colors[order(names(custom_colors))] <- custom_colors

# Plotting Union Density over time for each country with custom brighter colors and visible data points
p1 <- filtered_data %>%
  ggplot(aes(x = time, y = `Union Density`, group = ref_area, color = ref_area)) +
  geom_line() +
  geom_point(size = 2, aes(color = ref_area)) +  # Add points
  gghighlight(use_direct_label = FALSE, unhighlighted_params = list(colour = alpha("grey50", 0.5))) +
  facet_wrap(~ ref_area, scales = "free_x") +
  theme_minimal(base_family = "Gudea") +
  common_theme() +
  labs(title = "Union Density Over Time", x = "Year", y = "Union Density", color = "Country")

p1

# Plotting Collective Bargaining Coverage over time for each country with custom brighter colors and visible data points
p2 <- filtered_data %>%
  ggplot(aes(x = time, y = `Collective Bargaining Coverage`, group = ref_area, color = ref_area)) +
  geom_line() +
  geom_point(size = 2, aes(color = ref_area)) +  # Add points
  gghighlight(use_direct_label = FALSE, unhighlighted_params = list(colour = alpha("grey50", 0.5))) +
  facet_wrap(~ ref_area, scales = "free_x") +
  theme_minimal(base_family = "Gudea") +
  common_theme() +
  labs(title = "Collective Bargaining Coverage Over Time", x = "Year", y = "Collective Bargaining Coverage", color = "Country")

p2
