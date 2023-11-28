library(ggplot2)
library(dplyr)
library(forcats)

## This code demostrates how to use for loops
### Collective Bargaining Coverage
# Looping through each year from 2010 to 2020
for (year_focus in 2015:2020) {
  # Filter the data for the selected year
  CBC_year <- joined_full_data_set %>%
    filter(time == year_focus)
  
  # Reordering the ref_area based on the Collective Bargaining Coverage
  CBC_year <- CBC_year %>%
    mutate(ref_area = fct_reorder(ref_area, `Collective Bargaining Coverage`))
  
  # Comparative plot
  plot <- ggplot(CBC_year, aes(x = ref_area, y = `Collective Bargaining Coverage`)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(title = paste("Comparative Analysis of Collective Bargaining Coverage in", year_focus),
         x = "Country",
         y = "Collective Bargaining Coverage") +
    theme_minimal()
  
  # Print the plot
  print(plot)
}



### Labor By Country Analysis.
# Looping through each year from 2010 to 2020
for (year_focus in 2015:2020) {
  # Filter the data for the selected year
  WorkplaceRight_year <- joined_full_data_set %>%
    filter(time == year_focus)
  
  # Reordering the ref_area based on the Collective Bargaining Coverage
  WorkplaceRight_year <- WorkplaceRight_year %>%
    mutate(ref_area = fct_reorder(ref_area, `National_Compliance_wth_Labour_Rights`))
  
  # Comparative plot
  plot <- ggplot(WorkplaceRight_year, aes(x = ref_area, y = `National_Compliance_wth_Labour_Rights`)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(title = paste("Comparative Analysis of Countries Complaince with international labor law", year_focus),
         x = "Country",
         y = "National_Compliance_wth_Labour_Rights") +
    theme_minimal()
  
  # Print the plot
  print(plot)
}



for (year_focus in 2015:2020) {
  # Filter the data for the selected year
  UnionDensity_year <- joined_full_data_set %>%
    filter(time == year_focus)
  
  # Reordering the ref_area based on the Collective Bargaining Coverage
  UnionDensity_year <- UnionDensity_year %>%
    mutate(ref_area = fct_reorder(ref_area, `Union Density`))
  
  # Comparative plot
  plot <- ggplot(UnionDensity_year, aes(x = ref_area, y = `Union Density`)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(title = paste("Comparative Analysis of Union Density in different countries", year_focus),
         x = "Country",
         y = "National_Compliance_wth_Labour_Rights") +
    theme_minimal()
  
  # Print the plot
  print(plot)
  
}



