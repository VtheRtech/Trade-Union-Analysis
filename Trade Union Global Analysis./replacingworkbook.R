# Assuming your tibble is namedtsplots_tibble
# Display "Trade_Union_Density China" plot
desired_plot_row <- tsplots_tibble %>%
  filter(Plot_Type == "Trade_Union_Density China")
desired_plot <- desired_plot_row$Plot
print(desired_plot)
file_path <- ("~/Lab2/graphs/tudchina.pdf")
ggsave(file_path, plot = desired_plot)

# Display "Labor_Compliance China" plot
desired_plot_row <- tsplots_tibble %>%
  filter(Plot_Type == "Labor_Compliance China")
desired_plot <- desired_plot_row$Plot
print(desired_plot)
file_path <- ("~/Lab2/graphs/lcchina.pdf")
ggsave(file_path, plot = desired_plot)

# Display "CB_Coverage China" plot
desired_plot_row <- tsplots_tibble %>%
  filter(Plot_Type == "CB_Coverage China")
desired_plot <- desired_plot_row$Plot
print(desired_plot)
file_path <- ("~/Lab2/graphs/cbchina.pdf")
ggsave(file_path, plot = desired_plot)
