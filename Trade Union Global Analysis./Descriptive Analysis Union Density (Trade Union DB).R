# Descriptive statistics
summary_stats <- TradeUnionDensity %>%
  summarise(
    Mean = mean(Value, na.rm = TRUE),
    Median = median(Value, na.rm = TRUE),
    SD = sd(Value, na.rm = TRUE)
  )

print(summary_stats)

# Histogram of Union Density
HistogramUnionDensity <- ggplot(TradeUnionDensity, aes(x = Value)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  labs(
    title = "Histogram of Union Density",
    x = "Union Density",
    y = "Frequency"
  ) +
  theme_minimal()

print(HistogramUnionDensity)

# Union Density is right-skewed, it would suggest that most countries have a lower union
# density, but a few countries have a very high union density. The peak of the
# histogram would indicate the most common range of union density. If there are
# any gaps or isolated bars, they might indicate outliers or special cases.
# remember, a histogram is a tool for exploring your data. It provides a visual
# summary that can guide further analysis, but it's always important to consider
# other factors and context related to your data.




# Histogram workplace_rights
Histogramworkplace_rights <- ggplot(workplace_rights, aes(x = obs_value)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  labs(
    title = "Histogram of National Complianance with International Law (ILO)",
    x = "Workplace Rights Complianace with International Law",
    y = "Frequency"
  ) +
  theme_minimal()
print(Histogramworkplace_rights)

Histogramcbcr <- ggplot(cbcr, aes(x = obs_value)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  labs(
    title = "Histogram of National Collective Bargaining Coverage",
    x = "cbcr",
    y = "Frequency"
  ) +
  theme_minimal()

print(Histogramcbcr)


#### frequency tables to be added to to the Wiki
# Assuming TradeUnionDensity is your data frame and it
# has columns named 'Country' and 'Value'
US_UnionDensity <- TradeUnionDensity %>%
  filter(Country == "United States") %>%
  summarize(UnionDensityValue = mean(Value)) %>%
  .$UnionDensityValue # Extracting the numeric value

# Calculate the median of the Union Density
Median_UnionDensity <- median(TradeUnionDensity$Value)

# Create a data frame for the lines
lines_df <- data.frame(
  xintercept = c(US_UnionDensity, Median_UnionDensity),
  line_id = c("United States", "Median")
)

HistogramUnionDensity <- ggplot(TradeUnionDensity, aes(x = Value)) +
  geom_histogram(bins = 30, fill = "#3498DB", color = "#1F2E2E") +
  geom_vline(data = lines_df, aes(
    xintercept = xintercept,
    linetype = line_id, color = line_id
  ), size = 1) +
  scale_color_manual(name = "Line Type", values = c("United States" = "red", "Median" = "green")) +
  scale_linetype_manual(name = "Line Type", values = c("United States" = "dotted", "Median" = "dashed")) +
  labs(
    title = "Histogram of Union Density",
    x = "Union Density(in %)",
    y = "Frequency"
  ) +
  theme_minimal() +
  guides(color = guide_legend(override.aes = list(linetype = c("dashed", "dotted"))))

print(HistogramUnionDensity)




# Assuming cbcr is your data frame
US_cbcr <- cbcr %>%
  filter(ref_area == "United States") %>%
  summarize(cbcrValue = mean(obs_value)) %>%
  .$cbcrValue

# Calculate the median of Collective Bargaining Coverage
Median_cbcr <- median(cbcr$obs_value)

# Create a data frame for the lines
lines_df_cbcr <- data.frame(
  xintercept = c(US_cbcr, Median_cbcr),
  line_id = c("United States", "Median")
)

# Create histogram for Collective Bargaining Coverage
Histogramcbcr <- ggplot(cbcr, aes(x = obs_value)) +
  geom_histogram(bins = 30, fill = "#3498DB", color = "black") +
  geom_vline(data = lines_df_cbcr, aes(xintercept = xintercept, linetype = line_id, color = line_id), size = 1) +
  scale_color_manual(name = "Line Type", values = c("United States" = "red", "Median" = "green")) +
  scale_linetype_manual(name = "Line Type", values = c("United States" = "dotted", "Median" = "dashed")) +
  labs(
    title = "Histogram of National Collective Bargaining Coverage",
    x = "Collective Bargaining Coverage (in %)",
    y = "Frequency"
  ) +
  theme_minimal() +
  guides(color = guide_legend(override.aes = list(linetype = c("dashed", "dotted"))))

print(Histogramcbcr)


library(ggplot2)
library(dplyr)

# Assuming workplace_rights is your data frame
US_workplace_rights <- workplace_rights %>%
  filter(ref_area == "United States") %>%
  summarize(workplace_rightsValue = mean(obs_value)) %>%
  .$workplace_rightsValue

# Calculate the median of Workplace Rights
Median_workplace_rights <- median(workplace_rights$obs_value)

# Create a data frame for the lines
lines_df_wr <- data.frame(
  xintercept = c(US_workplace_rights, Median_workplace_rights),
  line_id = c("United States", "Median")
)

# Create histogram for Workplace Rights
Histogramworkplace_rights <- ggplot(workplace_rights, aes(x = obs_value)) +
  geom_histogram(bins = 30, fill = "#3498DB", color = "black") +
  geom_vline(data = lines_df_wr, aes(xintercept = xintercept, linetype = line_id, color = line_id), size = 1) +
  scale_color_manual(name = "Line Type", values = c("United States" = "red", "Median" = "green")) +
  scale_linetype_manual(name = "Line Type", values = c("United States" = "dotted", "Median" = "dashed")) +
  labs(
    title = "Histogram of National Compliance with International Law (ILO)",
    x = "Workplace Rights Compliance with International Law (Rating)",
    y = "Frequency"
  ) +
  theme_minimal() +
  guides(color = guide_legend(override.aes = list(linetype = c("dashed", "dotted"))))

print(Histogramworkplace_rights)
