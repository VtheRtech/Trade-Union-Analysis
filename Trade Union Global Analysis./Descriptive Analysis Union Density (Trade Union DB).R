# Descriptive statistics
summary_stats <- TradeUnionDensity %>%
  summarise(Mean = mean(Value, na.rm = TRUE),
            Median = median(Value, na.rm = TRUE),
            SD = sd(Value, na.rm = TRUE))

print(summary_stats)

# Histogram of Union Density
ggplot(TradeUnionDensity, aes(x = Value)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  labs(title = "Histogram of Union Density",
       x = "Union Density",
       y = "Frequency") +
  theme_minimal()


#Union Density is right-skewed, it would suggest that most countries have a lower union
#density, but a few countries have a very high union density. The peak of the
#histogram would indicate the most common range of union density. If there are
#any gaps or isolated bars, they might indicate outliers or special cases.
#remember, a histogram is a tool for exploring your data. It provides a visual
#summary that can guide further analysis, but it's always important to consider
#other factors and context related to your data.





ggplot(WorkplaceRights, aes(x = obs_value)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  labs(title = "Histogram of Complainance with international law (ILO)",
       x = "Workplace rights Complainace with international law",
       y = "Frequency") +
  theme_minimal()



ggplot(CBCR, aes(x = obs_value)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  labs(title = "Histogram of Collective Bargaining Coverage",
       x = "CBCR",
       y = "Frequency") +
  theme_minimal()













