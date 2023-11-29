#below is the data exploration & data modelings

joined_data2017 %>%
  select(National_Compliance_wth_Labour_Rights, `Collective Bargaining Coverage`) %>%
  cor(use = "complete.obs") # Handling missing values by excluding them

ggplot(joined_data2017, aes(x = National_Compliance_wth_Labour_Rights, y = `Collective Bargaining Coverage`)) +
  geom_point() +
  theme_minimal() +
  labs(x = "National_Compliance_wth_Labour_Rights", y = "Collective Bargaining Coverage", title = "Scatter Plot between National_Compliance_wth_Labour_Rights and Collective Bargaining Coverage")

model <- lm(`Collective Bargaining Coverage` ~ National_Compliance_wth_Labour_Rights, data = joined_data2017)
summary(model)

####full joined data exploration

joined_full_data_set %>%
  select(National_Compliance_wth_Labour_Rights, `Collective Bargaining Coverage`) %>%
  cor(use = "complete.obs")

ggplot(joined_full_data_set, aes(x = National_Compliance_wth_Labour_Rights, y = `Collective Bargaining Coverage`)) +
  geom_point() +
  theme_minimal() +
  labs(x = "National_Compliance_wth_Labour_Rights", y = "Collective Bargaining Coverage", title = "Scatter Plot between National_Compliance_wth_Labour_Rights and Collective Bargaining Coverage")

model2<- lm(`Collective Bargaining Coverage` ~ National_Compliance_wth_Labour_Rights, data = joined_full_data_set)
summary(model2)


## final analysis

joined_full_data_set %>%
  select(National_Compliance_wth_Labour_Rights, `Collective Bargaining Coverage`,`Union Density`) %>%
  cor(use = "complete.obs")

model3 <- lm(`Collective Bargaining Coverage` ~ National_Compliance_wth_Labour_Rights + `Union Density`, data = joined_full_data_set)

summary(model3)




ggpairs(joined_full_data_set, columns = c("National_Compliance_wth_Labour_Rights", "Collective Bargaining Coverage", "Union Density"),
        diag = list(continuous = "densityDiag"))



# Multiple regression model with National_Compliance_wth_Labour_Rights as the dependent variable
model_National_Compliance_wth_Labour_Rights <- lm(National_Compliance_wth_Labour_Rights ~ `Collective Bargaining Coverage` + `Union Density`, data = joined_full_data_set)

# Summary of the model
summary(model_National_Compliance_wth_Labour_Rights)
