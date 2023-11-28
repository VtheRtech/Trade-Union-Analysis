library(tidyverse)
library(RSQLite)
library(DBI)
library(ggplot2)
library(dplyr)
library(forcats)


setwd("~/workbook")

con <- dbConnect(RSQLite::SQLite(),"trade_union_data.db")

TUDR <- as_tibble(dbReadTable(con,"TUDR"))
CBCR <- as_tibble(dbReadTable(con,"CBCR"))
CollectiveBargaining <- as_tibble(dbReadTable(con,"CollectiveBargaining"))
TradeUnionDensity <- as_tibble(dbReadTable(con,"TradeUnionDensity"))
WorkplaceRights <- as_tibble(dbReadTable(con,"WorkplaceRights"))

dbDisconnect(con)


CollectiveBargaining %>% 
  group_by(Country) %>% 
  summary()

CBCR %>% 
  select(ref_area,time,obs_value) %>% 
  group_by(ref_area) %>%
  summary()

TUDR %>% 
  select(ref_area,time,obs_value) %>% 
  group_by(ref_area) %>% 
  summary()

TradeUnionDensity %>% 
  select(Country,Time,Value) %>% 
  group_by(Country) %>% 
  summary()

WorkplaceRights %>%
  select(ref_area, time, obs_value) %>% 
  group_by(ref_area) %>% 
  summary()

library(ggplot2)
library(dplyr)


CollectiveBargaining %>%
  mutate(Country = fct_reorder(Country, Country, .fun = length)) %>%
  ggplot(aes(x = Country)) +
  geom_bar() +
  coord_flip()  # Optional: flip coordinates to make it horizontal

  
CollectiveBargaining %>%
    mutate(Year = factor(Year),  # Convert Year to a factor
           Year = fct_infreq(Year)) %>%  # Reorder factor levels based on frequency
    ggplot(aes(x = Year)) +
    geom_bar()+
  coord_flip()
  
  
CollectiveBargaining %>% 
  filter(Country == "United States") %>% 
  count()
# counts how many times the "United State data point is found in a column it was found 21 times"

CollectiveBargaining %>% 
  filter(Year == "2015") %>% 
  count()

CollectiveBargaining %>% 
  distinct(Country, .keep_all = TRUE) %>% 
  count()

CollectiveBargaining %>% 
  distinct(Year, .keep_all = TRUE) %>% 
  count()

WorkplaceRights %>% 
  filter(time == "2021",obs_value > 5.0) %>% 
  ggplot(aes(x = ref_area, y = obs_value))+
  geom_point()+
  coord_flip()

WorkplaceRights %>% 
  filter(time == "2021",obs_value < 5.0) %>% 
  ggplot(aes(x = ref_area, y = obs_value))+
  geom_point()+
  coord_flip()

WorkplaceRights %>% 
  filter(time == "2021",obs_value == 0) %>% 
  View()

WorkplaceRights %>% 
  filter(time == "2021",obs_value == 0) %>% 
  count()
#14 out of the 39 counties are in compliance with international labor law 


# This data need to be joined with Collective Bargaining table left join on the
# mentioned table, as it contains a larger set of data than Collective
# Bargaining


# Count distinct values
count_CollectiveBargaining <- CollectiveBargaining %>% distinct(Country, .keep_all = TRUE) %>% count() %>% pull(n)
count_CBCR <- CBCR %>% distinct(ref_area, .keep_all = TRUE) %>% count() %>% pull(n)
count_TUDR <- TUDR %>% distinct(ref_area, .keep_all = TRUE) %>% count() %>% pull(n)
count_TradeUnionDensity <- TradeUnionDensity %>% distinct(Country, .keep_all = TRUE) %>% count() %>% pull(n)
count_WorkplaceRights <- WorkplaceRights %>% distinct(ref_area, .keep_all = TRUE) %>% count() %>% pull(n)

# Combine into a named vector
counts <- c(CollectiveBargaining = count_CollectiveBargaining,
            CBCR = count_CBCR,
            TUDR = count_TUDR,
            TradeUnionDensity = count_TradeUnionDensity,
            WorkplaceRights = count_WorkplaceRights)

# Convert to a tibble
counts_tibble <- tibble(Dataset = names(counts), Countrycount = counts)


#below is the combined data set for year

YearCount_WorkplaceRights <- WorkplaceRights %>% 
  distinct(time, .keep_all = TRUE) %>% 
  count() %>% 
  pull(n)

YearCount_TradeUnionDensity <- TradeUnionDensity %>% 
  distinct(Time, .keep_all = TRUE) %>% 
  count() %>% 
  pull(n)

YearCount_TUDR <- TUDR %>% 
  distinct(time, .keep_all = TRUE) %>% 
  count() %>% 
  pull(n)

YearCount_CBCR <- CBCR %>% 
  distinct(time, .keep_all = TRUE) %>% 
  count() %>% 
  pull(n)

YearCount_CollectiveBargaining <- CollectiveBargaining %>% 
  distinct(Year, .keep_all = TRUE) %>% 
  count() %>% 
  pull(n)

# Combine into a tibble
YearCounts_tibble <- tibble(
  Dataset = c("WorkplaceRights", "TradeUnionDensity", "TUDR", "CBCR", "CollectiveBargaining"),
  YearCount = c(YearCount_WorkplaceRights, YearCount_TradeUnionDensity, YearCount_TUDR, YearCount_CBCR, YearCount_CollectiveBargaining)
)

# Display the tibble
print(YearCounts_tibble)
print(counts_tibble)

combinedtibble <- bind_rows(counts_tibble,YearCounts_tibble)

print(combinedtibble)

#the more comprehensive data sets from this data base is TUDR, CBCR, workplace
#rights. (unsurprising because these where sources directly from the ILO's
#website)



TUDR_2017 <- TUDR %>% 
  filter(time == 2017)

CBCR_2017 <- CBCR %>% 
  filter(time == 2017)

WorkplaceRights_2017 <- WorkplaceRights %>% 
  filter(time == 2017)


joined_data <- inner_join(WorkplaceRights_2017, TUDR_2017, by = c("ref_area", "time"))

# View the structure of the combined data
str(joined_data)
View(joined_data)

joined_data2017 <- inner_join(joined_data, CBCR_2017, by = c("ref_area","time"))

View(joined_data2017)


joined_data2017 <- joined_data2017 %>% 
  rename("National_Compliance_wth_Labour_Rights" = obs_value.x,
         "Union Density"  = obs_value.y,
         "Collective Bargaining Coverage"   = obs_value
           )

joined_data2017 %>% 
  select(National_Compliance_wth_Labour_Rights,`Collective Bargaining Coverage`)


#full joined data is below
joined_data4 <- inner_join(WorkplaceRights, TUDR, by = c("ref_area", "time"))
joined_full_data_set <- inner_join(joined_data4, CBCR, by = c("ref_area","time"))

joined_full_data_set <- joined_full_data_set %>% 
  rename("National_Compliance_wth_Labour_Rights" = obs_value.x,
         "Union Density" = obs_value.y,
         "Collective Bargaining Coverage"    = obs_value
  )

#below is the data exploration

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


library(GGally)

ggpairs(joined_full_data_set, columns = c("National_Compliance_wth_Labour_Rights", "Collective Bargaining Coverage", "Union Density"),
        diag = list(continuous = "densityDiag"))



# Multiple regression model with National_Compliance_wth_Labour_Rights as the dependent variable
model_National_Compliance_wth_Labour_Rights <- lm(National_Compliance_wth_Labour_Rights ~ `Collective Bargaining Coverage` + `Union Density`, data = joined_full_data_set)

# Summary of the model
summary(model_National_Compliance_wth_Labour_Rights)

### Time Series Analysis

# Select a country for the analysis, e.g., "United States"
country_focus <- "United States"

# Filter the data for the selected country
TUD_country <- TradeUnionDensity %>%
  filter(Country == country_focus)

# Plotting the trend of Union Density over time
ggplot(TUD_country, aes(x = Time, y = Value)) +
  geom_line() +
  labs(title = paste("Time Series of Union Density in", country_focus),
       x = "Time",
       y = "Union Density") +
  theme_minimal()

### Comparative Analysis 

# Ensure the year_focus variable is set to the desired year
year_focus <- 2017

# Filter the data for the selected year
CBC_year <- joined_full_data_set %>%
  filter(time == year_focus)

# Reordering the ref_area based on the Collective Bargaining Coverage
CBC_year <- CBC_year %>%
  mutate(ref_area = fct_reorder(ref_area, `Collective Bargaining Coverage`))

# Comparative plot
ggplot(CBC_year, aes(x = ref_area, y = `Collective Bargaining Coverage`)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = paste("Comparative Analysis of Collective Bargaining Coverage in", year_focus),
       x = "Country",
       y = "Collective Bargaining Coverage") +
  theme_minimal()


### Descriptive Analysis

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



#test case 12

