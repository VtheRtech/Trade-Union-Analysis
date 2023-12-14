library(tidyverse)
library(RSQLite)
library(DBI)
library(ggplot2)
library(dplyr)
library(forcats)
library(GGally)
library(stringr)

setwd("~/workbook")

con <- dbConnect(RSQLite::SQLite(), "trade_union_data.db")

dbListTables(con)


tudr <- as_tibble(dbReadTable(con, "TUDR"))
cbcr <- as_tibble(dbReadTable(con, "CBCR"))
collective_bargaining <- as_tibble(dbReadTable(con, "CollectiveBargaining"))
trade_union_density <- as_tibble(dbReadTable(con, "TradeUnionDensity"))
workplace_rights <-
  as_tibble(dbReadTable(con, "WorkplaceRights"))

state_union_membership_density <-
  as_tibble(dbReadTable(con, "State_Union_Membership_Density_1964-2021"))

dbDisconnect(con)
# Summary statistics
collective_bargaining %>%
  group_by(Country) %>%
  summary()

cbcr %>%
  select(ref_area, time, obs_value) %>%
  group_by(ref_area) %>%
  summary()

tudr %>%
  select(ref_area, time, obs_value) %>%
  group_by(ref_area) %>%
  summary()

trade_union_density %>%
  select(Country, Time, Value) %>%
  group_by(Country) %>%
  summary()

workplace_rights %>%
  select(ref_area, time, obs_value) %>%
  group_by(ref_area) %>%
  summary()

collective_bargaining %>%
  mutate(Country = fct_reorder(Country, Country, .fun = length)) %>%
  ggplot(aes(x = Country)) +
  geom_bar() +
  coord_flip() # Optional: flip coordinates to make it horizontal


collective_bargaining %>%
  mutate(
    Year = factor(Year), # Convert Year to a factor
    Year = fct_infreq(Year)
  ) %>% # Reorder factor levels based on frequency
  ggplot(aes(x = Year)) +
  geom_bar() +
  coord_flip()


collective_bargaining %>%
  filter(Country == "United States") %>%
  count()
# counts how many times the "United State data point is found in a column it was found 21 times"

collective_bargaining %>%
  filter(Year == "2015") %>%
  count()

collective_bargaining %>%
  distinct(Country, .keep_all = TRUE) %>%
  count()

collective_bargaining %>%
  distinct(Year, .keep_all = TRUE) %>%
  count()

workplace_rights %>%
  filter(time == "2021", obs_value > 5.0) %>%
  ggplot(aes(x = ref_area, y = obs_value)) +
  geom_point() +
  coord_flip()

workplace_rights %>%
  filter(time == "2021", obs_value < 5.0) %>%
  ggplot(aes(x = ref_area, y = obs_value)) +
  geom_point() +
  coord_flip()

workplace_rights %>%
  filter(time == "2021", obs_value == 0) %>%
  view()

workplace_rights %>%
  filter(time == "2021", obs_value == 0) %>%
  count()
# 14 out of the 39 counties are in compliance with international labor law


# This data need to be joined with Collective Bargaining table left join on the
# mentioned table, as it contains a larger set of data than Collective
# Bargaining


# Count distinct values
count_CollectiveBargaining <- collective_bargaining %>%
  distinct(Country, .keep_all = TRUE) %>%
  count() %>%
  pull(n)
count_CBCR <- cbcr %>%
  distinct(ref_area, .keep_all = TRUE) %>%
  count() %>%
  pull(n)
count_TUDR <- tudr %>%
  distinct(ref_area, .keep_all = TRUE) %>%
  count() %>%
  pull(n)
count_TradeUnionDensity <- trade_union_density %>%
  distinct(Country, .keep_all = TRUE) %>%
  count() %>%
  pull(n)
count_WorkplaceRights <- workplace_rights %>%
  distinct(ref_area, .keep_all = TRUE) %>%
  count() %>%
  pull(n)

# Combine into a na2med vector
counts <- c(
  CollectiveBargaining = count_CollectiveBargaining,
  CBCR = count_CBCR,
  TUDR = count_TUDR,
  TradeUnionDensity = count_TradeUnionDensity,
  WorkplaceRights = count_WorkplaceRights
)

# Convert to a tibble
counts_tibble <- tibble(Dataset = names(counts), Countrycount = counts)


# below is the combined data set for year

YearCount_WorkplaceRights <- workplace_rights %>%
  distinct(time, .keep_all = TRUE) %>%
  count() %>%
  pull(n)

YearCount_TradeUnionDensity <- trade_union_density %>%
  distinct(Time, .keep_all = TRUE) %>%
  count() %>%
  pull(n)

YearCount_TUDR <- tudr %>%
  distinct(time, .keep_all = TRUE) %>%
  count() %>%
  pull(n)

YearCount_CBCR <- cbcr %>%
  distinct(time, .keep_all = TRUE) %>%
  count() %>%
  pull(n)

YearCount_CollectiveBargaining <- collective_bargaining %>%
  distinct(Year, .keep_all = TRUE) %>%
  count() %>%
  pull(n)

# Combine into a tibble
YearCounts_tibble <- tibble(
  Dataset = c(
    "WorkplaceRights", "TradeUnionDensity",
    "TUDR", "CBCR", "CollectiveBargaining"
  ),
  YearCount = c(
    YearCount_WorkplaceRights,
    YearCount_TradeUnionDensity,
    YearCount_TUDR,
    YearCount_CBCR,
    YearCount_CollectiveBargaining
  )
)

# Display the tibble
print(YearCounts_tibble)
print(counts_tibble)

combinedtibble <- bind_rows(
  counts_tibble,
  YearCounts_tibble
)

print(combinedtibble)

# the more comprehensive data sets from this data base is TUDR, CBCR, workplace
# rights. (unsurprising because these where sources directly from the ILO's
# website)



TUDR_2017 <- tudr %>%
  filter(time == 2017)

CBCR_2017 <- cbcr %>%
  filter(time == 2017)

WorkplaceRights_2017 <- workplace_rights %>%
  filter(time == 2017)


joined_data <- inner_join(WorkplaceRights_2017, TUDR_2017,
  by = c("ref_area", "time")
)

# View the structure of the combined data
str(joined_data)
view(joined_data)

joined_data2017 <- inner_join(joined_data, CBCR_2017,
  by = c("ref_area", "time")
)

view(joined_data2017)

joined_data2017 <- joined_data2017 %>%
  rename(
    "National_Compliance_wth_Labour_Rights" = obs_value.x,
    "Union Density" = obs_value.y,
    "Collective Bargaining Coverage" = obs_value
  )

joined_data2017 %>%
  select(
    National_Compliance_wth_Labour_Rights,
    `Collective Bargaining Coverage`
  )


# full joined data is below
joined_data4 <- inner_join(workplace_rights, tudr, by = c("ref_area", "time"))
joined_full_data_set <- inner_join(joined_data4, cbcr,
  by = c("ref_area", "time")
)

joined_full_data_set <- joined_full_data_set %>%
  rename(
    "National_Compliance_wth_Labour_Rights" = obs_value.x,
    "Union Density" = obs_value.y,
    "Collective Bargaining Coverage" = obs_value
  )


TradeUnionDensity <- trade_union_density %>%
  rename("Year" = Time)

OldData <- inner_join(TradeUnionDensity, collective_bargaining,
  by = c("Country", "Year")
)

colnames(OldData)


####                  this for state_union_membership_density       ###### #

# Assuming your tibble is named state_union_membership_density
state_union_membership_density <- state_union_membership_density %>%
  rename_with(~ str_replace(.x, "X.Mem", "19"), starts_with("X.Mem"))

# Assuming your tibble is named state_union_membership_density

# Get the current column names
col_names <- colnames(state_union_membership_density)

# Loop through each column name
for (i in seq_along(col_names)) {
  # Extract the year part of the column name and convert it to a numeric value
  year <- as.numeric(col_names[i])

  # Check if the year is less than 1960 and starts with 19
  if (!is.na(year) && year < 1960 && startsWith(col_names[i], "19")) {
    # Replace "19" with "20" in the year part
    new_year <- sub("19", "20", col_names[i])
    # Update the column name
    col_names[i] <- new_year
  }
}

# Assign the new column names back to the tibble
colnames(state_union_membership_density) <- col_names

state_union_membership_density %>%
  colnames()
