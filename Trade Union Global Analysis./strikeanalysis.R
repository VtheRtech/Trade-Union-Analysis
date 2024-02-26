library(tidyverse)
library(dplyr)
library(DBI)
library(RSQLite)
library(ggplot2)
library(lubridate)

setwd("~/workbook")
# Connect to an SQLite database (this creates the database if it doesn't exist)
con <- dbConnect(RSQLite::SQLite(), dbname = "trade_union_data.db")
dbListTables(con)
laborstrikes <- (as_tibble(dbReadTable(con, "Strikes_United_States")))
strikingworkers <- as_tibble(dbGetQuery(
  con,
  "SELECT
  zip_codes,
  state,
  union_name,
  number_of_participants,
  start_date
FROM
Strikes_United_States
GROUP BY union_name;"
))
laboractiontracker <- (as_tibble(dbReadTable(con, "ILRLaborActionTracker")))
# Disconnect from the database
dbDisconnect(con)


laborstrikes$start_date <- as.Date(laborstrikes$start_date)
laborstrikes$end_date <- as.Date(laborstrikes$end_date)
laborstrikes <- laborstrikes %>%
  mutate(
    start_year = year(start_date),
    end_year = year(end_date)
  )


head(laborstrikes)
tail(laborstrikes)

str(laborstrikes)

# Assuming 'laborstrikes' has a 'strike_id' or some combination
# of columns that can uniquely identify each event
laborstrikes %>%
  distinct(strike_id, .keep_all = TRUE) %>% # Replace 'strike_id' with the actual unique identifier or combination of columns
  group_by(start_year) %>%
  summarise(total_participants = sum(number_of_participants, na.rm = TRUE))





view(laborstrikes)

view(unique_strikes_sum)

# grouping unique data together to try and avoid repeats
unique_strikes_sum <- laborstrikes %>%
  filter(!is.na(number_of_participants)) %>% # Ensure participant numbers are available
  group_by(union_name, Employer, start_date, state) %>%
  summarise(number_of_participants = sum(number_of_participants, na.rm = TRUE), .groups = "drop") %>%
  distinct() # Ensure uniqueness

# state omitied in this data
unique_strikes_f <- unique_strikes_sum %>%
  group_by(union_name, Employer, start_date, number_of_participants) %>%
  summarise(number_of_participants = sum(number_of_participants, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(number_of_participants)) %>%
  distinct() %>%
  view()



# Assuming unique_strikes_sum is your starting point and already defined
unique_strikes_f <- unique_strikes_sum %>%
  group_by(union_name, number_of_participants) %>%
  summarise(number_of_participants = sum(number_of_participants, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(number_of_participants)) %>%
  distinct()
# Now, calculate the sum of number_of_participants across the entire dataset
total_sum <- unique_strikes_f %>%
  summarise(total_participants_sum = sum(number_of_participants, na.rm = TRUE))
# Display the total sum
total_sum


# Labor Action Tracker True analysis

laboractiontracker$DurationAmount <- as.integer(laboractiontracker$DurationAmount)
laboractiontracker$BargainingUnitSize <- as.integer(laboractiontracker$BargainingUnitSize)

laboractiontracker <- laboractiontracker %>%
  mutate(
    year = year(Timestamp),
    month = month(Timestamp)
  )

colnames(laboractiontracker)

View(laboractiontracker$BargainingUnitSize)

laboractiontracker %>%
  filter(year != "2024") %>%
  group_by(year) %>%
  summarize(
    NumberofLocations = sum(NumberofLocations),
    sizeofunit = sum(BargainingUnitSize, na.rm = TRUE)
  )
