library(tidyverse)
library(dplyr)
library(DBI)
library(RSQLite)
library(ggplot2)
library(lubridate)

setwd("~/workbook")
# Connect to an SQLite database (this creates the database if it doesn't exist)
con <- dbConnect(RSQLite::SQLite(), dbname = "trade_union_data.db")
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

# Disconnect from the database
dbDisconnect(con)


laborstrikes$start_date <- as.Date(laborstrikes$start_date)
laborstrikes$end_date <- as.Date(laborstrikes$end_date)
laborstrikes <- laborstrikes %>%
  mutate(start_year = year(start_date))



strikingworkers$start_date <- as.Date(strikingworkers$start_date)
strikingworkers$end_date <- as.Date(strikingworkers$end_date)
strikingworkers <- strikingworkers %>%
  mutate(start_year = year(start_date))

head(strikingworkers)
tail(strikingworkers)


strikingworkers %>%
  filter(start_date != 2024) %>%
  ggplot(aes(x = start_date, y = number_of_participants)) +
  geom_bar()


laborstrikes %>%
  filter(start_year != 2024) %>%
  ggplot(aes(x = start_year, y = number_of_participants)) +
  geom_col()

laborstrikes %>%
  filter(start_year != 2024) %>%
  ggplot(aes(x = start_year, y = number_of_locations)) +
  geom_col()


laborstrikes %>%
  group_by(state) %>%
  summarise(total_participants = sum(number_of_participants, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(state, total_participants), y = total_participants)) +
  geom_bar(stat = "identity") +
  coord_flip() + # This makes it easier to read the state names
  labs(
    title = "Total Number of Strike Participants by State",
    x = "State",
    y = "Number of Participants"
  ) +
  theme_minimal()



laborstrikes$strike_duration <- as.numeric(laborstrikes$end_date - laborstrikes$start_date)
laborstrikes %>%
  ggplot(aes(x = strike_duration)) +
  geom_histogram(bins = 30, fill = "blue", color = "white") +
  labs(
    title = "Distribution of Strike Durations",
    x = "Duration (Days)", y = "Frequency"
  ) +
  theme_minimal()



laborstrikes$start_year <- format(laborstrikes$start_date, "%Y")
laborstrikes %>%
  group_by(start_year) %>%
  summarise(number_of_strikes = n()) %>%
  ggplot(aes(x = start_year, y = number_of_strikes)) +
  geom_bar(stat = "identity", fill = "coral") +
  labs(
    title = "Number of Strikes Over Time",
    x = "Year", y = "Number of Strikes"
  ) +
  theme_minimal()

laborstrikes %>%
  group_by(state) %>%
  summarise(total_locations = sum(number_of_locations, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(state, total_locations), y = total_locations)) +
  geom_bar(stat = "identity", fill = "coral") +
  coord_flip() +
  labs(
    title = "Total Number of Locations Involved in Strikes by State",
    x = "State", y = "Number of Locations"
  ) +
  theme_minimal()


# Assuming your dataframe is named laborstrikes
laborstrikes %>%
  group_by(state) %>%
  summarise(total_participants = sum(number_of_participants, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(state, total_participants), y = total_participants)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = total_participants), position = position_dodge(width = 0.9), hjust = -0.1) +
  coord_flip() +
  labs(
    title = "Total Number of Strike Participants by State",
    x = "State", y = "Number of Participants"
  ) +
  theme_minimal()


laborstrikes %>%
  group_by(state) %>%
  summarise(total_locations = sum(number_of_locations, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(state, total_locations), y = total_locations)) +
  geom_bar(stat = "identity", fill = "coral") +
  geom_text(aes(label = total_locations), position = position_dodge(width = 0.9), hjust = -0.1) +
  coord_flip() +
  labs(
    title = "Total Number of Locations Involved in Strikes by State",
    x = "State", y = "Number of Locations"
  ) +
  theme_minimal()


# Assuming your dataframe is named laborstrikes
laborstrikes %>%
  group_by(state, start_year) %>%
  summarise(total_participants = sum(number_of_participants, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(state, total_participants), y = total_participants, fill = state)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = total_participants), position = position_dodge(width = 0.9), hjust = -0.1) +
  facet_wrap(~start_year, scales = "free_x") +
  coord_flip() +
  labs(
    title = "Total Number of Strike Participants by State and Year",
    x = "State",
    y = "Number of Participants"
  ) +
  theme_minimal() +
  theme(legend.position = "none")



laborstrikes %>%
  group_by(state, start_year) %>%
  summarise(total_locations = sum(number_of_locations, na.rm = TRUE)) %>%
  ggplot(aes(
    x = reorder(state, total_locations),
    y = total_locations, fill = state
  )) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = total_locations),
    position = position_dodge(width = 0.9), hjust = -0.1
  ) +
  facet_wrap(~start_year,
    scales = "free_x"
  ) +
  coord_flip() +
  labs(
    title = "Total Number of Locations Involved in Strikes by State and Year",
    x = "State", y = "Number of Locations"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


library(tidyverse)
library(slider)

laborstrikes <- laborstrikes %>%
  mutate(start_date = as.Date(start_date, format = "%Y-%m-%d"))

daily_summary <- laborstrikes %>%
  group_by(start_date) %>%
  summarise(
    total_locations = sum(number_of_locations, na.rm = TRUE),
    total_participants = sum(number_of_participants, na.rm = TRUE)
  )

daily_summary <- daily_summary %>%
  mutate(
    locations_ma = slide_dbl(total_locations,
      mean,
      .before = 6, .complete = TRUE
    ),
    participants_ma = slide_dbl(total_participants,
      mean,
      .before = 6, .complete = TRUE
    )
  )

ggplot(daily_summary, aes(x = start_date)) +
  geom_line(aes(y = total_locations), color = "gray", alpha = 0.5) +
  geom_line(aes(y = locations_ma), color = "blue") +
  labs(
    title = "Daily Total Locations with 7-Day Moving Average",
    x = "Date",
    y = "Total Locations"
  ) +
  theme_minimal()

ggplot(daily_summary, aes(x = start_date)) +
  geom_line(aes(y = total_participants), color = "gray", alpha = 0.5) +
  geom_line(aes(y = participants_ma), color = "red") +
  labs(
    title = "Daily Total Participants with 7-Day Moving Average",
    x = "Date",
    y = "Total Participants"
  ) +
  theme_minimal()
