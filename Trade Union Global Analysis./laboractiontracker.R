library(tidyverse)
library(dplyr)
library(DBI)
library(RSQLite)
library(ggplot2)
library(lubridate)
library(stringr)

setwd("~/workbook")
# Connect to an SQLite database (this creates the database if it doesn't exist)
con <- dbConnect(RSQLite::SQLite(), dbname = "trade_union_data.db")
dbListTables(con)
laboractiontracker <- (as_tibble(dbReadTable(con, "ILRLaborActionTracker")))
# Disconnect from the database
dbDisconnect(con)
# Labor Action Tracker True analysis

laboractiontracker$DurationAmount <- as.integer(laboractiontracker$DurationAmount)
laboractiontracker$BargainingUnitSize <- as.integer(laboractiontracker$BargainingUnitSize)
laboractiontracker$ApproximateNumberofParticipants  <- as.integer(laboractiontracker$ApproximateNumberofParticipants)
laboractiontracker <- laboractiontracker %>%
  mutate(
    year = year(Timestamp),
    month = month(Timestamp)
  )

head(laboractiontracker)
tail(laboractiontracker)
str(laboractiontracker)

colnames(laboractiontracker)

[1]"Timestamp""Employer"
[3]"LaborOrganization""Local"
[5]"Industry""BargainingUnitSize"
[7]"NumberofLocations""Address"
[9]"City""State"
[11]"ZipCode""LatitudeLongitude"
[13]"ApproximateNumberofParticipants""StartDate"
[15]"EndDate""DurationAmount"
[17]"DurationUnit""StrikeorProtest"
[19]"Authorized""WorkerDemands"
[21]"Source""Notes"
[23]"year""month"

laboractiontracker  %>%
  select(State) %>%
  filter(str_starts(State,"District"))

laboractiontracker %>%
filter(year != "2024") %>%
group_by(year) %>%
summarize(
NumberofLocations = sum(NumberofLocations),
Participants = sum(ApproximateNumberofParticipants, na.rm = TRUE))

laboractiontracker %>%
filter(year != "2024", State == "Maryland") %>%
group_by(year) %>%
summarize(
NumberofLocations = sum(NumberofLocations),
Participants = sum(ApproximateNumberofParticipants, na.rm = TRUE))

laboractiontracker %>%
filter(year != "2024",
       State == "District of Columbia") %>%
group_by(year) %>%
summarize(
NumberofLocations = sum(NumberofLocations),
Participants = sum(ApproximateNumberofParticipants, na.rm = TRUE))
