source("~/Lab2/Trade_Union_Global_Analysis/summary_analysis/Enviroment_Setup.R")

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
})

# connect to the database and pull out the sheet
setwd("~/workbook")
con <- dbConnect(RSQLite::SQLite(), "trade_union_data.db")
dbListTables(con)
#  [1] "CBCR"                                     "CollectiveBargaining"
#  [3] "ILRLaborActionTracker"                    "IRL&ScrappedDataComparision"
#  [5] "LAT-02.19.24"                             "LAT-03.04.24"
#  [7] "LAT-04.01.24"                             "State_Union_Coverage_Density_1977-2021"
#  [9] "State_Union_Membership_Density_1964-2021" "Strikes_United_States"
# [11] "TUDR"                                     "TradeUnionDensity"
# [13] "WorkplaceRights"                          "raw_strike_table"
# [15] "rawonlystrike_table"                      "state_union_coverage_density_long"
# [17] "state_union_membership_density_long"      "state_uniondc_join"
# [19] "strike_table"                             "summarytable_IRLvSData"
february <- as_tibble(dbReadTable(con, "LAT-02.19.24"))
dbDisconnect(con)

# mutate zip code to a character
february <- february %>%
  mutate(
    ZipCode = as.character(ZipCode),
    BargainingUnitSize = as.integer(BargainingUnitSize),
    ApproximateNumberofParticipants =
      as.integer(ApproximateNumberofParticipants),
    DurationAmount = as.integer(DurationAmount)
  )

# check the number of non NA and NA's in thsi column against the sheet
sum(!is.na(february$BargainingUnitSize))
sum(is.na(february$BargainingUnitSize))
# exmain the number of notes
sum(!is.na(february$Notes))


# Convert Timestamp column to POSIXct format
february$Timestamp <- as.POSIXct(february$Timestamp,
  format = "%m/%d/%Y %H:%M:%S"
)



february <- february %>%
  mutate(
    Date = format(Timestamp, "%Y-%m-%d"),
    month = format(Timestamp, "%B"),
    Year = format(Timestamp, "%Y"),
    BargainingUnitSize = parse_number(as.character(BargainingUnitSize)),
    ApproximateNumberofParticipants =
      parse_number(as.character(ApproximateNumberofParticipants))
  )

monthly_plot <- function(state_var, year_var) {
  if (length(year_var) == 1) {
    d <- february %>%
      filter(State == state_var, Year == year_var) %>%
      ggplot(aes(x = month)) +
      geom_bar() +
      labs(title = paste("Number of Strikes in", state_var)) +
      common_theme()
  } else {
    d <- february %>%
      filter(State == state_var, Year %in% year_var) %>%
      ggplot(aes(x = month)) +
      geom_bar() +
      labs(title = paste("Number of Strikes in", state_var, "Per Month")) +
      facet_wrap(~Year, scales = "free_x") +
      common_theme()
  }
  return(d)
}
years <- c("2021", "2022", "2023", "2024")

monthly_plot("District of Columbia", years)
monthly_plot("Maryland", years)
monthly_plot("Virginia", years)
