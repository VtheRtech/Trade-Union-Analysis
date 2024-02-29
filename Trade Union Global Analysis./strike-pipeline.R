library(tibble)
library(stringr)
library(tidyverse)
library(dplyr)


########################################################
### Test section
text <- "<strong>Employer</strong> : Condé Nast <br><strong>Labor Organization</strong> : The NewsGuild - CWA <br><strong>Local</strong> : Condé Nast Union - The NewsGuild of New York <br><strong>Industry</strong> : Information <br><strong>Number of Locations</strong> : 1 <br><strong>Address</strong> : 285 Fulton St <br><strong>City</strong> : New York <br><strong>State</strong> : New York <br><strong>Zip Code</strong> : 10007 <br><strong>Strike or Protest</strong> : Strike <br><strong>Approximate Number of Participants</strong> : 400 <br><strong>Start Date</strong> : 01/23/2024 <br><strong>End Date</strong> : 01/23/2024 <br><strong>Duration Amount</strong> : 1 <br><strong>Duration Unit</strong> : Days <br><strong>Authorized</strong> : Y <br><strong>Worker Demands</strong> : First contract, Job Security, End to anti-union retaliation <br><strong>Source</strong> : Source 1,Source 2 <br>"
clean_text <- str_remove_all(text, "<[^>]+>")
print(clean_text)
# Pre-processing to avoid splitting numbers from their labels
# This pattern looks for ": " followed by a number and replaces it with ":" directly followed by the number
preprocessed_text <- str_replace_all(clean_text, ": (\\d)", ":\\1")
# Now, split the preprocessed text by ": " to separate labels and values
split_text <- str_split(preprocessed_text, ": ", n = Inf)
# Convert list to vector for easier printing
full_text <- unlist(split_text)
print(full_text)
# Assuming full_text contains the text after splitting
full_text <- c(
  "Employer : Condé Nast",
  "Labor Organization : The NewsGuild - CWA",
  "Local : Condé Nast Union - The NewsGuild of New York",
  "Industry : Information", "Number of Locations : 1",
  "Address : 285 Fulton St", "City : New York",
  "State : New York",
  "Zip Code : 10007",
  "Strike or Protest : Strike",
  "Approximate Number of Participants : 400",
  "Start Date : 01/23/2024",
  "End Date : 01/23/2024",
  "Duration Amount : 1", "Duration Unit : Days",
  "Authorized : Y",
  "Worker Demands : First contract, Job Security, End to anti-union retaliation",
  "Source : Source 1,Source 2"
)
# Identifying elements that contain numbers
contains_numbers <- str_detect(full_text, "\\d")
# Extracting the numbers (and associated context if needed)
numbers_and_context <- full_text[contains_numbers]
# Extracting just the numbers
numbers_only <- str_extract(numbers_and_context, "\\d+")
print(numbers_and_context)
# Shows the elements containing numbers with their context


##########################################################
### please use strike-scheme-txtscheme.R file
zip_code_pattern <- "Zip Code : (\\d+)"
zip_codes_list <- str_extract(clean_text, zip_code_pattern)
print(zip_codes_list)
zip_codes <- unlist(zip_codes_list)
zip_codes <- gsub("Zip Code : ", "", zip_codes)
print(zip_codes)
length(zip_codes)


# Assuming clean_text is a vector of text strings
# Define the regular expression pattern to match zip codes directly
zip_code_pattern <- "Zip Code : (\\d+)"
# Use str_extract (not str_extract) to extract the first matchs
# str_extract returns NA for inputs with no match
zip_codes <- str_extract(clean_text, zip_code_pattern)
# The zip codes will include the "Zip Code : " prefix, so we remove it
# This gsub will also work fine with NA values (they'll be left unchanged)
zip_codes <- gsub("Zip Code : ", "", zip_codes)
# Print the extracted zip codes
print(zip_codes)

# If you need the length or the number of zip codes extracted, including NAs
length(zip_codes)

no_zip_codes <- grep("Zip Code : (\\d+)", clean_text, value = TRUE, invert = TRUE)
print(head(no_zip_codes)) # Print some entries that don't match the pattern



# strike extractions
# Define a pattern to extract the "Strike or Protest" value
strike_pattern <- "Strike or Protest : (\\w+)"
# Extract "Strike" or other values using the pattern
strike_values <- str_extract(clean_text, strike_pattern)
# Convert the extracted values to boolean: "Strike" -> TRUE, otherwise -> FALSE
# Assuming "Strike" indicates TRUE and any other value indicates FALSE
strike_boolean <- ifelse(strike_values == "Strike",
  TRUE, FALSE
)
# Print the boolean values
print(strike_values)
# Regular expression to match the last word in the string
pattern <- "\\w+$"
# Extract the last word
strikeorprotest <- str_extract(strike_values, pattern)
# Print the last word
print(strikeorprotest)

# Number of Participants Extraction
# Define the pattern to match
# "Approximate Number of Participants" followed by digits
participant_pattern <- "Approximate Number of Participants : (\\d+)"
# Assuming clean_text is a vector of
# strings that might contain the participant information
# This extracts the numeric part directly
numeric_values <- str_extract(clean_text, participant_pattern)
# Print the numeric values
print(numeric_values)
# Regular expression to match a sequence of digits
pattern <- "\\d+"
# Extract the first number
first_number <- str_extract(numeric_values, pattern)
# Convert to numeric
nparticipants <- as.numeric(first_number)
# Print the result
print(nparticipants)


# number of locations
pattern1 <- "Number of Locations : (\\d+)"
string1 <- str_extract(clean_text, pattern1)
print(string1)
pattern2 <- "\\d+"
second_number <- str_extract(string1, pattern2)
number_of_locations <- as.numeric(second_number)
print(number_of_locations)


# Start Dates
pattern <- "Start Date : (\\d{2}/\\d{2}/\\d{4})"
dates_extracted <- str_extract(clean_text, pattern)
dates_flat <- unlist(dates_extracted)
pattern2 <- "\\d{2}/\\d{2}/\\d{4}"
dates_only <- str_extract(dates_flat, pattern2)
dates_true <- as.Date(dates_only, format = "%m/%d/%Y")
# Print the Date objects
print(dates_true)



# Start Dates
pattern <- "End Date : (\\d{2}/\\d{2}/\\d{4})"
dates_extracted <- str_extract(clean_text, pattern)
dates_flat <- unlist(dates_extracted)
pattern2 <- "\\d{2}/\\d{2}/\\d{4}"
dates_only <- str_extract(dates_flat, pattern2)
dates_false <- as.Date(dates_only, format = "%m/%d/%Y")
# Print the Date objects
print(dates_false)


# Define the regular expression pattern to capture
# text between "Employer :" and "Labor Organization"
pattern <- "Employer : (.*?) Labor Organization"
# Extract the information using the defined pattern
extracted_info <- str_extract(clean_text, pattern)
# Remove the "Employer :" and "Labor Organization"
# parts to get just the information in between
extracted_info <- gsub("Employer : | Labor Organization",
  "", extracted_info,
  perl = TRUE
)
# Print the extracted information
print(extracted_info)



pattern <- "Organization : (.*?) Local"
unionname <- str_extract(clean_text, pattern)
unionname <- gsub("Organization : | Local",
  "", unionname,
  perl = TRUE
)
print(unionname)



# state information
pattern <- "State : (.*?) Zip Code"
state_match <- str_match(clean_text, pattern)
state <- state_match[, 2]
state_trimmed <- str_trim(state)
print(state_trimmed)

strikesinunitedstates <- tibble(
  zip_codes = zip_codes,
  state = state_trimmed,
  union_name = unionname,
  Employer = extracted_info,
  strike_boolean = strikeorprotest,
  number_of_participants = nparticipants,
  number_of_locations = number_of_locations,
  start_date = dates_true,
  end_date = dates_false
)
head(strikesinunitedstates)
tail(strikesinunitedstates)
colnames(strikesinunitedstates)
length(strikesinunitedstates)
str(strikesinunitedstates)



# Assuming `strikesinunitedstates` is your strikesinunitedstates frame
strikesinunitedstates <- strikesinunitedstates %>%
  mutate(
    start_date = as.character(start_date),
    end_date = as.character(end_date)
  )

library(DBI)
library(RSQLite)
setwd("~/workbook")
# Connect to an SQLite database (this creates the database if it doesn't exist)
con <- dbConnect(RSQLite::SQLite(), dbname = "trade_union_data.db")
# Write the data to the database
dbWriteTable(con, "Strikes_United_States",
  strikesinunitedstates,
  overwrite = TRUE
)
# Disconnect from the database
dbDisconnect(con)
