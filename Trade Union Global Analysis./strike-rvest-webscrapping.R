library(dplyr)
library(stringr)
library(purrr)
library(rvest)
library(RSelenium)


# Start the RSelenium driver and client
rD <- rsDriver(browser = "firefox", port = as.integer(1234), chromever = NULL)
remDr <- rD[["client"]]
# Start the RSelenium driver without specifying a port
remDr$navigate("https://striketracker.ilr.cornell.edu/")
# Wait for the elements to be present, adjusting sleep time as needed
Sys.sleep(5) # Adjust this value based on actual page load times
# Find all elements with the class '.tab-content'
webElems <- remDr$findElements(using = "css selector", value = ".tab-content")
# Initialize an empty list to store the inner HTML from each 'tab-content'
innerHtmlList <- list()
# Iterate over each element found and extract its inner HTML
for (i in seq_along(webElems)) {
  # Extract the inner HTML of the current element
  innerHtml <- webElems[[i]]$getElementAttribute("innerHTML")[[1]]
  # Store the extracted HTML in the list
  innerHtmlList[[i]] <- innerHtml
}
# At this point, innerHtmlList
# contains the inner HTML of each '.tab-content' element
# You can now process or analyze the contents of this list as needed
# Example: Print the contents of innerHtmlList to the console
print(innerHtmlList)
# Close the Selenium driver and stop the server
remDr$close() # Close the browser session
rD[["server"]]$stop() # Stop the Selenium server
clean_text_list <- lapply(
  innerHtmlList,
  function(html) {
    str_remove_all(html, "<[^>]+>")
  }
)
# Now clean_text_list contains cleaned strings from each HTML content
# Print the cleaned text of each element
print(clean_text_list)



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
