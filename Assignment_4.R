# Assignment 4 - Martians Are Coming!
# Coding in R
# Alanna Olteanu

# Installing readr package and dplyr to RStudio
install.packages("readr")
install.packages("dplyr")

# Adding both packages to the library 
library(readr)
library(dplyr)

# Reading the CSV file 
ufo_subset <- read.csv("ufo_subset.csv")
View(ufo_subset)

# Imputing missing Shape information
ufo_modified <- ufo_subset %>%
  mutate(shape = ifelse(shape == "", "unknown", shape))
View(ufo_modified)

# Removing rows without Country information
ufo_modified <- ufo_modified %>%
  filter(!country == "")
View(ufo_modified)

# Converting datetime column to appropriate format 
ufo_modified <- ufo_modified %>%
  mutate(date_posted = as.POSIXct(format(as.Date(date_posted, format = "%d-%m-%Y"), "%Y-%m-%d")))
View(ufo_modified)

# Creating a new column "is_hoax" and initialize with FALSE
ufo_modified <- ufo_modified %>%
  mutate(is_hoax = FALSE)
# Defining keywords indicating possible hoax reports
hoax_keywords <- c("fake", "hoax", "prank", "fabricated", "fraud")
# Filtering the rows based on comment keywords and update "is_hoax" column
ufo_modified$is_hoax <- grepl(paste0("\\b(", paste(hoax_keywords, collapse = "|"), ")\\b"), ufo_modified$comments, ignore.case = TRUE)
View(ufo_modified)

# Calculating the percentage of hoax sightings per country
hoax_percentage <- with(ufo_modified, prop.table(table(country, is_hoax), margin = 1) * 100)
# Creating a table with country names and hoax sighting percentages
table_data <- data.frame(Country = rownames(hoax_percentage), Percentage = hoax_percentage[, "TRUE"])
# Sorting the table in descending order based on the percentage column
sorted_table <- table_data[order(-table_data$Percentage), ]
# Printing the table
print(sorted_table)

# Adding a new column "report_delay" and calculate the time difference in days
ufo_modified <- ufo_modified %>%
  mutate(report_delay = as.numeric(difftime(date_posted, datetime, units = "days")))
View(ufo_modified)

# Removing rows where the sighting was reported before it happened
ufo_modified <- ufo_modified %>%
  filter(report_delay >= 0)
View(ufo_modified)

# Calculating the average report delay per country 
average_report_delay <- ufo_modified %>%
  group_by(country) %>%
  summarize(Average_Report_Delay = mean(report_delay))
# Sorting the table by average report delay
average_report_delay <- average_report_delay[order(average_report_delay$Average_Report_Delay), ]
# Printing the table
print(average_report_delay)

# Checking the duration seconds column for errors 
# Checking for NA by using the sum function
missing_values <- sum(is.na(ufo_modified$duration.seconds))
missing_values
ufo_modified <- ufo_modified[!ufo_modified$duration.seconds == "", ]
# Checking format
data_format <- class(ufo_modified$duration.seconds)
ufo_modified$duration_seconds <- as.numeric(ufo_modified$duration.seconds)
ufo_modified$duration_seconds <- as.numeric(as.character(ufo_modified$duration.seconds))
data_format
# Checking range and removing numbers that are not within it
ufo_modified <- ufo_modified[ufo_modified$duration.seconds >= 10 & ufo_modified$duration.seconds <= 1000, ]
View(ufo_modified)

# Plotting histogram 
hist(ufo_modified$duration_seconds, xlab = "Duration (seconds)", ylab = "Frequency", main = "Histogram of Duration in Seconds")
