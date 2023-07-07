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
ufo_subset$shape <- ifelse(ufo_subset$shape == "", "unknown", ufo_subset$shape)
View(ufo_subset)

# Removing rows without Country information
ufo_subset <- ufo_subset[!ufo_subset$country == "", ]
View(ufo_subset)

# Converting datetime column to appropriate format 
ufo_subset$date_posted <- as.Date(ufo_subset$date_posted, format = "%d-%m-%Y")
# Converting the date format to "YYYY-MM-DD"
ufo_subset$date_posted <- format(ufo_subset$date_posted, "%Y-%m-%d")

# Converting date_posted column to Date format 
ufo_subset$date_posted <- as.POSIXct(ufo_subset$date_posted, format = "%Y-%m-%d")
View(ufo_subset)

# Creating a new column "is_hoax" and initialize with FALSE
ufo_subset$is_hoax <- FALSE

# Defining keywords indicating possible hoax reports
hoax_keywords <- c("fake", "hoax", "prank", "fabricated", "fraud")

# Filtering the rows based on comment keywords and update "is_hoax" column
ufo_subset$is_hoax <- grepl(paste0("\\b(", paste(hoax_keywords, collapse = "|"), ")\\b"), ufo_subset$comments, ignore.case = TRUE)
View(ufo_subset)

# Calculating the count of hoax and non-hoax sightings per country
hoax_count <- table(ufo_subset$country, ufo_subset$is_hoax)
# Calculating the percentage of hoax sightings per country
hoax_percentage <- prop.table(hoax_count, margin = 1) * 100
# Creating a data frame from the table
hoax_percentage_df <- as.data.frame(hoax_percentage)
# Renaming the columns
colnames(hoax_percentage_df) <- c("Country", "Hoax Validity", "Percentage")
# Printing the table
print(hoax_percentage_df)

# Adding a new column "report_delay" and calculate the time difference in days
ufo_subset$report_delay <- as.numeric(difftime(ufo_subset$date_posted, ufo_subset$datetime, units = "days"))
View(ufo_subset)

# Removing rows where the sighting was reported before it happened
ufo_subset <- ufo_subset[!(ufo_subset$report_delay < 0), ]
View(ufo_subset)

# Calculating the report delay in days
ufo_subset$report_delay <- as.numeric(difftime(ufo_subset$date_posted, ufo_subset$datetime, units = "days"))
# Calculating the average report delay per country using dplyr
average_report_delay <- ufo_subset %>%
  group_by(country) %>%
  summarize(Average_Report_Delay = mean(report_delay))
# Sorting the table by average report delay
average_report_delay <- average_report_delay[order(average_report_delay$Average_Report_Delay), ]
# Printing the table
print(average_report_delay)

# Checking the duration seconds column for errors 
# Checking for NA

# Checking format

# Checking range 

# Plotting histogram