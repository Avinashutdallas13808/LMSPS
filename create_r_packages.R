# Coding Assignment: Developing an R Package for Political Science Research

# Install and load required packages
if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
if (!requireNamespace("roxygen2", quietly = TRUE)) install.packages("roxygen2")
library(devtools)
library(roxygen2)

# Set seed for reproducibility
set.seed(1234)

# Example: Create a simple political science dataset
create_example_data <- function(n = 1000) {
  data.frame(
    voter_id = 1:n,
    age = sample(18:90, n, replace = TRUE),
    party = sample(c("Democrat", "Republican", "Independent"), n, replace = TRUE),
    turnout = sample(c(0, 1), n, replace = TRUE, prob = c(0.4, 0.6))
  )
}

example_data <- create_example_data()

# Save dataset
write.csv(example_data, "political_science_data.csv", row.names = FALSE)

# Student assignment starts here
# ------------------------------

# Your task is to develop an R package for political science research.
# Follow the steps below and complete the tasks.

# Step 1: Set up the package structure
# Use create_package() to create a new package named "poliscitools"

create_package("poliscitools")
setwd("poliscitools")

# Step 2: Create R files for your functions
# Use use_r() to create files for at least three functions

use_r("data_cleaning")
use_r("analysis")
use_r("visualization")

# Step 3: Implement functions
# Write at least three functions relevant to political science research
# Example (replace with your own functions):

#' Clean political science data
#'
#' @param data A data frame containing political science data
#' @return A cleaned data frame
#' @export

#' Clean political science data
#'
#' This function cleans a data frame containing political science data.
#' It handles missing values, fixes data types, removes duplicates,
#' and standardizes formats for consistency in the data.
#'
#' @param data A data frame containing political science data
#' @return A cleaned data frame
#' @export

clean_political_data <- function(data) {

  # Remove rows with missing values
  data <- na.omit(data)

  # Convert character columns that represent dates into date format
  if ("date" %in% colnames(data)) {
    data$date <- as.Date(data$date, format = "%Y-%m-%d")
  }

  # Convert character columns that represent categories into factors
  if ("region" %in% colnames(data)) {
    data$region <- as.factor(data$region)
  }

  # Remove duplicate rows
  data <- data[!duplicated(data), ]

  # Trim white space and convert character columns to lowercase
  data <- data %>%
    mutate(across(where(is.character), ~ trimws(tolower(.))))

  # Rename columns to snake_case for consistency
  colnames(data) <- tolower(gsub(" ", "_", colnames(data)))

  # Reorder columns to move 'country' and 'year' to the front (if they exist)
  if (all(c("country", "year") %in% colnames(data))) {
    data <- data[, c("country", "year", setdiff(colnames(data), c("country", "year")))]
  }

  # Return the cleaned data
  return(data)
}

#' Analyze voter turnout
#'
#' @param data A data frame containing voter data
#' @return A summary of voter turnout
#' @export
#analyze_turnout <- function(data) {
  # Your analysis logic here
#  summary(data$turnout)
#}


#' Analyze voter turnout
#'
#' This function analyzes voter turnout data. It provides summary statistics,
#' and optionally, analyzes turnout by groups such as region or country.
#'
#' @param data A cleaned data frame containing voter data, including a 'turnout' column
#' @return A summary of voter turnout statistics and turnout by categorical variables
#' @export

analyze_turnout <- function(data) {

  # Basic summary statistics for voter turnout
  turnout_summary <- summary(data$turnout)

  # Mean voter turnout by region (if the 'region' column exists)
  if ("region" %in% colnames(data)) {
    turnout_by_region <- aggregate(turnout ~ region, data, mean)
  }

  # Mean voter turnout by country (if the 'country' column exists)
  if ("country" %in% colnames(data)) {
    turnout_by_country <- aggregate(turnout ~ country, data, mean)
  }

  # Return a list containing all analyses
  return(list(
    turnout_summary = turnout_summary,
    turnout_by_region = if (exists("turnout_by_region")) turnout_by_region else NULL,
    turnout_by_country = if (exists("turnout_by_country")) turnout_by_country else NULL
  ))
}

#' Visualize party distribution
#'
#' @param data A data frame containing party affiliation data
#' @return A ggplot object
#' @export
#visualize_party_distribution <- function(data) {
  # Your visualization logic here
#  ggplot(data, aes(x = party)) + geom_bar()
#}

#' Visualize party distribution
#'
#' This function creates a bar plot to visualize the distribution of party affiliation in the data.
#'
#' @param data A data frame containing party affiliation data, including a 'party' column
#' @return A ggplot object showing the distribution of party affiliation
#' @export
visualize_party_distribution <- function(data) {

  # Check if the 'party' column exists
  if (!"party" %in% colnames(data)) {
    stop("The 'party' column is not found in the dataset.")
  }

  # Remove rows with missing party data
  data <- data[!is.na(data$party), ]

  # Create the bar plot
  plot <- ggplot(data, aes(x = fct_infreq(party))) +  # Order by frequency
    geom_bar(aes(fill = party), color = "black") +    # Add color and border
    labs(title = "Party Affiliation Distribution",    # Add title and axis labels
         x = "Political Party", y = "Count") +
    theme_minimal() +                                 # Use a minimal theme for clean visuals
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for readability
    geom_text(stat = "count", aes(label = scales::percent(..count../sum(..count..), accuracy = 1)),  # Add percentage labels
              vjust = -0.5, size = 3)

  # Return the plot
  return(plot)
}


# Step 4: Document your functions
# Use roxygen2 comments to document your functions

# Step 5: Add package dependencies
# Use use_package() to add any necessary dependencies

use_package("dplyr")
use_package("ggplot2")

# Step 6: Include the example dataset in your package
# Use use_data() to add the political science dataset to your package

use_data(example_data)

# Step 7: Create a vignette
# Use use_vignette() to create a vignette demonstrating your package

use_vignette("introduction", "Introduction to poliscitools")

# Step 8: Add unit tests
# Use use_testthat() to set up testing infrastructure and write tests for your functions

use_testthat()
use_test("data_cleaning")
use_test("analysis")
use_test("visualization")

# Step 9: Build and check your package
# Use build() and check() to ensure your package is properly constructed

build()
check()

# Step 10: Use Git for version control
# Initialize a Git repository and make commits as you develop your package

use_git()

# Additional tasks:
# - Update the DESCRIPTION file with accurate package information
# - Create a README.md file explaining your package
# - (Optional) Set up continuous integration using use_github_action()

# Questions to answer:
# 1. Update the functions and commit the changes to your github page. Specifically, adjust the plot function so that they contain color and the summary function to make it more comprehensive.
# 2. What challenges did you face in implementing and documenting your functions?
# 3. How does your package contribute to political science research?
# 4. What are some potential extensions or improvements for your package?
# 5. How did using version control (Git) affect your development process?

# Your answers and discussion here:


# PhD student task:
# Implement a more advanced statistical method relevant to political science (e.g., a specialized regression technique) as a function in your package.
# How does this addition enhance the package's utility for political science researchers?
