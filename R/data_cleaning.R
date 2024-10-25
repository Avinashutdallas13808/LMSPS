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