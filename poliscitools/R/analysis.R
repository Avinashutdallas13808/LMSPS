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