#' Analyze voter turnout
#'
#' @param data A data frame containing voter data
#' @return A summary of voter turnout
#' @export
#analyze_turnout <- function(data) {
# Your analysis logic here
#  summary(data$turnout)
#}


#' Analyze voter turnout with additional details
#'
#' This function analyzes voter turnout data with additional details. It provides summary statistics,
#'analyzes turnout by groups such as region or country, turnout by party, and turnout by age groups.
#'
#' @param data A cleaned data frame containing voter data, including a 'turnout' column
#' @return A summary of voter turnout statistics and turnout by categorical variables
#' @export

analyze_turnout <- function(data) {
  
  # Basic summary statistics for voter turnout
  turnout_summary <- summary(data$turnout)
  
  # Turnout by party (if the 'party' column exists)
  if ("party" %in% colnames(data)) {
    turnout_by_party <- aggregate(turnout ~ party, data, mean)
  }
  
  # Turnout by age group
  data$age_group <- cut(data$age, breaks = c(18, 30, 50, 70, 90), labels = c("18-30", "31-50", "51-70", "71-90"))
  turnout_by_age_group <- aggregate(turnout ~ age_group, data, mean)
  
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
    turnout_by_country = if (exists("turnout_by_country")) turnout_by_country else NULL,
    turnout_by_party = if (exists("turnout_by_party")) turnout_by_party else NULL,
    turnout_by_age_group = turnout_by_age_group
  ))
}