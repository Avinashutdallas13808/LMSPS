#' Logistic Regression Analysis of Voter Turnout
#'
#' This function performs logistic regression to analyze the impact of various factors on voter turnout.
#' It returns the model summary, including coefficients and significance levels.
#'
#' @param data A cleaned data frame containing voter data with a binary 'turnout' column
#' @param predictors A character vector specifying the predictor variables to include in the model
#' @return A summary of the logistic regression model
#' @export

logistic_regression_voter_turnout <- function(data, predictors) {
  # Check if the required columns exist in the data
  if (!all(c("turnout", predictors) %in% colnames(data))) {
    stop("The data must contain 'turnout' and specified predictor columns.")
  }
  
  # Create a formula for the logistic regression
  formula <- as.formula(paste("turnout ~", paste(predictors, collapse = " + ")))
  
  # Fit the logistic regression model
  model <- glm(formula, data = data, family = binomial)
  
  # Return the model summary
  return(summary(model))
}
