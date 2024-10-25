#' Visualize party distribution
#'
#' @param data A data frame containing party affiliation data
#' @return A ggplot object
#' @export
#visualize_party_distribution <- function(data) {
# Your visualization logic here
#  ggplot(data, aes(x = party)) + geom_bar()
#}

#' Visualize party distribution with custom colors
#'
#' This function creates a bar plot to visualize the distribution of party affiliation in the data,
#' with custom colors for each party.
#'
#' @param data A data frame containing party affiliation data, including a 'party' column
#' @return A ggplot object showing the distribution of party affiliation with colors
#' @export
visualize_party_distribution <- function(data) {
  
  # Check if the 'party' column exists
  if (!"party" %in% colnames(data)) {
    stop("The 'party' column is not found in the dataset.")
  }
  
  # Remove rows with missing party data
  data <- data[!is.na(data$party), ]
  
  # Create a color palette
  party_colors <- c("Democrat" = "#1F78B4", "Republican" = "#E31A1C", "Independent" = "#33A02C")
  
  # Create the bar plot
  plot <- ggplot(data, aes(x = fct_infreq(party))) +  # Order by frequency
    geom_bar(color = "black") +                       # Add border
    scale_fill_manual(values = party_colors) +        # Apply custom colors
    labs(title = "Party Affiliation Distribution",    # Add title and axis labels
         x = "Political Party", y = "Count") +
    theme_minimal() +                                 # Use a minimal theme for clean visuals
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for readability
    geom_text(stat = "count", aes(label = scales::percent(..count../sum(..count..), accuracy = 1)),  # Add percentage labels
              vjust = -0.5, size = 3)
  
  # Return the plot
  return(plot)
}

