#intro and load ahp and libraries
rm(list = ls(all = TRUE)) 

library(openxlsx)
library(ahpsurvey)
library(readr)
library(tidyverse)
library(dplyr)
library(ggplot2)
install.packages("mc2d")
library(mc2d)

# Define the most likely weights
weights <- c(Rainfall = 0.21, Land_Use = 0.17, Soil = 0.14, Slope = 0.13, 
             Drainage_Density = 0.14, Aquifer_Recharge_Geology = 0.20, 
             Distance_to_Roads = 0.27, Distance_to_Wadis = 0.32, Population_Density = 0.41)

# Define the list of margins (10%, 20%, 30%, 40%, and 50%)
margins <- c(0.1, 0.2, 0.3, 0.4, 0.5)

# Create an empty list to store results for each margin level
all_pert_samples <- list()

# Loop through each margin and generate PERT samples
set.seed(123)  # Set seed for reproducibility

for (margin in margins) {
  # Create a list to store PERT samples for the current margin
  margin_samples <- list()
  
  # Generate PERT samples for each weight with the current margin
  for (name in names(weights)) {
    min_val <- weights[name] * (1 - margin)
    most_likely_val <- weights[name]
    max_val <- weights[name] * (1 + margin)
    
    # Generate PERT distribution samples
    pert_samples <- rpert(1000, min = min_val, mode = most_likely_val, max = max_val)
    
    # Store the results with the weight name and margin as identifiers
    margin_samples[[paste0(name, "_", margin * 100, "pct")]] <- pert_samples
  }
  
  # Add this margin's samples to the main list
  all_pert_samples[[paste0("Margin_", margin * 100, "pct")]] <- margin_samples
}

# Check structure to verify samples were generated correctly
str(all_pert_samples)
    
generate_pert <- function(weight, n = 1000, margin = 0.1) {
  min_val <- weight * (1 - margin)
  most_likely_val <- weight
  max_val <- weight * (1 + margin)
  rpert(n, min = min_val, mode = most_likely_val, max = max_val)
}

# Convert the list to a data frame for easier handling
# Create a combined data frame with margin levels as columns
pert_dataframes <- lapply(all_pert_samples, function(samples) as.data.frame(samples))
pert_combined_df <- do.call(cbind, pert_dataframes)

# Save the combined data frame to a CSV file
write.csv(pert_combined_df, "pert_samples_with_margins.csv", row.names = FALSE)

# Visualize PERT distributions for each weight at different margin levels
par(mfrow = c(3, 3))  # Adjust layout as needed

for (margin in margins) {
  for (name in names(weights)) {
    hist(all_pert_samples[[paste0("Margin_", margin * 100, "pct")]][[paste0(name, "_", margin * 100, "pct")]],
         breaks = 30, main = paste(name, "-", margin * 100, "% Variation"),
         xlab = "Weight", col = "lightblue", border = "white")
  }
}
par(mfrow = c(1, 1))  # Reset layout


# Save plots to a PDF file
pdf("pert_distributions.pdf", width = 10, height = 8)

# Plot histograms for each variable across all margins
par(mfrow = c(5, 2))  # Adjust layout for output to file

for (name in names(weights)) {
  for (margin in margins) {
    # Access the specific PERT data for the current variable and margin
    pert_data <- all_pert_samples[[paste0("Margin_", margin * 100, "pct")]][[paste0(name, "_", margin * 100, "pct")]]
    
    # Check if pert_data has a narrow range, then adjust xlim based on its min and max
    data_min <- min(pert_data, na.rm = TRUE)
    data_max <- max(pert_data, na.rm = TRUE)
    
    # Plot the histogram with adjusted xlim based on data range
    hist(pert_data, breaks = 30, main = paste(name, "-", margin * 100, "% Variation"),
         xlab = "Weight", col = "lightblue", border = "white", 
         ylab = "Frequency", xlim = c(data_min, data_max))
  }
}
# Close the PDF device
dev.off()