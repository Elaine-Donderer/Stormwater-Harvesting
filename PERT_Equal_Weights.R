# Load necessary packages and clear environment
rm(list = ls(all = TRUE)) 
library(openxlsx)
library(ahpsurvey)
library(readr)
library(tidyverse)
library(dplyr)
library(ggplot2)
install.packages("mc2d")
library(mc2d)

# Define equal weights for all variables
equal_weights <- rep(0.111, 9)  # Assuming there are 9 criteria
names(equal_weights) <- c("Rainfall", "Land_Use", "Soil", "Slope", 
                          "Drainage_Density", "Aquifer_Recharge_Geology", 
                          "Distance_to_Roads", "Distance_to_Wadis", "Population_Density")

# Define the list of margins (10%, 20%, 30%, 40%, and 50%)
margins <- c(0.1, 0.2, 0.3, 0.4, 0.5)

# Create an empty list to store results for each margin level
all_pert_samples_equal <- list()

# Loop through each margin and generate PERT samples with equal weights
set.seed(123)  # Set seed for reproducibility

for (margin in margins) {
  # Create a list to store PERT samples for the current margin
  margin_samples <- list()
  
  # Generate PERT samples for each equal weight with the current margin
  for (name in names(equal_weights)) {
    min_val <- equal_weights[name] * (1 - margin)
    most_likely_val <- equal_weights[name]
    max_val <- equal_weights[name] * (1 + margin)
    
    # Generate PERT distribution samples
    pert_samples <- rpert(1000, min = min_val, mode = most_likely_val, max = max_val)
    
    # Store the results with the weight name and margin as identifiers
    margin_samples[[paste0(name, "_", margin * 100, "pct")]] <- pert_samples
  }
  
  # Add this margin's samples to the main list
  all_pert_samples_equal[[paste0("Margin_", margin * 100, "pct")]] <- margin_samples
}

summary(pert_data)  # Provides min, max, and other summary stats


# Convert the list to a data frame for easier handling
# Create a combined data frame with margin levels as columns
pert_dataframes_equal <- lapply(all_pert_samples_equal, function(samples) as.data.frame(samples))
pert_combined_df_equal <- do.call(cbind, pert_dataframes_equal)

# Save the combined data frame to a CSV file
write.csv(pert_combined_df_equal, "equal_weight_pert_samples_with_margins.csv", row.names = FALSE)

# Save plots to a PDF file for equal weights
pdf("equal_weight_pert_distributions.pdf", width = 10, height = 8)

# Plot histograms for each variable across all margins
par(mfrow = c(5, 2))  # Adjust layout for output to file

for (name in names(equal_weights)) {
  for (margin in margins) {
    pert_data <- all_pert_samples_equal[[paste0("Margin_", margin * 100, "pct")]][[paste0(name, "_", margin * 100, "pct")]]
    hist(pert_data, breaks = 50, xlim = c(min(pert_data), max(pert_data)), 
         main = paste(name, "-", margin * 100, "% Variation (Equal Weight)"),
         xlab = "Weight", col = "lightblue", border = "white", ylab = "Frequency")
  }
}

for (name in names(equal_weights)) {
  for (margin in margins) {
    pert_data <- all_pert_samples_equal[[paste0("Margin_", margin * 100, "pct")]][[paste0(name, "_", margin * 100, "pct")]]
    plot(density(pert_data), main = paste(name, "-", margin * 100, "% Variation (Equal Weight)"),
         xlab = "Weight", col = "blue", ylab = "Density")
  }
}

# Close the PDF device
dev.off()