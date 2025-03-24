rm(list = ls(all = TRUE)) 
library(openxlsx)
library(ahpsurvey)
library(readr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(mc2d)
library(terra)
library(gstat)
library(raster)

# Load the raster files
drainage_density_layer <- raster("C:/Users/Elain/Downloads/Sensitivity_CD/Drain_CD.tif")
aquifer_recharge_layer <- raster("C:/Users/Elain/Downloads/Sensitivity_CD/Geo_CD.tif")
rainfall_layer <- raster("C:/Users/Elain/Downloads/Sensitivity_CD/Rain_CD.tif")
land_use_layer <- raster("C:/Users/Elain/Downloads/Sensitivity_CD/Land_CD.tif")
soil_layer <- raster("C:/Users/Elain/Downloads/Sensitivity_CD/Soil_CD.tif")
slope_layer <- raster("C:/Users/Elain/Downloads/Sensitivity_CD/Slope_CD.tif")
roads_layer <- raster("C:/Users/Elain/Downloads/Sensitivity_CD/Roads_CD.tif")
wadis_layer <- raster("C:/Users/Elain/Downloads/Sensitivity_CD/Wadis_CD.tif")
population_density_layer <- raster("C:/Users/Elain/Downloads/Sensitivity_CD/Pop_CD.tif")

# Check the class of the loaded object to ensure it's a valid raster
class(drainage_density_layer)
# Check summary statistics of the raster values
summary(drainage_density_layer)

# Extract all unique values from the raster to check the range
unique_values <- unique(values(aquifer_recharge_layer))
print(unique_values)

# Create a new mask that only keeps the cells with value 4
study_area_raster <- aquifer_recharge_layer
study_area_raster[study_area_raster == 0] <- NA  # Set 0 values to NA
plot(study_area_raster)  # Plot to confirm that the mask worked

# Convert study area (which has masked values) to terra's SpatRaster
study_area_terra <- rast(study_area_raster)

# List of all your layers in the RasterLayer format
layers <- list(aquifer_recharge_layer, roads_layer, wadis_layer, drainage_density_layer, 
               rainfall_layer, land_use_layer, soil_layer, slope_layer, population_density_layer)

# Names of the layers (to be used in plots or printouts)
layer_names <- c("Aquifer Recharge", "Roads", "Wadis", "Drainage Density", 
                 "Rainfall", "Land Use", "Soil", "Slope", "Population Density")

# Convert aquifer_recharge_layer to SpatRaster (reference layer)
aquifer_recharge_terra <- rast(aquifer_recharge_layer)

# Step 1: Align Wadis Layer Extent and Resolution to Aquifer Recharge Layer
wadis_aligned <- project(rast(wadis_layer), aquifer_recharge_terra, method = "bilinear")

# Step 2: Mask the Wadis Layer using the Study Area
masked_wadis <- mask(wadis_aligned, study_area_terra)

# Step 3: Plot to confirm the mask is applied correctly
plot(masked_wadis, main = "Wadis Layer (Aligned and Masked)")

# Update wadis_layer with the final masked version
wadis_layer <- masked_wadis

# Step 4: Repeat the same process for all layers to ensure extents and resolutions match
masked_layers <- list()
for (i in seq_along(layers)) {
  layer_terra <- rast(layers[[i]])  # Convert the layer to SpatRaster (terra)
  
  # Align the extent and resolution to the reference layer
  layer_aligned <- project(layer_terra, aquifer_recharge_terra, method = "bilinear")
  
  # Apply the mask using the study area
  masked_layer <- mask(layer_aligned, study_area_terra)
  
  # Store the masked layer in the list
  masked_layers[[i]] <- masked_layer
  
  # Plot each masked layer
  plot(masked_layer, main = paste(layer_names[i], "Masked"))
}

# Check the extent and resolution of the masked Wadis layer and other layers
print(ext(masked_wadis))
print(res(masked_wadis))

# Check if the extents and resolutions match the aquifer recharge layer
print(ext(aquifer_recharge_terra))
print(res(aquifer_recharge_terra))



for (i in 1:length(layers)) {
  na_count <- sum(is.na(values(layers[[i]])))
  print(paste("Original Layer", i, "has", na_count, "NA values"))
}

# Check original raster layers before any processing
for (j in 1:length(layers)) {
  print(paste("Original Layer", j))
  print(head(values(layers[[j]]), 10))  # Print the first 10 values of the original layers
}

# Check basic information for each layer
for (i in seq_along(layers)) {
  print(paste("Masked Layer", i, "has extent:"))
  print(ext(layers[[i]]))  # Print the extent of the masked layer
  print(paste("Resolution:", res(layers[[i]])))  # Print the resolution
}

# Create a raster stack from the list of masked layers
suitability_stack <- c(masked_layers[[1]], masked_layers[[2]], masked_layers[[3]],
                       masked_layers[[4]], masked_layers[[5]], masked_layers[[6]], 
                       masked_layers[[7]], masked_layers[[8]], masked_layers[[9]])

# Print the stack to check
print(suitability_stack)
plot(suitability_stack)

# Step 1: Create a comprehensive valid mask across all layers, excluding both NA and 0 values
valid_mask <- !is.na(values(masked_layers[[1]])) & (values(masked_layers[[1]]) != 0)  # Start with layer 1's valid mask
for (j in 2:9) {
  layer_values <- values(masked_layers[[j]])
  valid_mask <- valid_mask & !is.na(layer_values) & (layer_values != 0)  # Refine mask by excluding NA and 0 values
}

# Step 2: Get the total number of valid cells
n_valid_cells <- sum(valid_mask)
print(paste("Total valid cells across all layers:", n_valid_cells))

# Check the valid mask
valid_mask <- !is.na(values(masked_layers[[1]])) & (values(masked_layers[[1]]) != 0)  # Start with layer 1's valid mask
for (j in 2:9) {
  layer_values <- values(masked_layers[[j]])
  valid_mask <- valid_mask & !is.na(layer_values) & (layer_values != 0)  # Refine mask by excluding NA and 0 values
}

# Extract valid values from all layers
valid_values_list <- lapply(masked_layers, function(layer) values(layer)[valid_mask])


#upload PERT: # Step 1: Read the CSV file into R as a dataframe
pert_expert <- read.csv("C:/Users/Elain/OneDrive/Documents/Sensitivity_Thesis/pert_samples_with_margins.csv")
pert_equal <- read.csv("C:/Users/Elain/OneDrive/Documents/Sensitivity_Thesis/equal_weight_pert_samples_with_margins.csv")

# Step 2: Convert the dataframe into a numeric matrix
pert_expert_new <- as.matrix(pert_expert)
pert_equal_new <- as.matrix(pert_equal)

# Step 3: Check that the matrix is numeric and display the first few rows
print("PERT weight matrix (with 10% margin):")
print(head(pert_expert_new))
print("PERT weight matrix (with 10% margin):")
print(head(pert_equal_new))

# Step 4: Ensure all elements are numeric
print(paste("Matrix is numeric:", is.numeric(pert_expert_new)))
print(paste("Matrix is numeric:", is.numeric(pert_equal_new)))

# Monte Carlo simulation parameters
iterations <- 100
n_cells <- ncell(suitability_stack[[1]])  # Number of cells in each layer

# Initialize the matrix to store results
suitability_results_expert <- matrix(NA, nrow=n_valid_cells, ncol=iterations)
suitability_results_equal <- matrix(NA, nrow=n_valid_cells, ncol=iterations)

#monte-carlo
# Use the new pert_expert_new matrix for the Monte Carlo simulation
for (i in 1:iterations) {
  # Initialize weighted sum for valid cells
  weighted_sum_expert <- numeric(n_valid_cells)
  
  for (j in 1:9) {  # Loop through each layer
    layer_valid_values <- valid_values_list[[j]]  # Use only valid values for each layer
    pert_weight_expert <- pert_expert_new[i, j]   # Get PERT-sampled weight from the new matrix
    
    # Debugging: Print the PERT weight for this iteration and layer
    print(paste("PERT weight for Iteration", i, "Layer", j, ":", pert_weight_expert))
    
    # Calculate the weighted sum for valid cells
    weighted_sum_expert <- weighted_sum_expert + (layer_valid_values * pert_weight_expert)
    
    # Debugging: Check the weighted sum after this layer
    print(paste("Iteration", i, "Layer", j, "sum of weighted values:", sum(weighted_sum_expert)))
  }
  
  # Final debugging step: Check the weighted sum before storing the result
  print(paste("Iteration", i, "Total weighted sum:", sum(weighted_sum_expert)))
  
  # Store the result for this iteration
  suitability_results_expert[, i] <- weighted_sum_expert
  
  # Debugging: Check the result stored in the matrix
  print(paste("Result stored in suitability_results_expert for Iteration", i, ":", sum(suitability_results_expert[, i])))
}

# Use the new pert_equal_new matrix for the Monte Carlo simulation
for (i in 1:iterations) {
  # Initialize weighted sum for valid cells
  weighted_sum_equal <- numeric(n_valid_cells)
  
  for (j in 1:9) {  # Loop through each layer
    layer_valid_values <- valid_values_list[[j]]  # Use only valid values for each layer
    pert_weight_equal <- pert_equal_new[i, j]   # Get PERT-sampled weight from the new matrix
    
    # Debugging: Print the PERT weight for this iteration and layer
    print(paste("PERT weight for Iteration", i, "Layer", j, ":", pert_weight_equal))
    
    # Calculate the weighted sum for valid cells
    weighted_sum_equal <- weighted_sum_equal + (layer_valid_values * pert_weight_equal)
    
    # Debugging: Check the weighted sum after this layer
    print(paste("Iteration", i, "Layer", j, "sum of weighted values:", sum(weighted_sum_equal)))
  }
  
  # Final debugging step: Check the weighted sum before storing the result
  print(paste("Iteration", i, "Total weighted sum:", sum(weighted_sum_equal)))
  
  # Store the result for this iteration
  suitability_results_equal[, i] <- weighted_sum_equal
  
  # Debugging: Check the result stored in the matrix
  print(paste("Result stored in suitability_results_equal for Iteration", i, ":", sum(suitability_results_equal[, i])))
}


# Calculate the standard deviation (sensitivity) for each pixel
sensitivity_expert <- apply(suitability_results_expert, 1, sd)
sensitivity_equal <- apply(suitability_results_equal, 1, sd)

# Step 1: Get the total number of cells in the original raster (including excluded cells)
total_cells_in_raster <- ncell(suitability_stack[[1]])
print(total_cells_in_raster)
# Step 2: Create a full-length vector of NA values (to match the full grid)
full_sensitivity_expert <- rep(NA, total_cells_in_raster)
full_sensitivity_equal <- rep(NA, total_cells_in_raster)

# Step 3: Insert the valid sensitivity values back into the correct positions using valid_mask
# valid_mask identifies the valid cells that were used in the simulation
full_sensitivity_expert[valid_mask] <- sensitivity_expert
full_sensitivity_equal[valid_mask] <- sensitivity_equal

#Step 4: Reshape the full-length sensitivity vectors back into raster-compatible matrices
sensitivity_expert_matrix <- matrix(full_sensitivity_expert, 
                                    nrow = nrow(suitability_stack[[1]]), 
                                    ncol = ncol(suitability_stack[[1]]), 
                                    byrow = TRUE)

sensitivity_equal_matrix <- matrix(full_sensitivity_equal, 
                                   nrow = nrow(suitability_stack[[1]]), 
                                   ncol = ncol(suitability_stack[[1]]), 
                                   byrow = TRUE)

# Step 5: Convert the matrices back into rasters, using the same extent as the original stack
sensitivity_raster_expert <- rast(sensitivity_expert_matrix, ext = ext(suitability_stack[[1]]))
sensitivity_raster_equal <- rast(sensitivity_equal_matrix, ext = ext(suitability_stack[[1]]))
# Adjust the margins and set title size

# Save the plot as a larger image
png("sensitivity_expert_100_CD.png", width = 1200, height = 1000, res = 150)  # Set width, height, and resolution
par(mar = c(5, 5, 4, 2))  # Adjust the margins
plot(sensitivity_raster_expert, 
     main = "Sensitivity Map for CD (Expert Weights)", 
     cex.main = 2)  # Larger title size for better visibility
dev.off()  # Close the PNG device

# Save the plot as a larger image
png("sensitivity_equal_100_CD.png", width = 1200, height = 1000, res = 150)  # Set width, height, and resolution
par(mar = c(5, 5, 4, 2))  # Adjust the margins
plot(sensitivity_raster_equal, 
     main = "Sensitivity Map for CD (Equal Weights)", 
     cex.main = 2)  # Larger title size for better visibility
dev.off()  # Close the PNG device

# Optional: Save the rasters to files if needed
writeRaster(sensitivity_raster_expert, "sensitivity_expert_CD_100.tif", overwrite = TRUE)
writeRaster(sensitivity_raster_equal, "sensitivity_equal_CD_100.tif", overwrite = TRUE)

# Summary statistics for sensitivity maps
# For expert-weighted sensitivity
mean_sensitivity_expert <- mean(full_sensitivity_expert, na.rm = TRUE)
sd_sensitivity_expert <- sd(full_sensitivity_expert, na.rm = TRUE)

# For equal-weighted sensitivity
mean_sensitivity_equal <- mean(full_sensitivity_equal, na.rm = TRUE)
sd_sensitivity_equal <- sd(full_sensitivity_equal, na.rm = TRUE)

# Print summary statistics
cat("Expert-Weighted Sensitivity Map:\n")
cat("Mean Sensitivity:", mean_sensitivity_expert, "\n")
cat("Standard Deviation of Sensitivity:", sd_sensitivity_expert, "\n\n")

cat("Equal-Weighted Sensitivity Map:\n")
cat("Mean Sensitivity:", mean_sensitivity_equal, "\n")
cat("Standard Deviation of Sensitivity:", sd_sensitivity_equal, "\n")

# Assume n is the number of valid cells (from your mask)
n <- sum(valid_mask)  # Total number of valid cells

# Z-value for 95% confidence interval
z_value <- 1.96

# Calculate 95% confidence intervals for expert-weighted map
ci_expert_lower <- mean_sensitivity_expert - z_value * (sd_sensitivity_expert / sqrt(n))
ci_expert_upper <- mean_sensitivity_expert + z_value * (sd_sensitivity_expert / sqrt(n))

# Calculate 95% confidence intervals for equal-weighted map
ci_equal_lower <- mean_sensitivity_equal - z_value * (sd_sensitivity_equal / sqrt(n))
ci_equal_upper <- mean_sensitivity_equal + z_value * (sd_sensitivity_equal / sqrt(n))

# Print confidence intervals to check their size
cat("Expert-Weighted Sensitivity CI:", ci_expert_lower, "-", ci_expert_upper, "\n")
cat("Equal-Weighted Sensitivity CI:", ci_equal_lower, "-", ci_equal_upper, "\n")

# Create a data frame for visualization
sensitivity_stats_sd <- data.frame(
  Weighting = c("Equal", "Expert"),
  Mean = c(mean_sensitivity_equal, mean_sensitivity_expert),
  SD_Lower = c(mean_sensitivity_equal - sd_sensitivity_equal, mean_sensitivity_expert - sd_sensitivity_expert),
  SD_Upper = c(mean_sensitivity_equal + sd_sensitivity_equal, mean_sensitivity_expert + sd_sensitivity_expert)
)

# Load ggplot2 for visualization
library(ggplot2)

# Plot the means with error bars (confidence intervals)
# Plot with standard deviations and increased font size for labels
ggplot(sensitivity_stats_sd, aes(x = Weighting, y = Mean)) +
  geom_bar(stat = "identity", fill = "lightblue", width = 0.6) +
  geom_errorbar(aes(ymin = SD_Lower, ymax = SD_Upper), width = 0.2, color = "black") +
  labs(
    title = "Mean Sensitivity with Standard Deviations",
    y = "Mean Sensitivity",
    x = "Weighting Method"
  ) +
  ylim(0, max(sensitivity_stats_sd$SD_Upper) + 0.01) +  # Adjusting y-axis range for better visibility
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),   # Title font size
    axis.title.x = element_text(size = 14),                # X-axis label font size
    axis.title.y = element_text(size = 14),                # Y-axis label font size
    axis.text.x = element_text(size = 12),                 # X-axis tick labels font size
    axis.text.y = element_text(size = 12)                  # Y-axis tick labels font size
  )
# Number of layers and iterations
# Order based on your initial setup
suitability_matrices <- list(
  aquifer_recharge_layer,  # 1. Aquifer Recharge
  roads_layer,             # 2. Roads
  wadis_layer,             # 3. Wadis
  drainage_density_layer,  # 4. Drainage Density
  rainfall_layer,          # 5. Rainfall
  land_use_layer,          # 6. Land Use
  soil_layer,              # 7. Soil
  slope_layer,             # 8. Slope
  population_density_layer # 9. Population Density
)

# Ensure layer_names aligns with the order in suitability_matrices for consistency in analysis and labeling
layer_names <- c("Aquifer Recharge", "Roads", "Wadis", 
                 "Drainage Density", "Rainfall", "Land Use", 
                 "Soil", "Slope", "Population Density")

num_layers <- length(suitability_matrices)
iterations <- 100

# Initialize list to store sensitivity scores for each layer
sensitivity_by_layer <- list()

# Begin layer-wise sensitivity analysis
for (j in 1:num_layers) {
  cat("Starting sensitivity analysis for layer", j, "\n")
  
  # Initialize a matrix to store suitability results for each layer's sensitivity test
  suitability_results_layer <- matrix(0, nrow = ncell(suitability_matrices[[1]]), ncol = iterations)
  
  for (i in 1:iterations) {
    cat("  Iteration", i, "for layer", j, "\n")
    
    # Set weights to mean values initially
    weights <- apply(pert_expert, 2, mean)
    
    # Vary only the weight for layer 'j' using PERT sample
    weights[j] <- pert_expert[i, j]
    
    # Multiply each layer by its weight and calculate suitability for each cell
    suitability_calculation <- sapply(seq_along(suitability_matrices), function(k) {
      values(suitability_matrices[[k]]) * weights[k]
    })
    
    # Sum suitability values across all layers for each cell
    suitability_results_layer[, i] <- rowSums(suitability_calculation)
  }
  
  # Calculate standard deviation across iterations for this layer's sensitivity
  sensitivity_score <- apply(suitability_results_layer, 1, sd, na.rm = TRUE)
  
  # Store the sensitivity score for this layer in the list
  sensitivity_by_layer[[j]] <- sensitivity_score
  
  cat("Completed sensitivity analysis for layer", j, "\n")
}

# Combine sensitivity scores into a data frame or matrix for comparison
sensitivity_df <- do.call(cbind, sensitivity_by_layer)
colnames(sensitivity_df) <- paste("Layer", 1:num_layers)

# Calculate mean sensitivity for each layer
mean_sensitivity_per_layer <- sapply(sensitivity_by_layer, mean, na.rm = TRUE)
sd_sensitivity_per_layer <- sapply(sensitivity_by_layer, sd, na.rm = TRUE)
print(sd_sensitivity_per_layer)


# Create separate data frames for Equal and Expert weights
sensitivity_equal <- data.frame(
  Layer = "Equal Weighted",
  Mean = mean_sensitivity_equal,
  SD = sd_sensitivity_equal,
  Type = "Equal-weighted suitability"
)

sensitivity_expert <- data.frame(
  Layer = "Expert Weighted",
  Mean = mean_sensitivity_expert,
  SD = sd_sensitivity_expert,
  Type = "AHP-weighted suitability"
)

# Combine the individual layer sensitivities with the overall sensitivity scores
sensitivity_individual <- data.frame(
  Layer = layer_names,  # Names of individual layers
  Mean = mean_sensitivity_per_layer,  # Mean sensitivity per layer
  SD = sd_sensitivity_per_layer,  # Standard deviation per layer
  Type = "AHP-weighted suitability"
)

# Combine all data into one data frame for plotting
combined_sensitivity_stats <- rbind(sensitivity_equal, sensitivity_expert, sensitivity_individual)

# Define a threshold for minimum SD to be visible in the plot
min_sd_threshold <- 0.001

# Apply threshold to SD values
combined_sensitivity_stats$Adjusted_SD <- ifelse(
  combined_sensitivity_stats$SD < min_sd_threshold, 
  min_sd_threshold, 
  combined_sensitivity_stats$SD
)

# Reorder Layer factor to the desired order
desired_order <- c("Expert Weighted", "Equal Weighted", 
                   "Aquifer Recharge", "Drainage Density", "Rainfall", 
                   "Land Use", "Soil", "Slope", "Roads", "Wadis", "Population Density")

# Apply the order to the Layer factor
combined_sensitivity_stats$Layer <- factor(combined_sensitivity_stats$Layer, 
                                           levels = desired_order)

# Plot with adjusted order and color scheme
ggplot(combined_sensitivity_stats, aes(x = Layer, y = Mean, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_errorbar(aes(ymin = Mean - Adjusted_SD, ymax = Mean + Adjusted_SD), 
                width = 0.2, color = "black") +
  scale_fill_manual(values = c("AHP-weighted suitability" = "skyblue3", 
                               "Equal-weighted suitability" = "lightblue")) +
  labs(
    title = "Sensitivity of Suitability Scores",
    y = "Mean Sensitivity with Standard Deviation",
    x = ""
  ) +
  ylim(0, 0.115) +  # Set y-axis limit to 0.11
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12),
    legend.title = element_blank()
  )

# Calculate the variance (or standard deviation) for each factor across Monte Carlo iterations
# Initialize an empty list to store variance per layer
layer_variance_expert <- numeric(9)
layer_variance_equal <- numeric(9)

# Loop through each layer one by one layer to calculate variance in influence across iterations
for (j in 1:9) {
  print(paste("Processing layer", j))
  
  # Extract the suitability results for the current layer for both expert and equal weights
  pert_expert_layer <- pert_expert[, j]
  pert_equal_layer <- pert_equal[, j]
  
  # Initialize vectors to store the variance per valid cell
  variance_expert_per_cell <- numeric(n_valid_cells)
  variance_equal_per_cell <- numeric(n_valid_cells)
  
  # Loop through each valid cell to compute variance for the current layer
  for (i in 1:n_valid_cells) {
    # Calculate the variance for the expert and equal weights at each valid cell
    variance_expert_per_cell[i] <- var(suitability_results_expert[i, ] * pert_expert_layer)
    variance_equal_per_cell[i] <- var(suitability_results_equal[i, ] * pert_equal_layer)
  }
  
  # Sum up the variance for all cells in the current layer
  layer_variance_expert[j] <- sum(variance_expert_per_cell, na.rm = TRUE)
  layer_variance_equal[j] <- sum(variance_equal_per_cell, na.rm = TRUE)
  
  print(paste("Finished layer", j))
}
# Create a summary data frame with the variances for each layer
layer_influence_summary <- data.frame(
  Layer = 1:9,  # Your 9 layers
  Expert_Variance = layer_variance_expert,
  Equal_Variance = layer_variance_equal
)

# Sort by the highest expert variance
layer_influence_summary <- layer_influence_summary[order(-layer_influence_summary$Expert_Variance),]

# Print the result
print(layer_influence_summary)
# Load necessary library
library(ggplot2)

# Correct layer names in the correct order
layer_names <- c("Aquifer Recharge/Geology", "Roads", "Wadis", "Drainage Density", "Rainfall", "Land Use", 
                 "Soil", "Slope", "Population Density")

variance_df <- data.frame(
  Layer = factor(layer_names, levels = layer_names),  # Order the layers
  Expert_Variance = layer_influence_summary$Expert_Variance,
  Equal_Variance = layer_influence_summary$Equal_Variance
)

# Reshape data for plotting
variance_df_melt <- reshape2::melt(variance_df, id.vars = "Layer")

# Plot
ggplot(variance_df_melt, aes(x = Layer, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Variance Comparison by Layer (Expert vs Equal Weights)", 
       y = "Variance", x = "Layer") +
  scale_fill_manual(values = c("Expert_Variance" = "blue", "Equal_Variance" = "red"),
                    name = "Weighting") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Define your layer names
layer_names <- c("Aquifer Recharge/Geology", "Roads", "Wadis", "Drainage Density", 
                 "Rainfall", "Land Use", "Soil", "Slope", "Population Density")

# Create the data frame with variance values
layer_influence_summary <- data.frame(
  Layer = layer_names,  # Now, we use the actual layer names here
  Expert_Variance = layer_variance_expert,
  Equal_Variance = layer_variance_equal
)

# Order the data frame by Expert Variance
layer_influence_summary <- layer_influence_summary[order(-layer_influence_summary$Expert_Variance), ]

# Now we plot with the correct ordering
library(ggplot2)

# Convert the data frame to long format for better plotting
library(reshape2)
variance_df_melt <- melt(layer_influence_summary, id.vars = "Layer")

# Define your layer names
layer_names <- c("Aquifer Recharge/Geology", "Roads", "Wadis", "Drainage Density", 
                 "Rainfall", "Land Use", "Soil", "Slope", "Population Density")

# Create the data frame with variance values
layer_influence_summary <- data.frame(
  Layer = layer_names,  # Now, we use the actual layer names here
  Expert_Variance = layer_variance_expert,
  Equal_Variance = layer_variance_equal
)

# Order the data frame by Expert Variance
layer_influence_summary <- layer_influence_summary[order(-layer_influence_summary$Expert_Variance), ]

# Now we plot with the correct ordering
library(ggplot2)

# Convert the data frame to long format for better plotting
library(reshape2)
variance_df_melt <- melt(layer_influence_summary, id.vars = "Layer")

# Plot
ggplot(variance_df_melt, aes(x = Layer, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Variance Comparison by Layer (Expert vs Equal Weights)", 
       y = "Variance", x = "Layer") +
  scale_fill_manual(values = c("Expert_Variance" = "blue", "Equal_Variance" = "red"),
                    name = "Weighting") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot
ggplot(variance_df_melt, aes(x = Layer, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Variance Comparison by Layer (Expert vs Equal Weights)", 
       y = "Variance", x = "Layer") +
  scale_fill_manual(values = c("Expert_Variance" = "blue", "Equal_Variance" = "red"),
                    name = "Weighting") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Create the plot
plot_variance <- ggplot(variance_df_melt, aes(x = Layer, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Variance Comparison by Layer (Expert vs Equal Weights)", 
       y = "Variance", x = "Layer") +
  scale_fill_manual(values = c("Expert_Variance" = "blue", "Equal_Variance" = "red"),
                    name = "Weighting") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save the plot directly as a PNG image
ggsave("variance_comparison_CD_100.png", plot = plot_variance, width = 10, height = 6, dpi = 300)
