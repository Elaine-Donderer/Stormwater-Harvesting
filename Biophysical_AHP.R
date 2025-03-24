rm(list = ls(all = TRUE))

library(openxlsx)
library(ahpsurvey)
library(readr)
library(tidyverse)
library(dplyr)
library(ggplot2)

setwd("C:/Users/Elain/OneDrive/Documents/Survey_Thesis_AHP")

bio <- read_csv("BIOBIO.csv")
bio <- bio %>% select(-Resp_Id)

print(names(bio))
str(bio)
n <-17

attr <- c("Prec", "Slope", "Drain", "Soil", "Geo", "Land")
head(bio)
ahp_matrix <- ahp.mat(bio, atts = attr, negconvert = TRUE, reciprocal = TRUE)
head(ahp_matrix)

bio_clean <- bio %>%
  mutate(across(everything(), ~ ifelse(. < 0, 1 / abs(.), .)))  # Convert negative values to reciprocals

# Step 2: Create the AHP matrix using manually adjusted data
bio_ahp_matrix <- ahp.mat(bio_clean, atts = attr, negconvert = FALSE, reciprocal = FALSE)
head(bio_ahp_matrix)

bioahp <- bio_clean %>% 
  ahp.mat(atts = attr, negconvert = T)
head(bioahp)

bio_weights <- ahp.indpref(bio_ahp_matrix, atts = attr, method = "eigen")
bio_aggregated_weights <- apply(bio_weights, 2, mean)

amean_corrected <- ahp.aggpref(bio_ahp_matrix, atts = attr, method = "arithmetic")
print(amean_corrected)

amean <- ahp.aggpref(bioahp, atts = attr, method = "arithmetic")
amean

sd <- bio %>%
  ahp.mat(atts = atts, negconvert = TRUE) %>% 
  ahp.aggpref(atts, method = "arithmetic", aggmethod = "sd")
sd_corrected <- ahp.aggpref(bio_ahp_matrix, atts = attr, method = "arithmetic", aggmethod = "sd")
print(sd_corrected)
sd


# Means and SDs from your calculated results
mean_values <- amean_corrected
sd_values <- sd_corrected

# Create the data frame with the correct values
tableBio <- data.frame(
  Attribute = c("Prec", "Slope", "Drain", "Soil", "Geo", "Land"),
  Mean = mean_values,
  SD = sd_values
)


print(tableBio)

p <- ggplot(tableBio, aes(x = Attribute, y = Mean)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +  # Bar plot
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.2) +  # Error bars
  theme_classic() +  # Classic theme
  labs(title = "", x = "Attribute", y = "Mean Value") +  # Labels
  theme(text = element_text(family = "Times New Roman"))  # Font settings

# Print the plot
print(p)


# Step 6: Calculate the aggregated mean and standard deviation for the corrected weights
bio_agg_mean <- colMeans(bio_weights)  # Mean of the corrected individual weights
bio_agg_sd <- apply(bio_weights, 2, sd)  # Standard deviation of the corrected individual weights

bio_result_table <- data.frame(
  Attribute = attr,
  Aggregated_Weight = bio_aggregated_weights,
  Aggregated_Mean = bio_agg_mean,
  Standard_Deviation = bio_agg_sd
)

# Display the results
print(bio_result_table)

ggplot(bio_result_table, aes(x = Attribute, y = Aggregated_Weight)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  geom_errorbar(aes(ymin = Aggregated_Weight - Standard_Deviation,
                    ymax = Aggregated_Weight + Standard_Deviation), width = 0.2) +
  labs(
    title = "",
    x = "Attribute",
    y = "Aggregated Weight"
  ) +
  theme_classic()

aggregated_matrix <- ahp.aggjudge(bio_ahp_matrix, atts = attr, aggmethod = "geometric")
print(aggregated_matrix)

library(ahpsurvey)
weights <- ahp.aggpref(list(aggregated_matrix), atts = attr, method = "eigen")
print(weights)



# Step 3: Calculate individual preference weights using both methods
# Calculate weights using the eigenvalue method
eigen_weights <- ahp.indpref(bio_ahp_matrix, atts = attr, method = "eigen")

# Calculate weights using the mean aggregation method (arithmetic mean)
mean_weights <- ahp.indpref(bio_ahp_matrix, atts = attr, method = "arithmetic")

# Step 4: Calculate the maximum absolute difference between the weights for each respondent
max_diff <- data.frame(
  id = 1:nrow(eigen_weights),
  maxdiff = apply(abs(eigen_weights - mean_weights), 1, max)
)

# Step 5: Plot the maximum differences
ggplot(max_diff, aes(x = id, y = maxdiff)) +
  geom_point() +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "blue") +  # Example threshold line
  geom_hline(yintercept = 0, color = "gray50") +  # Zero line for reference
  scale_x_continuous("Respondent ID") +
  scale_y_continuous("Maximum Difference") +
  labs(
    title = "",
    subtitle = "",
    x = "Respondent ID",
    y = "Maximum Difference"
  ) +
  theme_classic()

cr_values <- bio %>%
  ahp.mat(atts = attr, negconvert = FALSE) %>%
  ahp.cr(atts = attr)
print(cr_values)
cat("Length of cr_values:", length(cr_values), "\n")

cr.df <- data.frame(cr = cr_values) %>%
  mutate(rowid = 1:length(cr),
         cr.dum = as.factor(ifelse(cr <= 0.1, 1, 0))) %>%  # Label CR < 0.1 as 1 (consistent), others as 0
  select(cr.dum, rowid)

weights <- bio %>%
  ahp.mat(atts = attr, negconvert = FALSE) %>%  # Correct handling of negconvert
  ahp.indpref(atts = attr, method = "eigen") %>%
  mutate(rowid = 1:nrow(.))

weights_long <- weights %>%
  left_join(cr.df, by = 'rowid') %>%
  gather(key = "var", value = "pref", -rowid, -cr.dum)


ggplot(weights_long, aes(x = var, y = pref)) + 
  geom_violin(alpha = 0.6, width = 0.8, color = "transparent", fill = "gray") +
  geom_jitter(alpha = 0.6, height = 0, width = 0.1, aes(color = cr.dum)) +
  geom_boxplot(alpha = 0, width = 0.3, color = "#808080") +
  scale_x_discrete("Attribute", labels = c("Prec" = "Prec", 
                                           "Slope" = "Slope", 
                                           "Drain" = "Drai",
                                           "Soil" = "Soil",
                                           "Geo" = "Geo",
                                           "Land" = "Land")) +
  scale_y_continuous("Weight (dominant eigenvalue)", 
                     labels = scales::percent, 
                     breaks = seq(0, 0.7, 0.1)) +
  guides(color = guide_legend(title = NULL)) +
  scale_color_discrete(breaks = c(0, 1), 
                       labels = c("CR > 0.1", "CR < 0.1")) +
  labs(NULL, caption = paste("n =", nrow(bio), 
                             ",", "Mean CR =", round(mean(cr_values, na.rm = TRUE), 3))) +
  theme_classic()

# Step 5: Iteratively improve consistency ratios using the Harker method
crmat <- matrix(NA, nrow = length(cr_values), ncol = 11)
colnames(crmat) <- 0:10
crmat[, 1] <- cr_values

max_iterations <- min(10, choose(length(attr), 2))

for (it in 1:max_iterations) {
  harker_values <- bio %>%
    ahp.mat(atts = attr, negconvert = FALSE) %>%
    ahp.harker(atts = attr, iterations = it, stopcr = 0.1, limit = TRUE, round = TRUE, printiter = FALSE) %>%
    ahp.cr(atts = attr)
  
  crmat[, it + 1] <- harker_values
}

print(crmat)

consistency_summary <- data.frame(
  "Consistent?" = c("FALSE", "TRUE"),  
  "No Iteration" = as.numeric(table(crmat[, 1] <= 0.1)[c("FALSE", "TRUE")]),
  "2Iterations" = as.numeric(table(crmat[, 3] <= 0.1)[c("FALSE", "TRUE")]),
  "4Iterations" = as.numeric(table(crmat[, 5] <= 0.1)[c("FALSE", "TRUE")])
) %>%
  mutate(across(everything(), ~ replace_na(.x, 0)))

print(consistency_summary)

# Step 1: Calculate initial consistency ratios without altering matrices
cr_values <- bio_clean %>%
  ahp.mat(atts = attr, negconvert = FALSE) %>%  # Use negconvert = FALSE to maintain matrix integrity
  ahp.cr(atts = attr)

cat("Length of cr_values:", length(cr_values), "\n")
n_rows <- length(cr_values)
n_cols <- 15
ncol(bio_clean)

# Print the length of consistency ratios
cat("Length of cr_values:", length(cr_values), "\n")

crmat <- matrix(NA, nrow = n_rows, ncol = n_cols)
colnames(crmat) <- 0:10

# Fill the first column with the initial consistency ratios
crmat[, 1] <- cr_values

# Determine the maximum allowable iterations based on the upper triangular matrix
max_iterations <- min(10, choose(length(attr), 2))  # The maximum iterations should not exceed the upper triangular elements

# Step 2: Iteratively improve consistency ratios using the Harker method
for (it in 1:max_iterations) {
  harker_values <- bio_clean %>%
    ahp.mat(atts = attr, negconvert = FALSE) %>%  # Keep negconvert = FALSE
    ahp.harker(atts = attr, iterations = it, stopcr = 0.1, 
               limit = TRUE, round = TRUE, printiter = FALSE) %>%
    ahp.cr(atts = attr)
  
  # Store the improved CR values in the respective column
  crmat[, it + 1] <- harker_values
}

print(crmat)

# Step 3: Create a summary of consistency ratios across selected iterations
consistency_summary <- data.frame(
  "Consistent?" = c("FALSE", "TRUE"),  # Predefine rows to ensure consistency
  "No Iteration" = as.numeric(table(crmat[, 1] <= 0.1)[c("FALSE", "TRUE")]),
  "2Iterations" = as.numeric(table(crmat[, 3] <= 0.1)[c("FALSE", "TRUE")]),
  "4Iterations" = as.numeric(table(crmat[, 5] <= 0.1)[c("FALSE", "TRUE")])
) %>%
  mutate(across(everything(), ~ replace_na(.x, 0)))  # Replace NA values with zero to handle missing data

# Print the consistency summary
print(consistency_summary)

crmat_clean <- crmat %>%
  as.data.frame() %>%
  drop_na()

crmat_clean %>%
  as.data.frame() %>%
  gather(key = "iter", value = "cr", 1:max_iterations) %>%
  mutate(iter = as.integer(iter)) %>%
  ggplot(aes(x = iter, y = cr, group = iter)) +
  geom_hline(yintercept = 0.1, color = "red", linetype = "dashed") +
  geom_jitter(alpha = 0.2, width = 0.3, height = 0, color = "blue") +
  geom_boxplot(fill = "transparent", color = "#808080", outlier.shape = NA) +
  scale_x_continuous("Iterations", breaks = 0:max_iterations) +
  scale_y_continuous("Consistency Ratio") +
  theme_classic()



# Step 2: Create a matrix to store consistency ratios for iterations
n_rows <- length(cr_values)
n_cols <- 11  # Number of iterations + 1 for initial values
crmat <- matrix(NA, nrow = n_rows, ncol = n_cols)
colnames(crmat) <- 0:10
crmat[, 1] <- cr_values

# Determine the maximum allowable iterations based on the matrix dimensions
max_iterations <- min(10, choose(length(attr), 2))  # Ensure not exceeding the matrix's triangular elements

# Step 3: Iteratively improve consistency ratios using the Harker method
for (it in 1:max_iterations) {
  # Calculate consistency ratios using Harker iterations
  harker_values <- bio %>%
    ahp.mat(atts = attr, negconvert = FALSE) %>%
    ahp.harker(atts = attr, iterations = it, stopcr = 0.1, limit = TRUE, round = TRUE, printiter = FALSE) %>%
    ahp.cr(atts = attr)
  
  # Store the consistency ratios in the matrix
  crmat[, it + 1] <- harker_values
}

# Print consistency ratio matrix
print(crmat)

# Step 4: Create a summary of consistency ratios across iterations
consistency_summary <- data.frame(
  "Consistent?" = c("FALSE", "TRUE"),
  "No Iteration" = as.numeric(table(crmat[, 1] <= 0.1)[c("FALSE", "TRUE")]),
  "2Iterations" = as.numeric(table(crmat[, 3] <= 0.1)[c("FALSE", "TRUE")]),
  "4Iterations" = as.numeric(table(crmat[, 5] <= 0.1)[c("FALSE", "TRUE")])
) %>%
  mutate(across(everything(), ~ replace_na(.x, 0)))

# Print the consistency summary
print(consistency_summary)

# Step 5: Plotting the change in consistency ratios across iterations
crmat %>%
  as.data.frame() %>%
  gather(key = "iter", value = "cr", 1:max_iterations) %>%
  mutate(iter = as.integer(iter)) %>%
  ggplot(aes(x = iter, y = cr, group = iter)) +
  geom_hline(yintercept = 0.1, color = "red", linetype = "dashed") +
  geom_jitter(alpha = 0.2, width = 0.3, height = 0, color = "blue") +
  geom_boxplot(fill = "transparent", color = "#808080", outlier.shape = NA) +
  scale_x_continuous("Iterations", breaks = 0:max_iterations) +
  scale_y_continuous("Consistency Ratio") +
  theme_classic()

# Step 1: Calculate initial consistency ratios
cr_values <- bio %>%
  ahp.mat(atts = attr, negconvert = FALSE) %>%
  ahp.cr(atts = attr)

# Print the length of consistency ratios
cat("Length of cr_values:", length(cr_values), "\n")

# Create a matrix to store consistency ratios for each iteration
n_rows <- length(cr_values)
n_cols <- 11
crmat <- matrix(NA, nrow = n_rows, ncol = n_cols)
colnames(crmat) <- 0:10
crmat[, 1] <- cr_values

# Determine the maximum allowable iterations based on the matrix dimensions
max_iterations <- min(10, choose(length(attr), 2))

# Step 2: Iteratively improve consistency ratios using the Harker method
for (it in 1:max_iterations) {
  harker_values <- bio %>%
    ahp.mat(atts = attr, negconvert = FALSE) %>%
    ahp.harker(atts = attr, iterations = it, stopcr = 0.1, limit = TRUE, round = TRUE, printiter = FALSE) %>%
    ahp.cr(atts = attr)
  
  crmat[, it + 1] <- harker_values
}

# Convert the consistency matrix into a DataFrame with respondent IDs
cr_df <- as.data.frame(crmat) %>%
  mutate(Respondent_ID = 1:nrow(crmat)) %>%  # Add respondent IDs
  pivot_longer(cols = -Respondent_ID, names_to = "Iteration", values_to = "CR") %>%
  mutate(Iteration = as.integer(Iteration))

# Print consistency ratios for each respondent to identify problematic ones
print(cr_df)

# Identify respondents with consistency ratios above the threshold in any iteration
problematic_respondents <- cr_df %>%
  filter(CR > 0.1) %>%
  arrange(desc(CR))

# Print respondents causing consistency issues
cat("Respondents with Consistency Ratios > 0.1:\n")
print(problematic_respondents)

# Optional: Highlight problematic respondents on the plot
ggplot(cr_df, aes(x = Iteration, y = CR, group = Respondent_ID)) +
  geom_line(aes(color = CR > 0.1), size = 1) +
  geom_point(aes(color = CR > 0.1)) +
  geom_hline(yintercept = 0.1, color = "red", linetype = "dashed") +
  scale_color_manual(values = c("FALSE" = "blue", "TRUE" = "red")) +
  labs(title = "Consistency Ratios by Respondent and Iteration",
       x = "Iteration", y = "Consistency Ratio",
       color = "CR > 0.1") +
  theme_classic()





# Step 1: Use the corrected AHP matrix from consistency adjustments (e.g., Harker method)
# Assume `corrected_socahp` is the consistency-corrected matrix from previous iterations
corrected_bioahp <- bio_clean %>%
  ahp.mat(atts = attr, negconvert = FALSE) %>%
  ahp.harker(atts = attr, iterations = 9, stopcr = 0.1, limit = TRUE, round = TRUE)

# Step 2: Calculate individual preference weights using the eigenvalue method on corrected matrices
corrected_weights <- ahp.indpref(corrected_bioahp, atts = attr, method = "eigen")

# Step 3: Aggregate the weights using the arithmetic mean method
# Here we use the ahp.aggpref function for correct aggregation
aggregated_weights <- ahp.aggpref(corrected_bioahp, atts = attr, method = "arithmetic")
print(aggregated_weights)


# Step 4: Calculate the aggregated mean and standard deviation
lapply(ahp_matrix, print)
amean_corrected <- ahp.aggpref(corrected_bioahp, atts = attr, method = "arithmetic")
print(amean_corrected)

amean <- ahp.aggpref(socahp, atts, method = "arithmetic")
amean

sd_corrected <- ahp.aggpref(corrected_bioahp, atts, method = "arithmetic", aggmethod = "sd")
print(sd_corrected)

sd <- corrected_bioahp %>%
  ahp.mat(atts = attr, negconvert = TRUE) %>% 
  ahp.aggpref(atts = attr, method = "arithmetic", aggmethod = "sd")
sd_corrected <- ahp.aggpref(corrected_bioahp, atts = attr, method = "arithmetic", aggmethod = "sd")
print(sd_corrected)
sd

agg_mean <- colMeans(corrected_weights)  # Mean of the corrected individual weights
agg_sd <- apply(corrected_weights, 2, sd)  # Standard deviation of the corrected individual weights

# Step 5: Create a table of results
result_table <- data.frame(
  Attribute = attr,
  Aggregated_Weight = aggregated_weights,
  Aggregated_Mean = amean_corrected,
  Standard_Deviation = sd_corrected
)

# Display the results
print(result_table)

# Optional: Plot the results to visualize weights, mean, and standard deviation
ggplot(result_table, aes(x = Attribute, y = Aggregated_Weight)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  geom_errorbar(aes(ymin = Aggregated_Weight - Standard_Deviation,
                    ymax = Aggregated_Weight + Standard_Deviation), width = 0.2) +
  labs(
    title = "",
    x = "Attribute",
    y = "Aggregated Weight"
  ) +
  theme_classic()
