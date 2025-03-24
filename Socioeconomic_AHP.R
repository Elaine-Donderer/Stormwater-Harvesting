rm(list = ls(all = TRUE))

library(openxlsx)
library(ahpsurvey)
library(readr)
library(tidyverse)
library(dplyr)
library(ggplot2)

setwd("C:/Users/Elain/OneDrive/Documents/Survey_Thesis_AHP")
soc <- read_csv("SOCSOC.csv")
soc <- soc %>% select(-Resp_Id)
print(names(soc))
str(soc)
n <-17
atts <- c("Pop", "Wadis", "Roads")
head(soc)            
ahp_matrix <- ahp.mat(soc, atts = atts, negconvert = TRUE, reciprocal = TRUE)
head(ahp_matrix)

atts <- c("Roads", "Wadis", "Pop")
head(soc)            
ahp_matrix <- ahp.mat(soc, atts = atts, negconvert = TRUE, reciprocal = TRUE)
head(ahp_matrix)
ahp_matrix <- ahp.mat(soc, atts = atts, negconvert = TRUE, reciprocal = FALSE)

soc_clean <- soc %>%
  mutate(
    Pop_Wadi = ifelse(Pop_Wadi < 0, 1 / abs(Pop_Wadi), Pop_Wadi),
    Pop_Roads = ifelse(Pop_Roads < 0, 1 / abs(Pop_Roads), Pop_Roads),
    Wadis_Roads = ifelse(Wadis_Roads < 0, 1 / abs(Wadis_Roads), Wadis_Roads)
  )

# Create AHP matrix with proper data
ahp_matrix <- ahp.mat(soc_clean, atts = atts, negconvert = FALSE, reciprocal = TRUE)

# Pre-process the data to ensure all values are positive or reciprocals
soc_clean_manual <- soc %>%
  mutate(
    Pop_Wadi = ifelse(Pop_Wadi < 0, 1 / abs(Pop_Wadi), Pop_Wadi),
    Pop_Roads = ifelse(Pop_Roads < 0, 1 / abs(Pop_Roads), Pop_Roads),
    Wadis_Roads = ifelse(Wadis_Roads < 0, 1 / abs(Wadis_Roads), Wadis_Roads)
  )

ahp_matrix <- ahp.mat(soc_clean_manual, atts = atts, negconvert = FALSE, reciprocal = TRUE)
head(ahp_matrix)

socahp <- soc_clean %>% 
  ahp.mat(atts, negconvert = T)
head(socahp)


# Step 2: Calculate preference weights using both methods
eigentrue <- ahp.indpref(socahp, atts = atts, method = "eigen")      # Eigenvalue method
geom <- ahp.indpref(socahp, atts = atts, method = "arithmetic")      # Arithmetic mean method

# Step 3: Calculate the maximum difference between the two methods for each respondent
error <- data.frame(
  id = 1:length(socahp),                                   # Respondent IDs
  maxdiff = apply(abs(eigentrue - geom), 1, max)           # Maximum absolute difference
)


error %>%
  ggplot(aes(x = id, y = maxdiff)) +
  geom_point() +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "blue") +  # Threshold line
  geom_hline(yintercept = 0, color = "gray50") +                        # Zero line
  scale_x_continuous("Respondent ID") +
  scale_y_continuous("Maximum difference") +
  labs(title = "") +
  theme_classic()

# Check the consistency of matrix inputs
lapply(ahp_matrix, print)
amean_corrected <- ahp.aggpref(ahp_matrix, atts, method = "arithmetic")
print(amean_corrected)

amean <- ahp.aggpref(socahp, atts, method = "arithmetic")
amean

sd <- soc %>%
  ahp.mat(atts = atts, negconvert = TRUE) %>% 
  ahp.aggpref(atts, method = "arithmetic", aggmethod = "sd")
sd_corrected <- ahp.aggpref(ahp_matrix, atts, method = "arithmetic", aggmethod = "sd")
print(sd_corrected)
sd


# Means and SDs from your calculated results
mean_values <- amean_corrected
sd_values <- sd_corrected

# Create the data frame with the correct values
tableSoc <- data.frame(
  Attribute = c("Pop", "Wadis", "Roads"),
  Mean = mean_values,
  SD = sd_values
)

# Create the base plot with ggplot
p <- ggplot(tableSoc, aes(x = Attribute, y = Mean)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +  # Bar plot
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.2) +  # Error bars
  theme_classic() +  # Classic theme
  labs(title = "", x = "Attribute", y = "Mean Value") +  # Labels
  theme(text = element_text(family = "Times New Roman"))  # Font settings

# Print the plot
print(p)

aggregated_matrix <- ahp.aggjudge(ahp_matrix, atts, aggmethod = "geometric")
print(aggregated_matrix)

library(ahpsurvey)
weights <- ahp.aggpref(list(aggregated_matrix), atts = atts, method = "eigen")
print(weights)

# If it's not a matrix, convert it explicitly
if (!is.matrix(aggregated_matrix)) {
  aggregated_matrix <- as.matrix(aggregated_matrix)
}


# Step 1: Calculate initial consistency ratios without altering matrices
# Ensure that the negconvert is set to FALSE to avoid unintended reciprocal handling
cr_values <- soc_clean_manual %>%
  ahp.mat(atts = atts, negconvert = FALSE) %>%
  ahp.cr(atts = atts)

# Print the length of consistency ratios to verify
cat("Length of cr_values:", length(cr_values), "\n")

# Create a dataframe to label which CRs are consistent based on a threshold of 0.1
cr.df <- data.frame(cr = cr_values) %>%
  mutate(rowid = 1:length(cr),
         cr.dum = as.factor(ifelse(cr <= 0.1, 1, 0))) %>%  # Label CR < 0.1 as 1 (consistent), others as 0
  select(cr.dum, rowid)

# Step 2: Calculate individual preference weights without altering input data
weights <- soc_clean_manual %>%
  ahp.mat(atts = atts, negconvert = FALSE) %>%  # Correct handling of negconvert
  ahp.indpref(atts = atts, method = "eigen") %>%
  mutate(rowid = 1:nrow(.))

# Step 3: Join the weights with consistency data and reshape for plotting
weights_long <- weights %>%
  left_join(cr.df, by = 'rowid') %>%
  gather(key = "var", value = "pref", -rowid, -cr.dum)

# Step 4: Plot the weights with violin plots, jitter points, and boxplots
ggplot(weights_long, aes(x = var, y = pref)) + 
  geom_violin(alpha = 0.6, width = 0.8, color = "transparent", fill = "gray") +
  geom_jitter(alpha = 0.6, height = 0, width = 0.1, aes(color = cr.dum)) +
  geom_boxplot(alpha = 0, width = 0.3, color = "#808080") +
  scale_x_discrete("Attribute", labels = c("Pop" = "Pop", 
                                           "Wadis" = "Wadis", 
                                           "Roads" = "Roads")) +
  scale_y_continuous("Weight (dominant eigenvalue)", 
                     labels = scales::percent, 
                     breaks = seq(0, 0.7, 0.1)) +
  guides(color = guide_legend(title = NULL)) +
  scale_color_discrete(breaks = c(0, 1), 
                       labels = c("CR > 0.1", "CR < 0.1")) +
  labs(NULL, caption = paste("n =", nrow(soc_clean_manual), 
                             ",", "Mean CR =", round(mean(cr_values, na.rm = TRUE), 3))) +
  theme_classic()

crsoc <- soc_clean_manual %>%
  ahp.mat(atts = atts, negconvert = FALSE) %>%  # Avoid automatic negconvert to maintain intended values
  ahp.cr(atts = atts) 
# Create a data frame to label consistent judgments
thres <- 0.1
cr.df <- data.frame(cr = crsoc) %>%
  mutate(rowid = 1:length(cr),
         cr.dum = as.factor(ifelse(cr <= thres, 1, 0))) %>%
  select(cr.dum, rowid)

print(cr.df)

weights <- soc_clean_manual %>%
  ahp.mat(atts = atts, negconvert = FALSE) %>%  # Avoid automatic reciprocal conversions
  ahp.indpref(atts = atts, method = "eigen") %>%
  mutate(rowid = 1:nrow(.))
 
print(weights)


# Step 1: Calculate initial consistency ratios using clean matrices without altering them
cr_values <- soc_clean_manual %>%
  ahp.mat(atts = atts, negconvert = FALSE) %>%  # Use negconvert = FALSE to maintain matrix integrity
  ahp.cr(atts = atts)

cat("Length of cr_values:", length(cr_values), "\n")
n_rows <- length(cr_values)
n_cols <- 11 

#create matrix to store CR values
crmat <- matrix(NA, nrow = n_rows, ncol = n_cols)
colnames(crmat) <- 0:10

# Fill the first column with the initial consistency ratios
crmat[, 1] <- cr_values

# Determine the maximum allowable iterations based on the upper triangular matrix
max_iterations <- min(10, choose(length(atts), 2))  # The maximum iterations should not exceed the upper triangular elements

# Step 2: Iteratively improve consistency ratios using the Harker method
for (it in 1:max_iterations) {
  harker_values <- soc_clean_manual %>%
    ahp.mat(atts = atts, negconvert = FALSE) %>%  # Keep negconvert = FALSE
    ahp.harker(atts = atts, iterations = it, stopcr = 0.1, 
               limit = TRUE, round = TRUE, printiter = FALSE) %>%
    ahp.cr(atts = atts)
  
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

crmat %>%
  as.data.frame() %>%
  gather(key = "iter", value = "cr", `0`, 1:max_iterations) %>%
  mutate(iter = as.integer(iter)) %>%
  ggplot(aes(x = iter, y = cr, group = iter)) +
  geom_hline(yintercept = 0.1, color = "red", linetype = "dashed") +
  geom_jitter(alpha = 0.2, width = 0.3, height = 0, color = "blue") +
  geom_boxplot(fill = "transparent", color = "#808080", outlier.shape = NA) +
  scale_x_continuous("Iterations", breaks = 0:max_iterations) +
  scale_y_continuous("Consistency Ratio") +
  theme_classic()


# Step 1: Use the corrected AHP matrix from consistency adjustments (e.g., Harker method)
# Assume `corrected_socahp` is the consistency-corrected matrix from previous iterations
corrected_socahp <- soc_clean_manual %>%
  ahp.mat(atts = atts, negconvert = FALSE) %>%
  ahp.harker(atts = atts, iterations = 3, stopcr = 0.1, limit = TRUE, round = TRUE)

# Step 2: Calculate individual preference weights using the eigenvalue method on corrected matrices
corrected_weights <- ahp.indpref(corrected_socahp, atts = atts, method = "eigen")

# Step 3: Aggregate the weights using the arithmetic mean method
aggregated_weights <- apply(corrected_weights, 2, mean)

# Step 3: Aggregate the weights using the arithmetic mean method
# Here we use the ahp.aggpref function for correct aggregation
aggregated_weights <- ahp.aggpref(corrected_socahp, atts = atts, method = "arithmetic")
print(aggregated_weights)


# Step 4: Calculate the aggregated mean and standard deviation
lapply(ahp_matrix, print)
amean_corrected <- ahp.aggpref(corrected_socahp, atts = atts, method = "arithmetic")
print(amean_corrected)

amean <- ahp.aggpref(socahp, atts, method = "arithmetic")
amean

sd_corrected <- ahp.aggpref(corrected_socahp, atts, method = "arithmetic", aggmethod = "sd")
print(sd_corrected)

sd <- corrected_socahp %>%
  ahp.mat(atts = atts, negconvert = TRUE) %>% 
  ahp.aggpref(atts = atts, method = "arithmetic", aggmethod = "sd")
sd_corrected <- ahp.aggpref(corrected_socahp, atts = atts, method = "arithmetic", aggmethod = "sd")
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


# Step 4: Calculate the aggregated mean and standard deviation
agg_mean <- colMeans(corrected_weights)  # Mean of the corrected individual weights
agg_sd <- apply(corrected_weights, 2, sd)  # Standard deviation of the corrected individual weights

# Step 5: Create a table of results
result_table <- data.frame(
  Attribute = atts,
  Aggregated_Weight = aggregated_weights,
  Aggregated_Mean = agg_mean,
  Standard_Deviation = agg_sd
)

# Display the results
print(result_table)

# Replace "Roads" with "Pop" and "Pop" with "Roads"
result_table <- result_table %>%
  mutate(Attribute = recode(Attribute, "Roads" = "Pop", "Pop" = "Roads"))

# Create the plot with the updated labels
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
