rm(list = ls(all = TRUE))

library(openxlsx)
library(ahpsurvey)
library(readr)
library(tidyverse)
library(dplyr)
library(ggplot2)

setwd("C:/Users/Elain/OneDrive/Documents/Survey_Thesis_AHP")

biso <- read_csv("BiSi.csv")

print(names(biso))
str(biso)
n <-17

attr <- c("Bio", "Soc")
head(biso)
ahp_matrix <- ahp.mat(biso, atts = attr, negconvert = TRUE, reciprocal = TRUE)
head(ahp_matrix)

biso_clean <- biso %>%
  mutate(across(everything(), ~ ifelse(. < 0, 1 / abs(.), .)))  # Convert negative values to reciprocals
head(biso_clean)
ahp_matrix <- ahp.mat(biso_clean, atts = attr, negconvert = FALSE, reciprocal = FALSE)
head(ahp_matrix)

biso_weights <- ahp.indpref(ahp_matrix, atts = attr, method = "eigen")
biso_aggregated_weights <- apply(biso_weights, 2, mean)

print(biso_aggregated_weights)

amean_corrected <- ahp.aggpref(ahp_matrix, atts = attr, method = "arithmetic")
print(amean_corrected)

amean <- ahp.aggpref(ahp_matrix, atts = attr, method = "arithmetic")
amean

mean_weights <- ahp.indpref(ahp_matrix, atts = attr, method = "arithmetic")
print(mean_weights)

sd <- bi %>%
  ahp.mat(atts = atts, negconvert = TRUE) %>% 
  ahp.aggpref(atts, method = "arithmetic", aggmethod = "sd")
sd_corrected <- ahp.aggpref(ahp_matrix, atts = attr, method = "arithmetic", aggmethod = "sd")
print(sd_corrected)
sd


# Means and SDs from your calculated results
mean_values <- amean_corrected
sd_values <- sd_corrected

# Create the data frame with the correct values
tableBiso <- data.frame(
  Attribute = c("Bio", "Soc"),
  Mean = mean_values,
  SD = sd_values
)


print(tableBiso)

p <- ggplot(tableBiso, aes(x = Attribute, y = Mean)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +  # Bar plot
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.2) +  # Error bars
  theme_classic() +  # Classic theme
  labs(title = "", x = "Attribute", y = "Mean Value") +  # Labels
  theme(text = element_text(family = "Times New Roman"))  # Font settings

# Print the plot
print(p)



# Install the necessary packages
install.packages("ggplot2")
install.packages("reshape2")

# Load the libraries
library(ggplot2)
library(reshape2)

# Create the data for the heatmap
data <- data.frame(
  Area = c("Area A (556km2)", "Area B (535 km2)", "Area C (1651 km2)", "Israeli Nature Reserve (312 km2)", "Military Area (499 km2)"),
  Check_Dams = c(3.46, 3.28, 8.86, 1.09, 1.68),
  Infiltration = c(1.86, 2.12, 9.30, 0.84, 1.46),
  Bioswale = c(2.99, 3.79, 5.87, 0.33, 0.18),
  Wetland_Retention_Detention = c(4.81, 5.54, 16.81, 2.52, 3.94)
)

# Melt the data into long format
melted_data <- melt(data, id = "Area")

# Create the heatmap
ggplot(melted_data, aes(x = variable, y = Area, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "#fde0dd", high = "#67000d", name = "High Suitability Area (%)") +  # Adjust color range
  theme_minimal() +
  labs(title = "",
       x = "",
       y = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Rename columns for better formatting (capitalization and commas)
system_labels <- c("Check dams", "Infiltration", "Bioswale", "Wetland, Retention, Detention")

# Create the heatmap with a lighter pink color scale and properly formatted labels
ggplot(melted_data, aes(x = variable, y = Area, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.1f", value)), size = 3) +  # Add percentage annotations
  scale_fill_gradient(low = "#fde0dd", high = "red", name = "High Suitability Area (%)", size = 3) +  # Adjust color range
  scale_x_discrete(labels = system_labels) +  # Correct labels
  theme_minimal() +
  labs(title = "",
       x = "",
       y = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))