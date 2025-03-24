rm(list = ls(all = TRUE))

library(openxlsx)
library(ahpsurvey)
library(readr)
library(tidyverse)
library(dplyr)
library(ggplot2)

setwd("C:/Users/Elain/OneDrive/Documents/GIS_R")

#load files
flood <- read_csv("flo.csv")
fin_flood <- read_csv("FINAL_flood.csv")

head(flood)
head(fin_flood)


#make sure both datasets match
identical(flood$`ZONE_CODE,N,10,0`, fin_flood$`ZONE_CODE,N,10,0`)
# Check for missing or extra row between the two datasets
missing_row <- setdiff(flood$`ZONE_CODE,N,10,0`, fin_flood$`ZONE_CODE,N,10,0`)
extra_row <- setdiff(fin_flood$`ZONE_CODE,N,10,0`, flood$`ZONE_CODE,N,10,0`)
print(missing_row)
print(extra_row)

flood <- flood[flood$`ZONE_CODE,N,10,0` != 209, ]

# Extract the relevant columns: MEAN flood susceptibility before and after
mean_before <- flood$`MEAN,N,19,11`   # MEAN flood susceptibility before intervention
mean_after <- fin_flood$`MEAN,N,19,11`  # MEAN flood susceptibility after intervention

# Perform the paired t-test to assess significance of flood reduction
t_test_result <- t.test(mean_before, mean_after, paired = TRUE)

# Print the result of the paired t-test
print(t_test_result)

# Optional: Calculate and compare the mean flood susceptibility before and after
mean_before_val <- mean(mean_before, na.rm = TRUE)
mean_after_val <- mean(mean_after, na.rm = TRUE)

cat("Mean flood susceptibility before intervention:", mean_before_val, "\n")
cat("Mean flood susceptibility after intervention:", mean_after_val, "\n")




#check for normal distribution; alternative test for significance if data is not normally distributed
# Perform Shapiro-Wilk test for normality
shapiro_test_before <- shapiro.test(mean_before)
shapiro_test_after <- shapiro.test(mean_after)

# Print the results
print(shapiro_test_before)
print(shapiro_test_after)

# Plot histogram for visual check
hist(mean_before, main="Histogram of Flood Susceptibility (Before)", xlab="Flood Susceptibility")
hist(mean_after, main="Histogram of Flood Susceptibility (After)", xlab="Flood Susceptibility")

# QQ-Plots to check normality
qqnorm(mean_before)
qqline(mean_before, col = "red")

qqnorm(mean_after)
qqline(mean_after, col = "red")

#thus, we perform w Wilkow instead of a t-test

# Perform a Wilcoxon signed-rank test
wilcox_test_result <- wilcox.test(mean_before, mean_after, paired = TRUE)

# Print the result of the Wilcoxon test
print(wilcox_test_result)