rm(list = ls(all = TRUE))

library(openxlsx)
library(ahpsurvey)
library(readr)
library(tidyverse)
library(dplyr)
library(ggplot2)

# Define constants
A_suitable_m2 <- 957 * 1e6  # Suitable area in square meters (957 km²)
efficiency <- seq(0, 1, 0.1)  # Efficiency from 0 to 100% (converted to decimal)
precip_200 <- 0.2  # 200 mm/year in meters
precip_455 <- 0.455  # 455 mm/year in meters
precip_650 <- 0.65  # 650 mm/year in meters

# Calculate the water harvesting potential for each efficiency and rainfall scenario
rain_200 <- A_suitable_m2 * precip_200 * efficiency / 1e6  # Convert to MCM
rain_455 <- A_suitable_m2 * precip_455 * efficiency / 1e6  # Convert to MCM
rain_650 <- A_suitable_m2 * precip_650 * efficiency / 1e6  # Convert to MCM

# Create a dataframe with the calculated values
df <- data.frame(efficiency = efficiency * 100, rain_200, rain_455, rain_650)  # Convert efficiency to percentage

# Load ggplot2 package
library(ggplot2)

# Plot the scatterplot
ggplot(df, aes(x = efficiency)) +
  geom_line(aes(y = rain_200, color = "200 mm/year"), size = 1) +
  geom_line(aes(y = rain_455, color = "455 mm/year"), size = 1) +
  geom_line(aes(y = rain_650, color = "650 mm/year"), size = 1) +
  labs(title = "",
       x = "Efficiency (%)",
       y = "Water Harvesting Potential (MCM)") +
  scale_color_manual(name = "Rainfall Scenario",
                     values = c("200 mm/year" = "blue", "455 mm/year" = "green", "650 mm/year" = "red")) +
  theme_minimal()


# Dataframe for the horizontal line (total demand)
total_demand <- data.frame(efficiency = efficiency * 100, demand = rep(65, length(efficiency)))

# Plot the scatterplot with a legend entry for the total water demand line
ggplot(df, aes(x = efficiency)) +
  geom_line(aes(y = rain_200, color = "200 mm/year"), size = 1) +
  geom_line(aes(y = rain_455, color = "455 mm/year"), size = 1) +
  geom_line(aes(y = rain_650, color = "650 mm/year"), size = 1) +
  geom_line(data = total_demand, aes(y = demand, color = "Total Domestic Demand"), linetype = "dashed", size = 1) +  # Add total demand to legend
  labs(title = "",
       x = "Efficiency (%)",
       y = "Water Harvesting Potential (MCM)") +
  scale_color_manual(name = "Rainfall Scenario",
                     values = c("200 mm/year" = "blue", "455 mm/year" = "green", "650 mm/year" = "red", "Total Demand" = "black")) +
  theme_minimal()




######

rm(list = ls(all = TRUE))

library(openxlsx)
library(ahpsurvey)
library(readr)
library(tidyverse)
library(dplyr)
library(ggplot2)

# Define constants
A_suitable_m2 <- 957 * 1e6  # Suitable area in square meters (957 km²)
efficiency <- seq(0, 1, 0.1)  # Efficiency from 0 to 100% (converted to decimal)
precip_200 <- 0.2  # 200 mm/year in meters
precip_455 <- 0.455  # 455 mm/year in meters
precip_650 <- 0.65  # 650 mm/year in meters

# Calculate the water harvesting potential for each efficiency and rainfall scenario
rain_200 <- A_suitable_m2 * precip_200 * efficiency / 1e6  # Convert to MCM
rain_455 <- A_suitable_m2 * precip_455 * efficiency / 1e6  # Convert to MCM
rain_650 <- A_suitable_m2 * precip_650 * efficiency / 1e6  # Convert to MCM

# Create a dataframe with the calculated values
df <- data.frame(efficiency = efficiency * 100, rain_200, rain_455, rain_650)  # Convert efficiency to percentage

ggplot(df, aes(x = efficiency)) +
  geom_line(aes(y = rain_200, color = "200 mm/year"), size = 1) +
  geom_line(aes(y = rain_455, color = "455 mm/year"), size = 1) +
  geom_line(aes(y = rain_650, color = "650 mm/year"), size = 1) +
  geom_hline(aes(yintercept = 65, color = "Total Demand"), linetype = "dashed", size = 1, show.legend = TRUE) +  # Force total demand into legend
  labs(title = "",
       x = "Efficiency (%)",
       y = "Water Harvesting Potential (MCM)") +
  scale_color_manual(name = "Rainfall Scenario",
                     values = c("200 mm/year" = "lightblue", "455 mm/year" = "blue", "650 mm/year" = "darkblue", "Total Domestic Demand" = "black")) +
  guides(color = guide_legend(override.aes = list(linetype = c("solid", "solid", "solid", "dashed")))) + # Customize legend line types
  theme_minimal()


#
rm(list = ls(all = TRUE))

library(openxlsx)
library(ahpsurvey)
library(readr)
library(tidyverse)
library(dplyr)
library(ggplot2)

# Define constants
A_suitable_m2 <- 957 * 1e6  # Suitable area in square meters (957 km²)
efficiency <- seq(0, 1, 0.1)  # Efficiency from 0 to 100% (converted to decimal)
precip_200 <- 0.2  # 200 mm/year in meters
precip_455 <- 0.455  # 455 mm/year in meters
precip_650 <- 0.65  # 650 mm/year in meters

# Calculate the water harvesting potential for each efficiency and rainfall scenario
rain_200 <- A_suitable_m2 * precip_200 * efficiency / 1e6  # Convert to MCM
rain_455 <- A_suitable_m2 * precip_455 * efficiency / 1e6  # Convert to MCM
rain_650 <- A_suitable_m2 * precip_650 * efficiency / 1e6  # Convert to MCM

# Create a dataframe with the calculated values
df <- data.frame(efficiency = efficiency * 100, rain_200, rain_455, rain_650)  # Convert efficiency to percentage


# Plot the scatterplot with a legend entry for the total water demand line
ggplot(df, aes(x = efficiency)) +
  geom_line(aes(y = rain_200, color = "200 mm/year"), size = 1) +
  geom_line(aes(y = rain_455, color = "455 mm/year"), size = 1) +
  geom_line(aes(y = rain_650, color = "650 mm/year"), size = 1) +
  geom_hline(yintercept = 65, linetype = "dashed", color = "black", size = 1) +  # Add total demand as a horizontal line
  labs(title = "",
       x = "Efficiency (%)",
       y = "Water Harvesting Potential (MCM)") +
  scale_color_manual(name = "Legend",
                     values = c("200 mm/year" = "lightblue", "455 mm/year" = "blue", "650 mm/year" = "darkblue", "Total Demand" = "black"),
                     labels = c("200 mm/year", "455 mm/year", "650 mm/year", "Total Domestic Demand")) + # Ensure labels match color keys
  guides(color = guide_legend(override.aes = list(linetype = c("solid", "solid", "solid", "dashed")))) + # Customize legend line types
  theme_minimal()