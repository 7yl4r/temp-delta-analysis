library(dplyr)
library(ggplot2)

RESAMPLE_DELTA <- 10
YEARS <- c(0, 2100)
# === ice core data
# Load CSV
data <- read.csv("data.tsv", sep='\t')

# Drop unwanted columns
data_clean <- data %>%
  select(age_calBP, Temperature)

data_clean$Temperature <- data_clean$Temperature - 55

# Add a source column to distinguish between datasets
data_clean <- data_clean %>%
  mutate(source = "ice_core")

# Convert age_calBP (years before 1950) to standard calendar years
data_clean$calendar_year <- 1950 - data_clean$age_calBP

# === air temp station data
data2 <- read.csv("data2.csv")
data2$annual_avg <- rowMeans(data2[ , 2:13])
data2_clean <- data2 %>%
  select(year, annual_avg) %>%
  rename(calendar_year = year, Temperature = annual_avg) %>%
  mutate(source = "station")

# ===
# Combine the two datasets by stacking rows
combined_data <- bind_rows(data_clean, data2_clean)

# ===


# Resample calendar_year
new_age <- seq(min(combined_data$calendar_year), max(combined_data$calendar_year), by = RESAMPLE_DELTA) # Adjust interval as needed
interpolated_data <- approx(combined_data$calendar_year, combined_data$Temperature, xout = new_age)

# Create new dataframe
resampled_data <- data.frame(calendar_year = interpolated_data$x, Temperature = interpolated_data$y)

# === 
# Calculate delta_temp
resampled_data$delta_temp <- c(NA, diff(resampled_data$Temperature))

# Plot delta_temp vs calendar_year
ggplot(resampled_data, aes(x = calendar_year, y = delta_temp)) +
  geom_line() +
  labs(title = "Differential of Temperature vs Time (Last 30,000 Years)",
       x = "Calendar Year (CE/BCE)",
       y = "Delta Temperature per RESAMPLE_DELTA") +
  scale_x_continuous(limits = YEARS, labels = scales::comma) +  # Show only the last 40,000 years
  theme_minimal()

# Plot delta_temp vs calendar_year
ggplot(resampled_data, aes(x = calendar_year, y = Temperature)) +
  geom_line() +
  labs(title = "Differential of Temperature vs Time (Last 30,000 Years)",
       x = "Calendar Year (CE/BCE)",
       y = "Delta Temperature per RESAMPLE_DELTA") +
  scale_x_continuous(limits = YEARS, labels = scales::comma) +  # Show only the last 40,000 years
  theme_minimal()
