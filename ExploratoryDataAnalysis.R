# Load the dataset
HouseRentData <- read.csv("data/House_Rent_Dataset.csv", colClasses = c(
  BHK = "numeric",                     # Number of bedrooms
  Rent = "numeric",                    # Rent amount
  Size = "numeric",                    # Size in square feet
  Area_Type = "factor",                # Type of area (e.g., Super Area)
  Area_Locality = "factor",         # Locality name
  City = "factor",                     # City name
  Furnishing_Status = "factor",        # Furnishing status (e.g., Furnished)
  Tenant_Preferred = "factor",         # Tenant preference
  Bathroom = "numeric",                # Number of bathrooms
  Point_of_Contact = "factor"       # Point of contact information
))

# Display structure to verify data types
str(HouseRentData)

# Display first few rows to ensure data is loaded correctly
head(HouseRentData)

# Optional: View the dataset in spreadsheet-like interface
View(HouseRentData)

# Load necessary libraries
library(dplyr)  # For data manipulation
library(ggplot2)  # For visualizations
library(corrplot)  # For correlation plots

# Assuming `HouseRentData` is already loaded

# Measures of Frequency
# Frequency distribution for categorical variables
frequency_analysis <- function(data, column_name) {
  return(data %>%
           group_by(!!sym(column_name)) %>%
           summarize(Frequency = n()) %>%
           arrange(desc(Frequency)))
}

# Example: Frequency of cities
city_frequency <- frequency_analysis(HouseRentData, "City")
print(city_frequency)

# Measures of Central Tendency
# Mean, Median, Mode
mean_rent <- mean(HouseRentData$Rent, na.rm = TRUE)
median_rent <- median(HouseRentData$Rent, na.rm = TRUE)
mode_rent <- as.numeric(names(sort(table(HouseRentData$Rent), decreasing = TRUE)[1]))

cat("Mean Rent:", mean_rent, "\n")
cat("Median Rent:", median_rent, "\n")
cat("Mode Rent:", mode_rent, "\n")

# Measures of Distribution
# Range, Variance, Standard Deviation, Interquartile Range
range_rent <- range(HouseRentData$Rent, na.rm = TRUE)
variance_rent <- var(HouseRentData$Rent, na.rm = TRUE)
sd_rent <- sd(HouseRentData$Rent, na.rm = TRUE)
iqr_rent <- IQR(HouseRentData$Rent, na.rm = TRUE)

cat("Range of Rent:", range_rent, "\n")
cat("Variance of Rent:", variance_rent, "\n")
cat("Standard Deviation of Rent:", sd_rent, "\n")
cat("Interquartile Range of Rent:", iqr_rent, "\n")

# Measures of Relationship
# Correlation Matrix
# Selecting numeric variables for correlation
numeric_columns <- HouseRentData %>%
  select_if(is.numeric)

correlation_matrix <- cor(numeric_columns, use = "complete.obs")
print(correlation_matrix)

# Correlation heatmap
corrplot(correlation_matrix, method = "circle", type = "upper", tl.col = "black")

# Example: Scatter plot to visualize relationship
ggplot(HouseRentData, aes(x = Size, y = Rent)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Relationship between Size and Rent",
       x = "Size (sq ft)",
       y = "Rent (₹)")

# Load necessary libraries
library(dplyr)  # For data manipulation
library(ggplot2)  # For visualizations
library(corrplot)  # For correlation plots

# Load necessary library
library(ggplot2)

# Assuming `HouseRentData` is already loaded
# Example: Perform ANOVA to test if Rent differs significantly across Furnishing_Status groups

# Check structure of the dataset
str(HouseRentData)

# Perform one-way ANOVA
anova_result <- aov(Rent ~ Furnishing_Status, data = HouseRentData)

# Display ANOVA summary
summary(anova_result)

# Post-hoc test (if ANOVA is significant, use Tukey's HSD to determine pairwise differences)
tukey_result <- TukeyHSD(anova_result)
print(tukey_result)

# Visualize group means
ggplot(HouseRentData, aes(x = Furnishing_Status, y = Rent, fill = Furnishing_Status)) +
  geom_boxplot() +
  labs(title = "Boxplot of Rent by Furnishing Status",
       x = "Furnishing Status",
       y = "Rent") +
  theme_minimal()

# Load necessary libraries
library(ggplot2)
library(corrplot)
library(dplyr)

# Univariate Plots

# Histogram for numeric variable (e.g., Rent)
ggplot(HouseRentData, aes(x = Rent)) +
  geom_histogram(binwidth = 5000, fill = "blue", color = "black") +
  labs(title = "Histogram of Rent", x = "Rent (₹)", y = "Frequency") +
  theme_minimal()

# Boxplot for numeric variable (e.g., Size)
ggplot(HouseRentData, aes(y = Size)) +
  geom_boxplot(fill = "orange", color = "black") +
  labs(title = "Boxplot of House Size", y = "Size (sq ft)") +
  theme_minimal()

# Bar plot for categorical variable (e.g., Furnishing_Status)
ggplot(HouseRentData, aes(x = Furnishing_Status)) +
  geom_bar(fill = "purple", color = "black") +
  labs(title = "Bar Plot of Furnishing Status", x = "Furnishing Status", y = "Count") +
  theme_minimal()

# Multivariate Plots

# Scatter plot for two numeric variables (e.g., Size vs. Rent)
ggplot(HouseRentData, aes(x = Size, y = Rent)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Scatter Plot of Size vs Rent", x = "Size (sq ft)", y = "Rent (₹)") +
  theme_minimal()

# Boxplot for numeric variable grouped by categorical variable (e.g., Rent by Furnishing_Status)
ggplot(HouseRentData, aes(x = Furnishing_Status, y = Rent, fill = Furnishing_Status)) +
  geom_boxplot() +
  labs(title = "Boxplot of Rent by Furnishing Status", x = "Furnishing Status", y = "Rent (₹)") +
  theme_minimal()

# Faceted plots to compare distributions (e.g., Rent distribution by City)
ggplot(HouseRentData, aes(x = Rent)) +
  geom_histogram(binwidth = 5000, fill = "green", color = "black") +
  facet_wrap(~City) +
  labs(title = "Histogram of Rent by City", x = "Rent (₹)", y = "Frequency") +
  theme_minimal()

# Heatmap of correlation matrix (numeric variables)
numeric_columns <- HouseRentData %>%
  select_if(is.numeric)
correlation_matrix <- cor(numeric_columns, use = "complete.obs")
corrplot(correlation_matrix, method = "color", type = "upper", tl.col = "black")

