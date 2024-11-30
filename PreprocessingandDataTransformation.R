# Check for missing values in the dataset

# Summarize missing values for each column
missing_summary <- colSums(is.na(HouseRentData))

# Display the summary of missing values
print(missing_summary)

# Check if there are any missing values in the entire dataset
any_missing <- any(is.na(HouseRentData))
cat("Are there any missing values in the dataset? ", any_missing, "\n")

# Visualize missing values (optional, requires the 'naniar' library)
# Install naniar if not already installed
if (!require(naniar)) install.packages("naniar", dependencies = TRUE)
library(naniar)

# Plot missing values
gg_miss_var(HouseRentData) +
  labs(title = "Missing Values by Variable",
       x = "Variables", y = "Number of Missing Values") +
  theme_minimal()
