# Load the dataset
HouseRentData <- read.csv("data/House_Rent_Dataset.csv", colClasses = c(
  BHK = "numeric",                     # Number of bedrooms
  Rent = "numeric",                    # Rent amount
  Size = "numeric",                    # Size in square feet
  Floor = "factor",                 # Floor information
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
