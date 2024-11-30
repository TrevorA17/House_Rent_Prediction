# Saving the Ridge Regression model
saveRDS(ridge_model, "./models/saved_ridge_model.rds")

# Load the saved Ridge Regression model
loaded_ridge_model <- readRDS("./models/saved_ridge_model.rds")

# Example new data for prediction (adjust based on your features)
new_data <- data.frame(
  BHK = 2,                       # Number of bedrooms
  Size = 1000,                    # Size in square feet
  Floor = "1 out of 3",           # Floor information
  Area_Type = "Super Area",       # Type of area
  Area_Locality = "Salt Lake City", # Locality name
  City = "Kolkata",               # City name
  Furnishing_Status = "Semi-Furnished", # Furnishing status
  Tenant_Preferred = "Bachelors/Family", # Tenant preference
  Bathroom = 1,                    # Number of bathrooms
  Point_of_Contact = "Contact Owner" # Point of contact (ensure this is included)
  
)

# Use the loaded model to make predictions
predictions_loaded_model <- predict(loaded_ridge_model, newdata = new_data)

# Print predictions
print(predictions_loaded_model)
