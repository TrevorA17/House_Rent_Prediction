# Load the saved Ridge model
loaded_ridge_model <- readRDS("./models/saved_ridge_model.rds")

#* @apiTitle House Rent Prediction Model API

#* @apiDescription Used to predict the rent of a house based on various features like the number of bedrooms, size, locality, and more.

#* @param BHK Number of bedrooms
#* @param Size Size of the house in square feet
#* @param Floor The floor information (e.g., "1 out of 3")
#* @param Area_Type Type of area (e.g., "Super Area")
#* @param Area_Locality Locality name (e.g., "Salt Lake City")
#* @param City City name (e.g., "Kolkata")
#* @param Furnishing_Status Furnishing status (e.g., "Semi-Furnished")
#* @param Tenant_Preferred Tenant preference (e.g., "Bachelors/Family")
#* @param Bathroom Number of bathrooms
#* @param Point_of_Contact Point of contact (e.g., "Contact Owner")

#* @get /predict_rent

predict_rent <- function(BHK, Size, Floor, Area_Type, Area_Locality, City, Furnishing_Status, Tenant_Preferred, Bathroom, Point_of_Contact) {
  
  # Create a data frame using the arguments
  to_be_predicted <- data.frame(
    BHK = as.numeric(BHK),
    Size = as.numeric(Size),
    Floor = as.character(Floor),
    Area_Type = as.factor(Area_Type),
    Area_Locality = as.character(Area_Locality),
    City = as.factor(City),
    Furnishing_Status = as.factor(Furnishing_Status),
    Tenant_Preferred = as.factor(Tenant_Preferred),
    Bathroom = as.numeric(Bathroom),
    Point_of_Contact = as.character(Point_of_Contact)
  )
  
  # Use the loaded model to make predictions
  prediction <- predict(loaded_ridge_model, newdata = to_be_predicted)
  
  # Return the prediction (rent price)
  return(prediction)
}
