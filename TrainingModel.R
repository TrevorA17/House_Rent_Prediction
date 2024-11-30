# Load necessary library
library(caret)

# Set seed for reproducibility
set.seed(123)

# Data Splitting: Split data into training (70%) and testing (30%)
trainIndex <- createDataPartition(HouseRentData$Rent, p = 0.7, list = FALSE)
trainData <- HouseRentData[trainIndex, ]
testData <- HouseRentData[-trainIndex, ]
cat("Training data rows:", nrow(trainData), "\n")
cat("Testing data rows:", nrow(testData), "\n")

# Bootstrapping: Create bootstrapped samples
bootstrapSamples <- createResample(HouseRentData$Rent, times = 1000, list = TRUE)
bootstrapMeans <- sapply(bootstrapSamples, function(indices) {
  mean(HouseRentData$Rent[indices], na.rm = TRUE)
})
cat("Bootstrap Means Summary:\n")
print(summary(bootstrapMeans))

# Basic Cross-Validation: 5-fold CV
trainControlCV <- trainControl(method = "cv", number = 5)
cvModel <- train(Rent ~ ., data = trainData, method = "lm", trControl = trainControlCV)
cat("Basic CV Model Summary:\n")
print(cvModel)

# Repeated Cross-Validation: 5-fold CV repeated 3 times
trainControlRepeated <- trainControl(method = "repeatedcv", number = 5, repeats = 3)
repeatedCvModel <- train(Rent ~ ., data = trainData, method = "lm", trControl = trainControlRepeated)
cat("Repeated CV Model Summary:\n")
print(repeatedCvModel)

# Leave-One-Out Cross-Validation (LOOCV)
trainControlLOOCV <- trainControl(method = "LOOCV")
loocvModel <- train(Rent ~ ., data = trainData, method = "lm", trControl = trainControlLOOCV)
cat("LOOCV Model Summary:\n")
print(loocvModel)

# Load necessary libraries
library(caret)
library(glmnet)
library(randomForest)

# Set seed for reproducibility
set.seed(123)

# Linear Regression (LM)
lm_model <- train(Rent ~ ., data = trainData, method = "lm")
cat("Linear Regression Model Summary:\n")
print(lm_model)

# Ridge Regression (using glmnet)
ridge_model <- train(Rent ~ ., data = trainData, method = "glmnet", 
                     tuneGrid = expand.grid(alpha = 0, lambda = seq(0.001, 0.1, by = 0.001)))
cat("Ridge Regression Model Summary:\n")
print(ridge_model)

# Random Forest Regression (using randomForest package)
rf_model <- randomForest(Rent ~ ., data = trainData)
cat("Random Forest Regression Model Summary:\n")
print(rf_model)

# Optional: Predicting using the models on the test set
lm_predictions <- predict(lm_model, newdata = testData)
ridge_predictions <- predict(ridge_model, newdata = testData)
rf_predictions <- predict(rf_model, newdata = testData)

# Evaluate performance of models (using RMSE as example)
lm_rmse <- sqrt(mean((lm_predictions - testData$Rent)^2))
ridge_rmse <- sqrt(mean((ridge_predictions - testData$Rent)^2))
rf_rmse <- sqrt(mean((rf_predictions - testData$Rent)^2))

cat("Linear Regression RMSE:", lm_rmse, "\n")
cat("Ridge Regression RMSE:", ridge_rmse, "\n")
cat("Random Forest Regression RMSE:", rf_rmse, "\n")

# Load necessary libraries
library(caret)
library(glmnet)
library(randomForest)

# Set seed for reproducibility
set.seed(123)

# Train the models (Linear Regression, Ridge Regression, Random Forest)
lm_model <- train(Rent ~ ., data = trainData, method = "lm")
ridge_model <- train(Rent ~ ., data = trainData, method = "glmnet", 
                     tuneGrid = expand.grid(alpha = 0, lambda = seq(0.001, 0.1, by = 0.001)))
rf_model <- train(Rent ~ ., data = trainData, method = "rf")

# Combine the models into a list for resampling
model_list <- list(LM = lm_model, Ridge = ridge_model, RF = rf_model)

# Resampling to compare the models
resamples_results <- resamples(model_list)

# Summary of resamples for performance comparison
summary(resamples_results)

# Boxplot to visually compare the model performances
bwplot(resamples_results, metric = "RMSE") # Boxplot of RMSE for all models
bwplot(resamples_results, metric = "Rsquared") # Boxplot of R-squared for all models


