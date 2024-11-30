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
