## Header: Load required libraries
library(e1071)  # For SVR
library(caret)  # For data splitting and evaluation metrics

## Header: Function to fit SVR model
getBenchmark <- function(X_train, y_train) {
  # Fit SVR model
  mod <- svm(y_train ~ ., data = data.frame(X_train), type = "eps-regression", kernel = "radial")
  
  return(mod)
}

## Header: Function to get data
getData <- function() {
  # Load your dataset
  company_data <- read.csv("D:/mod_opt/Book2.csv")
  
  
  # Assuming the target variable is the last column
  target_var <- ncol(company_data)
  
  # Split data into features and target
  X <- data.matrix(company_data[, -target_var])  # Convert to matrix
  y <- company_data[, target_var]
  
  # Split data into train and test sets
  set.seed(123)  # For reproducibility
  train_idx <- createDataPartition(y, p = 0.8, list = FALSE)
  X_train <- X[train_idx, ]
  y_train <- y[train_idx]
  X_test <- X[-train_idx, ]
  y_test <- y[-train_idx]
  
  return(list(X_train = X_train, y_train = y_train, X_test = X_test, y_test = y_test))
}

## Header: Feature Fitness Function for SVR
featureFitness <- function(string, X_train, y_train, X_test, y_test) {
  # print(string)  # Uncomment this line if you want to print every single solution
  inc <- which(string == 1)  # 'inc' includes those features/variables for which 'string' contains 1
  if (length(inc) == 0) return(-10E20)  # If no feature is selected, give a terrible fitness to this solution
  
  # Create a matrix of values for all the variables contained in 'inc'
  X_train_subset <- X_train[, inc, drop = FALSE]  # Ensure X_train_subset is a matrix
  X_test_subset <- X_test[, inc, drop = FALSE]  # Ensure X_test_subset is a matrix
  
  # Fit SVR model on the subset of features
  mod <- svm(y_train ~ ., data = data.frame(X_train_subset, y_train), type = "eps-regression", kernel = "radial")
  
  # Predict on test set and calculate evaluation metrics
  y_pred <- predict(mod, X_test_subset)
  mse <- mean((y_test - y_pred)^2)  # Mean Squared Error
  rmse <- sqrt(mse)  # Root Mean Squared Error
  
  -rmse
}
