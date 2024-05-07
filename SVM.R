# Load required library for SVM
library(e1071)

# Define the function to split data into training and testing sets
split_data <- function(data, split_ratio = 0.8) {
  set.seed(123) # for reproducibility
  indices <- sample(1:nrow(data), round(split_ratio * nrow(data)))
  train_data <- data[indices, ]
  test_data <- data[-indices, ]
  return(list(train_data = train_data, test_data = test_data))
}

# Read data from the CSV file
data <- read.csv("D:/morden_opt_project/split3_data/Book2.csv")

# Assuming the target variable is in the last column
target_col <- ncol(data)
X <- data[, -target_col]  # Features
y <- data[, target_col]   # Target variable

# Split the data into training and testing sets with a ratio of 0.8
data_split <- split_data(data)

# Define predictors (features) and target variable
features <- c("open", "high", "low", "volume", "daily_return_in_perc", "rsi", "obv") # Adjust with your features
target <- "close" # Adjust with your target column name

# Train SVR model
svm_model <- svm(as.formula(paste(target, "~ .")), data = data_split$train_data[, c(features, target)], kernel = "radial")

# Make predictions on the testing set
predictions <- predict(svm_model, newdata = data_split$test_data[, features])

# Get actual target values from the test set
actual_target <- data_split$test_data[[target]]

# Evaluate model's performance using Mean Absolute Error (MAE)
mae <- mean(abs(predictions - actual_target))

# Evaluate model's performance using Mean Squared Error (MSE)
mse <- mean((predictions - actual_target)^2)

# Evaluate model's performance using Root Mean Squared Error (RMSE)
rmse <- sqrt(mean((predictions - actual_target)^2))

# Define fitness function
get_fitness <- function(predictions, actual_target, threshold) {
  abs_diff <- abs(predictions - actual_target)
  ifelse(abs_diff <= threshold, 1, 0)
}

# Define threshold for the fitness function
threshold <- 5  # You can adjust this threshold as needed

# Calculate fitness using the defined function
fitness <- get_fitness(predictions, actual_target, threshold)

# Print evaluation metrics
cat("Mean Absolute Error (MAE):", mae, "\n")
cat("Mean Squared Error (MSE):", mse, "\n")
cat("Root Mean Squared Error (RMSE):", rmse, "\n")
cat("Fitness:", mean(fitness), "\n")
