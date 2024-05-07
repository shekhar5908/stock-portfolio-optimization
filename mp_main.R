# Load necessary libraries
library(dplyr)
library(ggplot2)
library(zoo)
library(readr)
library(TTR)
library(tidyr)
library(stats)
# Load necessary libraries for SVR
library(e1071)
# Load data files
file_paths <- list.files("D:/morden_opt_project/individual_stocks_5yr", pattern = "*.csv", full.names = TRUE)

# Select specific company data files
company_list <- c(
  "D:/morden_opt_project/individual_stocks_5yr/AAPL_data.csv",
  "D:/morden_opt_project/individual_stocks_5yr/AMGN_data.csv",
  "D:/morden_opt_project/individual_stocks_5yr/GOOG_data.csv",
  "D:/morden_opt_project/individual_stocks_5yr/MSFT_data.csv",
  "D:/morden_opt_project/individual_stocks_5yr/KO_data.csv"
)

# Combine data into a single table
company_df <- data.frame()

for (file in company_list) {
  current_df <- read_csv(file)
  company_df <- bind_rows(company_df, current_df)
}
# Printing shape and first few rows of the dataframe
dim(company_df)
# Check column types using str()
str(company_df)

# Check column types using sapply() and class()
sapply(company_df, class)

head(company_df, 10)

# Unique values in 'Name' column
unique(company_df$name)

# Rename columns
names(company_df)[names(company_df) == 'Name'] <- 'name'

# Convert 'date' column to Date type
company_df$date <- as.Date(company_df$date)

# Define a list of unique company names
companies <- unique(company_df$name)

# Perform time series analysis for each company separately
for (company in companies) {
  # Select data for the current company
  company_data <- subset(company_df, name == company)
  
  # Plot stock prices over time
  plot(company_data$date, company_data$close, type = "l",
       main = paste("Stock Prices Over Time for", company),
       xlab = "Date", ylab = "Closing Price ($)")
  
  # Plot rolling means over time for different window sizes
  window_sizes <- c(30, 180, 365)
  for (window in window_sizes) {
    company_data[[paste0('rolling_mean_', window)]] <- rollmean(company_data$close, k = window, fill = NA)
    lines(company_data$date, company_data[[paste0('rolling_mean_', window)]], 
          col = ifelse(window == 30, "blue", ifelse(window == 180, "red", "green")), 
          lty = ifelse(window == 30, "solid", ifelse(window == 180, "dashed", "dotted")))
  }
  
  legend("topleft", legend = paste("Rolling Mean (", window_sizes, " days)", sep = ""), 
         col = c("blue", "red", "green"), lty = c("solid", "dashed", "dotted"), cex = 0.8)
}

# Calculate percentage change in stock prices or returns
company_df <- company_df %>%
  group_by(name) %>%
  mutate(daily_return_in_perc = (close - lag(close)) / lag(close) * 100)

# Function to calculate RSI
calculate_rsi <- function(data, window = 14) {
  # Calculate price differences
  price_diff <- diff(data)
  
  # Calculate gains and losses
  gains <- price_diff
  losses <- price_diff
  
  gains[gains < 0] <- 0
  losses[losses > 0] <- 0
  
  # Calculate average gains and losses over the specified window
  avg_gain <- rollapply(gains, width = window, FUN = function(x) mean(x, na.rm = TRUE), fill = NA, align = "right")
  avg_loss <- rollapply(losses, width = window, FUN = function(x) mean(abs(x), na.rm = TRUE), fill = NA, align = "right")
  
  # Calculate RS (Relative Strength)
  rs <- avg_gain / avg_loss
  
  # Calculate RSI
  rsi <- 100 - (100 / (1 + rs))
  
  # Add NA values to match the length of the original data
  rsi <- c(rep(NA, length(data) - length(rsi)), rsi)
  
  return(rsi)
}

# Apply RSI calculation for each company separately
company_df <- company_df %>%
  group_by(name) %>%
  mutate(rsi = calculate_rsi(close)) %>%
  ungroup()

# View the first few rows of the dataframe with RSI values
head(company_df)

# Assuming 'company_df' contains the DataFrame with stock data including 'date', 'close', 'volume', and 'name' columns

# Define a function to calculate OBV
calculate_obv <- function(close_prices, volumes) {
  obv <- ifelse(close_prices > lag(close_prices), volumes, ifelse(close_prices < lag(close_prices), -volumes, 0))
  obv <- cumsum(c(0, obv[-1]))  # Adjust for the first NA value
  return(obv)
}

# Calculate OBV for each company separately
company_df <- company_df %>%
  group_by(name) %>%
  mutate(obv = calculate_obv(close, volume)) %>%
  ungroup()

# Print the DataFrame with OBV
print(head(company_df))


# Plot OBV for each company separately
for (company in unique(company_df$name)) {
  company_data <- filter(company_df, name == company)
  
  plot(company_data$date, company_data$obv, type = "l", col = "blue",
       main = paste("On-Balance Volume (OBV) for", company),
       xlab = "Date", ylab = "OBV")
}

# Replace NA values with 0 in the dataset
company_df[is.na(company_df)] <- 0



# Assuming you have a data frame named company_df

# Find the index of the "close" column
close_index <- which(names(company_df) == "close")

# Move the "close" column to the last position
company_df <- company_df[, c(1:(close_index - 1), (close_index + 1):ncol(company_df), close_index)]

# Print the first few rows of the modified dataset to verify the changes
print(head(company_df))


# Split the dataframe by 'name' and save each company's data into a separate CSV file
split_and_save <- function(data, folder_path) {
  unique_names <- unique(data$name)
  for (name in unique_names) {
    company_data <- subset(data, name == name)
    file_path <- file.path(folder_path, paste0(name, "_data.csv"))
    write.csv(company_data, file_path, row.names = FALSE)
  }
}
            
# Folder path to save the split CSV files
output_folder <- "D:/morden_opt_project/split3_data"

# Create the output folder if it doesn't exist
if (!dir.exists(output_folder)) {
  dir.create(output_folder)
}

# Split the data and save each company's data into a separate CSV file
split_and_save(company_df, output_folder)

# Define the file path where you want to save the CSV file
file_path <- "D:/morden_opt_project/company_df.csv"

# Write the dataframe to a CSV file
write.csv(company_df, file = file_path, row.names = FALSE)

# Print a message indicating that the file has been saved
cat("DataFrame saved to:", file_path, "\n")





