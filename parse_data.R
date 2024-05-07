parse_data <- function(results) {
  num_runs <- length(results)
  best_mean <- numeric(num_runs)
  standard_deviation <- numeric(num_runs)
  
  for (i in seq_along(results)) {
    fitness <- results[[i]]$fitness
    
    # Calculate mean fitness and standard deviation
    mean_fitness <- mean(fitness)
    std_deviation <- sd(fitness)
    
    # Store mean fitness and standard deviation for each run
    best_mean[i] <- mean_fitness
    standard_deviation[i] <- std_deviation
  }
  
  return(list(num_runs = num_runs, best_mean = best_mean, standard_deviation = standard_deviation))
}

# Example usage:
parsed_data1 <- parse_data(result1)
#parsed_data2 <- parse_data(result2)
#parsed_data3 <- parse_data(result3)

 #Print parsed data
print(parsed_data1)
#print(parsed_data2)
#print(parsed_data3)
