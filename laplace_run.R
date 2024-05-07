library(GA)




laplace_crossover <- function(parent1, parent2) {
  # Generate a random crossover point
  crossover_point <- sample(seq_along(parent1), 1)
  
  # Define parameters for Laplace distribution
  location <- 0  # Mean of the distribution
  scale <- 1     # Scale parameter of the distribution
  
  # Generate Laplace-distributed random numbers
  laplace1 <- rlaplace(1, location = location, scale = scale)
  laplace2 <- rlaplace(1, location = location, scale = scale)
  
  # Perform Laplace crossover
  offspring1 <- parent1 + (parent2 - parent1) * laplace1
  offspring2 <- parent2 + (parent1 - parent2) * laplace2
  
  return(list(offspring1 = offspring1, offspring2 = offspring2))
}


# Define the monitor function
monitor <- function(obj) {
  # gaMonitor(obj)                      #call the default gaMonitor to print the usual messages during evolution
  iter <- obj@iter                      #get the current iternation/generation number
  if (iter == 0) {  # Handle the case before the first generation
    cat(paste("\rGA | generation =", obj@iter, "\n"))
    flush.console()
  } else if (iter <= 0) {  # Skip printing mean and best fitness for the first few generations
    cat(paste("\rGA | generation =", obj@iter, "\n"))
    flush.console()
  } else if (iter <= maxGenerations) {  #some error checking
    fitness <- obj@fitness              #get the array of all the fitness values in the present population
    #<<- assigns a value to the global variable declared outside the scope of this function.    
    thisRunResults[iter, 1] <<- max(fitness)
    thisRunResults[iter, 2] <<- mean(fitness)    
    thisRunResults[iter, 3] <<- median(fitness)
    cat(paste("\rGA | generation =", obj@iter, "Mean =", thisRunResults[iter, 2], "| Best =", thisRunResults[iter, 1], "\n"))
    flush.console()
  }  
  else {                               #print error messages
    cat("ERROR: iter = ", iter, "exceeds maxGenerations = ", maxGenerations, ".\n")   
    cat("Ensure maxGenerations == nrow(thisRunResults)")
  }
}
bestFitness <<- -Inf
bestSolution <<- NULL
bestSolutionBinary <- character(30)  # Initialize a vector to store binary solutions
bestSolutionFeatures <- character(30)  # Initialize a vector to store selected feature names

runGA <- function(noRuns = 30, problem = "feature", popSize = 15, pcrossover = 0.5, pmutation = 0.1) {
  # Specify GA parameter values; using the default values below.
  if (problem == "feature") {
    maxGenerations <<- 30    # <<- makes it a global variable. So it will be visible to other functions e.g. monitor()
    type = "binary"
    crossover = laplace_crossover  # Use Arithmetic Crossover
    data <- getData()
    X_train <- data$X_train  # Extract train feature matrix from the list
    y_train <- data$y_train  # Extract train target variable from the list
    X_test <- data$X_test  # Extract test feature matrix from the list
    y_test <- data$y_test  # Extract test target variable from the list
    fitness = featureFitness  # fitness function defined above
  }
  # ... (rest of the code remains the same)
  
  else if (problem == "tsp") {
    maxGenerations <<- 50
    popSize = 50
    pcrossover = 0.8
    pmutation = 0.2
    type = "permutation"
    data = getData()
    min = 1                             #minimum is city indexed 1
    max = nrow(getData())               #maximum is the number of cities in the data set
    fitness = tspFitness                #fitness function defined in TSP.R
  }
  else {
    cat("invalid problem specified. Exiting ... \n")
    return()
  }
  
  #Set up what stats you wish to note.    
  statnames = c("best", "mean", "median")
  thisRunResults <<- matrix(nrow = maxGenerations, ncol = length(statnames)) #stats of a single run
  resultsMatrix = matrix(1:maxGenerations, ncol = 1)  #stats of all the runs
  
  resultNames = character(length(statnames) * noRuns)
  resultNames[1] = "Generation"
  
  for (i in 1:noRuns) {
    cat(paste("Starting Run ", i, "\n"))
    if (problem == "feature") {
      GA <- ga(type = type, fitness = fitness, X = X_train, y = y_train, nBits = ncol(X_train),
               names = colnames(X_train), seed = i, popSize = popSize,
               pcrossover = pcrossover, pmutation = pmutation,
               maxiter = maxGenerations, monitor = monitor,
               X_test = X_test, y_test = y_test)  # Pass X_test and y_test as additional arguments
    }
    else if (problem == "tsp")
      GA <- ga(type = type, fitness = fitness, distMatrix = data, 
               min = min, max = max, popSize = popSize, maxiter = maxGenerations,
               pcrossover=pcrossover, pmutation = pmutation, monitor= monitor, seed = i )
    else if (problem == "vehicle")
      GA <- ga(type = type, fitness = fitness, 
               min = min, max = max, popSize = popSize, maxiter = maxGenerations,
               pcrossover=pcrossover, pmutation = pmutation, monitor= monitor, seed = i )
    
    resultsMatrix = cbind(resultsMatrix, thisRunResults)
    
    if (GA@fitnessValue > bestFitness) {
      bestFitness <<- GA@fitnessValue
      bestSolution <<- GA@solution
      
      bestSolutionBinary[i] <<- paste(GA@population[GA@solution, ], collapse = "") # Store binary solution
      bestSolutionFeatures[i] <<- paste(colnames(X_train)[GA@population[GA@solution, ] == 1], collapse = ", ") # Store selected feature names
    }
    #Create column names for the resultsMatrix
    for (j in 1:length(statnames)) resultNames[1+(i-1)*length(statnames)+j] = paste(statnames[j], i)
  }
  
  # Print the final best chromosome in binary format and selected feature names for each run
  cat("\n## Final Best Chromosomes (Binary) and Selected Features:\n")
  for (i in 1:noRuns) {
    cat(paste("Run", i, ":\n"))
    cat(paste("Binary:", bestSolutionBinary[i], "\n"))
    cat(paste("Selected Features:", bestSolutionFeatures[i], "\n\n"))
  }
  
  colnames(resultsMatrix) = resultNames
  return(resultsMatrix)
}
getBestFitness <- function() {
  return(bestFitness)
}

getBestSolution <- function() {
  return(bestSolution)
}
                                #Laplace Crossover
# Run with default population size (50), pcrossover = 0.5, pmutation = 0.1
results1 <- runGA(noRuns = 30, problem = "feature")
# Run with a smaller population size (100) 
#results2 <- runGA(noRuns = 30, problem = "feature", popSize = 100, pcrossover = 0.7, pmutation = 0.2)
# Run with a larger population size (200)
#results3 <- runGA(noRuns = 30, problem = "feature", popSize = 200, pcrossover = 0.9, pmutation = 0.05)