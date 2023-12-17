# Required libraries
library(MASS) # For mvrnorm function

# Function to initialize parameters
initparameters <- function(X, G, modelNames) {
  # Your initialization logic goes here
  # Example: Randomly initialize means, variances, and proportions
  means <- runif(G, min = min(X), max = max(X))
  variances <- runif(G, min = 0.1, max = 1)
  proportions <- rep(1/G, G)
  
  # For simplicity, assuming a univariate Gaussian mixture model
  parameters <- list(means = means, variances = variances, proportions = proportions)
  return(parameters)
}

# E-step function
E.step <- function(X, parameters) {
  # Your E-step logic goes here
  # Calculate the responsibility matrix
  # Example: Use dnorm to compute the probability density for each data point
  #         under each Gaussian component and update the responsibility matrix
  G <- length(parameters$means)
  T <- matrix(0, nrow = length(X), ncol = G)
  
  for (g in 1:G) {
    T[, g] <- parameters$proportions[g] * dnorm(X, mean = parameters$means[g], sd = sqrt(parameters$variances[g]))
  }
  
  T <- T / rowSums(T) # Normalize to get the responsibilities
  return(T)
}
# M-step function
M.step <- function(X, T, G, modelNames) {
  # Your M-step logic goes here
  # Update the parameters based on the responsibility matrix
  # Example: Update means, variances, and proportions
  parameters <- list()
  
  for (g in 1:G) {
    N_g <- sum(T[, g])
    if (modelNames == "V") {
      # Update for model "V"
      parameters$means[g] <- sum(T[, g] * X) / N_g
      parameters$variances[g] <- sum(T[, g] * (X - parameters$means[g])^2) / N_g
    }else if (modelNames == "E") {
      # Update for model "E" (add your logic here)
      # Example: Update means only, and keep variances fixed
      parameters$means[g] <- sum(T[, g] * X) / N_g
      parameters$variances[g] <- 1  # Fix variances to 1 in this example
    }
    parameters$proportions[g] <- N_g / length(X)
  }
  
  return(parameters)
}

# Function to perform EM algorithm
EM.algorithm <- function(X, G = 2, modelNames = "V", max_iter = 100, tol = 1e-6) {
  # Initialization
  parameters <- initparameters(X, G, modelNames)
  
  for (iter in 1:max_iter) {
    # E-step
    T <- E.step(X, parameters)
    
    # M-step
    new_parameters <- M.step(X, T, G, modelNames)
    
    # Check convergence
    if (sum(abs(unlist(new_parameters) - unlist(parameters))) < tol) {
      break
    }
    
    parameters <- new_parameters
  }
  
  return(list(parameters = parameters, T = T))
}

# Simulate data from Exercise 1
n <- 1000
mu1 <- 0
sigma1 <- 1
pi1 <- 1/3
mu2 <- 4
sigma2 <- 1/2
pi2 <- 2/3
data <- c(rnorm(n * pi1, mean = mu1, sd = sigma1), rnorm(n * pi2, mean = mu2, sd = sigma2))
G <- 2
result <-  EM.algorithm(data, G = 2, modelNames = "E")

# Display the results
print("Estimated Parameters:")
print(result$parameters)
