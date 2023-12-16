# Function to initialize parameters
initparameters <- function(X, G, modelNames) {
  # Your initialization logic goes here
  # You need to initialize means, variances, and proportions for each component
  # For simplicity, let's initialize means randomly, variances to 1, and equal proportions
  means <- runif(G)
  variances <- rep(1, G)
  proportions <- rep(1/G, G)
  
  parameters <- list(means = means, variances = variances, proportions = proportions)
  
  return(parameters)
}

# Function for the E-step
E.step <- function(X, parameters) {
  # Your E-step logic goes here
  # Calculate the probabilities for each data point belonging to each component
  # Using the probability density function of a normal distribution
  
  # For simplicity, let's assume a univariate normal distribution
  likelihoods <- sapply(1:length(parameters$means), function(j) {
    dnorm(X, mean = parameters$means[j], sd = sqrt(parameters$variances[j]))
  })
  
  # Calculate the responsibilities
  responsibilities <- t(likelihoods) * parameters$proportions
  responsibilities <- responsibilities / rowSums(responsibilities)
  
  return(responsibilities)
}
# Function for the M-step
M.step <- function(X, responsibilities, G, modelNames) {
  # Your M-step logic goes here
  # Update the parameters based on the responsibilities
  
  # Update means
  means <- colSums(responsibilities * X) / colSums(responsibilities)
  
  # Update variances
  variances <- colSums(responsibilities * (X - means)^2) / colSums(responsibilities)
  
  # Update proportions
  proportions <- colSums(responsibilities) / sum(responsibilities)  # Update to use sum(responsibilities)
  
  parameters <- list(means = means, variances = variances, proportions = proportions)
  
  return(parameters)
}


# Function for the EM algorithm
EM <- function(X, G = 2, modelNames = "V") {
  # Initialize parameters
  parameters <- initparameters(X, G, modelNames)
  
  # EM algorithm loop
  max_iter <- 100  # Set the maximum number of iterations
  for (iter in 1:max_iter) {
    # E-step
    responsibilities <- E.step(X, parameters)
    
    # M-step
    parameters <- M.step(X, responsibilities, G, modelNames)
  }
  
  # Return the final parameters and responsibilities
  return(list(parameters = parameters, T = max_iter))
}

# Simulate data
set.seed(123)
z <- rmultinom(1000, 1, prob = c(1/3, 2/3))
Effectif <- rowSums(z)
x <- c(rnorm(Effectif[1]), rnorm(Effectif[2], mean = 8, sd = 1))

result <- EM(x, G = 2, modelNames = "V")


# Afficher la moyenne pour chaque composante
for (i in 1:length(mean_values)) {
  cat("Moyenne pour la composante", i, ":", mean_values[i], "\n")
}


# Display the result
print(result)
