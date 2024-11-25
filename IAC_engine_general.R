# Create an index for units dynamically
generateIndex <- function(personInfo, features){
  I <- setNames(seq_along(c(personInfo, features)), c(personInfo, features))
  return(I)
}

# Initialize the activations and weights
initializeNetwork <- function(numUnits){
  a <- numeric(numUnits)  # Activations
  w <- matrix(0, numUnits, numUnits)  # Weights
  return(list(a = a, w = w))
}

# Reset all activations
resetActivations <- function(network){
  network$a <- numeric(length(network$a))
  return(network)
}

# Change activation value
changeActivation <- function(network, I, name, value){
  if (!is.null(I[name])) {
    network$a[as.numeric(I[name])] <- value
  } else {
    stop(paste("Error: Name", name, "not found in index I"))
  }
  return(network)
}

# Change weight value
changeWeight <- function(network, I, name1, name2, value){
  if (!is.null(I[name1]) && !is.null(I[name2])) {
    i <- as.numeric(I[name1])
    j <- as.numeric(I[name2])
    network$w[i, j] <- value
    network$w[j, i] <- value
  } else {
    stop(paste("Error: One or both names", name1, "or", name2, "not found in index I"))
  }
  return(network)
}

setWeightsGeneral <- function(network, I, personInfo, features, groupConnections, featureConnections, negativeConnections, weightValue = 0.5){
  # Connect each person to their unique features
  for (person in names(personInfo)){
    if (!is.null(personInfo[[person]])) {
      network <- changeWeight(network, I, person, personInfo[[person]], weightValue)
    }
  }
  
  # Connect people to groups
  for (group in names(groupConnections)){
    for (person in groupConnections[[group]]){
      if (!is.null(I[person]) && !is.null(I[group])) {
        network <- changeWeight(network, I, person, group, weightValue)
      }
    }
  }
  
  # Connect people to features
  for (feature in names(featureConnections)){
    for (person in featureConnections[[feature]]){
      if (!is.null(I[person]) && !is.null(I[feature])) {
        network <- changeWeight(network, I, person, feature, weightValue)
      }
    }
  }
  
  # Add negative connections for mutually exclusive relationships
  for (group in names(negativeConnections)){
    for (name1 in negativeConnections[[group]]){
      for (name2 in negativeConnections[[group]]){
        if (name1 != name2){
          network <- changeWeight(network, I, name1, name2, -weightValue)
        }
      }
    }
  }
  
  return(network)
}


# Run n activation cycles
cycle <- function(network, I, n = 1, max = 1, min = 0, decay = 0.05, rest = 0){
  for (iterations in 1:n){
    newa <- numeric(length(network$a))
    for (i in 1:length(network$a)){
      net <- 0
      for (j in 1:length(network$a)){
        net <- net + network$w[i, j] * network$a[j]
      }
      newa[i] <- network$a[i] + net - decay * (network$a[i] - rest)
      
      # Ensure activation remains within [min, max]
      newa[i] <- max(min(newa[i], max), min)
    }
    network$a <- newa
  }
  return(network)
}


# Show the values of the activations
show <- function(network = network, I = I){
  return(data.frame(Activation = network$a, row.names = names(I)))
}
