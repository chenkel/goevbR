# TODO: Create Function that gets all the shape for all routes.

NormalizeFreq <- function(tripFreq) {
  # Computes the sample covariance between two vectors.
  #
  # Args:
  #   x: One of two vectors whose sample covariance is to be calculated.
  #   y: The other vector. x and y must have the same length, greater than one,
  #      with no missing values.
  #   verbose: If TRUE, prints sample covariance; if not, not. Default is TRUE.
  #
  # Returns:
  #   The sample covariance between x and y.
  return(as.integer((log2(tripFreq) * 10) ^ (1.2)))
}

GenerateRadius <- function(tripFreq) {
  # Computes the sample covariance between two vectors.
  #
  # Args:
  #   x: One of two vectors whose sample covariance is to be calculated.
  #   y: The other vector. x and y must have the same length, greater than one,
  #      with no missing values.
  #   verbose: If TRUE, prints sample covariance; if not, not. Default is TRUE.
  #
  # Returns:
  #   The sample covariance between x and y.
  
  return(tripFreq)
}

GenerateAlpha <- function(tripFreq) {
  # Computes the sample covariance between two vectors.
  #
  # Args:
  #   x: One of two vectors whose sample covariance is to be calculated.
  #   y: The other vector. x and y must have the same length, greater than one,
  #      with no missing values.
  #   verbose: If TRUE, prints sample covariance; if not, not. Default is TRUE.
  #
  # Returns:
  #   The sample covariance between x and y.
  maxTrip <- max(tripFreq) * 2
  return(tripFreq / maxTrip)
}


GenerateColor <- function(tripFreq) {
  # Computes the sample covariance between two vectors.
  #
  # Args:
  #   x: One of two vectors whose sample covariance is to be calculated.
  #   y: The other vector. x and y must have the same length, greater than one,
  #      with no missing values.
  #   verbose: If TRUE, prints sample covariance; if not, not. Default is TRUE.
  #
  # Returns:
  #   The sample covariance between x and y.
  maxTrip <- max(tripFreq)
  percentFreq <- tripFreq / maxTrip
  # Hex Lowest: 00b8e5
  # Hex Highest: bf0000
  resultRed <- (0 + percentFreq * (191 - 0)) / 255
  resultGreen <- (184 + percentFreq * (0 - 184)) / 255
  resultBlue <- (229 + percentFreq * (0 - 229)) / 255
  
  return(rgb(resultRed, resultGreen, resultBlue))
}
