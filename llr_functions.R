llr = function(x, y, z, omega) {
  fits = sapply(z, compute_f_hat, x, y, omega)
  return(fits)
}


make_weight_matrix <- function(z, x, omega) {
  
  #normal matrix calc
  # weights <- exp(-((x - z)^2) / (2 * omega^2))
  # W <- diag(weights)
  
  # Calculate weights without constructing a full diagonal matrix
  exp(-((x - z)^2) / (2 * omega^2))
  
  # return(W)
}

make_predictor_matrix <- function(x) {
  X <- cbind(1, x)
  return(X)
}


compute_f_hat = function(z, x, y, omega) {
  Wz = make_weight_matrix(z, x, omega)
  X = make_predictor_matrix(x)

  ####------ Speed-test-1 ------####
  # Apply the weights directly to each row of X and y using apply
  weighted_X <- apply(X, 2, function(col) col * Wz)
  weighted_y <- y * Wz
  
  # Compute f_hat using the modified weighted_X and weighted_y
  f_hat <- c(1, z) %*% solve(t(X) %*% weighted_X) %*% (t(weighted_X) %*% weighted_y)
  return(f_hat)
  
  # f_hat = c(1, z) %*% solve(t(X) %*% Wz %*% X) %*% t(X) %*% Wz %*% y
  # return(f_hat)
}

