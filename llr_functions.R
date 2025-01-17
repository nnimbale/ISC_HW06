llr = function(x, y, z, omega) {
  fits = sapply(z, compute_f_hat, x, y, omega)
  return(fits)
}


make_weight_matrix <- function(z, x, omega) {
  weights <- exp(-((x - z)^2) / (2 * omega^2))
  W <- diag(weights)
  return(W)
}

make_predictor_matrix <- function(x) {
  X <- cbind(1, x)
  return(X)
}


compute_f_hat = function(z, x, y, omega) {
  Wz = make_weight_matrix(z, x, omega)
  X = make_predictor_matrix(x)
  f_hat = c(1, z) %*% solve(t(X) %*% Wz %*% X) %*% t(X) %*% Wz %*% y
  return(f_hat)
}

