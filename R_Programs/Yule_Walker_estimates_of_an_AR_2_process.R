set.seed(5)

n <- 150
burnin <- 100

sigma <- 1 # standard deviation of White Noise Z_t

x_full <- arima.sim(model = list(ar = c(1.5, -0.75)), n = burnin + n, sd = sigma)

x <- tail(x_full, n) # last n values

gamma_hat <- function(s, h){
  n0 <- length(s)
  
  x_bar <- mean(s)
  
  a <- s[1:(n0 - abs(h))] - x_bar
  b <- s[(1 + abs(h)):n0] - x_bar
  
  c <- sum(a * b)
  
  return(c / n0)
}

gamma_hat(x, 0); gamma_hat(x, 1); gamma_hat(x, 2)

# rho_hat, for lag 1, 2, ..., 40
rho_hats <- c()

for (i in 1:2) {
  rho_hats[i] <- gamma_hat(x, i) / gamma_hat(x, 0)
}

rho_hats

Gamma_2_hat <- matrix(c(gamma_hat(x, 0), gamma_hat(x, 1),
                        gamma_hat(x, 1), gamma_hat(x, 0)), nrow = 2, byrow = TRUE)

gamma_2_hat <- matrix(c(gamma_hat(x, 1),
                        gamma_hat(x, 2)), nrow = 2, byrow = TRUE)

phi_hat <- solve(Gamma_2_hat) %*% gamma_2_hat
phi_hat

sigma_square_hat <- gamma_hat(x, 0) - t(phi_hat) %*% gamma_2_hat
sigma_square_hat
