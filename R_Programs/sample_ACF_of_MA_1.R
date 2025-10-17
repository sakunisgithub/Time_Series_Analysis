set.seed(7)

# MA(1) process : X_t = Z_t - 0.8 Z_{t-1}
theta <- -0.8

n <- 200
burnin <- 100

sigma <- 1 # standard deviation of White Noise Z_t

x_full <- arima.sim(model = list(ma = theta), n = burnin + n, sd = sigma)

x <- tail(x_full, n) # last n values

# a user-defined function for sample auto-covariance function
gamma_hat <- function(s, h){
  n0 <- length(s)
  
  x_bar <- mean(s)
  
  a <- s[1:(n0 - abs(h))] - x_bar
  b <- s[(1 + abs(h)):n0] - x_bar
  
  c <- sum(a * b)
  
  return(c / n0)
}

# rho_hat, for lag 1, 2, ..., 40
rho_hats <- c()

for (i in 1:40) {
  rho_hats[i] <- gamma_hat(x, i) / gamma_hat(x, 0)
}

rho_hats[1]

any(abs(rho_hats[2:40]) > 1.96 / sqrt(n)) == TRUE


rho_1 <- theta / (1 + theta^2)

1 + 2 * rho_1^2

any(abs(rho_hats[2:40]) > (1.96 * sqrt(1 + 2 * rho_1^2)) / sqrt(n)) == TRUE


1 + 2 * rho_hats[1]^2

any(abs(rho_hats[2:40]) > (1.96 * sqrt(1 + 2 * rho_hats[1]^2)) / sqrt(n)) == TRUE


library(tidyverse)

df <- data.frame(h = 1:40, rho_hat = rho_hats)

df %>%
  ggplot(aes(x = h, y = rho_hat)) +
  geom_hline(yintercept = 0, 
             col = "black", linewidth = 1) +
  geom_hline(yintercept = -1.96 / sqrt(n), 
             col = "red", linewidth = 1, linetype = "dashed") +
  geom_hline(yintercept = 1.96 / sqrt(n), 
             col = "red", linewidth = 1, linetype = "dashed") +
  geom_hline(yintercept = -1.96 * sqrt(1 + 2 * rho_hats[1]^2) / sqrt(n), 
col = "#22C93C", linewidth = 1, linetype = "dotdash") +
  geom_hline(yintercept = 1.96 * sqrt(1 + 2 * rho_hats[1]^2) / sqrt(n), 
col = "#22C93C", linewidth = 1, linetype = "dotdash") +
  geom_segment(aes(x = h, xend = h, y = 0, yend = rho_hat), linewidth = 1, col = 'blue') +
  geom_point(shape = 20, stroke = 1.5) +
  scale_x_continuous(breaks = c(1, seq(5, 40, 5))) +
  labs(x = "Lag h", y = "Sample ACF", title = "Sample ACF as a Function of Lag h")

ggsave(path = "D:\\Users\\Documents\\Time_Series_Analysis\\figures",
       width = 10,
       height = 8,
       device='png',
       dpi=500,
       filename = "sample_ACF_of_MA_1.png",
       units = "in")
