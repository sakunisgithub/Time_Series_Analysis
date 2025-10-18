set.seed(10)

# data simulation
theta <- -0.8
sigma <- 1

n <- 100
burnin <- 200

x_full <- arima.sim(model = list(ma = theta), n = burnin + n, sd = sigma)

x <- tail(x_full, n)

library(tidyverse)

df1 <- data.frame(t = 1:100, x_t = x)

df1 %>%
  ggplot(aes(x = t, y = x_t)) +
  geom_hline(yintercept = 0, col = "black", linewidth = 1) +
  geom_line(linewidth = 1, col = "blue") +
  geom_point(size = 2, col = "red", shape = 15) +
  scale_y_continuous(breaks = -4:4) +
  labs(x = "t", y = expression(X[t]), 
       title = expression("Simulation of MA(1) with " * theta == -0.8))

ggsave(path = "D:\\Users\\Documents\\Time_Series_Analysis\\figures",
       width = 11,
       height = 8,
       device='png',
       dpi=500,
       filename = "simulation_of_MA_1_with_theta_negative_0.8.png",
       units = "in")

# estimation of rho(1)
gamma_hat <- function(s, h){
  n0 <- length(s)
  
  x_bar <- mean(s)
  
  a <- s[1:(n0 - abs(h))] - x_bar
  b <- s[(1 + abs(h)):n0] - x_bar
  
  c <- sum(a * b)
  
  return(c / n0)
}

rho_1_hat <- gamma_hat(x, 1) / gamma_hat(x, 0)
rho_1_hat

# residual from X_{t} - rho_1_hat * X_{t-1}
resid_1 <- x[3:n] - rho_1_hat * x[2:(n-1)]
length(resid_1)

# residual from X_{t-2} - rho_1_hat * X_{t-1}
resid_2 <- x[1:(n-2)] - rho_1_hat * x[2:(n-1)]
length(resid_2)

df2 <- data.frame(x = resid_1, y = resid_2)

df2 %>%
  ggplot(aes(x = x, y = y)) +
  geom_point(size = 2, col = "red") +
  geom_smooth(formula = 'y ~ x', method = "lm") +
  labs(x =  bquote(X[t] - hat(rho) * "(" * .(1) * ")" * X[t-1]),
       y =  bquote(X[t-2] - hat(rho) * "(" * .(1) * ")" * X[t-1]),
       title = expression("Simulation of " * alpha(2) * " in MA(1) with " * theta == -0.8))

ggsave(path = "D:\\Users\\Documents\\Time_Series_Analysis\\figures",
       width = 10,
       height = 10,
       device='png',
       dpi=500,
       filename = "simulation_of_alpha_2_in_MA_1_with_theta_negative_0.8.png",
       units = "in")

cor(resid_1, resid_2) # sample alpha(2), theoretical alpha(2) = -0.312
