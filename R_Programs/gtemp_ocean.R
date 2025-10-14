# install.packages('astsa')

library(astsa)
data("gtemp_ocean") 
# Global mean ocean temperature deviations (from 1951-1980 average), 
# measured in degrees centigrade, 
# for the years 1850-2023

str(gtemp_ocean)

df <- data.frame(time_points = as.numeric(time(gtemp_ocean)),
                 values = as.numeric(gtemp_ocean))

library(tidyverse)

df %>%
  ggplot(aes(x = time_points, y = values)) +
  geom_line(linewidth = 1, col = "blue") +
  geom_point(size = 2, col = "red") +
  labs(x = "Year", y = "Deviation in ocean temperature",
       title = "Deviation in ocean temperature from 1951-1980 average")

ggsave(path = "D:\\Users\\Documents\\Time_Series_Analysis\\figures",
       width = 10,
       height = 7,
       device='png',
       dpi=500,
       filename = "gtemp_ocean.png",
       units = "in")

# Simple Moving Average of order 9

m <- 9
fltr <- rep(1/m, m)

sma_values <- stats::filter(gtemp_ocean, filter = fltr)

df1 <- data.frame(time_points = as.numeric(time(sma_values)),
                  values = as.numeric(sma_values))

df1 %>%
  ggplot(aes(x = time_points, y = values)) +
  geom_line(linewidth = 1, col = "blue") +
  geom_point(size = 2, col = "red") +
  labs(x = "Year", y = "SMA(9) of deviation in ocean temperature",
       title = "SMA(9) of Deviation in ocean temperature from 1951-1980 average")

ggsave(path = "D:\\Users\\Documents\\Time_Series_Analysis\\figures",
       width = 10,
       height = 7,
       device='png',
       dpi=500,
       filename = "gtemp_ocean_sma_9.png",
       units = "in")

# Detrend

i_t_hat <- df$values - df1$values

df2 <- data.frame(time_points = as.numeric(time(sma_values)),
                  residuals = i_t_hat)

df2 %>%
  ggplot(aes(x = time_points, y = residuals)) +
  geom_line(linewidth = 1, col = "blue") +
  geom_point(size = 2, col = "red") +
  labs(x = "Year", y = "Detrend component",
       title = "Detrend Component in Deviation in ocean temperature from 1951-1980 average after SMA(9)")

ggsave(path = "D:\\Users\\Documents\\Time_Series_Analysis\\figures",
       width = 10,
       height = 7,
       device='png',
       dpi=500,
       filename = "gtemp_ocean_detrend.png",
       units = "in")

df2 %>%
  ggplot(aes(x = residuals)) +
  geom_histogram(bins = 13, fill = "#0FD8F0", color = "black", linewidth = 1) +
  labs(x = "Residuals", y = "Frequency", title = "Histogram of Residuals")

ggsave(path = "D:\\Users\\Documents\\Time_Series_Analysis\\figures",
       width = 10,
       height = 7,
       device='png',
       dpi=500,
       filename = "gtemp_ocean_residual_histogram.png",
       units = "in")
