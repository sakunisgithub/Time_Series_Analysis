AirPassengers

df <- data.frame(time_point = as.numeric(time(AirPassengers)),
                 value = as.numeric(AirPassengers))

library(tidyverse)

df %>%
  ggplot(aes(x = time_point, y = value)) +
  geom_line(linewidth = 1, col = "blue") +
  geom_point(size = 2, col = "red") +
  labs(x = "Year", y = "AirPassengers", title = "Monthly Airline Passenger Numbers 1949-1960")


ggsave(path = "D:\\Users\\Documents\\Time_Series_Analysis\\figures",
       width = 10,
       height = 7,
       device='png',
       dpi=500,
       filename = "AirPassengers.png",
       units = "in")

# Simple Moving Average of order m = 12

m <- 12
fltr <- c(1/2, rep(1, m-1), 1/2)/m

sma_values <- stats::filter(AirPassengers, filter = fltr)

sma_values

df1 <- data.frame(time_points = as.numeric(time(AirPassengers)),
                  sma = sma_values)

df1 %>%
  ggplot(aes(x = time_points, y = sma)) +
  geom_line(linewidth = 1, col = "blue") +
  labs(x = "Year", y = "SMA(12) of AirPassengers",
       title = "SMA of order 12 of Monthly Airline Passenger Numbers 1949-1960")

ggsave(path = "D:\\Users\\Documents\\Time_Series_Analysis\\figures",
       width = 10,
       height = 7,
       device='png',
       dpi=500,
       filename = "AirPassengers_SMA_12.png",
       units = "in")
