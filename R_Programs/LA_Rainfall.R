# install.packages('TSA')
library(TSA)
data("larain")

str(larain)

larain

library(tidyverse)

df <- data.frame(time_point = as.integer(time(larain)),
                 rainfall = as.numeric(larain))

View(df)

df %>%
  ggplot(aes(x = time_point, y = rainfall)) +
  geom_line(linewidth = 1, col = "blue") +
  geom_point(col = "red", size = 2) +
  labs(x = "Year", y = "Rainfall (Inches)", title = "Annual Rainfall in Los Angeles")

ggsave(path = "D:\\Users\\Documents\\Time_Series_Analysis\\figures",
       width = 10,
       height = 7,
       device='png',
       dpi=500,
       filename = "LA_rainfall.png",
       units = "in")
