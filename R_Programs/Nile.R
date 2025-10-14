Nile

df <- data.frame(year = as.integer(time(Nile)),
                 flow = as.numeric(Nile))

library(tidyverse)

df %>%
  ggplot(aes(x = year, y = flow)) +
  geom_line(linewidth = 1, col = "blue") +
  geom_point(size = 2, col = "red") +
  labs(x = "Year", 
       y = expression("Flow (10"^8*" cubic meters)"), 
       title = "Annual Flow of the River Nile")

ggsave(path = "D:\\Users\\Documents\\Time_Series_Analysis\\figures",
       width = 10,
       height = 7,
       device='png',
       dpi=500,
       filename = "Nile.png",
       units = "in")
