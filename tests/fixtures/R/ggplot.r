library(ggplot2);

data <- data.frame(x = 1:100, y = 100:1)

ggplot(data, aes(x, y)) + geom_line(aes(y))
