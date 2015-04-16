emissions <- c(60, 50, 45, 20, 11, 15, 35, 50, 55, 50, 35, 13, 15, 35, 37, 50, 48, 22)
proportion <- c(12, 14, 20, 40, 65, 55, 38, 30, 30, 31, 32, 60, 55, 33, 25, 25, 30, 40)
time <- c(1:18)
data <- cbind.data.frame(emissions, proportion, time)
library(ggplot2)
library(dplyr)
data <- data %>%
  mutate(proportion = (proportion/100))
data <- data %>%
  mutate(labelled = (proportion * emissions))
ggplot(data, aes(x = time, y = labelled)) + geom_point(size = 5, shape = 1) + geom_line(size = 1.5) + ylim(c(0, 80)) + xlab("Sampling point") + ylab("Labelled emission (nmol/gFw/3h)") + ggtitle("Total labelled myrcene") +theme_bw()