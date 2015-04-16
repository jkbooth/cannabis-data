library(ggplot2)
library(ggthemes)
monoterps <- c(39.12, 55.46, 60.41, 65.42, 70.55)
sesquiters <- c(60.88, 44.55, 39.59, 34.58, 29.45)
#enter the data

str(monoterps)

chart <- data.frame(monoterpenes = monoterps,
                    sesquiterpenes = sesquiters,
                    week = 1:5)


#turn it into a dataframe

str(chart)

ggplot(data = chart, aes(x = week, y = monoterpenes, group = 1)) + geom_line() + geom_point()

chart2 <- data.frame(week = 1:5,
                     percent = c(monoterps,sesquiters), 
                     legend = c(rep("monoterpenes", 5), rep("sesquiterpenes", 5)))

plot <- ggplot(data = chart2,
               aes(x = week, y = percent, group = legend, color = legend, ymax = 100, )) + geom_line(size = 1.5) +
  geom_point(size = 4)

plot <- plot + ggtitle("Floral Terpene Content")
plot + xlab("Weeks Post Flowering") + ylab("% of total terpenes") + theme_few() + scale_colour_gdocs()

#exports the most recent plot to file:
ggsave("potter_terpene_plot.png")

cans <- c(8.17, 9.18, 9.16, 9.17, 8.55)
candat <- data.frame(week = 1:5, CanContent = cans)

plot2 <- ggplot(data = candat, aes(x = week, y = CanContent))
plot2 + geom_point(size = 4) + theme_bw() + geom_line(size = 1.5) + xlab("Weeks Post Flowering") +
  ylab("Cannabinoid content (%w/w)") + ggtitle("Cannabinoid content")
ggsave("potter_cannabinoid_plot.png")
