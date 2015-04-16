conc <- c(0, 1, 3, 5, 7.5, 10, 20, 30)
units <- c(0.002153507, 0.002403347, 0.005670536, 0.007651825, 0.009395717, 0.010749898, 0.012028827, 0.012532533)
std <- c(0.000245914,
         0.000908983,
         0.001019377,
         0.001462926,
         0.0009785,
         0,
         0.002168216,
         0.002469424
)
library(ggplot2)

dat <- cbind.data.frame(conc, units, std)
limits <- aes(ymax = units + std, ymin=units - std)
ggplot(dat, aes(x = conc, y = units)) + geom_point(size = 3) + geom_line(colour = "green") + geom_errorbar(limits)+
  theme_bw() + labs(x = "FPP (uM)", y = "Units produced", title = "Total sesquiterpene signal (SIM)")
