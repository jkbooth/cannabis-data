library(plyr)
library(dplyr)
library(ggplot2)

#make a design frame
des <- data.frame(time = as.factor(rep(c(1, 2), times = 6)),
                  plant = rep(c("A", "B"), each = 6),
                  rep = as.factor(rep(1:3, each = 2)),
                  sample = c("A11", "A12", "A21", "A22", "A31", "A32", "B11", "B12", "B21", "B22", "B31", "B32"))
des$group <- paste(des$plant, des$rep)

#load the data
load <- function(file, samp){
  dat <- read.delim(file, header = FALSE, sep = "\t")
  dat <- dat[4:nrow(dat), 1:2]
  colnames(dat) <- c("RT", "area")
  dat$area <- as.numeric(levels(dat$area))[dat$area]
  dat$RT <- as.factor(round(as.numeric(levels(dat$RT))[dat$RT] ,2))
  dat$sample <- samp
  return(dat)
}

A11 <- load("mtbe_extracts//plant1rep11.txt", "A11")
A12 <- load("mtbe_extracts//plant1rep12.txt", "A12")
A21 <- load("mtbe_extracts/plant1rep21.txt", "A21")
A22 <- load("mtbe_extracts//plant1rep22.txt", "A22")
A31 <- load("mtbe_extracts//plant1rep31.txt", "A31")
A32 <- load("mtbe_extracts//plant1rep32.txt", "A32")
B11 <- load("mtbe_extracts//plant2rep11.txt", "B11")
B12 <- load("mtbe_extracts//plant2rep12.txt", "B12")
B21 <- load("mtbe_extracts//plant2rep21.txt", "B21")
B22 <- load("mtbe_extracts//plant2rep22.txt", "B22")
B31 <- load("mtbe_extracts//plant2rep31.txt", "B31")
B32 <- load("mtbe_extracts//plant2rep32.txt", "B32")

scale.dat <- function(x, s){
  dat <- x %>%
    filter(RT != "6.94") %>%
    mutate(area = area/s)
    droplevels
  return(dat)
}

A11.s <- scale.dat(A11, 0.1)
A12.s <- scale.dat(A12, 0.1)
A21.s <- scale.dat(A21, 0.09)
A22.s <- scale.dat(A22, 0.09)
A31.s <- scale.dat(A31, 0.16)
A32.s <- scale.dat(A32, 0.16)
B11.s <- scale.dat(B11, 0.07)
B12.s <- scale.dat(B12, 0.07)
B21.s <- scale.dat(B21, 0.11)
B22.s <- scale.dat(B22, 0.11)
B31.s <- scale.dat(B31, 0.10)
B32.s <- scale.dat(B32, 0.10)

alldat <- rbind(A11, A12, A21, A22, A31, A32, B11, B12, B21, B22, B31, B32)
nalldat <- rbind(A11.scale, A12.scale, A21.s, A22.s, A31.s, A32.s,
                 B11.s, B12.s, B21.s, B22.s, B31.s, B32.s)


#pull out variables of interest
ibbs <- alldat %>%
  filter(RT == "6.94") %>%
  droplevels

sd(ibbs$area)

nibbs <- alldat %>%
  filter(RT != "6.94") %>%
  droplevels



#examine whole sample differences
tots <- summarise(group_by(nalldat, sample), sum = sum(area))
tots <- cbind(des, tots = tots$sum)

ggplot(tots, aes(x = group, y = tots, colour = time)) + geom_point()
ggplot(tots, aes(x = plant, y = tots, colour = rep)) + geom_point()


make.terpframe <- function(terpene, class, time){
  time <- as.character(time)
  terp <- nalldat[(nalldat$RT %in% time), ]
  terp <- cbind(des, area = terp$area)
  terp <- terp %>%
    mutate(scaled = area/(tots$tots))
  terp$id <- terpene
  terp$class <- class
  return(terp)
}

#alpha pinene
(apinene <- rbind(nalldat[(nalldat$RT %in% "5.93"), ],nalldat[(nalldat$RT %in% "5.92"), ]))
(apinene <- apinene[match(des$sample, apinene$sample), ])
apinene <- cbind(des, area = apinene$area)
apinene$scaled <- apinene$area/tots$tots
apinene$id <- "apinene"
apinene$class <- "mono"

ggplot(apinene, aes(x = plant, y = scaled)) + geom_point()

#beta pinene
bpinene <- make.terpframe("bpinene", "mono", 6.56)
ggplot(bpinene, aes(x = plant, y = scaled)) + geom_point()

#myrcene
myrcene <- nalldat[(nalldat$RT %in% "6.66"), ]
myrcene[12,3] <- "A11"
(myrcene <- myrcene[match(des$sample, myrcene$sample), ])
myrcene <- cbind(des, area = myrcene$area)
myrcene$scaled <- myrcene$area/tots$tots
myrcene$id <- "myrcene"
myrcene$class <- "mono"
ggplot(myrcene, aes(x = plant, y = scaled)) + geom_point()

#limonene
limonene <- make.terpframe("limonene", "mono", 7.21)
ggplot(limonene, aes(x = plant, y = scaled)) + geom_point()

#3carene
threecarene <- make.terpframe("threecarene", "mono", 7.38)
ggplot(threecarene, aes(x = plant, y = scaled)) + geom_point()

#caryophyllene
bcaryophyllene <- make.terpframe("bcaryophyllene", "sesqui", 11.33)
ggplot(bcaryophyllene, aes(x = plant, y = scaled)) + geom_point()

#farnesene
farnesene <- make.terpframe("farnesene", "sesqui", 11.46)
ggplot(farnesene, aes(x = plant, y = scaled)) + geom_point()

#ahumulene
ahumulene <- make.terpframe("ahumulene", "sesqui", 11.64)
ggplot(ahumulene, aes(x = plant, y = scaled)) + geom_point()

#cbd
cbd <- make.terpframe("cbd", "cannabinoid", 11.33)
ggplot(cbd, aes(x = plant, y = scaled)) + geom_point()

#thc
thc <- make.terpframe("thc", "cannabinoid", 11.64)
ggplot(thc, aes(x = plant, y = scaled)) + geom_point()

#cbg
cbg <- make.terpframe("cbg", "cannabinoid", 18.89)
ggplot(cbg, aes(x = plant, y = scaled)) + geom_point()


#put it all together

itall <- rbind(apinene, bpinene,myrcene,limonene,threecarene,ahumulene,bcaryophyllene,farnesene,cbd,cbg,thc)

ggplot(itall, aes(x = plant, y = scaled, colour = class)) +
  stat_summary(fun.y = mean, geom = "point") + 
  theme_bw() + labs(x = "Plant", y = "Proportion of total peak area")

ggplot(itall, aes(x = plant, y = scaled, colour = id)) +
  geom_point() + facet_wrap(~class, scales = "free") +
  theme_bw() + labs(x = "Plant", y = "Proportion of total peak area")

ggplot(itall, aes(x = plant, y = area, colour = id)) +
  geom_point() + facet_wrap(~class, scales = "free") +
  theme_bw() + labs(x = "Plant", y = "total peak area")

ggplot(tots, aes(x = time, y = tots)) + geom_boxplot() +
  geom_jitter(aes(colour = plant), position = position_jitter(width = 0.1)) +
  theme_bw() + labs(x = "Time (hours)", y = "TIC minus standard")
