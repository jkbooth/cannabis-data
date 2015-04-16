library(plyr)
suppressMessages(library(dplyr))
library(ggplot2)
library(reshape)
alldat <- read.delim("141010_tps7EEFPP.txt")
#remove NAs
alldat <- alldat[complete.cases(alldat),]
str(alldat)
tail(alldat)

peakns <- names(alldat[,7:16])

#add unique numeric ID and sum of all product peaks
salldat <- alldat %>%
  mutate(ID = 1:nrow(alldat))%>%
  mutate(peaks = rowSums(alldat[, c(7:16)]))


ibbtime <- ggplot(salldat, aes(x = mins, y = IBB))
ibbtime + geom_point() + geom_smooth(method = "lm")
peakstime <- ggplot(salldat, aes(x = mins, y = sums))
peakstime + geom_point()

treat1 <- salldat %>%
  filter(assay.ID == 1)
treat2 <- salldat %>%
  filter(assay.ID == 2)

ggplot(salldat, aes(x = mins, y = IBB)) + geom_point() + geom_smooth(se = F, method = "lm") +
  facet_wrap(facets = "assay.ID") + ggtitle("IBB") + theme_bw() + ylab("IBB peak area")
ggplot(salldat, aes(x = mins, y = peaks)) + geom_point(aes(colour = IBB)) + geom_smooth(se = F) +
  facet_wrap(facets = "assay.ID") + theme_bw() + ggtitle("total peak area")

ibblm <- lm(data = alldat, IBB ~ mins)
ibblm
summary(ibblm)

nalldat <- salldat %>%
  mutate(normal = (peaks / IBB) * 100)
nalldat %>%
  select(peaks, IBB, normal)
ggplot(nalldat, aes(x = mins, y = normal)) + geom_point() + geom_smooth(se = F) +
  facet_wrap(facets = "assay.ID") + theme_bw() + ggtitle("normalized peak area")

ggplot(salldat, aes(x = mins, y = p13.042)) + geom_point(aes(colour = assay.ID))

testpeak <- "p10.772"
along <- function(peak) {
  return(ggplot(salldat, aes_string(x = "mins", y = peak)) + geom_point(aes(colour = assay.ID)))
}
along("p11.422")
subs <- subset(salldat, select = c("ID", "assay.ID", peakns))
subs

subs$assay.ID <- as.character(subs$assay.ID)
subst <- t(subs)

mdf <- melt(salldat, ID = c("ID", "mins", "ID"), measure.vars = peakns)

bypeak <- ggplot(mdf, aes(x = mins, y = value, group = variable, colour = variable)) +
  geom_point() + geom_point() + geom_smooth(se = F)  + theme_bw() + ylab("peak area")
bypeak + geom_point(aes(x = mdf$mins, y = mdf$peaks)) +
  geom_smooth(aes(x = mdf$mins, y = mdf$peaks), size = 3, se = F)
allplot <- ggplot(mdf, aes(x = mins, y = peaks)) + geom_point() + geom_smooth(aes(size = 3), se = F)
allplot
