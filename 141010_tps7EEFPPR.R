library(plyr)
suppressMessages(library(dplyr))
library(ggplot2)
alldat <- read.delim("141010_tps7EEFPP.txt")
#remove NAs
alldat <- alldat[complete.cases(alldat),]
str(alldat)
tail(alldat)
alldat <- tbl_df(alldat)
glimpse(alldat)


peaks <- names(alldat[,7:16])

ddply(palldat, .variables = "ID", .fun = sum(palldat[,7:16]))

#add unique numeric ID and sum of all product peaks
salldat <- alldat %>%
  mutate(ID = 1:nrow(alldat))%>%
  mutate(peaks = rowSums(temp[, c(7:16)]))


glimpse(salldat)

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
