library(reshape2)
library(plyr)
library(dplyr)
library(ggplot2)
?read.delim
str(dat)

colnames(dat)
age.confidence <- t.test(dat$Age, dat$How.confident.are.you.)
t.test(dat$Age, dat$How.confident.are.you., var.equal = TRUE)
plot(dat$Age, dat$How.confident.are.you.)
summary(age.confidence)
age.confidence

p.age.confidence <- ggplot(dat, aes(x = Age, y = How.confident.are.you)) + geom_point()
p.age.confidence
cor(dat.clean$Age, dat.clean$How.confident.are.you.)
dat.clean <- na.omit(dat)
ggplot(dat, aes(x = Age, y = How.confident.are.you.)) + geom_point()

dat.clean <- dat.clean %>%
  filter(Age != 99)
colnames(dat)

plot(dat.1removed$Age, dat.1removed$How.confident.are.you.)
ggplot(dat.1removed, aes(x = Age, y = How.confident.are.you.)) + geom_point()

t.test(dat.1removed$Age, dat.1removed$How.confident.are.you.)
cor(dat.1removed$Age, dat.1removed$How.confident.are.you.)

testfun.age <- function(x) {
  ttest <- t.test(as.numeric(x), dat.clean$Age)
  cortest <- cor(as.numeric(x), dat.clean$Age)
  return(list(ttest, cortest))
}

(age.impulse <- testfun(dat.clean$Age, dat.clean$How.impulsive.are.you.))
(age.individual <- testfun.age(dat.clean$How.individualistic.are.you.))
(age.dominant <- testfun.age(dat.clean$How.dominant.are.you.))
(age.ambition <- testfun.age(dat.clean$How.ambitious.are.you.))
(age.libido <- testfun.age(dat.clean$How.libidinous.are.you.))
(age.altruism <- testfun.age(dat.clean$How.altruistic.are.you.))
(age.orientation <- testfun.age(dat.clean$Sexual.orientation))
(age.awesome <- testfun.age(dat.clean$Which.are.most.awesome.))
plot(dat.clean$Age, dat.clean$Which.are.most.awesome.)

colnames(dat.clean)
levels(dat.clean$Which.are.most.awesome.)




dat.mat <- na.omit(as.matrix(dat[, 3:10]))
str(dat.mat)
num.cor <- cor(dat.mat)
diag(num.cor) <- NA
library(pheatmap)
pheatmap(num.cor)

dat.mat2 <- as.matrix(dat.clean[2:ncol(dat.clean)])
View(dat.mat2)

library(car)
dat.num <- as.matrix(sapply(dat.clean, as.numeric))
dat.num <- dat.num[, 2:ncol(dat.num)]
allcor <- cor(dat.num)
diag(allcor) <- NA
pheatmap(allcor)

ggplot(dat.clean, aes(x = Which.is.most.awesome..2, factor = Gender)) + 
  geom_histogram(aes(fill = Gender)) + theme_light()

ggplot(dat.clean, aes(x = Which.are.most.awesome..2, factor = Gender)) +
  geom_bar(aes(fill = Gender)) + theme_light()
ggplot(dat.clean, aes(x = Which.are.most.awesome..2, factor = Gender)) +
  geom_bar(aes(fill = Gender)) + theme_light() + facet_wrap(~Sexual.orientation)
ggplot(dat.clean, aes(x = Which.are.most.awesome..4)) + geom_bar(aes(fill = Gender)) + theme_light()
ggplot(dat.clean, aes(x = Which.are.most.awesome..4)) + geom_bar(aes(fill = Which.are.most.awesome.)) + theme_light()
ggplot(dat.clean, aes(x = Sexual.orientation, y = How.confident.are.you.))  +
  geom_violin() + geom_jitter(aes(colour = Gender))
ggplot(dat.clean, aes(x = How.ambitious.are.you., y = How.confident.are.you.)) + geom_jitter() + geom_smooth(method = "lm", se = FALSE)
ggplot(dat.clean, aes(x = How.dominant.are.you., y = How.confident.are.you.)) + geom_jitter() + geom_smooth(method = "lm", se = FALSE)
ggplot(dat.clean, aes(x = Which.are.most.awesome..1, y = How.dominant.are.you.)) + geom_boxplot() + geom_jitter() + theme_light()
ggplot(dat.clean, aes(x = Gender, y = How.confident.are.you.)) + geom_jitter()

summary(dat.clean)


#working with factors
colnames(dat.clean)
summary(lm(Age ~ factor(How.dominant.are.you.), data = dat.clean))
testfun.age <- function(x) {
  xn <- factor(x)
  res <- summary(lm(0 + Age ~ xn, data = dat.clean))
  return(res)
}
age.batman.superman <- testfun.age(dat.clean$Who.is.most.awesome.)
ggplot(dat.clean, aes(x = Who.is.most.awesome., y = Age)) + geom_violin() + geom_jitter()
age.nymphs.princesses.witches <- testfun.age(dat.clean$Which.are.most.awesome..3)
ggplot(dat.clean, aes(x = Which.are.most.awesome..3, y = Age)) + geom_violin() + geom_jitter()
testfun.age(dat.clean$Which.is.most.awesome..2)

with(dat.clean, prop.table(table(Which.are.most.awesome..4, Gender)))
with(dat.clean, addmargins(prop.table(table(Which.are.most.awesome..4, Which.is.most.awesome..2))))

testfun.elf.dwarf <- function(x) {
  xn <- factor(x)
  res <- chisq.test(dat.clean$Which.are.most.awesome..4, xn)
  return(res)
}
elfdwarf.mythology <- testfun.elf.dwarf(dat.clean$Which.is.most.awesome..2)
elfdwarf.vampwere <- testfun.elf.dwarf(dat.clean$Which.are.most.awesome.)
elfdwarf.nymphs.princesses.witches <- testfun.elf.dwarf(dat.clean$Which.are.most.awesome..3)


testfun.npw <- function(x) {
  xn <- factor(x)
  res <- chisq.test(dat.clean$Which.are.most.awesome..3, xn)
  return(res)
}
npw.batsuper <- testfun.npw(dat.clean$Who.is.most.awesome.)
ggplot(dat.clean, aes(x = Which.are.most.awesome..3, factor = Who.is.most.awesome.)) + 
  geom_bar(aes(fill = Who.is.most.awesome.), position = position_dodge())
#not a strong difference, but people who like witches are slightly more likely to prefer batman (probably a function of age)

ggplot(dat.clean, aes(x = Which.are.most.awesome..4, factor = Which.are.most.awesome..3)) + 
  geom_bar(aes(fill = Which.are.most.awesome..3), position = position_dodge())
testfun.dominance <- function(x) {
  xn <- factor(x)
  res <- chisq.test(factor(dat.clean$Which.are.most.awesome..3), xn)
  return(res)
}
dom.batsuper <- testfun.dominance(dat.clean$Who.is.most.awesome.)
dom.elfdwarf <- testfun.dominance(dat.clean$Which.are.most.awesome..4)
testfun.dominance(dat.clean$Which.is.most.awesome..2)

summary(with(dat.clean, lm(Age ~ Sexual.orientation*Which.are.most.awesome..3)))
