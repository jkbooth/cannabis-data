library(ggplot2)
library(plyr)
library(dplyr)
library(reshape2)
library(edgeR)
library(limma)
library(ggthemes)

dat <- read.table("msu_est_num.txt", header = TRUE, row.names = 1)
dat.filt <- dat[(rowMeans(dat) > 1), ]

dat.large <- read.delim("csa.matrix.FPKM.vf.082511.txt", header = TRUE, row.names = 1, sep = "\t", stringsAsFactors = FALSE)
des <- read.table("csa.matrix.design.txt", header = TRUE)
head(rownames(dat))

prepareData <- function(v) {
  stopifnot(class(v) == "character")
  subs <- t(dat[v,])
  count <- as.vector(subs)
  gene <- as.factor(rep(colnames(subs), each = nrow(subs)))
  d.f <- cbind(des, count, gene)
  return(d.f)
}

samp <- sample(rownames(dat), size = 3)

test <- prepareData(samp)
talldat <- prepareData(rownames(dat))
talldat.filt <- prepareData(rownames(dat.filt))

ggplot(talldat, aes(x = log2(count))) + geom_density()
ggplot(talldat.filt, aes(x = log2(count))) + geom_density()

dat.log2 <- data.frame(sapply(dat, log2))
talldat.log <- mutate(talldat, count = log2(talldat$count))

ggplot(talldat.log, aes(x = count, colour = ID)) + geom_density() + facet_wrap(~tissue)


#normalization

dat.dge <- DGEList(dat)
dat.dge <- calcNormFactors(dat.dge)

#multiply data by norm factors
dat.norm <- sweep(dat,MARGIN=2,dat.dge$samples$norm.factors,`*`)


#make a skinny normalized df
subs <- t(dat.norm[rownames(dat.norm),])
count <- as.vector(subs)
gene <- as.factor(rep(colnames(subs), each = nrow(subs)))
talldat.norm <- cbind(des, count, gene)

ggplot(talldat.norm, aes(x = log2(count), colour = ID)) + geom_density() + facet_wrap(~tissue)


#function
dat.large$UNIFIED.FUNCTIONAL.ANNOTATION <- tolower(dat.large$UNIFIED.FUNCTIONAL.ANNOTATION)
dat.large$UNIFIED.FUNCTIONAL.ANNOTATION[200:300]

got.id <- c("9717_iso_1_len_1296", "4168_iso_1_len_1266", "5486_iso_1_len_407")
thcas.id <- c("3815_iso_2_len_1820")

toMatch <- c("terpene","terpenoid", "secondary", "tps", "mevalonate", "hmg", "isoprene",
             "cdp", "hmgr", "hmgs", "aact", "mevalonate", "dxr", "dxs")
(matchedGenes <- unique(grep(paste(toMatch, collapse = "|"),
                            dat.large$UNIFIED.FUNCTIONAL.ANNOTATION, value = TRUE)))
matchedId <-  dat[(dat.large$UNIFIED.FUNCTIONAL.ANNOTATION %in% matchedGenes), ]
matched.fun <- cbind.data.frame(id = rownames(matchedId), func = dat.large[(dat.large$UNIFIED.FUNCTIONAL.ANNOTATION %in% matchedGenes), 1])

terphits.skinny <- prepareData(rownames(matchedId))
terphits.skinny$func <- rep(matched.fun$func, each = ncol(dat.filt))

ggplot(terphits.skinny, aes(x = tissue, y = log2(count), colour = gene))+ geom_point() +
  facet_wrap(~ func, scales = "free") +
  theme_few() + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") +
  labs(x = "Tissue", y = "log2 FPKM", title = "Expression of isoprenoid biosynthesis genes")


#limma
mat <- model.matrix(~0 + tissue, des)
dat.voom <- voom(counts = dat.norm, design = mat, plot = TRUE)
mat
fit <- lmFit(dat.voom, design = mat)
ebfit <- eBayes(fit)
colnames(ebfit$coefficients)

#flower de
res.flower <- topTable(ebfit, coef = 2, n = Inf)
fdr.flower <- subset(res.flower, adj.P.Val < 1e-5)


plot(density(res.flower$P.Value))
plot(density(res.flower$adj.P.Val))

head(res.flower)

(got.de <- unique(grep(paste(got.id, collapse = "|"),
                             rownames(res.flower), value = TRUE)))

(res.flower.got <- res.flower[got.de, ])

res.flower.got.skin <- prepareData(rownames(res.flower.got))

ggplot(res.flower.got.skin, aes(x = tissue, y = log2(count), colour = gene)) + geom_point() +
  facet_wrap(~ gene, scales = "free") +
  theme_few() + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") +
  labs(x = "Tissue", y = "log2 FPKM", title = "Expression of prenyltransferase")

#de for all cannabinoid-producing tissues
res.cantis <- topTable(ebfit, coef = c(1:3, 7), n = Inf)
fdr.cannis <- subset(res.cantis, adj.P.Val < 1e-5)

plot(density(res.cantis$P.Value))
plot(density(res.cantis$adj.P.Val))

head(fdr.cannis)
(got.de <- unique(grep(paste(got.id, collapse = "|"),
                       rownames(res.cantis), value = TRUE)))
(res.can.got <- res.cantis[got.de, ])

(thcas.de <- unique(grep(paste(thcas.id, collapse = "|"),
                                   rownames(res.flower), value = TRUE)))
thcas.skinny <- prepareData(thcas.de)

ggplot(thcas.skinny, aes(x = tissue, y = log2(count))) + geom_point() +
  theme_few() + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") +
  labs(x = "Tissue", y = "log2 FPKM", title = "Expression of THCAS")
