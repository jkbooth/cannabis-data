
dat <- read.delim("csa.matrix.FPKM.vf.082511.txt", header = TRUE, row.names = 1, sep = "\t", stringsAsFactors = FALSE)
rownames(dat) <- gsub("csa_locus_", "", rownames(dat))
colnames(dat) <- paste(colnames(dat), dat[1, ], sep = ".")
dat <- dat[2:nrow(dat), ]
str(dat)

head(rownames(dat))
ncol(dat)
dat.fpkm <- dat[ ,2:18]
colnames(dat.fpkm)
dat.fpkm <- data.frame(sapply(dat.fpkm, as.numeric))
rownames(dat.fpkm) <- rownames(dat)
write.table(dat.fpkm, "msu_est_num.txt")

des <- data.frame(ID = colnames(dat.fpkm),
                  tissue = c(rep("root", times = 3), "petiole", rep("buds", times = 2), rep("flower", times = 3), rep("young.leaf", times = 3), "mature.leaf", "stem", "buds", rep("mature.leaf", times = 2)))
write.table(des, "csa.matrix.design.txt")

