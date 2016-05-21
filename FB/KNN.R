set.seed(1)
# create index for train and validation sets from trn_raw
idx <- sample(1:nrow(trn_raw), 35000)
all <- trn_raw[idx, ]
idx_trn <- sample(idx, 30000)
idx_tst <- idx[-idx_trn]

# train and validation sets
trn <- trn_raw[idx_trn, ]
tst <- trn_raw[idx_tst, ]

# input for KNN
library(class)
all <- rbind(trn, tst)
all[is.na.data.frame(all)] <- 0
std_X <- scale(all[, -6])
trn_X <- std_X[1:nrow(trn), ]
tst_X <- std_X[-(1:nrow(trn)), ]
trn_Y <- trn[, 6]
tst_Y <- tst[, 6]

p <- knn(trn_X, tst_X, trn_Y, k = 10)
write.csv(data.frame(p, tst_Y), file = "pred.csv")
save.image()
