trn_raw <- read.table("train.csv", header = T, sep = ",")
cls_raw <- read.table("test.csv", header = T, sep = ",")
cls_raw$place_id <- 0
library(MASS)
sample_submission <- read.table("sample_submission.csv", header = T, sep = ",")
save.image()
sm