set.seed(1)
# create index for train and validation sets from trn_raw
n_all <- 40000
n_trn <- 30000
idx <- sample(1:nrow(trn_raw), n_all)
all <- trn_raw[idx, ]
idx_trn <- sample(1:n_all, n_trn)

# train and validation sets
trn <- all[idx_trn, ]
tst <- all[-idx_trn, ]

# input for KNN
library(class)
all <- rbind(trn, tst)
std_X <- scale(all[, -6])
trn_X <- std_X[1:nrow(trn), ]
tst_X <- std_X[-(1:nrow(trn)), ]
trn_Y <- trn[, 6]
tst_Y <- tst[, 6]

system.time(expr =  p <- knn(trn_X, tst_X, trn_Y, k = 20))
pred <- data.frame(p = p, tst_Y = factor(tst_Y))
mapk(1, pred$tst_Y, pred$p)

save.image()

# Just x,y inputs not good

# go parallel
library(parallel)
(no_cores <- detectCores()-1)
cl <- makeCluster(no_cores)

#parameters
k <- round(seq(1,30,length.out = no_cores),0)
clusterExport(cl, c("trn_X", "tst_X", "trn_Y", "tst_Y", "k"))
# KNN returns mapk value
KNN <- function(k){
  
  #' Compute the average precision at k
  #'
  #' This function computes the average precision at k
  #' between two sequences
  #'
  #' @param k max length of predicted sequence
  #' @param actual ground truth set (vector)
  #' @param predicted predicted sequence (vector)
  #' @export
  apk <- function(k, actual, predicted)
  {
    score <- 0.0
    cnt <- 0.0
    for (i in 1:min(k,length(predicted)))
    {
      if (predicted[i] %in% actual && !(predicted[i] %in% predicted[0:(i-1)]))
      {
        cnt <- cnt + 1
        score <- score + cnt/i 
      }
    }
    score <- score / min(length(actual), k)
    score
  }
  
  
  #' Compute the mean average precision at k
  #'
  #' This function computes the mean average precision at k
  #' of two lists of sequences.
  #'
  #' @param k max length of predicted sequence
  #' @param actual list of ground truth sets (vectors)
  #' @param predicted list of predicted sequences (vectors)
  #' @export
  #' 
  mapk <- function (k, actual, predicted)
  {
    if( length(actual)==0 || length(predicted)==0 ) 
    {
      return(0.0)
    }
    
    scores <- rep(0, length(actual))
    for (i in 1:length(scores))
    {
      scores[i] <- apk(k, actual[[i]], predicted[[i]])
    }
    score <- mean(scores)
    score
  }
  p <- class::knn(trn_X, tst_X, trn_Y, k)
  map <- mapk(1, factor(tst_Y), p)
  return(map)
}
# compute!
system.time(te <- parLapply(cl, k, KNN))
# result
cbind(k, te = unlist(te))
which.max(te)
stopCluster(cl)