
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

set.seed(1)
# create index for train and validation sets from trn_raw
idx <- sample(1:nrow(trn_raw), 35000)
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
