# modified by keguoh
set.seed(1)

# input for kknn
trn <- trn_raw
tst <- cls_raw
all <- rbind(trn, tst)
# all$place_id <- factor(all$place_id)
std_X <- scale(all[, -6])
trn_X <- std_X[1:nrow(trn), ]
tst_X <- std_X[-(1:nrow(trn)), ]
trn_std <- data.frame(trn_X, trn[, "place_id"])
tst_std <- data.frame(tst_X, tst[, "place_id"])
names(trn_std)[6] <- "place_id"
names(tst_std)[6] <- "place_id"
system.time(expr = KKNN <- kknn_fb(place_id~., trn_std, k = 7, tst_std, distance = 2, kernel = "triangular"))
save.image()
formula = place_id ~ .
train = trn_std
k = 7
cutoff = 3
distance = 2
na.action = na.omit()
kernel = "optimal"
ykernel = NULL
scale = TRUE
contrasts = c(unordered = "contr.dummy", ordered = "contr.ordinal")
library(kknn)
if (is.null(ykernel)) 
  ykernel = 0
weight.y = function(l = 1, diff = 0) {
  k = diff + 1
  result = matrix(0, l, l)
  diag(result) = k
  for (i in 1:(k - 1)) {
    for (j in 1:(l - i)) {
      result[j, j + i] = k - i
      result[j + i, j] = k - i
    }
  }
  result
}
kernel <- match.arg(kernel, c("rectangular", "triangular", 
                              "epanechnikov", "biweight", "triweight", "cos", "inv", 
                              "gaussian", "rank", "optimal"), FALSE)
ca <- match.call()
response = NULL
old.contrasts <- getOption("contrasts")
options(contrasts = contrasts)
formula = as.formula(formula)
mf <- model.frame(formula, data = train)
mt <- attr(mf, "terms")
mt2 <- delete.response(mt)
cl <- model.response(mf)
d <- sum(attr(mt, "order"))
if (is.ordered(cl)) {
  response <- "ordinal"
  lev <- levels(cl)
}
if (is.numeric(cl)) 
  response <- "continuous"
if (is.factor(cl) & !is.ordered(cl)) {
  response <- "nominal"
  lev <- levels(cl)
}
if (distance <= 0) 
  stop("distance must >0")
if (k <= 0) 
  stop("k must >0")
learn <- model.matrix(mt, mf)
valid <- model.matrix(mt2, test)
m <- dim(learn)[1]
p <- dim(valid)[1]
q <- dim(learn)[2]
ind <- attributes(learn)$assign
d.sd <- numeric(length(ind)) + 1
we <- numeric(length(ind)) + 1
d.sd = apply(learn, 2, stats::var)
for (i in unique(ind)) {
  d.sd[ind == i] = sqrt(mean(d.sd[ind == i]))
  we[ind == i] = 1/sum(ind == i)
}
we[d.sd == 0] = 0
d.sd[d.sd == 0] = 1
if (scale) {
  learn <- sweep(learn, 2L, d.sd, "/", check.margin = FALSE)
  valid <- sweep(valid, 2L, d.sd, "/", check.margin = FALSE)
}
ord = order(we * apply(learn, 2, sd), decreasing = TRUE)
we = we[ord]
learn = learn[, ord, drop = FALSE]
valid = valid[, ord, drop = FALSE]
Euclid <- FALSE
if (distance == 2) 
  Euclid <- TRUE
if (Euclid) 
  dmtmp <- .C("dmEuclid", as.double(learn), as.double(valid), 
              as.integer(m), as.integer(p), as.integer(q), dm = double((k + 
                                                                          1L) * p), cl = integer((k + 1L) * p), k = as.integer(k + 
                                                                                                                                 1), as.double(distance), as.double(we), PACKAGE = "kknn")
else dmtmp <- .C("dm", as.double(learn), as.double(valid),
                 as.integer(m), as.integer(p), as.integer(q), dm = double((k +
                                                                             1L) * p), cl = integer((k + 1L) * p), k = as.integer(k +
                                                                                                                                    1), as.double(distance), as.double(we), PACKAGE = "kknn")
D <- matrix(dmtmp$dm, nrow = p, ncol = k + 1)
C <- matrix(dmtmp$cl, nrow = p, ncol = k + 1)
maxdist <- D[, k + 1]
maxdist[maxdist < 1e-06] <- 1e-06
D <- D[, 1:k]
C <- C[, 1:k] + 1
CL <- matrix(cl[C], nrow = p, ncol = k)
if (response != "continuous") {
  l <- length(lev)
  weightClass <- matrix(0, p, l)
}
# if (response == "continuous") {
#   weightClass <- NULL
# }
W <- D/maxdist
W <- pmin(W, 1 - (1e-06))
W <- pmax(W, 1e-06)
if (kernel == "rank") 
  W <- (k + 1) - t(apply(as.matrix(D), 1, rank))
if (kernel == "inv") 
  W <- 1/W
if (kernel == "rectangular") 
  W <- matrix(1, nrow = p, ncol = k)
if (kernel == "triangular") 
  W <- 1 - W
if (kernel == "epanechnikov") 
  W <- 0.75 * (1 - W^2)
if (kernel == "biweight") 
  W <- dbeta((W + 1)/2, 3, 3)
if (kernel == "triweight") 
  W <- dbeta((W + 1)/2, 4, 4)
if (kernel == "cos") 
  W <- cos(W * pi/2)
if (kernel == "triweights") 
  W <- 1
if (kernel == "gaussian") {
  alpha = 1/(2 * (k + 1))
  qua = abs(qnorm(alpha))
  W = W * qua
  W = dnorm(W, sd = 1)
}
if (kernel == "optimal") {
  W = rep(optKernel(k, d = d), each = p)
}
W <- matrix(W, p, k)

# modified by keguoh
if (response != "continuous") {
  submit_fb <- function(x){
    top3 <- names(rank(table(x)))[1:cutoff]
    return(paste(top3[1], top3[2], top3[3]))
  }
  top <- list()
  fit_fb <- function(x){
    top3 <- names(rank(table(x)))[1:cutoff]
    return(top3)
  }
  submit <- apply(CL, 1, submit_fb)
  fit <- lapply(CL, fit_fb)
}
# if (response == "nominal") {
#   fit <- apply(weightClass, 1, order, decreasing = TRUE)[1, ]
#   fit <- factor(fit, levels = 1:l, labels = lev)
#   if (kernel == "rectangular" && k > 1) {
#     blub <- apply(weightClass, 1, rank, ties.method = "max")
#     indices = (1:p)[colSums(blub == l) > 1]
#     blub = t(blub)
#     nM = matrix(0, p, l)
#     colnames(nM) = lev
#     for (i in 1:l) nM[, i] = apply((CL == lev[i]) %*% 
#                                      diag(1:k), 1, max)
#     nM = (blub == l) * nM
#     nM[nM == 0] <- k + 1
#     fitv = numeric(p)
#     for (i in indices) fitv[i] = which(nM[i, ] == min(nM[i, 
#                                                          ]))
#     fit[indices] <- factor(fitv[indices], levels = 1:l, 
#                            labels = lev)
#   }
# }
# if (response == "continuous") 
#   fit <- rowSums(W * CL)/pmax(rowSums(W), 1e-06)
options(contrasts = old.contrasts)
result <- submit
result
write.csv(data.frame(row_id = tst[, 1], place_id = result), file = "KKNN_FB.csv", row.names = F)
