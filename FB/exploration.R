summary(trn_raw$time)
head(trn_raw$time)
which(trn_raw$time %in% c(1))
hist(trn_raw$accuracy)
hist(trn_raw$x)
hist(trn_raw$y - trn_raw$x)

hist(cls_raw$x)
hist(cls_raw$y - cls_raw$x)
head(trn_raw$place_id)

#### grid size, location effects
s = 30
a = rep(0, s)
sdx = rep(0, s)
sdy = rep(0, s)
plot(0, xlim = c(0,10), ylim = c(0,10), type = "n", xlab = "x", ylab = "y")
for(i in 1:s){
  trn11 <- trn_raw[which(trn_raw$place_id == trn_raw$place_id[i]), ]
  points(trn11$x[trn11$accuracy<50], trn11$y[trn11$accuracy<50], col=i)
  sdx[i] = sd(trn11$x)
  sdy[i] = sd(trn11$y)
  a[i] = sdx[i]/sdy[i]
}
a
mean(sdx)
mean(sdy)
mean(a)

plot(0, xlim = c(0,10), ylim = c(0,10), type = "n", xlab = "x", ylab = "y")
i=1
trn11 <- trn_raw[which(trn_raw$place_id == trn_raw$place_id[i]), ]
points(trn11$x, trn11$y, col=i)
points(trn11$x[trn11$accuracy<80], trn11$y[trn11$accuracy<80], col=i)
points(trn11$x[trn11$accuracy>80], trn11$y[trn11$accuracy>80], col=i)
text(trn11$x[trn11$x>1.5], trn11$y[trn11$x>1.5], trn11$accuracy[trn11$x>1.5], cex= 2, pos = 1)
range(trn11$x)
range(trn11$y)

mean(trn11$accuracy[which(trn11$x>5)])
mean(trn11$accuracy[which(trn11$x<2)])

#### time effects
i=1
trn11 <- trn_raw[which(trn_raw$place_id == trn_raw$place_id[i]), ]
plot(0, xlim = c(0,10), ylim = range(trn11$time), type = "n", xlab = "x", ylab = "time")
points(trn11$x, trn11$time, col=i)
plot(0, xlim = c(0,10), ylim = range(trn11$time), type = "n", xlab = "y", ylab = "time")
points(trn11$y, trn11$time, col=i)


s = 30
plot(0, xlim = c(0,10), ylim = range(trn_raw$time), type = "n", xlab = "x", ylab = "y")
for(i in 1:s){
  trn11 <- trn_raw[which(trn_raw$place_id == trn_raw$place_id[i]), ]
  points(trn11$x, trn11$time, col=i)
}

s = 30
plot(0, xlim = c(0,10), ylim = range(trn_raw$time), type = "n", xlab = "x", ylab = "time")
for(i in 1:s){
  trn11 <- trn_raw[which(trn_raw$place_id == trn_raw$place_id[i]), ]
  points(trn11$y, trn11$time, col=i)
}
