r <- 0.025; K <- 30; M <- 15

N0 <- c(15)

N <- matrix(0, ncol = 50, nrow = 100)
N[, 1] <- N0
for(i in 1:nrow(N)){
  for(t in 1:49){
    n <- rpois(1, N[i, t])
    N[i, t+1] <- n + r*n*(1-n/K)*(n-M) 
  }
}

N.df <- data.frame(t(N))

N.m <- rowMeans(N.df)

plot(1:50, N.df[,1], type = "l", ylim = c(0, 35),
     xlab = "Tiempo", ylab = "N", cex.axis = 2, lwd = 1,
     col = "grey")
for(i in 2:nrow(N))lines(1:50, N.df[,i], col = "grey", lwd = 1)
lines(1:50, N.m)
