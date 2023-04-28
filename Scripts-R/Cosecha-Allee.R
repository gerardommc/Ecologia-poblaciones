r <- 0.025; K <- 40; M <- 5
c <- 0.25

N0 <- 20

N <- matrix(0, ncol = 50, nrow = 100)
N[, 1] <- N0

for(i in 1:nrow(N)){
  for(t in 2:50){
    n <- rpois(1, N[i, t-1])  # Tau leap
    C <- rpois(1, c * N[i, t-1])
    
    dN <- r*n*(1-n/K)*(n-M) - C
    
    while((n + dN) < 0){
      n <- rpois(1, N[i, t-1])  # Tau leap
      C <- rpois(1, c * N[i, t-1])
      
      dN <- r*n*(1-n/K)*(n-M) - C
    }
    
    N[i, t] <- n + dN
  }
}


N.df <- data.frame(t(N))

N.m <- rowMeans(N.df)

plot(1:50, N.df[,1], type = "l", ylim = c(0, 45),
     xlab = "Tiempo", ylab = "N", cex.axis = 2, lwd = 1,
     col = "grey")
for(i in 2:nrow(N))lines(1:50, N.df[,i], col = "grey", lwd = 1)
lines(1:50, N.m)
