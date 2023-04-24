r <- 0.025; K <- 30; M <- 15

N0 <- c(10, 20)

N <- matrix(0, ncol = 50, nrow = 2)
N[, 1] <- N0
for(i in 1:2){
  for(t in 1:49){
    N[i, t+1] <- N[i, t] + r*N[i, t]*(1-N[i, t]/K)*(N[i, t]-M) 
  }
}

N.df <- data.frame(t(N))

png("Allee.png", width = 800, height = 800)
plot(1:50, N.df$X1, type = "l", ylim = c(0, 30),
     xlab = "Tiempo", ylab = "N", cex.axis = 2, lwd = 2)
lines(1:50, N.df$X2, col = "red", lwd = 2)
dev.off()
