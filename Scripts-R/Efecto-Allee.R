r <- 0.025; K <- 30; M <- 15

N0 <- c(14.9, 15.1)

N <- matrix(0, ncol = 50, nrow = 2)
N[, 1] <- N0

dt <- 1

for(i in 1:2){
  for(t in 2:50){
    N[i, t] <- N[i, t-1] + r*N[i, t-1]*(1-N[i, t-1]/K)*(N[i, t-1]-M) * dt #Integracion con Euler
  }
}

N.df <- data.frame(t(N))

plot(1:50, N.df$X1, type = "l", ylim = c(0, 30),
     xlab = "Tiempo", ylab = "N", cex.axis = 2, lwd = 2)
lines(1:50, N.df$X2, col = "red", lwd = 2)

png("Allee.png", width = 800, height = 800)
plot(1:50, N.df$X1, type = "l", ylim = c(0, 30),
     xlab = "Tiempo", ylab = "N", cex.axis = 2, lwd = 2)
lines(1:50, N.df$X2, col = "red", lwd = 2)
dev.off()
