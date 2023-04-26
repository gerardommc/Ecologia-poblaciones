r <- 0.025; K <- c(20, 30, 40); M <- 15

N0 <- 20

p.ext <- matrix(NA, nrow = 100, ncol = length(K))

for(j in 1:length(K)){
N <- matrix(0, ncol = 50, nrow = 100)
N[, 1] <- N0
for(i in 1:nrow(N)){
  for(t in 2:50){
    n <- rpois(1, N[i, t-1])  # Tau leap
    N[i, t] <- n + r*n*(1-n/K[j])*(n-M) 
  }
}
p.ext[, j] <- N[,50]
}

#Gillespie Stochastic Simulation Algorithm

#Probabilidad de extinción en tres capacidades de carga diferentes
p.df <- data.frame(p.ext)
names(p.df) <- c("K1", "K2", "K3")

p.m <- reshape2::melt(p.df)

boxplot(value ~ variable, p.m)



N.df <- data.frame(t(N))

N.m <- rowMeans(N.df)

plot(1:50, N.df[,1], type = "l", ylim = c(0, 35),
     xlab = "Tiempo", ylab = "N", cex.axis = 2, lwd = 1,
     col = "grey")
for(i in 2:nrow(N))lines(1:50, N.df[,i], col = "grey", lwd = 1)
lines(1:50, N.m)
