c <- 0.05; e <- 0.05

N <- c()

N0 <- 0.1

N[1] <- N0

tiempo <- 1000

for(i in 2:tiempo){
  N[i] <- N[i-1] + c*N[i-1] * (1 - N[i-1]) - e*N[i-1]
}

plot(1:tiempo, N, type = "l")
