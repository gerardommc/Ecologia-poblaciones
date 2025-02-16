r1 <- 0.1; r2 <- 0.05
e1 <- 0.05; e2 <- 0.01
m1 <- 0.025; m2 <- 0.07
K1 <- 30; K2 <- 20

N1 <- c(); N2 <- c()

N1[1] <- 2; N2[1] <- 0

for(i in 2:500){
  N1[i] <- N1[i-1] + r1*N1[i-1] * (1 - N1[i-1]/K1) - e1*N1[i-1] + e2*N2[i-1] - m1 * N1[i-1]
  N2[i] <- N2[i-1] + r2*N2[i-1] * (1 - N2[i-1]/K2) - e2*N2[i-1] + e1*N1[i-1] - m2 * N2[i-1]
}

tabla <- data.frame(N1 = N1, N2 = N2, tiempo = 1:500)

with(tabla, plot(tiempo, N1, type = "l", ylim = c(0, 10)))
with(tabla, lines(tiempo, N2, col = "red"))
