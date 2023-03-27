## Matrices de proyección poblacional
s01 <- 0.5; s12 <- 0.5; s23 <- 0.5; s34 <- 0.5; s45 <- 0.5
f3 <- 5; f4 <- 5; f5 <- 5


A <- matrix(c(0,   0, s01*f3, s01*f4, s01*f5,
              s12, 0, 0 ,     0,      0,
              0,  s23,0,      0,      0,
              0,  0,  s34,    0,      0,
              0,  0,  0,      s45,    0), byrow = T, ncol = 5)

N0 <- c(0, 0, 10, 10, 10)

tiempo <- 25

N <- matrix(0, ncol = tiempo, nrow = 5)
N[, 1] <- N0

for(t in 1:(tiempo-1)){
  N[, t+1] <- N[, t] %*% A
}

library(ggplot2)

N.df <- data.frame(t(N))
names(N.df) <- c("N1", "N2", "N3", "N4", "N5")
N.df$Tiempo <- 1:tiempo

N.df.m <- reshape2::melt(N.df, id.vars = "Tiempo")

ggplot(N.df.m) + geom_line(aes(x = Tiempo, y = value, colour = variable))

