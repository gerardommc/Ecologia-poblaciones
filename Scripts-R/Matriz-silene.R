A <- as.matrix(read.csv("Scripts-R/Silene-matriz.csv"))

tiempo = 20

N <- matrix(0, nrow = 6, ncol = tiempo)

N0 <- c(0, 0, 0, 5, 5, 5)

N[, 1] <- N0

for(i in 2:tiempo){
  N[, i] <- A %*% N[, i-1]
}

N.df <- data.frame(t(N))
names(N.df) <- c("N1", "N2", "N3", "N4", "N5", "N6")
N.df$Tiempo <- 1:tiempo

N.m <- reshape2::melt(N.df, id.vars = "Tiempo")

library(ggplot2)

ggplot(N.m) + geom_line(aes(x = Tiempo, y = value, colour = variable))
