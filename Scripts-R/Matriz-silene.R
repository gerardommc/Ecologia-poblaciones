A <- as.matrix(read.csv("Scripts-R/Silene-matriz.csv"))

tiempo = 10

N <- matrix(0, nrow = 6, ncol = tiempo)

N0 <- c(0, 0, 0, 5, 5, 5)

N[, 1] <- N0

set.seed(183)

for(i in 2:tiempo){
  a <- A
  a[] <- sapply(A[], function(x){rnorm(1, x, sd = x/4)})
  a[] <- ifelse(a[] < 0, 0, a[])
  n <-  a %*% N[, i-1]
  N[, i] <- sapply(n, function(x){rpois(1, x)})
}

N.df <- data.frame(t(N))
names(N.df) <- c("N1", "N2", "N3", "N4", "N5", "N6")
N.df$Tiempo <- 1:tiempo

N.m <- reshape2::melt(N.df, id.vars = "Tiempo")

library(ggplot2)

ggplot(N.m) + geom_line(aes(x = Tiempo, y = value, colour = variable))

eigen(A)
