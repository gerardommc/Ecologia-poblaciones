x <- c(); y <- c()

set.seed(123)
x[1] <- runif(1); y[1] <- runif(1)

for(i in 2:50){
  x[i] <- x[i-1] + runif(1, -1, 1)
  y[i] <- y[i-1] + rnorm(1, 1, 4)
}

tabla <- data.frame(x = x, y = y)

library(ggplot2)
ggplot(tabla) + geom_path(aes(x = x, y = y))
