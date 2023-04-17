set.seed(123)

wt <- exp(rnorm(28, 0.025, 2))

r <-  wt^runif(28, 0.6, 0.8)/exp(rnorm(28, 0, 1))

df <- data.frame(Spp = 1:28, Masa = wt, r = r)

plot(r, wt)

write.csv(df, "Scripts-R/Escalamiento-alometrico.csv", row.names = F)
