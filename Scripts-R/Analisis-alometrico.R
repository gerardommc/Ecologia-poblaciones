tabla <- read.csv("Descargas/Escalamiento-alometrico.csv")

with(tabla, plot(log(Masa), log(r)))

#Linear model
m1 <- lm(log(r) ~ log(Masa), data = tabla)
summary(m1)

t.nueva <- data.frame(Masa = seq(min(tabla$Masa), max(tabla$Masa), len = 10))

pred <- predict(m1, newdata = t.nueva)
t.nueva$r <- pred

with(tabla, plot(log(Masa), log(r)))
with(t.nueva, lines(log(Masa), r))
     