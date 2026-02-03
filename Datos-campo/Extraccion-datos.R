library(terra)

f <- list.files("Datos-campo/", "gpx", full.names = T)

p <- lapply(f, vect)

p.df <- lapply(p, function(x){
  df <- crds(x) |> data.frame()
  df <- data.frame(ID = 1:nrow(df), df)
})

p.df[[1]]$lado <- "Derecha"
p.df[[1]]$lado <- "Izquierda"

df.todas <- rbind(p.df[[1]], p.df[[2]])

write.csv(df.todas, "Datos-campo/Coordenadas-Mayo-2025.csv", row.names = F)
