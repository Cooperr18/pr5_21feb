nuevo_dir = "C:/pr5_21feb"
setwd(nuevo_dir)

set.seed(1000)
#1
n_registros = 200

  sites = sample(c("Site1","Site2","Site3","Site4","Site5","Site6","Site7","Site8","Site9","Site10"),n_registros, replace = TRUE)
  tipo_artefacto = sample(c("Pottery","Tools","Jewerly","Weapons"),n_registros, replace = TRUE)
  numero_artefactos = sample(c(1:1000),n_registros, replace = TRUE)
  contexto = c("Habitacional","Funerario","Otros", n_registros, TRUE)
  latitud = runif(n_registros, min = 0, max = 90)
  longitud = runif(n_registros, min = -180, max = 180)
  
  archaeological_data = data.frame(
    site = sites,
    tipo_artefacto = tipo_artefacto,
    numero_artefactos = numero_artefactos,
    contexto = contexto,
    latitud = latitud,
    longitud = longitud
  )

#2
media_artefactos = mean(numero_artefactos)
cuartil_artefactos = 
  
#3
histograma = hist(numero_artefactos)
median(numero_artefactos)
#La asimetría es con cola a la derecha, ya que se encuentra más cerca del 0% que del 100%, y es menor que la media

#4
boxplot(x= archaeological_data$numero_artefactos)
#Los valores máximos y mínimos se encuentran en 997 y 10 respectivamente, siendo los límites del gráfico, y la superficie indicada con color es el margen de mayor densidad, entre el 25% y 75%, siendo la línea negra la que indica el 50%. Los cuartiles restantes (25% y 75%) están indicados por la línea discontinua.

library(ggplot2)

#5
tabla_media_artefactos = table(media_artefactos)
tabla_yacimientos = table(sites)
tabla_mezclada = xtabs(~tabla_media_artefactos + tabla_yacimientos, data = archaeological_data)
grafico_media = barplot(x = tabla_yacimientos, y= tabla_media_artefactos,
                        main = "Media artefactos por yacimiento",
                        xlab = "Yacimientos",
                        ylab = "Media artefactos",
                        col = "khaki1")

#6
mapa_calor = ggplot(archaeological_data, aes(x = longitude, y = latitud)) +
  geom_bind2d(numero_artefactos) +
  labs(title = "Mapa de calor de artefactos", x = "Longitud", y= "Latitud")

#7
total_artefactos = sum(archaeological_data$numero_artefactos)

#8
mediana_artefactos_yacimiento = aggregate(numero_artefactos ~ sites, data = archaeological_data, FUN = median)

#9
desviacion_estandar = aggregate(numero_artefactos ~ sites, data = archaeological_data, FUN = sd)

#10
maximo_artefactos = which.max(numero_artefactos)
indice_max <- which(archaeological_data$numero_artefactos == maximo_artefactos)
cat("El valor máximo se encuentra en la fila:", indice_max)

#11
resumen <- aggregate(numero_artefactos ~ site, archaeological_data, function(numero_artefactos) c(media = mean(numero_artefactos), mediana = median(numero_artefactos), desviacion_estandar = sd(numero_artefactos)))
print(resumen)

#12
diagrama_cajas = boxplot(numero_artefactos ~ site, data = archaeological_data,
        main = "Frecuencia de artefactos por yacimiento",
        xlab = "Yacimiento", ylab = "Artefactos",
        col = "khaki1")
