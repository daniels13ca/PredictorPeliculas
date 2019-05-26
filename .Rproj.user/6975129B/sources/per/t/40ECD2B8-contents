#Carga de librerias necesarias
library(xlsx)

#Carga de la data
data <- read.xlsx("Formulario de calificación de películas - Depurado.xlsx", sheetIndex = 1, encoding="UTF-8")
head(data)

#calculo del promedio general y por genero
mean(data$Calificación)

mean_by_genre <- aggregate(data[,8], list(data$Genero), mean)
mean_by_genre

#histograma de calificaciones
hist(data$Calificación)