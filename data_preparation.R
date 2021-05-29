data <- read.csv2(file="Formulario de calificación de películas (respuestas).csv", sep = ";", header = TRUE,
                  encoding="UTF-8")

col_names <- c("Fecha", "Titulo", "Resumen", "Link en The Movie DB", "Sexo", "Edad al momento de ver la pelicula", "Calificacion", "Genero")

names(data) <- col_names
data$Index <- seq.int(nrow(data))

list_genre <- str_split_fixed(data$Genero, ",", n=Inf)

col_number <- ncol(list_genre)
row_number <- nrow(list_genre)

genres_view <- data.frame(Index=integer(),
                          Genero=character())

for ( i in 1:row_number){
  for ( j in 1:col_number ){
    index_t <- i
    genre_t <- list_genre[i, j]
    
    if(genre_t != ""){
      genres_view[nrow(genres_view) + 1,] = c(index_t,genre_t)
    }
  }
}

data$Genero <- NULL

genres_view$Genero <- trimws(genres_view$Genero, which = c("both"))
data <- merge(x = data, y = genres_view, by = "Index", all.y = TRUE)

data$Ano <-str_sub(data$Titulo,-5,-2)
data$Titulo <- substr(data$Titulo,1,nchar(data$Titulo)-7)