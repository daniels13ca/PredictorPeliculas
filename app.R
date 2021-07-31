library(shiny)
library(shinydashboard)
library(stringr)
library(ggplot2)
library(plotly)
library(dygraphs)
library(DT)

#Preprocesamiento de datos
source("data_preparation.R", encoding = "UTF-8")
listado_generos <- unique(data$Genero)
generos_seleccionados <- listado_generos

top_movies <- data[order(-data$Calificacion),]
top_movies <- top_movies[, c(3, 10, 9, 8)]


#Cliente
ui <- dashboardPage(
  
  skin = "black",
  
  dashboardHeader(title = "Ratingpelis 0.2"),
  
  dashboardSidebar(sidebarMenu(
    checkboxGroupInput(
      "movie_type",
      label = NULL,
      listado_generos,
      selected = generos_seleccionados
    )
  )),
  
  dashboardBody(
    
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "movies_style.css")
    ),
    
    
    tabBox(
      title = "Información de las películas", width = 100, id = "tabset1",
      box(plotOutput("plot1"),
          valueBoxOutput("box1"),
          valueBoxOutput("box2"),
          valueBoxOutput("box3")),
      dygraphOutput("trend")
    ),
    tabBox(
      title = "Películas calificadas", width = 100, id = "tabset2",
      box(dataTableOutput("table"))
    ),
    tabBox(
      title = "¿Comó funciona?", width = 100, id = "tabset3",
      p("A raíz de lo poco acertadas que resultaban ser las calificaciones que
         los sitios de ratings más populares le daban a las películas
         frente a mi opinión personal, lo cual me llevaba a ver películas que
         no satisfacían mis expectativas, decidí crear mi propio sistema de
         calificación y a la vez un predictor de la posible calificación que yo
         le otorgaría a la película. Para esto me base en él o en los géneros de 
         las películas según", a("The Movie DB", href= "https://www.themoviedb.org")), 
      p("En una segunda fase esta contemplado pronosticar la calificación de películas 
        similares, incluyendo predictores tales como:"),
      p("- Año de lanzamiento"),
      p("- Edad del observador"),
      p("- Sexo del observador"),
      p("- Palabras claves del resumen"),
      p("Si desean contribuir a enriquecer la base de datos lo pueden hacer diligenciando el 
        siguiente formulario" , a("aquí", href= "https://forms.gle/y6bMuf16cKmyD6549")),
      p("Contacto: ",a("silvdaniel@gmail.com", href= "silvdaniel@gmail.com"))
    )

  )
  
  
)


#Servidor
server <- function(input, output) {
  output$plot1 <- renderPlot({
    movies <-
      unique(data[data$Genero %in% input$movie_type, c("Titulo", "Calificacion")])
    
    ggplot(movies, aes(movies$Calificacion)) +
      geom_histogram(binwidth = 1,
                     fill = "blue",
                     color = "white") +
      labs(title = "Distribución de las calificaciones",
           y = "Cantidad", x = "Calificación") +
      theme(plot.title = element_text(hjust = 0.5))
    
  })
  
  output$box1 <- renderValueBox({
    movies <-
      unique(data[data$Genero %in% input$movie_type, c("Titulo", "Calificacion")])
    
    valueBox(round(mean(movies$Calificacion), 2),
             "Calificación media",
             color = "blue")
  })
  
  output$box2 <- renderValueBox({
    movies <-
      unique(data[data$Genero %in% input$movie_type, c("Titulo", "Calificacion")])
    
    valueBox(nrow(movies),
             "Películas calificadas",
             color = "blue")
  })
  
  output$box3 <- renderValueBox({
    movies <-
      unique(data[data$Genero %in% input$movie_type, c("Titulo", "Calificacion")])
    
    valueBox(round(sd(movies$Calificacion), 2),
             "Desviación estándar",
             color = "blue")
  })
  
  output$trend <- renderDygraph({
    years <-
      unique(data[data$Genero %in% input$movie_type, c("Ano", "Calificacion")])
    mean_by_year = tapply(years$Calificacion, years$Ano, mean)
    ## Create ts object
    x = ts(as.vector(mean_by_year),
           start = 1920,
           end = 2020)
    y = cbind(Rating = x)
    
    ## Plot code
    dygraph(y,
            main = "Tendencia de la calificación media por año",
            ylab = "Calificación",
            group = "Ratings") %>%
      dyRangeSelector() %>%
      dyOptions(stepPlot = TRUE) %>%
      dySeries("V1", label = "Calificación")
    
  })
  
  output$table <- renderDataTable({
    movies <- unique(data[data$Genero %in% input$movie_type, c("Titulo", "Calificacion")])
    datatable(movies, rownames = FALSE)
    
  })
  
}


shinyApp(ui, server)