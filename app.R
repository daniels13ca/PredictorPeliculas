library(shinydashboard)
library(stringr)
library(ggplot2)
library(plotly)

#Preprocesamiento de datos
source("data_preparation.R", encoding = "UTF-8")
listado_generos <- unique(data$Genero)
generos_seleccionados <- listado_generos

top_movies <- data[order(-data$Calificacion), ]
top_movies <- top_movies[, c(3, 10, 9, 8)]


#Cliente
ui <- dashboardPage(
  dashboardHeader(title = "Ratingpelis 0.2"),
  
  dashboardSidebar(sidebarMenu(
    checkboxGroupInput(
      "movie_type",
      label = NULL,
      listado_generos,
      selected = generos_seleccionados
    )
  )),
  
  dashboardBody(fluidRow(box(
    plotOutput("plot1", height = 450)
  ),
  box(
    valueBoxOutput("box1")
  ),
  box(
    valueBoxOutput("box2")
  ),
  box(
    valueBoxOutput("box3")
  )))
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
    
    valueBox(
      round(mean(movies$Calificacion), 2),
      "Calificación media",
      color = "blue"
    )
  })
  
  output$box2 <- renderValueBox({
    movies <-
      unique(data[data$Genero %in% input$movie_type, c("Titulo", "Calificacion")])
    
    valueBox(
      nrow(movies),
      "Películas calificadas",
      color = "blue"
    )
  })
  
  output$box3 <- renderValueBox({
    movies <-
      unique(data[data$Genero %in% input$movie_type, c("Titulo", "Calificacion")])
    
    valueBox(
      round(sd(movies$Calificacion), 2),
      "Desviación estándar",
      color = "blue"
    )
  })
  
}


shinyApp(ui, server)