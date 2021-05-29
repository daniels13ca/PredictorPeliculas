library(shinydashboard)
library(stringr)

#Preprocesamiento de datos
source("data_preparation.R", encoding = "UTF-8")
listado_generos <- unique(data$Genero)
generos_seleccionados <- listado_generos

####################################################################

ui <- dashboardPage(
  dashboardHeader(title = "Ratingpelis 0.2"),
  dashboardSidebar(
    sidebarMenu(
      checkboxGroupInput("movie_type", label = NULL, listado_generos, selected = generos_seleccionados)
    )
  ),
  dashboardBody(
    tabItems(

      # Second tab content
      tabItem(tabName = "widgets",
              h2("Widgets tab content")
      )
    )
  )
)

server <- function(input, output) {
  
  #output$Genero <- preprocesamiento()


}


shinyApp(ui, server)