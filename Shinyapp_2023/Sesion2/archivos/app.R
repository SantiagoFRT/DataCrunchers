
library(shiny)

# Cargar las funciones y los datos:
source("global_juve.R")


# Intefaz de usuario:

ui <- fluidPage(
  titlePanel(title = "Tablero de indicadores"),
  sidebarLayout(
    sidebarPanel(
      h1("Controles"),
      selectizeInput(inputId = "selEntidad",
                     label = "Seleccione entidad",
                     choices = entidades_a_escoger
                     )
      
                 ),
    mainPanel()
  
    
  )
  
  
)

# Servidor
server <- function(input, output, session) {
  
}

shinyApp(ui, server)

