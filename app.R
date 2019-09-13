library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title="Predicción Carbón"),
  dashboardSidebar(h5("Parámetros de afinación"),
numericInput("p1","ntree",534),
numericInput("p2","mtry",200),
h5("*El valor de mtry es llevado a su raíz cuadrada"),
numericInput("p3","seed",777),
h5("Predicciones"),
sliderInput("p4","Número de meses a futuro",min=1,max=12,value=c(1,1))

),
  dashboardBody(tabItem('item',tabsetPanel(tabPanel('Validación'),
tabPanel('Evaluación'),tabPanel('Predicciones')

))))

server <- function(input, output) { }

shinyApp(ui, server)



