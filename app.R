library(shiny)
library(shinydashboard)
library(randomForest)
library(caret)
library(zoo)
library(dplyr)
library(groupdata2)
library(ggplot2)

normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}

df=read.csv('carbon_3.csv')

df <- select (df,-starts_with("date"))%>%
  select(-starts_with("date."))%>%
  na.aggregate(by='Anio')%>%
  na.aggregate(by='Mes')%>%
  na.aggregate(by='Dia')%>%
  na.aggregate()

df$cat= paste(df[,1], df[,2])
df$cat=as.factor(df$cat)
levels(df$cat) <- 1:118
df$cat <- as.numeric(df$cat)

df <- balance(df,size='max', cat_col='cat')%>%
	lapply(normalize)%>%
	as.data.frame()


ui <- dashboardPage(
dashboardHeader(title="Predicción Carbón"),
dashboardSidebar(h5("Parámetros de afinación del Algoritmo Random Forest"),
numericInput("p1","ntree",534),
numericInput("p2","mtry",200),
h5("*El valor de mtry es llevado a su raíz cuadrada"),
numericInput("p3","seed",777),
h5("Predicciones"),
sliderInput("p4","Ajuste de probabilidad de clases",min=0.1,max=0.9),
sliderInput("p5","Número de meses a futuro",min=1,max=12,value=c(1,1))

),
  dashboardBody(tabItem('item',tabsetPanel(tabPanel('Validación'),
tabPanel('Evaluación'),tabPanel('Predicciones')

))))

server <- function(input, output) { }

shinyApp(ui, server)



