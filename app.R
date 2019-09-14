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
numericInput("p2","mtry",sqrt(200)),
h5("*El valor de mtry es llevado a su raíz cuadrada"),
numericInput("p3","seed",777),
h5("Predicciones"),
sliderInput("p4","Ajuste de probabilidad de clases",min=0.1,max=0.9,value=0.5),
sliderInput("p5","Número de meses a futuro",min=1,max=12,value=c(1,1))

),
  dashboardBody(tabItem('item',tabsetPanel(tabPanel('Validación',h5('hello'),
plotOutput('distPlot')
),
tabPanel('Evaluación'),tabPanel('Predicciones')

))))

server <- function(input, output) {
output$distPlot <- renderPlot({

m=1

n=28*m

var_expl<- df

target<-df$DLI_PESO_A_PAGAR

df_cruz <- data.frame(target,var_expl)

target<- vector()
 
for (i in 1:NROW(df_cruz[,1])) {
 
target[i]<-ifelse( df_cruz[i,1]<df_cruz[i+n,1],1,0 )

 }

df_cruz$target<- target

df_cruz<-df_cruz[1:(nrow(df_cruz)-n),]

test= df_cruz[(nrow(df_cruz)-220):(nrow(df_cruz)),]

df_cruz <- df_cruz[1:(nrow(df_cruz)-220),]
set.seed(input$p3)

index <- sample(1:nrow(df_cruz),nrow(df_cruz))

train <- df_cruz[1:floor(nrow(df_cruz)*0.7),]

val <- df_cruz[(floor(nrow(df_cruz)*0.7)+1):nrow(df_cruz),]

rF <- randomForest (as.factor(train$target)~.,ntree=533, mtry=sqrt(200) ,data=train[,-1], scale=T, importance=T,replace=T)

p2 <- predict(rF, val[,-1])
l <- confusionMatrix(p2,as.factor(val[,1]))

})
}

shinyApp(ui, server)







