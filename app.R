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
sliderInput("p5","Número de meses a futuro",min=1,max=12,value=1)

),
  dashboardBody(tabItem('item',tabsetPanel(tabPanel('Validación',h5('Predicciones del algoritmo Random Forest para las subidas y bajadas en el peso del Carbón ingresado y del valor facturado'),
plotOutput('distPlot'),
h5('VN = Verdaderos Negativos, FN = Falsos Negativos, FP = Falsos Positivos, VP = Verdaderos Positivos')
),
tabPanel('Evaluación'),tabPanel('Predicciones')

))))

server <- function(input, output) {
output$distPlot <- renderPlot({

m=input$p5

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

rF <- randomForest (as.factor(train$target)~.,ntree=input$p1, mtry=sqrt(input$p2) ,data=train[,-1], scale=T, importance=T,replace=T)

p2 <- predict(rF, val[,-1])
cM <- confusionMatrix(p2,as.factor(val[,1]))

l<- as.data.frame(cM[2])

l<-l[3]
l<-as.vector(l)
l<-c(l)


ctable <- as.table(matrix(as.vector(unlist(l)), nrow = 2, byrow = TRUE))
fourfoldplot(ctable, color = c( "#CC6666","#99CC99"),
             conf.level = 0, margin = 2,main="Matríz de Validación (Verde para éxito en la predicción, rojo para predicción errada)") + 
    text(-0.4,0.4, "VN", cex=1) + 
    text(0.4, -0.4, "VP", cex=1) + 
    text(0.4,0.4, "FN", cex=1) + 
    text(-0.4, -0.4, "FP", cex=1)


})
}

shinyApp(ui, server)

