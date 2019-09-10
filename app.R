library(shiny)



library(dplyr)
library(zoo)
library(randomForest)
library(groupdata2)

RMSE = function(esperados, observados){
sqrt(mean((esperados - observados)^2))
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

df <- balance(df,size='max', cat_col='cat')


# Define UI for app that draws a histogram ----
ui <- fluidPage(

  # App title ----
  titlePanel("Predicciones del precio futuro del carbón con algoritmos de aprendizaje automatizado"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Slider for the number of bins ----
      sliderInput(inputId = "mes_fut",
                  label = "Meses futuros a predecir",
                  min = 1,
                  max = 50,
                  value = 1)

    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Histogram ----
      plotOutput(outputId = "distPlot")

    )
  )
)

server <- function(input, output) {

  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({

m=input$mes_fut
n=28*m

input<- df[1:(nrow(df)-n),-47]

output<-df$DLI_PESO_A_PAGAR[(n+1):nrow(df)]

df_cruz <- data.frame(output,input)

test= df_cruz[(nrow(df_cruz)-220):(nrow(df_cruz)),]

df_cruz <- df_cruz[1:(nrow(df_cruz)-220),]
set.seed(777)

index <- sample(1:nrow(df_cruz),nrow(df_cruz))

train <- df_cruz[1:floor(nrow(df_cruz)*0.7),]

val <- df_cruz[(floor(nrow(df_cruz)*0.7)+1):nrow(df_cruz),]

rF <- randomForest (train$DLI_PESO_A_PAGAR.x~., data=train, scale=T)

p <- predict(rF, val[,-12])

cor(p,val[,12])
RMSE(p,val[,12])
plot(p,val[,12])

p <- predict(rF, test)
plot(test[,12])
lines(test[,12])
lines(p,col=2)

    })

}

shinyApp(ui,server)
